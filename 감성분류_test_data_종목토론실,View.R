#### test데이터 - 마지막 수집일~ 2022.04.27
#### View, twitter, 종목토론실 감성 분류 후 
#### 기존 test데이터와 병합

##### 수집된 데이터 파일명
# - View : view_카카오035720_test_data.csv
# - twitter : ?
# - 종목토론 : 종목토론실_test_data.csv
# ----------------------------------------------------

# 필요패키지 설치
library(dplyr)
library(readr)
library(textclean)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(KoNLP)
library(RcppMeCab)
library(purrr)

setwd('C:/Users/admin/BigOne')
getwd()


# ----------------------------------------------------

##### 1차. 종목토론실 감성분류 진행

# 데이터 불러오기
# Sys.setlocale("LC_ALL", "C")
raw_discuss <- read_csv("crawl_data/종목토론실_test_data.csv")
# Sys.setlocale("LC_ALL", "Korean")
View(raw_discuss)
nrow(raw_discuss['제목']) # 1667
glimpse(raw_discuss)

## 도배글 찾기 
sum(duplicated(raw_discuss$제목)) # [1] 101
sum(!duplicated(raw_discuss$제목)) # [1] 1566

# 제목 컬럼, 중복 내용 정리
raw_discuss[!duplicated(raw_discuss$제목),]
raw_discuss2 <- raw_discuss[!duplicated(raw_discuss$제목),]
View(raw_discuss2)
sum(duplicated(raw_discuss2$제목)) # 중복내용 0개

# ----------------------------------------------------

# 기본적인 전처리
raw_discuss3 <- raw_discuss2 %>%
  mutate(id = row_number(), # id컬럼에 순서대로 행번호 넣기
         제목 = str_squish(replace_html(제목)),
         제목 = str_replace_all(제목, "[^가-힣]", " "),
         제목 = str_replace_all(제목, "[^0-9a-zA-Zㄱ-ㅎㅏ-ㅣ가-힣[:space:]]", " ")) 
# 한글만 남기기
# str_squish : 문자열 공백문자를 단일 스페이스로 취급
# replace_html : HTML 마크업을 대체

# 데이터 구조 확인
glimpse(raw_discuss3)
nrow(raw_discuss3) # 1566
View(raw_discuss3)


### 토큰화 제목 열에 있는 문장을 단어단위로 쪼개는 과정

word_discuss <- raw_discuss3 %>%
  # unnest_tokens : 다루고자하는 텍스트 데이터 객체
  unnest_tokens(input = 제목, # 정돈할 열이름
                output = word, # 정돈된 결과열의 이름
                token = SimplePos09, # "words" -> extractNoun 수정
                drop = F)

# 명사만 추출해서 형태소 정보를 제거함
word_discuss_n <- word_discuss %>% 
  filter(str_detect(word, "/n")) %>% 
  mutate(word = str_remove(word, "/.*$"))

# 형용사도 추출
word_discuss_p <- word_discuss %>% 
  filter(str_detect(word, "/p")) %>%
  mutate(word = str_replace_all(word, "/.*$", "다"))

# 명사만을 추출한 데이터와 형용사만 추출한 데이터 합치기
# 한 글자는 전후 맥락없이 의미를 파악하기 어렵기 때문에 제거
word_discuss_done <- bind_rows(word_discuss_n, word_discuss_p) %>% 
  arrange(id) %>% 
  filter(nchar(word) > 1) %>% 
  select(작성일, 제목, id, word)

View(word_discuss_done)

# 사용된 단어별로 count
word_discuss_done %>% count(word, sort=T) %>% View()
nrow(word_discuss_done) # [1] 3631


# ---------------------------------------------------
# 감정사전 정리된 완성본으로 다시 감정점수 분류

# Sys.setlocale("LC_ALL", "C") # - read_csv 에러 : invalid multibyte string, element 1
knu_dic2 <- read.csv("data/단어합본.csv", encoding="UTF-8")
# Sys.setlocale("LC_ALL", "Korean")
nrow(knu_dic2) # 17941
knu_dic2 <- knu_dic2[-1] #첫번째 컬럼은 필요없어서 제외
head(knu_dic2)


# 사전에 없는단어는 polarity NA, 제거
senti_word2 <- word_discuss_done %>%
  left_join(knu_dic2, by = "word") %>% 
  filter(!is.na(polarity))

nrow(senti_word2) # 1864

# polarity 점수별로 긍정/부정/중립 분류
senti_word3 <- senti_word2 %>% 
  mutate(sentiment = ifelse(polarity >= 1, "pos",
                            ifelse(polarity <= -1, "neg", "neu")))

View(senti_word3)


########## 문장별 감정 점수 구하기

nrow(senti_word3) # 1864
View(senti_word3) # id는 1566까지

# 감정점수 합계 구한 df 따로 생성
score <- aggregate(polarity ~ id, senti_word3, sum)
names(score) <- c('id', 'score')
View(score)
nrow(score) # 1040

# 감정점수 df와 score합계 df - inner_join
senti_score <- merge(senti_word3, score,
                     by="id", all=F) %>% 
  select(`작성일`, `제목`, score) %>% group_by(`제목`)

View(senti_score)

# id별로 합쳐서 합계가 중복해서 들어감 -> 정리
sum(duplicated(senti_score$`제목`)) # 중복내용 831개 처리
senti_score <- senti_score[!duplicated(senti_score$`제목`),]
sum(duplicated(senti_score$`제목`)) # 중복내용 0개
nrow(senti_score) # 1033

View(senti_score)


#----------------- 정리된 score로 라벨링
senti_score <- senti_score %>%
  mutate(sentiment = ifelse(score >= 1, "pos",
                            ifelse(score <= -1, "neg", "neu")))

View(senti_score)
glimpse(senti_score)

# 컬럼 수정
names(senti_score) <- c('date', 'text', 'score', 'senti')

# 날짜컬럼 test데이터와 동일하게 형식 맞추기
senti_score$date <- as.Date(gsub('\\.','-',senti_score$date))
senti_score$date <- as.character(senti_score$date)
glimpse(senti_score)

View(senti_score)

# 기존 데이터의 test데이터 가져오기
test_before <- read.csv("data/test_감정점수합계_종목토론실.csv", encoding="UTF-8")
head(test_before)
test_before <- test_before[-1] # 첫번째 컬럼은 제외
View(test_before)


# test_before 데이터는 train데이터 수집시 같이 수집한 데이터.
# test data로 사용하기위해 위 가공한 senti_score와 행 결합
test_after <- rbind(senti_score, test_before)
View(test_after)


# csv파일로 저장
# write.csv(test_after, "data/test_감정점수합계_종목토론실.csv")



# ----------------------------------------------------

##### 3차. View 감성분류 진행

# 데이터 불러오기
raw_view <- read.csv("crawl_data/view_카카오035720_test_data.csv", encoding="UTF-8")
View(raw_view)
nrow(raw_view['content']) # 80

## NA값이 들어있는 행은 삭제
sum(is.na(raw_view$content)) # 0개
raw_view2 <- raw_view
# ----------------------------------------------------

# 기본적인 전처리
raw_view3 <- raw_view2 %>%
  mutate(id = row_number(), # id컬럼에 순서대로 행번호 넣기
         content = str_squish(replace_html(content)),
         content = str_replace_all(content, "[^가-힣]", " "),
         content = str_replace_all(content, "[^0-9a-zA-Zㄱ-ㅎㅏ-ㅣ가-힣[:space:]]", " ")) 
# 한글만 남기기
# str_squish : 문자열 공백문자를 단일 스페이스로 취급
# replace_html : HTML 마크업을 대체
View(raw_view3)


# 데이터 구조 확인
glimpse(raw_view3)


### 토큰화 제목 열에 있는 문장을 단어단위로 쪼개는 과정

word_view <- raw_view3 %>%
  # unnest_tokens : 다루고자하는 텍스트 데이터 객체
  unnest_tokens(input = content, # 정돈할 열이름
                output = word, # 정돈된 결과열의 이름
                token = SimplePos09, # "words" -> extractNoun 수정
                drop = F)

# 명사만 추출해서 형태소 정보를 제거함
word_view_n <- word_view %>% 
  filter(str_detect(word, "/n")) %>% 
  mutate(word = str_remove(word, "/.*$"))

# 형용사도 추출
word_view_p <- word_view %>% 
  filter(str_detect(word, "/p")) %>%
  mutate(word = str_replace_all(word, "/.*$", "다"))

# 한 글자는 전후 맥락없이 의미를 파악하기 어렵기 때문에 제거
word_view_done <- bind_rows(word_view_n, word_view_p) %>% 
  arrange(id) %>% 
  filter(nchar(word) > 1) %>% 
  select(date, content, id, word)

View(word_view_done)

# 사용된 단어별로 count
word_view_done %>% count(word, sort=T) %>% View()

nrow(word_view_done) # [1] 23845


# ------------------------------------------------------

# 감정사전 정리된 완성본으로 다시 감정점수 분류
# Sys.setlocale("LC_ALL", "C") # - read_csv 에러 : invalid multibyte string, element 1
knu_dic2 <- read.csv("data/단어합본.csv", encoding="UTF-8")
# Sys.setlocale("LC_ALL", "Korean")
nrow(knu_dic2) # 16090 -> 17941
knu_dic2 <- knu_dic2[-1] #첫번째 컬럼은 필요없어서 제외
head(knu_dic2)


# 감정 점수 부여하기
# dplyr::left_join() : 감정사전 word 기준 결합
# 없는단어는 polarity NA -> 0 처리
senti_word <- word_view_done %>%
  left_join(knu_dic2, by = "word") %>% 
  filter(!is.na(polarity))

nrow(senti_word) # 11161

## polarity 점수별로 긍정/부정/중립 분류
senti_word2 <- senti_word %>% 
  mutate(sentiment = ifelse(polarity >= 1, "pos",
                            ifelse(polarity <= -1, "neg", "neu")))


#---------- 콘텐츠별 감정 점수 구하기

View(senti_word2)
nrow(senti_word2) # 11161 / id- 80

# 감정점수 합계 구한 df 따로 생성
score <- aggregate(polarity~id,senti_word2,sum)
names(score) <- c('id', 'score')
head(score)

# 감정점수 df와 score합계 df - inner_join
senti_score <- merge(senti_word2, score,
                     by="id", all=F) %>% 
  select(date, content, score) %>% group_by(content)

View(senti_score)

# id별로 합쳐서 합계가 중복해서 들어감 -> 정리
sum(duplicated(senti_score$content)) # 중복내용 9202개 처리
senti_score <- senti_score[!duplicated(senti_score$content),]
sum(duplicated(senti_score$content)) # 중복내용 0개

View(senti_score)

#------------ 정리된 감정score로 라벨링
senti_score <- senti_score %>%
  mutate(sentiment = ifelse(score >= 1, "pos",
                            ifelse(score <= -1, "neg", "neu")))
View(senti_score)
glimpse(senti_score)

# 컬럼 수정
names(senti_score) <- c('date', 'text', 'score', 'senti')

# 날짜컬럼 test데이터와 동일하게 형식 맞추기
senti_score$date <- as.Date(gsub('\\.','-',senti_score$date))
senti_score$date <- as.character(senti_score$date)
glimpse(senti_score)

# 날짜 컬럼 na값 = 오늘 날짜에 포스팅된 글이라 시간으로 표시되어 na로 처리되었음
senti_score$date <- ifelse(is.na(senti_score$date), "2022-04-27", senti_score$date)
View(senti_score)

# Sys.setlocale("LC_ALL", "C")
# Sys.setlocale("LC_ALL", "Korean")
# 기존 데이터의 test데이터 가져오기
test_before <- read.csv("data/test_감정점수합계_View.csv", encoding="UTF-8")
head(test_before)
test_before <- test_before[-1] # 첫번째 컬럼은 제외
glimpse(test_before)
View(test_before)


# test_before 데이터는 train데이터 수집시 같이 수집한 데이터.
# test data로 사용하기위해 위 가공한 senti_score와 행 결합
test_after <- rbind(senti_score, test_before)
View(test_after)


# csv파일로 저장
# write.csv(test_after, "data/test_감정점수합계_View.csv")
