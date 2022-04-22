#### View, twitter, 종목토론실 감성 분류
##### 수집된 데이터 파일명
# - View : view_카카오035720_info_df.csv
# - twitter : twitter.csv
# - 종목토론 : discuss_2022_2020_df.csv
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

##### 3차. View 감성분류 진행

# 데이터 불러오기
raw_view <- read_csv("crawl_data/view_카카오035720_info_df.csv")
View(raw_view)
nrow(raw_view['content']) # 92

## NA값이 들어있는 행은 삭제
sum(is.na(raw_view$content)) # 4개
raw_view2 <- raw_view[!is.na(raw_view$content),]
sum(is.na(raw_view2$content)) # 0개

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

# 방법1)
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

nrow(word_view_done) # [1] 17543


# ------------------------------------------------------

# 수정된 감정사전 활용

knu_dic <- read_csv("data/knu_SentiWord_Dict_2.csv")
knu_dic <- knu_dic[-1] #첫번째 컬럼은 필요없어서 제외
knu_dic

# ------------------------------------------------------
# 감정 점수 부여하기
# dplyr::left_join() : 감정사전 word 기준 결합
# 없는단어는 polarity NA -> 0 처리
senti_word <- word_view_done %>%
  left_join(knu_dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

View(senti_word)

##########################  감정사전 수정 ########################## 
# 단어가 포함된 글 확인
senti_word %>%
  filter(str_detect(word, "성장")) %>% View()
# --> 긍정어

senti_word %>%
  filter(str_detect(word, "증가")) %>% View()
# --> 긍정어

senti_word %>%
  filter(str_detect(word, "늘다")) %>% View()
# --> 긍정어


# ----------------------------------------------------

# 감정사전에 해당 단어 확인
knu_dic %>% filter(word %in% c("성장")) # 사전에없음
knu_dic %>% filter(word %in% c("증가")) # 사전에없음
knu_dic %>% filter(word %in% c("늘다")) # 사전에없음

# 새로운 감정사전에 수정
# 해당 단어가 사전에 포함되지않아서 추가
knu_dic2 <- rbind(knu_dic, data.frame(word=c("성장","증가","늘다"), 
                                      polarity=c(+1,+1,+1)))

# 사전에 있는 단어 감정 점수 수정
knu_dic2 <- knu_dic2 %>%
  mutate(polarity = ifelse(word %in% c(""), 0, polarity))

# ----------------------------------------------------
# 확인 후 수정된 사전 다시 csv로 저장
tail(knu_dic2,20)

write.csv(knu_dic2, "data/knu_SentiWord_Dict_2.csv")


######################## 다시 감정점수 분류
senti_word <- word_view_done %>%
  left_join(knu_dic2, by = "word") %>% 
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

## polarity 점수별로 긍정/부정/중립 분류
senti_word2 <- senti_word %>% 
  mutate(sentiment = ifelse(polarity >= 1, "pos",
                            ifelse(polarity <= -1, "neg", "neu")))


### 긍정, 부정단어별 가장 빈도가 많은 20개 추출
top10_senti <- senti_word2 %>%
  filter(sentiment != "neu") %>%
  count(sentiment, word) %>% filter(n > 1) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n = 20) 
View(top10_senti)

# ------> 그래프
ggplot(top10_senti, aes(x = reorder(word, n),
                        y = n,
                        fill = sentiment)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(x = NULL) +
  ggtitle("View 검색시 가장 빈도가 많은 긍정,부정 단어 20개") +
  theme(title = element_text(size=15, face='bold'))



########## 콘텐츠별 감정 점수 구하기

# 감정점수 합계 구한 df 따로 생성
score <- aggregate(polarity~id,senti_word2,sum)
names(score) <- c('id', 'score')
head(score)

# 감정점수 df와 score합계 df - inner_join
senti_score <- merge(senti_word2, score,
                     by="id", all=F) %>% select(content, score) %>% group_by(content)

# id별로 합쳐서 합계가 중복해서 들어감 -> 정리
sum(duplicated(senti_score$content)) # 중복내용 62247개 처리
senti_score <- senti_score[!duplicated(senti_score$content),]
sum(duplicated(senti_score$content)) # 중복내용 0개

View(senti_score)

########### 정리된 감정score로 라벨링
senti_score <- senti_score %>%
  mutate(sentiment = ifelse(score >= 1, "pos",
                            ifelse(score <= -1, "neg", "neu")))

# 감정 점수 높은순으로 정렬
### 긍정
senti_score %>% select(score, 제목) %>% arrange(-score)
### 부정
senti_score %>% select(score, 제목) %>% arrange(score)

count(senti_score)

# 감정별로 그룹화하여 개수세기
freq_score <- senti_score %>%
  group_by(sentiment) %>% 
  count(sentiment)

# 감정별 개수를 비율로 계산
ratio <- (freq_score$n/sum(freq_score$n))*100

freq_score$ratio <- ratio

freq_score


# --> 비율 누적 그래프 만들기
freq_score$dummy <- 0
freq_score

ggplot(freq_score, aes(x = dummy, y = ratio, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = paste0(round(ratio, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_blank(), # x축 이름 삭제
        axis.text.x = element_blank(), # x축 값 삭제
        axis.ticks.x = element_blank(), # x축 눈금 삭제
        axis.title.y = element_blank())+ 
  ggtitle('View 검색 게시글 감정 빈도 비율그래프') +
  theme(title = element_text(size=15, face='bold'))

##################################################################
# 긍정 73.9% / 중립 10.2% / 부정 15.9%
freq_score



#################
# view 날짜별 게시글수

# 데이터 불러오기
raw_view <- read_csv("crawl_data/view_카카오035720_info_df.csv")
View(raw_view)
nrow(raw_view['content']) # 92

## NA값이 들어있는 행은 삭제
sum(is.na(raw_view$content)) # 4개
raw_view2 <- raw_view[!is.na(raw_view$content),]
sum(is.na(raw_view2$content)) # 0개

# 날짜별로 count
date_view <- raw_view2 %>% group_by(date) %>% count()
View(date_view %>% arrange(-n))

# 데이터형 확인
glimpse(date_view)

# chr 형 -> 날짜형으로 변경해야함
# 이건 일단 보류 ㅠㅠ 
# 검색 시 게시글 수가 많지가 않아 일별로 1~2개 사이 



