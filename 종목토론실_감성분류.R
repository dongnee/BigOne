#### View, twitter, 종목토론실 감성 분류
##### 수집된 데이터 파일명
# - View : view_카카오035720_info_df.csv
# - twitter : twitter.csv
# - 종목토론 : discuss_2022_2020_df.csv
# ----------------------------------------------------

# 필요패키지 설치
install.packages("textclean")
install.packages("tidytext")
install.packages("RcppMeCab")
install.packages("purrr")

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
raw_discuss <- read_csv("crawl_data/discuss_2022_2020_df.csv")
View(raw_discuss)
nrow(raw_discuss['제목']) # 221497

## 도배글 찾기 
sum(duplicated(raw_discuss$제목)) # [1] 35588
sum(!duplicated(raw_discuss$제목)) # [1] 185909

# 제목 컬럼, 중복 내용 정리
raw_discuss[!duplicated(raw_discuss$제목),]
raw_discuss2 <- raw_discuss[!duplicated(raw_discuss$제목),]
View(raw_discuss2)
typeof(raw_discuss2) # [1] "list"
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
nrow(raw_discuss3) # 185909
View(raw_discuss3)


### 토큰화 제목 열에 있는 문장을 단어단위로 쪼개는 과정

# 방법1)
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
nrow(word_discuss_done) # [1] 493052


# ------------------------------------------------------
# 감정사전 활용 예시

# 기존 원본 파일 사용 -> 팀 전체가 분류한 단어합본.csv로 사용
# knu_dic <- read_csv("data/knu_SentiWord_Dict.csv")
# Sys.setlocale("LC_ALL", "C") - read_csv 에러 : invalid multibyte string, element 1
knu_dic <- read_csv("data/단어합본.csv")
# Sys.setlocale("LC_ALL", "Korean")
nrow(knu_dic) # 16090
knu_dic <- knu_dic[-1] #첫번째 컬럼은 필요없어서 제외
knu_dic


# 수정 전 사전으로 감정 점수 부여하기
# dplyr::left_join() : 감정사전 word 기준 결합

# NA값은 처리하지 않고 확인
senti_word <- word_discuss_done %>%
  left_join(knu_dic, by = "word") 
  #mutate(polarity = ifelse(is.na(polarity), 0, polarity))

View(senti_word)

# 위 df -> csv로 내보내서 감정단어 사전수정작업은 다른 파일에서 정리
write.csv(senti_word, "data/종목토론실_단어별_감정점수.csv")


# ---------------------------------------------------
# 감정사전 정리된 완성본으로 다시 감정점수 분류

# Sys.setlocale("LC_ALL", "C") # - read_csv 에러 : invalid multibyte string, element 1
knu_dic2 <- read.csv("data/단어합본.csv", encoding="UTF-8")
# Sys.setlocale("LC_ALL", "Korean")
nrow(knu_dic2) # 16090 -> 17941
knu_dic2 <- knu_dic2[-1] #첫번째 컬럼은 필요없어서 제외
head(knu_dic2)


# 사전에 없는단어는 polarity NA, 제거
senti_word2 <- word_discuss_done %>%
  left_join(knu_dic2, by = "word") %>% 
  filter(!is.na(polarity))

nrow(senti_word2) # 270342

# polarity 점수별로 긍정/부정/중립 분류
senti_word3 <- senti_word2 %>% 
  mutate(sentiment = ifelse(polarity >= 1, "pos",
                            ifelse(polarity <= -1, "neg", "neu")))


### 긍정, 부정단어별 가장 빈도가 많은 20개 추출
top_senti <- senti_word3 %>%
  filter(sentiment != "neu") %>%
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n = 20) 
View(top_senti)

# ------> 그래프
ggplot(top_senti, aes(x = reorder(word, n),
                            y = n,
                            fill = sentiment)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(x = NULL) +
  ggtitle("종목토론실 내 가장 빈도가 많은 긍정,부정 단어 20개") +
  theme(title = element_text(size=15, face='bold'))



########## 문장별 감정 점수 구하기

# 감정점수 합계 구한 df 따로 생성
score <- aggregate(polarity~id,senti_word3,sum)
names(score) <- c('id', 'score')
head(score)

# 감정점수 df와 score합계 df - inner_join
senti_score <- merge(senti_word3, score,
                     by="id", all=F) %>% select(제목, score) %>% group_by(제목)

# id별로 합쳐서 합계가 중복해서 들어감 -> 정리
sum(duplicated(senti_score$제목)) # 중복내용 373909개 처리
senti_score <- senti_score[!duplicated(senti_score$제목),]
sum(duplicated(senti_score$제목)) # 중복내용 0개

View(senti_score)

#----------------- 정리된 score로 라벨링
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
  ggtitle('종목토론실 게시글 감정 빈도 비율그래프') +
  theme(title = element_text(size=15, face='bold'))

##################################################################
# 긍정 7.3% / 중립 87.6% / 부정 5.1%
# -> 수정 후 15% / 70.3% / 14.6%
# -> 32.9% / 34.9% / 32.2%
freq_score



#################
# 네이버 종목실 날짜별 게시글수

# 데이터 불러오기
raw_discuss <- read_csv("crawl_data/discuss_2022_2020_df.csv")
View(raw_discuss)
nrow(raw_discuss['제목']) # 221497

## 도배글 찾기 
sum(duplicated(raw_discuss$제목)) # [1] 35588
sum(!duplicated(raw_discuss$제목)) # [1] 185909

# 제목 컬럼, 중복 내용 정리
raw_discuss[!duplicated(raw_discuss$제목),]
raw_discuss2 <- raw_discuss[!duplicated(raw_discuss$제목),]
typeof(raw_discuss2) # [1] "list"
sum(duplicated(raw_discuss2$제목)) # 중복내용 0개
View(raw_discuss2)

# 날짜별로 count
date_discuss <- raw_discuss2 %>% group_by(작성일) %>% count()
View(date_discuss %>% arrange(-n))

# 위치찾기
which(date_discuss$n==1263)
date_discuss[618,]


# 선그래프 - 강조부분 화살표그리기
ggplot(date_discuss, aes(x=작성일, y=n, group=1))+
  geom_line()+ 
  ggtitle('종목토론실 날짜별 작성된 게시글 빈도 수') + # 타이틀
  theme(title = element_text(size=15, face='bold'), # 타이틀 텍스트 설정
        axis.text.x = element_text(angle=45, face='italic'), # x축 라벨이름
        axis.title.x = element_blank(), # x축 이름 삭제
        axis.title.y = element_blank()) + # y축 이름 삭제
  # 화살표 표시
  geom_segment(aes(x =618, 
                   y = 5400, 
                   xend = 618, 
                   yend = 5250), 
               arrow=arrow(ends='last', length = unit(0.2, "cm")), 
               color='orange',size=1.2) +
  geom_segment(aes(x =541, 
                   y = 4700, 
                   xend = 541, 
                   yend = 4550), 
               arrow=arrow(ends='last', length = unit(0.2, "cm")), 
               color='orange',size=1.2)+
  geom_segment(aes(x =471, 
                   y = 3200, 
                   xend = 471, 
                   yend = 3100), 
               arrow=arrow(ends='last', length = unit(0.2, "cm")), 
               color='orange',size=1.2)+
  geom_segment(aes(x =741, 
                   y = 2150, 
                   xend = 741, 
                   yend = 2050), 
               arrow=arrow(ends='last', length = unit(0.2, "cm")), 
               color='orange',size=1.2)+
  geom_segment(aes(x =146, 
                   y = 1800, 
                   xend = 146, 
                   yend = 1700), 
               arrow=arrow(ends='last', length = unit(0.2, "cm")), 
               color='orange',size=1.2) +
  geom_segment(aes(x =191, 
                   y = 2095, 
                   xend = 191, 
                   yend = 1700), 
               arrow=arrow(ends='last', length = unit(0.2, "cm")), 
               color='orange',size=1.2)+
  # 배경색 음영 표시
  annotate("rect", 
           xmin=610, xmax=630, ymin=0, ymax=6000, alpha=0.1, fill="red") +
  # 배경 텍스트 표시
  annotate("text", 
           x=610, y=5500, col="orange",
           fontface = "italic", size=5, label='2021.09.08 ~ 09.14')+
  annotate("text", 
           x=541, y=4800, col="orange",
           fontface = "italic", size=5, label='2021.06.23 ~ 06.25')+
  annotate("text", 
           x=471, y=3300, col="orange",
           fontface = "italic", size=5, label='2021.04.15 ~ 04.16')+
  annotate("text", 
           x=741, y=2250, col="orange",
           fontface = "italic", size=5, label='2022.01.10 ~ 01.19')+
  annotate("text", 
           x=146, y=1900, col="orange",
           fontface = "italic", size=5, label='2020.05.25 ~ 05.27')+
  annotate("text", 
           x=191, y=2200, col="orange",
           fontface = "italic", size=5, label='2020.07.09 ~ 07.10')





