#### View, twitter, 종목토론실 감성 분류
##### 수집된 데이터 파일명
# - View : view_카카오035720_info_df.csv
# - twitter : twitter.csv
# - 종목토론 : discuss_2022_2020_df.csv
# ----------------------------------------------------

# 필요패키지 설치
library(dplyr)
library(readr)
install.packages("textclean")
library(textclean)
library(stringr)
install.packages("tidytext")
library(tidytext)
library(ggplot2)
library(tidyr)


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
sum(duplicated(raw_discuss2$제목)) # 0

# ----------------------------------------------------

# 기본적인 전처리
raw_discuss3 <- raw_discuss2 %>%
  mutate(id = row_number(), # id컬럼에 순서대로 행번호 넣기
         제목 = str_squish(replace_html(제목)),
         제목 = str_replace_all(제목, "[^가-힣]", " ")) # 한글만 남기기
          # str_squish : 문자열 공백문자를 단일 스페이스로 취급
          # replace_html : HTML 마크업을 대체

View(raw_discuss3)

# 데이터 구조 확인
glimpse(raw_discuss3)

# 토큰화
word_discuss <- raw_discuss3 %>%
  # unnest_tokens : 다루고자하는 텍스트 데이터 객체
  unnest_tokens(input = 제목, # 목표 텍스트 열
                output = word, # 결과열의 이름
                token = "words",
                drop = F)

word_discuss %>%
  select(word, 제목)

View(word_discuss)

# ------------------------------------------------------

# 감정사전 활용

knu_dic <- read_csv("data/knu_SentiWord_Dict.csv")
knu_dic

# 긍정단어 정렬
knu_dic %>% 
  filter(polarity == 2) %>% 
  arrange(word)

# 부정 단어 정렬
knu_dic %>% 
  filter(polarity == -2) %>% 
  arrange(word)

# 총 단어, 긍정/부정/중립 카운트
knu_dic %>% 
  mutate(sentiment = ifelse(polarity >= 1, "pos",
                            ifelse(polarity <= -1, "neg", "neu"))) %>%
  count(sentiment)


# ------------------------------------------------------
# 감정 점수 부여하기
# dplyr::left_join() : knu_dic word 기준 결합
# 없는단어는 polarity NA -> 0 처리
word_discuss <- word_discuss %>%
  left_join(knu_dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

word_discuss %>%
  select(word, polarity)

View(word_discuss)

# 사용된 감정 단어 살펴보기

# 1. 감정 분류
## polarity 점수별로 긍정/부정/중립 분류
word_discuss <- word_discuss %>% 
  mutate(sentiment = ifelse(polarity >= 1, "pos",
                            ifelse(polarity <= -1, "neg", "neu")))
  

word_discuss %>% count(sentiment)

# 2. 막대그래프 만들기
top10_sentiment <- word_discuss %>%
  filter(sentiment != "neu") %>%
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10)
top10_sentiment

# 그래프 그리기
ggplot(top10_sentiment, aes(x = reorder(word, n),
                            y = n,
                            fill = sentiment)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(x = NULL) +
  theme(text = element_text(family = "NanumBarunGothicBold"))


# ------------------------------------------------------
# 댓글별 감정 점수 구하기
score_discuss <- word_discuss %>% 
  group_by(id, 제목) %>% 
  summarise(score = sum(polarity)) %>% ungroup()

# score_discuss id 로 merge , word_discuss id

score_discuss

# 2. 감정 점수 높은순으로 정렬
### 긍정
score_discuss %>% select(score, 제목) %>% arrange(-score)
### 부정
score_discuss %>% select(score, 제목) %>% arrange(score)
### 감정점수 빈도 구하기
score_discuss %>% count(score)


# 3. 감정 분류하고 막대그래프 만들기
## polarity 점수별로 긍정/부정/중립 분류
score_discuss <- score_discuss %>%
  mutate(sentiment = ifelse(score >= 1, "pos",
                            ifelse(score <= -1, "neg", "neu")))

# 감정 빈도와 비율 구하기
frequency_score <- score_discuss %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)

frequency_score

# 막대 그래프 만들기
ggplot(frequency_score, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3) +
  scale_x_discrete(limits = c("pos", "neu", "neg"))

# 3. 비율 누적 그래프 만들기
frequency_score$dummy <- 0
frequency_score

ggplot(frequency_score, aes(x = dummy, y = ratio, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = paste0(round(ratio, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_blank(), # x축 이름 삭제
        axis.text.x = element_blank(), # x축 값 삭제
        axis.ticks.x = element_blank()) # x축 눈금 삭제

##################################################################
# 중립에 해당하는 글이 94.1%가 된다.
score_discuss

#####################################################
# 감정별 주요 단어
# 1. 토큰화하고 두글자 이상 단어만 남기기

discuss <- word_discuss %>%
  unnest_tokens(input = 제목, # 단어 기준 토큰화
                output = word2,
                token = "words",
                drop = F) %>%
  filter(str_detect(word2, "[가-힣]") & # 한글 추출
           str_count(word2) >=2 ) # 두글자 이상 추출

# 감정 및 단어별 빈도 구하기
frequency_word <- discuss %>%
  filter(str_count(word2) >= 2) %>%
  count(sentiment, word2, sort = T)
frequency_word

# 긍정 고빈도
frequency_word %>%
  filter(sentiment == "pos")

# 부정 고빈도
frequency_word %>%
  filter(sentiment == "neg")

# 1. 로즈 오즈비 구하기
# wide form으로 변환

discuss_wide <- frequency_word %>%
  filter(sentiment != "neu") %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n = 0))
discuss_wide

# 로그비 구하기
discuss_wide <- discuss_wide %>%
  mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                                ((neg + 1) / (sum(neg + 1)))))
discuss_wide

# 2. 로그 오즈비가 가장 큰 단어 10개씩 추출
top10 <- discuss_wide %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)
top10

# 3. 막대 그래프 만들기
ggplot(top10, aes(x = reorder(word2, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme(text = element_text(family = "NanumBarunGothicBold"))

#################################################################
####  감정사전 수정

# 단어가 포함된 글 확인
score_discuss %>%
  filter(str_detect(제목, "대단하다")) %>%
  select(제목, id, score) %>% View()
# 대단하다 .. 부정어로 사용하는거같음

# 감정사전에 해당 단어 확인
knu_dic %>% filter(word %in% c("대단하다"))

# 새로운 감정사전에 수정
# 대단하다 점수 +2 -> -2 로 수정
new_dic <- knu_dic %>%
  mutate(polarity = ifelse(word %in% c("대단하다"), -2, polarity))

new_dic %>% filter(word %in% c("대단하다"))

# 수정한 사전으로 감정 점수 부여하기
new_word_discuss <- word_discuss %>%
  select(-polarity) %>%
  left_join(new_dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

# 댓글별 감정 점수 구하기
new_score_discuss <- new_word_discuss %>%
  group_by(id, 제목) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()

## polarity 점수별로 긍정/부정/중립 분류
new_score_discuss <- new_score_discuss %>%
  mutate(sentiment = ifelse(score >= 1, "pos",
                            ifelse(score <= -1, "neg", "neu")))

# 감정 빈도와 비율 구하기
frequency_score2 <- new_score_discuss %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)

# 3. 비율 누적 그래프 만들기
frequency_score2$dummy <- 0
frequency_score2

ggplot(frequency_score2, aes(x = dummy, y = ratio, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = paste0(round(ratio, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_blank(), # x축 이름 삭제
        axis.text.x = element_blank(), # x축 값 삭제
        axis.ticks.x = element_blank()) # x축 눈금 삭제


##### 감정별 주요 단어
# 1. 토큰화하고 두글자 이상 단어만 남기기

discuss2 <- new_word_discuss %>%
  unnest_tokens(input = 제목, # 단어 기준 토큰화
                output = word2,
                token = "words",
                drop = F) %>%
  filter(str_detect(word2, "[가-힣]") & # 한글 추출
           str_count(word2) >=2 ) # 두글자 이상 추출

# 감정 및 단어별 빈도 구하기
frequency_word2 <- discuss2 %>%
  filter(str_count(word2) >= 2) %>%
  count(sentiment, word2, sort = T)
frequency_word2

# 긍정 고빈도
frequency_word2 %>%
  filter(sentiment == "pos")

# 부정 고빈도
frequency_word2 %>%
  filter(sentiment == "neg")

# 1. 로즈 오즈비 구하기
# wide form으로 변환

discuss_wide2 <- frequency_word2 %>%
  filter(sentiment != "neu") %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n = 0))
discuss_wide2

# 로그비 구하기
discuss_wide2 <- discuss_wide2 %>%
  mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                                ((neg + 1) / (sum(neg + 1)))))
discuss_wide2

# 2. 로그 오즈비가 가장 큰 단어 10개씩 추출
top10_2 <- discuss_wide2 %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)
top10_2

# 3. 막대 그래프 만들기
ggplot(top10_2, aes(x = reorder(word2, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme(text = element_text(family = "NanumBarunGothicBold"))

new_score_discuss %>%
  filter(str_detect(제목, "대단하다")) %>%
  select(제목, id, score) %>% View()



