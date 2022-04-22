###### 네이버 뉴스 원문 그대로 감성분석 ######

library(dplyr)
library(readr)
library(textclean)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)

getwd()
setwd('C:/RStudy/data')

getwd()


# ------------------------------------------------------
# jupyter notebook에서 전처리 시킨 후 토큰화한 데이터 불러오기
raw_news_token <- read_csv("news_data_최종.csv")
View(raw_news_token)


# ------------------------------------------------------
# 사전 불러오기
pos_dict <- read_csv('positive_words.txt', headers=False)
View(pos_dict)

neg_dict <- read_csv('negative_words.txt')
View(neg_dict)

zero_dict <- read_csv('zero_words.txt')
View(zero_dict)

class(zero_dict)

# ------------------------------------------------------
# 각 사전별 가중치 붙이기
# 긍정은 +1, 부정은 -1, 중립은 0
nrow(pos_dict)

pos_dict$가충치 <- c(rep(1,349))
View(pos_dict)

nrow(neg_dict)

neg_dict$가충치 <- c(rep(-1,372))
View(neg_dict)

nrow(zero_dict)

zero_dict$가충치 <- c(rep(0,130))
View(zero_dict)


# ------------------------------------------------------
# 부여된 감정 점수 가중치 스코어 계산
# 긍정/(긍정+부정)
# --> 계산해서 새로운 열로 저장


raw_news_token$스코어 <- 
  raw_news_token$긍정가중치+raw_news_token$부정가중치

View(raw_news_token)

# ------------------------------------------------------
# 분류 작업 테스트
raw_news_token$분류 <-
  if (raw_news_token$스코어 <= 0.48) {
    '긍정'
  } else if (raw_news_token$스코어 <= 0.52) {
    '중립'
  } else {
    '부정'
  }

View(raw_news_token)



