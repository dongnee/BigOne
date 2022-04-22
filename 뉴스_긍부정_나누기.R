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

raw_news_token$분류 <- ifelse(raw_news_token$스코어 >= 1,'긍정',
                          ifelse(raw_news_token$스코어 == 0,'중립','부정'))

View(raw_news_token)


# ------------------------------------------------------
# 긍정/부정/중립 그래프로 비교

freq_score <- raw_news_token %>%
  group_by(분류) %>% 
  count(분류)

freq_score

# 비율로 계산
ratio <- (freq_score$n/sum(freq_score$n))*100

freq_score$ratio <- ratio

freq_score


# 누적 그래프 만들기
colorchip <- c("#d1495b", "#edae49", "#66a182")

freq_score$dummy <- 0
freq_score

ggplot(freq_score, aes(x = dummy, y = ratio, fill = 분류)) +
  geom_col() +
  geom_text(aes(label = paste0(round(ratio, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme(axis.title.x = element_blank(), # x축 이름 삭제
        axis.text.x = element_blank(), # x축 값 삭제
        axis.ticks.x = element_blank(), # x축 눈금 삭제
        axis.title.y = element_blank())+ 
  ggtitle('뉴스 데이터 긍정/부정/중립 비율') +
  theme(title = element_text(size=15, face='bold')) +
  scale_fill_manual(values=colorchip)












