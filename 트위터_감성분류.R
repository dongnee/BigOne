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

##### 2차. 트위터 감성분류 진행

# 데이터 불러오기
raw_twitter <- read_csv("crawl_data/twitter.csv")
View(raw_twitter)
nrow(raw_twitter['Text']) # 3802

## 도배글 찾기 
sum(duplicated(raw_twitter$Text)) # [1] 74
sum(!duplicated(raw_twitter$Text)) # [1] 3728

# 제목 컬럼, 중복 내용 정리 안하고
raw_twitter[!duplicated(raw_twitter$Text),]
raw_twitter2 <- raw_twitter[!duplicated(raw_twitter$Text),]
View(raw_twitter2)
typeof(raw_twitter2) # [1] "list"
sum(duplicated(raw_twitter2$Text)) # 중복내용 0개

raw_twitter2 <- raw_twitter

# ----------------------------------------------------

# 기본적인 전처리
raw_twitter3 <- raw_twitter2 %>%
  mutate(id = row_number(), # id컬럼에 순서대로 행번호 넣기
         Text = str_squish(replace_html(Text)),
         Text = str_replace_all(Text, "[^가-힣]", " "),
         Text = str_replace_all(Text, "[^0-9a-zA-Zㄱ-ㅎㅏ-ㅣ가-힣[:space:]]", " ")) 
# 한글만 남기기
# str_squish : 문자열 공백문자를 단일 스페이스로 취급
# replace_html : HTML 마크업을 대체
View(raw_twitter3)

### 중복처리 x
sum(duplicated(raw_twitter3$Text)) # 중복내용 78개 처리
raw_twitter3 <- raw_twitter3[!duplicated(raw_twitter3$Text),]
sum(duplicated(raw_twitter3$Text)) # 중복내용 0개
View(raw_twitter3) # 행 3,650개

# 데이터 구조 확인
glimpse(raw_twitter3)


### 토큰화 제목 열에 있는 문장을 단어단위로 쪼개는 과정

# 방법1)
word_twitter <- raw_twitter3 %>%
  # unnest_tokens : 다루고자하는 텍스트 데이터 객체
  unnest_tokens(input = Text, # 정돈할 열이름
                output = word, # 정돈된 결과열의 이름
                token = SimplePos09, # "words" -> extractNoun 수정
                drop = F)

# 명사만 추출해서 형태소 정보를 제거함
word_twitter_n <- word_twitter %>% 
  filter(str_detect(word, "/n")) %>% 
  mutate(word = str_remove(word, "/.*$"))

# 형용사도 추출
word_twitter_p <- word_twitter %>% 
  filter(str_detect(word, "/p")) %>%
  mutate(word = str_replace_all(word, "/.*$", "다"))

# 한 글자는 전후 맥락없이 의미를 파악하기 어렵기 때문에 제거
word_twitter_done <- bind_rows(word_twitter_n, word_twitter_p) %>% 
  arrange(id) %>% 
  filter(nchar(word) > 1) %>% 
  select(Datetime, Text, id, word)

View(word_twitter_done)

# 사용된 단어별로 count
word_twitter_done %>% count(word, sort=T) %>% View()

nrow(word_twitter_done) # [1] 65882


# ------------------------------------------------------

# 종목토론실에서 사용했던 감정사전에서 수정된 2번째 사전 활용

knu_dic <- read_csv("data/knu_SentiWord_Dict_2.csv")
knu_dic <- knu_dic[-1] #첫번째 컬럼은 필요없어서 제외
knu_dic

# ------------------------------------------------------
# 감정 점수 부여하기
# dplyr::left_join() : 감정사전 word 기준 결합
# 없는단어는 polarity NA -> 0 처리
senti_word <- word_twitter_done %>%
  left_join(knu_dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

View(senti_word)

##########################  감정사전 수정 ########################## 
# 단어가 포함된 글 확인
senti_word %>%
  filter(str_detect(word, "네이버")) %>% View()
# --> 네이버 기업이 포함된 글이 많아서 여기서는 감정보다는 중립으로 분류

senti_word %>%
  filter(str_detect(word, "않다")) %>% View()
# --> 부정어로 분류

senti_word %>%
  filter(str_detect(word, "깨다")) %>% View()
# --> 부정어로 분류

senti_word %>%
  filter(str_detect(word, "오르")) %>% View()
# --> 긍정어로 분류

senti_word %>%
  filter(str_detect(word, "모르다")) %>% View()
# --> 부정어로 분류

senti_word %>%
  filter(str_detect(word, "카카오페이")) %>% View()
# --> 기업정보 글이 많아서 여기서는 감정보다는 중립으로 분류

senti_word %>%
  filter(str_detect(word, "카카오뱅크")) %>% View()
# --> 기업정보 글이 많아서 여기서는 감정보다는 중립으로 분류


# ----------------------------------------------------

# 감정사전에 해당 단어 확인
knu_dic %>% filter(word %in% c("네이버")) 
knu_dic %>% filter(word %in% c("않다")) # 사전에없음
knu_dic %>% filter(word %in% c("깨다")) # 사전에없음
knu_dic %>% filter(word %in% c("오르")) # 사전에없음
knu_dic %>% filter(word %in% c("모르다")) # 사전에없음
knu_dic %>% filter(word %in% c("카카오페이")) 
knu_dic %>% filter(word %in% c("카카오뱅크")) 



# 새로운 감정사전에 수정
# 해당 단어가 사전에 포함되지않아서 추가
knu_dic2 <- rbind(knu_dic, data.frame(word=c("않다","깨다","오르","모르다"), 
                                      polarity=c(-1,-1,1,-1)))

# 사전에 있는 단어 감정 점수 수정
knu_dic2 <- knu_dic2 %>%
  mutate(polarity = ifelse(word %in% c("네이버"), 0, polarity))
knu_dic2 <- knu_dic2 %>%
  mutate(polarity = ifelse(word %in% c("카카오페이"), 0, polarity))
knu_dic2 <- knu_dic2 %>%
  mutate(polarity = ifelse(word %in% c("카카오뱅크"), 0, polarity))

# ----------------------------------------------------
# 확인
tail(knu_dic2,20)

write.csv(knu_dic2, "data/")


######################## 다시 감정점수 분류
senti_word <- word_twitter_done %>%
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
  ggtitle("트위터에서 가장 빈도가 많은 긍정,부정 단어 20개") +
  theme(title = element_text(size=15, face='bold'))



########## 댓글별 감정 점수 구하기

# 감정점수 합계 구한 df 따로 생성
score <- aggregate(polarity~id,senti_word2,sum)
names(score) <- c('id', 'score')
head(score)

# 감정점수 df와 score합계 df - inner_join
senti_score <- merge(senti_word2, score,
                     by="id", all=F) %>% select(Text, score) %>% group_by(Text)

# id별로 합쳐서 합계가 중복해서 들어감 -> 정리
sum(duplicated(senti_score$Text)) # 중복내용 62247개 처리
senti_score <- senti_score[!duplicated(senti_score$Text),]
sum(duplicated(senti_score$Text)) # 중복내용 0개

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
  ggtitle('트위터 게시글 감정 빈도 비율그래프') +
  theme(title = element_text(size=15, face='bold'))

##################################################################
# 긍정 36.6% / 중립 35.8% / 부정 27.6%
# -> 수정 후 33.5% / 40.6% / 25.9%
freq_score



#################
# 트위터 날짜별 게시글수

# 데이터 불러오기
raw_twitter <- read_csv("crawl_data/twitter.csv")
View(raw_twitter)
nrow(raw_twitter['Text']) # 3802


# 날짜별로 count
# 일별로 하니까 평균 1, 최대 2개는 4일정도밖에 안나타남
# 주별로 묶어서 그래프 시각화
date_twitter <- raw_twitter %>% group_by(Datetime) %>% count()
View(date_twitter %>% arrange(-n))

# 데이터형 확인
glimpse(date_twitter)
date_twitter$Datetime <- as.Date(date_twitter$Datetime)

# 날짜의 월만 추출하기위한 패키지
library(lubridate) 
# 월별 쪼개기
date_twitter2 <- cbind(date_twitter, month=month(date_twitter$Datetime))
date_twitter2
# 주별쪼개기 
date_twitter2$week <- cut(date_twitter2$Datetime, breaks="week")
date_twitter2

# 주별 작성된 게시글 수 카운트
date_twitter3 <- date_twitter2 %>% group_by(week) %>% count()
View(date_twitter3)

# 위치찾기
which(date_twitter3$n==178)
date_twitter3[88:95,]


# 선그래프 - 강조부분 화살표그리기
ggplot(date_twitter3, aes(x=week, y=n, group=1))+
  geom_line() + geom_point() +
  ggtitle('트위터 날짜별 작성된 게시글 빈도 수') + # 타이틀
  theme(title = element_text(size=15, face='bold'), # 타이틀 텍스트 설정
        axis.text.x = element_text(angle=80, face='italic'), # x축 라벨이름
        axis.title.x = element_blank(), # x축 이름 삭제
        axis.title.y = element_blank()) +  # y축 이름 삭제 
  # 화살표 표시
  geom_segment(aes(x =90, 
                   y = 185, 
                   xend = 90, 
                   yend = 180), 
               arrow=arrow(ends='last', length = unit(0.2, "cm")), 
               color='orange',size=1.2)+
  # 배경색 음영 표시
  annotate("rect", 
           xmin=89, xmax=92.5, ymin=0, ymax=180, alpha=0.1, fill="red") +
  # 배경 텍스트 표시
  annotate("text", 
           x=89, y=188, col="orange",
           fontface = "italic", size=5, label='2021-09-06 ~ 09-27 week')





