
getwd()
setwd('프로젝트/데이터') # 저는 C:/RStudy/프로젝트/데이터  폴더에 자료를 넣었습니다#############################
setwd('../')

# 필요패키지
library(dplyr)
library(ggplot2)
library(reshape2)

################################################################################
################################################################################
# 카카오 주식

# 데이터 불러오기

ju <- read.csv('카카오주가.csv')
head(ju)
View(ju)

str(ju)
# Date열은 chr, 나머지는 int나 num
# Date를 날짜형으로 변경
ju$Date <- as.Date(ju$Date, format='%Y-%m-%d')
str(ju)

head(ju)
dim(ju)
# [1] 574   7

ls(ju)
# [1] "Adj.Close" "Close"     "Date"      "High"      "Low"       "Open"      "Volume"   


#-----------------------------------
# 일일종가차이 컬럼 만들기


# 일일종가차이 열 만들기 위해 테스트
ju$Close[3+1]-ju$Close[3]
ju$Close[3+1]
ju$Close[574]

# 맨 첫번째 열은 -200으로 설정
ju$일일종가차이 <- -200 #2019-12-31 종가가 30700원
head(ju)

for(i in 1:573){
  j = ju$Close[i+1]-ju$Close[i] # 첫번째 열은 0이고, 2번째 열부터 값 변경
  ju$일일종가차이[i+1] <- j
  print(j)
}
head(ju)
tail(ju)

#---
# 주가 상승 폭이 가장 높았던 상위 5일
up <- head(arrange(ju, desc(일일종가차이)),5)
up
#---
# 주가 상승 폭이 가장 낮았던 상위 5일
down <- head(arrange(ju, 일일종가차이),5)
down

#-----------------------------------
# 종가차이절대값 컬럼 추가

ju$종가차이절대값 <- abs(ju$일일종가차이)
head(ju)
tail(ju)

#---
# 주가 변동 폭이 가장 큰 상위 5일
updo_abs <- head(arrange(ju, desc(종가차이절대값)),5)
updo_abs


#-----------------------------------
# 고가-저가, 종가-시가 컬럼 추가

ju$고가저가차이 <- ju$High-ju$Low

ju$종가시가차이 <- ju$Close-ju$Open

head(ju)
tail(ju)

#---
# 하루 중 고가저가 차이가 가장 큰 상위 5일
hl <- head(arrange(ju, desc(고가저가차이)),5)
hl

#---
# 종가-시가가 가장 높은 상위 5일(상승)
co_plus <- head(arrange(ju, desc(종가시가차이)),5)
co_plus

#---
# 종가-시가가 가장 낮은 상위 5일(하락)
co_minus <- head(arrange(ju, 종가시가차이),5)
co_minus


################################################################################
################################################################################
# 코스피 주가

# 데이터 불러오기
ko <- read.csv('코스피지수_출처KRX.csv')
# 단위: 원, 주, 거래대금과 상장시가총액은 일백만원
head(ko)

dim(ko)
# 564  10 : 카카오주식과 행의 개수 같음 = 같은 날짜 추출해옴

str(ko)
# 일자는 chr, 그외 num
# 일자를 날짜형으로 변경
ko$일자 <- as.Date(ko$일자, format='%Y-%m-%d')
str(ko)

# 컬럼 이름 변경 : 일자->Date
library(dplyr)

ko <- rename(ko, 'Date'='일자')
head(ko)

# Date 오름차순으로 정렬
ko <- arrange(ko, Date)
head(ko)
tail(ko)

################################################################################
################################################################################
# 코스피주가, 카카오 종가 합치기
head(ko) # 필요한 열은 Date, 종가
head(ju) # 필요한 열은 Date, Close

# 필요데이터만 추출
kojong <- ko %>% select(Date, 종가)
head(kojong)

jujong <- ju %>% select(Date, Close)
head(jujong)

# 데이터프레임을 Date기준으로 합치기
koka_jong <- merge(kojong, jujong, by='Date')
head(koka_jong)

# 둘 다 종가라서 헷갈리니까 구분위해 열이름 변경
koka_jong <- rename(koka_jong, '코스피종가'='종가', '카카오종가'='Close')
head(koka_jong)


################################################################################
################################################################################
# 긍부정 점수 불러오기
com <- read.csv('합친파일_score.csv')
head(com)
tail(com)
str(com)

# date를 날짜형으로 변환
com$date <- as.Date(com$date, format='%Y.%m.%d')
head(com)
View(com)

# score를 날짜별로 그룹화해서 합산하는데, 기사 개수만큼 평균을 냄
comean <- aggregate(score~date,com,mean)
head(comean)
View(comean)

# 뉴스 감정분석
news <- read.csv('뉴스.csv')
str(news) 

# date 날짜형으로 변환
news$date <- as.Date(news$date, format='%Y.%m.%d.')
head(news)


nemean <- aggregate(score~date,news,mean)
head(nemean)

################################################################################
################################################################################
# 크롤링 뉴스 기사
art <- read.csv('총_news_data.csv')
head(art)
View(art)

################################################################################
################################################################################
# 그래프 시각화
# 필요패키지
library(ggplot2)

#-----------------------------------
# 2020년도부터 22년 4월 13일 까지 종가 추이
ls(ju)
head(ju)

ggplot(data=ju, aes(x=Date, y=Close)) +
  geom_line(color='orange') +
  labs(title='2020년 1월 1일~2022년 4월 27일까지 일자별 카카오 종가 추이',
       x='일자',
       y='종가 (원)',
       caption='출처 : 야후파이넨스')

#-----------------------------------
# 카카오 주가가 전날대비 상승폭이 가장 높았던 날짜가 언제인가? (top5)
up
ggplot(data=up, aes(x=reorder(Date,-일일종가차이), y=일일종가차이)) +
  geom_bar(stat='identity', fill='brown') +
  geom_text(aes(label = 일일종가차이), vjust=1.5, color='white') +
  labs(title='전날대비 상승폭이 가장 높은 상위 5일',
       x='일자',
       y='전날 대비 종가 차이 금액 (원)')

head(arrange(comean, desc(score)),30)
head(arrange(nemean, desc(score)),30)
View(arrange(comean, desc(score)))
View(arrange(nemean, desc(score)))

# 해당 날짜의 score 값
comean %>% filter(date=='2021-03-14' | date=='2021-03-14' | date=='2021-06-14' | date=='2021-06-23'| date=='2021-04-15'| date=='2021-05-27'| date=='2021-06-17')
mean(comean$score)

View(art %>% filter(날짜=='2021.03.14.' | 날짜=='2021.06.23.' | 날짜=='2021.04.06.'| 날짜=='2021.04.15.'| 날짜=='2021.06.14.'| 날짜=='2021.06.23.') %>% select('날짜','제목'))


#-----------------------------------
# 카카오 주가가 전날대비 하락폭이 가장 높았던 날짜가 언제인가? (top5)
down
ggplot(data=down, aes(x=reorder(Date,일일종가차이), y=일일종가차이)) +
  geom_bar(stat='identity', fill='navy') +
  geom_text(aes(label = 일일종가차이), vjust=-1.5, color='white') +
  labs(title='전날대비 하락폭이 가장 높은 상위 5일',
       x='일자',
       y='전날 대비 종가 차이 금액 (원)')

head(arrange(comean, score),30)
head(arrange(nemean, (score)),30)

# 해당 날짜의 score 값
comean %>% filter(date=='2021-06-24' | date=='2021-07-21' | date=='2021-09-08' | date=='2021-09-09' | date=='2022-01-05')

View(art %>% filter(날짜=='2021.06.24.' | 날짜=='2021.09.08.' | 날짜=='2021.09.09.') %>% select('날짜','제목'))





#-----------------------------------
# 하루 중 주가(고가-저가) 차이가 가장 큰 상위 5일
hl
ggplot(data=hl, aes(x=reorder(Date,-고가저가차이), 고가저가차이)) +
  geom_bar(stat='identity', fill='brown') +
  geom_text(aes(label = 고가저가차이), vjust=1.5, color='white') +
  labs(title='하루 중 주가 차이가 가장 큰 상위 5일',
       x='일자',
       y='고가-저가 (원)')

#-----------------------------------
# 가장 높은 상승가를 보인 상위 5일
co_plus
ggplot(data=co_plus, aes(x=reorder(Date,-종가시가차이), 종가시가차이)) +
  geom_bar(stat='identity', fill='brown') +
  geom_text(aes(label = 종가시가차이), vjust=1.5, color='white') +
  labs(title='가장 높은 상승가를 보인 상위 5일',
       x='일자',
       y='종가-시가 (원)')


#-----------------------------------
# 가장 높은 하락가를 보인 상위 5일
co_minus
ggplot(data=co_minus, aes(x=reorder(Date,종가시가차이), 종가시가차이)) +
  geom_bar(stat='identity', fill='navy') +
  geom_text(aes(label = 종가시가차이), vjust=-1.5, color='white') +
  labs(title='가장 높은 하락가를 보인 상위 5일',
       x='일자',
       y='종가-시가 (원)')


#-----------------------------------
# 일자별 코스피 지수 종가 추이
ko
ggplot(data=ko, aes(x=Date, y=종가)) +
  geom_line(color='royalblue') +
  labs(title='일자별 코스피 지수 종가 추이',
       x='일자',
       y='종가 (원)',
       caption='출처 : KRX정보데이터시스템')

#-----------------------------------
# 일자별 카카오 주식과 코스피지수의 변화 비교
head(koka_jong)

ggplot(data=koka_jong) +
  geom_line(aes(x=Date, y=코스피종가), color='royalblue') +
  geom_line(aes(x=Date, y=카카오종가), color='orange') +
  labs(title='일자별 카카오와 코스피 종가 추이',
       x='일자',
       y='종가 단위(원)')
# 코스피는 천원대, 카카오는 만~십만원대라 그래프상 비교 불능, 수치를 정규화시켜야 함

#-----------------------------------
# 정규화 함수
nor_sd = function(x){
  result = (x - mean(x)) / sd(x)
  return(result)
}

nor_sd(koka_jong$코스피종가)
nor_sd(koka_jong$카카오종가)

# koka_jong에 열 추가
koka_jong$코스피종가표준화 <- nor_sd(koka_jong$코스피종가)
koka_jong$카카오종가표준화 <- nor_sd(koka_jong$카카오종가)
head(koka_jong)

ggplot(data=koka_jong) +
  geom_line(aes(x=Date, y=코스피종가표준화), color='royalblue') +
  geom_line(aes(x=Date, y=카카오종가표준화), color='orange') +
  labs(title='일자별 코스피 지수와 카카오 종가 추이',
       x='일자',
       y='표준화수치')
# 이렇게하면 범례를 붙일 수 없으니 long형으로 데이터 변환
head(koka_jong)

library(reshape2)
koka_jong_long <- melt(koka_jong,
                       id.vars = 'Date',
                       measure.vars = c('코스피종가표준화','카카오종가표준화'),
                       variable.name = '표준화',
                       value.name = '수치')

head(koka_jong_long)
tail(koka_jong_long)

ggplot(data=koka_jong_long) +
  geom_line(aes(x=Date, y=수치, color=표준화)) +
  labs(title='일자별 코스피 지수와 카카오 종가 추이',
       x='일자',
       y='표준화수치') +
  theme(legend.position ='top') +
  scale_color_manual(values = c('royalblue', 'orange'))

# 그래프상 최저점과 최고점의 구간이 비슷함을 알 수 있음

#-----------------------------------
# 실제 데이터로 최고, 최저점인 날짜를 확인
head(koka_jong)
head(arrange(koka_jong, desc(코스피종가표준화)),15)
# 코스피지수표준화 최고 : 2021-07-06  1.291973
head(arrange(koka_jong, desc(카카오종가표준화)),1)
# 카카오주가표준화 최고 : 2021-06-23  2.201414

head(arrange(koka_jong, 코스피종가표준화),1)
# 코스피지수표준화 최저 : 2020-03-19  -2.627271
head(arrange(koka_jong, 카카오종가표준화),1)
# 카카오주가표준화 최저 : 2020-03-19  -1.727466

g <- ggplot(data=koka_jong_long) +
      geom_line(aes(x=Date, y=수치, color=표준화)) +
      labs(title='일자별 코스피 지수와 카카오 종가 표준화의 추이',
           x='일자',
           y='표준화수치') +
      theme(legend.position ='top') +
      scale_color_manual(values = c('royalblue', 'orange'))

# 최저점, 최고점 표시
g +
  # 최고
  annotate('rect',
           xmin=as.Date('2021-06-15'), xmax= as.Date('2021-07-13'),
           ymin=1.1, ymax=2.3,
           fill='tomato', alpha=0.4) +
  # 최저
  annotate('rect',
           xmin=as.Date('2020-03-05'), xmax= as.Date('2020-03-30'),
           ymin=-2.7, ymax=-1.4,
           fill='purple', alpha=0.3)

#-----------------------------------
# 표준화 최고점 210623
ggplot(data=koka_jong_long) +
  geom_line(aes(x=Date, y=수치, color=표준화)) +
  labs(title='카카오 종가 표준화의 최고점',
       x='일자',
       y='표준화수치') +
  xlim(as.Date('2021-06-15'),as.Date('2021-07-07')) + #날짜 범위 설정
  ylim(0.5,2.5) +
  theme(legend.position ='top') +
  scale_color_manual(values = c('royalblue', 'orange')) +
  # 카카오 최고점
  annotate('segment', 
           x=as.Date('2021-06-27'), xend=as.Date('2021-06-23'),
           y=2.201414, yend=2.201414, color='tomato',
           arrow=arrow()) +
  annotate('text',
           x=as.Date('2021-06-30'), y=2.23, label='카카오 2.201414') +
  # 코스피 최고점
  annotate('segment', 
           x=as.Date('2021-06-27'), xend=as.Date('2021-06-23'),
           y=1, yend=1.291973, color='purple',
           arrow=arrow()) +
  annotate('text',
           x=as.Date('2021-06-30'), y=1, label='코스피 1.354516')
# 카카오 기준 최고전

#-----------------------------------
# 표준화 최저점 200319
ggplot(data=koka_jong_long) +
  geom_line(aes(x=Date, y=수치, color=표준화)) +
  labs(title='코스피, 카카오 종가 표준화의 최저점',
       x='일자',
       y='표준화수치') +
  xlim(as.Date('2020-03-16'),as.Date('2020-03-24')) + #날짜 범위 설정
  ylim(-2.7,-1.5) +
  theme(legend.position ='top') +
  scale_color_manual(values = c('royalblue', 'orange')) +
  # 카카오 최저점
  annotate('segment', 
           x=as.Date('2020-03-20'), xend=as.Date('2020-03-19'),
           y=-1.9, yend=-1.727466, color='tomato',
           arrow=arrow()) +
  annotate('text',
           x=as.Date('2020-03-21'), y=-1.9, label='카카오 -1.727466') +
  # 코스피 최저점
  annotate('segment', 
           x=as.Date('2020-03-20'), xend=as.Date('2020-03-19'),
           y=-2.25, yend=-2.627271, color='purple',
           arrow=arrow()) +
  annotate('text',
           x=as.Date('2020-03-21'), y=-2.2, label='코스피 -2.627271')





# # 코스피
# ggplot(data=ko, aes(x=Date, y=종가)) +
#   geom_line(color='blue') +
#   labs(title='일자별 코스피 지수 종가 추이',
#        x='일자',
#        y='종가 단위(원)')
# 
# # 카카오
# ggplot(data=ju, aes(x=Date, y=Close)) +
#   geom_line(color='orange') +
#   labs(title='2020년 1월 1일~2022년 4월 13일 까지 종가 추이',
#        x='일자',
#        y='종가 단위(원)')

