
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
ju <- read.csv('카카오주가_출처야후파이넨스.csv')
head(ju)
View(ju)

str(ju)
# Date열은 chr, 나머지는 int나 num
# Date를 날짜형으로 변경
ju$Date <- as.Date(ju$Date, format='%Y-%m-%d')
str(ju)

head(ju)
dim(ju)
# [1] 564   7

ls(ju)
# [1] "Adj.Close" "Close"     "Date"      "High"      "Low"       "Open"      "Volume"   


#-----------------------------------
# 일일종가차이 컬럼 만들기


# 일일종가차이 열 만들기 위해 테스트
ju$Close[3+1]-ju$Close[3]
ju$Close[3+1]
ju$Close[564]

# 맨 첫번째 열은 -200으로 설정
ju$일일종가차이 <- -200 #2019-12-31 종가가 30700원
head(ju)

for(i in 1:563){
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
ko <- read.csv('코스피지수.csv')
# 단위: 원, 주, 거래대금과 상장시가총액은 일백만원
head(ko)

dim(ko)
# 564  10 : 카카오주식과 행의 개수 같음 = 같은 날짜 추출해옴

str(ko)
# 일자는 chr, 그외 num
# 일자를 날짜형으로 변경
ko$일자 <- as.Date(ko$일자, format='%Y/%m/%d')
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
# 그래프 시각화
# 필요패키지
library(ggplot2)

#-----------------------------------
# 2020년도부터 22년 4월 13일 까지 종가 추이
ls(ju)
head(ju)

ggplot(data=ju, aes(x=Date, y=Close)) +
  geom_line(color='orange') +
  labs(title='2020년 1월 1일~2022년 4월 13일까지 일자별 카카오 종가 추이',
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
#
# 2022-03-14
# 김범수 이사회 의장이 사임하고 해외 진출에 주력
# - 카카오톡 쇼핑하기, 기프티카드 부정거래 단속
# - 카카오뱅크, 1주택자 전월세보증금대출 재개
# - 카카오뱅크, 삼성증권 주식계좌 추가
# 
# 2021-06-23
# 카카오-카카오커머스 합병
# e커머스(선물하기, 쇼핑하기, 쇼핑라이브) 시장에서 경쟁력 강화
# 카카오톡 플랫폼 기반 커머스 사업을 맡고 있는 카카오커머스의 발행주식을 100% 취득하기로 결의했다고 전일공시 함
# - 8월 구독형 콘텐츠 플랫폼 출시
# - 시가총액 3위에 오름
# 
# 2021-04-15
# 주식 액면 분할후 첫 거래일임
# -액면분할 카카오 신주 첫날 7% 급등
# - 지그재그앱 인수
# - 웹소설업체, 웹툰 인수
#
# 2021-04-06
# 카카오뱅크 올해 하반기에 기업공개절차(IPO)를 준비(회사가 상장하기 위해서 심사 받고 주주를 모집하기 위해 기업의 중요정보를 공개하는 절차를 IPO라 함)
# 모빌리티 투자 유치 긍정적
# 카카오페이,뱅크 상장 기대감
# 미국 웹소설 플랫폼인 '래디쉬' 인수추진
# 카카오재팬(일본 내 웹툰 시장 점유율 1위 플랫폼인 '픽코마'운영중)은 신규 투자 유치를 두고 PEF운용사 앵커에쿼티파트너스(앵커PE)와 협상을 진행 중, 최대 5천억원 가량 유치 계획
# - 카카오, 2027년까지 금융규제 유예받음
# - 카카오모빌리티, 구글과 손잡음
# - 스타트업
# - 무료 웹소설 연제 사이트 론칭 예정
# - 카카오뱅크, IPO 앞둠
# - 카카오, 유망 스타트업을 인수해 정보기술(IT) 서비스 경쟁력을 높이고 있음

# 2021-06-14
# 카카오-카카오커머스 재합병 - 첫 기사 난 일자
# - 카카오커머스 3년만에 재합병
# - 카카오엔터, 한국문학번역원과 MOU
# - 카카오뱅크, 8월에 상장 절차
# - 카카오 '구독ON' 시작
# - 카카오김범수, 재산 절반 기부
# - 1년만에 400만계좌
# - 카카오모빌리티, 테슬라 모델X 택시 선보임

#-----------------------------------
# 카카오 주가가 전날대비 하락폭이 가장 높았던 날짜가 언제인가? (top5)
down
ggplot(data=down, aes(x=reorder(Date,일일종가차이), y=일일종가차이)) +
  geom_bar(stat='identity', fill='navy') +
  geom_text(aes(label = 일일종가차이), vjust=-1.5, color='white') +
  labs(title='전날대비 하락폭이 가장 높은 상위 5일',
       x='일자',
       y='전날 대비 종가 차이 금액 (원)')
#
# 2021-09-08
# 여당, 인터넷플랫폼 기업의 시장 독점 문제를 지적하며 규제 강화 의지를 밝힘
# 금융상품 중개를 하는 금융플랫폼은 금소법(금융소비자보호법) 대상이 되기 때문에 금융사처럼 금융위에 등록하거나 인허가를 받아야하지만, 카카오페이는 인허가 없이 상품을 판매하기 때문에 법령을 위반한 셈이 된다. 
# 카카오뱅크, 마이너스통장 최대 한도 2천만원씩 더 줄임
# - 공매도 폭탄
# - 온라인 플랫폼 공정화법 제정
# - 사업구조 대전환
# - 카카오뱅크, 중저신용자 첫 달 이자 지원 내달 9일까지 연장
# 
# 2021-06-24
# 고평가 우려(전날 고가에 장 마감함)
# 주가수익비율(PER)은 228.53배(동일업종은 8.98배) PER은 값이 높을수록 회사가 벌어들이는 이익금에 비해 주가가 고평가 됐다는 의미
# - 카카오모빌리티, 셔틀 등 버스 기반 신사업 협력
# - 카카오, 네이버와 시가총액 3위 다툼
# - 카카오뱅크, 신용 높아도 금리 올림
# 
# 2021-09-09
# (어제의 연속)
# - 공정위, 카카오에 규제
# - 카카오페이 상장 또 미뤄짐
# - 카카오엔터, 멜론과 합병 완료
# - 카카오뱅크, 마이너스통장 한도 5천만원에서 3천만원으로
# 
# 2021-07-21
# 카카오페이의 상장 일정이 밀림
# - 카카오페이 여름 상장 무산
# - 카카오 '픽코마', 전세계 매출순위 7위
# - 카카오스타일, 패션플랫폼'포스티'오픈
# - 카카오페이, 청약 보름 앞두고 IPO 연기
# 
# 2022-01-05
# 4분기 실적 부진
# 카카오노조, 카카오페이 대표 내정자에 사퇴 요구
# 카카오페이 차기대표 내정자(신원근)와 현대표(류영준)가 주식대량 매각
# - 먹튀논란 류영준대표



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
#
# 2021-04-06
# 카카오뱅크 올해 하반기에 기업공개절차(IPO)를 준비(회사가 상장하기 위해서 심사 받고 주주를 모집하기 위해 기업의 중요정보를 공개하는 절차를 IPO라 함)
# 모빌리티 투자 유치 긍정적
# 카카오페이,뱅크 상장 기대감
# 미국 웹소설 플랫폼인 '래디쉬' 인수추진
# 카카오재팬(일본 내 웹툰 시장 점유율 1위 플랫폼인 '픽코마'운영중)은 신규 투자 유치를 두고 PEF운용사 앵커에쿼티파트너스(앵커PE)와 협상을 진행 중, 최대 5천억원 가량 유치 계획
# 
# 2021-06-23
# 카카오-카카오커머스 합병
# e커머스(선물하기, 쇼핑하기, 쇼핑라이브) 시장에서 경쟁력 강화
# 카카오톡 플랫폼 기반 커머스 사업을 맡고 있는 카카오커머스의 발행주식을 100% 취득하기로 결의했다고 전일공시 함
# 
# 2021-06-14
# 카카오-카카오커머스 재합병 - 첫 기사 난 일자
# 
# 2021-06-17
# 카카오모빌리티, 카카오T항공 서비스 시작, 카카오T에서 비행기표 예약
# - 카카오T앱, 국내선 항공권 예약, 발권 가능
# 
# 2021-08-04
# 카카오게임 실적 발표
# ‘오딘’ 대박 낸 카카오게임즈, 하반기도 믿고 간다
# 오딘 구글 앱마켓 매출 1위
# 오딘 출시 19일 만에 누적 매출 1천억원 달성
# - 카카오뱅크, 시총 10위에 오름
# - 카카오웹튼, 초반 기대치 미달

#-----------------------------------
# 가장 높은 하락가를 보인 상위 5일
co_minus
ggplot(data=co_minus, aes(x=reorder(Date,종가시가차이), 종가시가차이)) +
  geom_bar(stat='identity', fill='navy') +
  geom_text(aes(label = 종가시가차이), vjust=-1.5, color='white') +
  labs(title='가장 높은 하락가를 보인 상위 5일',
       x='일자',
       y='종가-시가 (원)')
#
# 2021-06-24
# 고평가 우려(전날 고가에 장 마감함)
# 주가수익비율(PER)은 228.53배(동일업종은 8.98배) PER은 값이 높을수록 회사가 벌어들이는 이익금에 비해 주가가 고평가 됐다는 의미
# 
# 2021-09-08
# 여당, 인터넷플랫폼 기업의 시장 독점 문제를 지적하며 규제 강화 의지를 밝힘
# 금융상품 중개를 하는 금융플랫폼은 금소법(금융소비자보호법) 대상이 되기 때문에 금융사처럼 금융위에 등록하거나 인허가를 받아야하지만, 카카오페이는 인허가 없이 상품을 판매하기 때문에 법령을 위반한 셈이 된다. 
# 카카오뱅크, 마이너스통장 최대 한도 2천만원씩 더 줄임
# 
# 2021-07-21
# 카카오페이의 상장 일정이 밀림
# 
# 2022-01-05
# 4분기 실적 부진
# 카카오노조, 카카오페이 대표 내정자에 사퇴 요구
# 카카오페이 차기대표 내정자(신원근)와 현대표(류영준)가 주식대량 매각
# 
# 2021-09-09
# (어제의 연속)

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
# 표준화 최고점
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
# 표준화 최저점
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

