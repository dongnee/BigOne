# 단어별로 감정점수가 분류된 파일을 불러와
# NA값은 사전에 추가한다

# 필요패키지
library(dplyr)
library(readr)
library(stringr)

setwd('C:/Users/admin/BigOne')
getwd()

# --------------------------------

# 1. 종목토론실 데이터불러오기 

Sys.setlocale("LC_ALL", "C") # - read_csv 에러 : invalid multibyte string, element 1
senti_word <- read_csv("data/종목토론실_단어별_감정점수.csv")
# Sys.setlocale("LC_ALL", "Korean")
senti_word

# 감정사전 데이터 불러오기
# Sys.setlocale("LC_ALL", "C")
knu_dic <- read.csv("data/단어합본.csv", header=T, sep=",", encoding="UTF-8")
# Sys.setlocale("LC_ALL", "Korean")
nrow(knu_dic) # 16090 -> 16165
knu_dic <- knu_dic[-1] #첫번째 컬럼은 필요없어서 제외
knu_dic

# 단어별로 카운트
count_senti <- senti_word %>% 
  count(word, polarity) %>% arrange(-n)
nrow(count_senti) # 97166
View(count_senti)

# count_senti를 csv로 저장해서 감정점수 분류 후 사전에 합칠 예정
#write.csv(count_senti, "data/종목토론실_단어count.csv")

#################
# 위에서 저장한 count파일을 엑셀로 분류후 
# 새로 불러와서 rbind

### 예시

ex_count <- rbind(ex_count, data.frame(word=c('보보'), polarity=c(1), n=c(1)))
ex_dic <- rbind(ex_dic, data.frame(word=c('보보'), polarity=c(2)))
t = as.character(ex_count[4, 'word'])

# 1부터 단어 카운트 행 개수만큼 순서대로 i에 대입
for (i in 1:nrow(ex_count)){
  t = as.character(ex_count[i, 'word'])
  
  # 사전 word컬럼에 i 행의 단어가 없으면
  if(sum(ex_dic$word %in% t) == 0){
    # i 행의 극성점수가 na값이 아니면
    if(is.na(ex_count$polarity[i]) == F) {
      # 새로운 사전에 추가한다.
      ex_dic2 <- rbind(ex_dic2, ex_count[i,1:2])
    }
  } else {
    # 사전 word컬럼에 i 행의 단어가 있음
    # polarity 값이 다르다면 count에 있는 polarity값을 dic polarity값으로 변경
    if (ex_dic[ex_dic$word == t, 'polarity'] != ex_count[ex_count$word == t, 'polarity']) {
      ex_dic[ex_dic$word == t, 'polarity'] <- ex_count[ex_count$word == t, 'polarity']
    }
  }
}
#--------------------------------------------

# 종목토론실_단어count.csv 가져오기
sentiword_count <- read.csv("data/종목토론실_단어count.csv", header=T, sep=",", encoding="UTF-8")
sentiword_count
nrow(sentiword_count) # 97332행 

# 중복단어 제거
sentiword_count <- sentiword_count[!duplicated(sentiword_count$word),]
nrow(sentiword_count) # 97116행 
head(sentiword_count)
# word, polarity 컬럼만 추출
sentiword_count <- sentiword_count[,c(-1,-4)]
# NA값 제외하고 추출
sentiword_count <- sentiword_count[!is.na(sentiword_count$polarity),]
View(sentiword_count)
nrow(sentiword_count) # 2454행 

# (감정사전) 새로 리베이스한 단어합본.csv 가져오기
# Sys.setlocale("LC_ALL", "C")
knu_dic <- read_csv("data/단어합본.csv")
# Sys.setlocale("LC_ALL", "Korean")
tail(knu_dic)
nrow(knu_dic) #17544행
# word, polarity 컬럼만 추출
knu_dic <- knu_dic[,-1]
View(knu_dic)
# NA값 확인후 제거
sum(is.na(knu_dic)) # 2개
knu_dic <- na.omit(knu_dic)
sum(is.na(knu_dic)) # 0개
nrow(knu_dic) #17543행

# 복사본
knu_dic2 <- knu_dic
knu_dic3 <- knu_dic2

View(sentiword_count)

# sentiword_count 에서 
# 1행부터 행 총 개수(97116행)만큼 순서대로 i에 대입
for (i in 1:nrow(sentiword_count)){
  t = as.character(sentiword_count[i, 'word'])
  
  # 사전에 t(i행의 단어)가 없으면 : 카카오는 F
  if(sum(knu_dic$word %in% t) == 0){
    
      knu_dic2 <- rbind(knu_dic2, sentiword_count[i,])

  } else {
    # 사전 word컬럼에 i 행의 단어가 있음
    # polarity 값이 다르다면 count에 있는 polarity값을 dic polarity값으로 변경
    if (knu_dic[knu_dic$word == t, 'polarity'] != sentiword_count[sentiword_count$word == t, 'polarity']) {

        knu_dic2[knu_dic2$word == t, 'polarity'] <- sentiword_count[sentiword_count$word == t, 'polarity']
    }
  }
}

View(knu_dic2)
nrow(knu_dic2) #18441 (898행 추가됨)

##############################
# 2차 분류

senti_word %>%
  filter(str_detect(word, "효자")) %>% View()

# --> 점수가 2개임
knu_dic2 %>% filter(word %in% c("개미")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='개미')
knu_dic2 %>% filter(word %in% c("단타")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='단타')
knu_dic2 %>% filter(word %in% c("고평가")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='고평가')
knu_dic2 %>% filter(word %in% c("마이너스")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='마이너스')
knu_dic2 %>% filter(word %in% c("무너지다")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='무너지다')
knu_dic2 %>% filter(word %in% c("후회")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='후회')
knu_dic2 %>% filter(word %in% c("나쁘다")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='나쁘다')
knu_dic2 %>% filter(word %in% c("물리다")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='물리다')
knu_dic2 %>% filter(word %in% c("울다")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='울다')
knu_dic2 %>% filter(word %in% c("잘못")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='잘못')
knu_dic2 %>% filter(word %in% c("혁신")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='혁신')
knu_dic2 %>% filter(word %in% c("독점")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='독점')
knu_dic2 %>% filter(word %in% c("싸우다")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='싸우다')
knu_dic2 %>% filter(word %in% c("소각")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='소각')
knu_dic2 %>% filter(word %in% c("액면분할")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='액면분할')
knu_dic2 %>% filter(word %in% c("부자")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='부자')
knu_dic2 %>% filter(word %in% c("성공")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='성공')
knu_dic2 %>% filter(word %in% c("대출")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='대출')
knu_dic2 %>% filter(word %in% c("횡령")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='횡령')
knu_dic2 %>% filter(word %in% c("축하")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='축하')
knu_dic2 %>% filter(word %in% c("행복")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='행복')
knu_dic2 %>% filter(word %in% c("불법")) # 사전 점수가 2개여서 삭제하고 다시 추가
knu_dic2 <- subset(knu_dic2, word!='불법')

# 새로 추가할 단어점수
new_dic <- data.frame(word=c('개미','고평가','마이너스','무너지다','후회',
                             '나쁘다',"물리다","울다","잘못","혁신",
                             "독점","싸우다","액면분할","부자","성공",
                             '대출','횡령','축하','행복','불법'),
                      polarity=c(0,-1,-1,-2,-2,
                                 -1,-1,-1,-1,1,
                                 -1,-1,1,1,2,
                                 0,-2,1,2,-2))

knu_dic3 <- rbind(knu_dic2, new_dic)
nrow(knu_dic3) # 18413행

# 확인
View(knu_dic3)

# 중복단어 처리
sum(duplicated(knu_dic3$word)) # [1] 472
knu_dic3 <- knu_dic3[!duplicated(knu_dic3$word),]
sum(duplicated(knu_dic3$word)) # [1] 0
nrow(knu_dic3) # 17941
write.csv(knu_dic3, "data/단어합본.csv")





# 
# ####### 1차 분류 ################################################
# ####### 형태소로 나뉘어진 단어별 사용되어진 글 전체 확인 ########
# senti_word %>%
#   filter(str_detect(word, "대단")) %>% View()
# # --> 대단하다 .. 부정어로 사용하는거같음
# 
# senti_word %>%
#   filter(str_detect(word, "사다")) %>% View()
# # --> 가격이 떨어졌으니 매수, 사다 좀더 긍정적인 반응
# 
# senti_word %>%
#   filter(str_detect(word, "가다")) %>% View()
# # --> 가격이 오르길바라는 기대적인 반응
# 
# senti_word %>%
#   filter(str_detect(word, "많다")) %>% View()
# # --> 감정 해석이 안됌 중립으로 변경
# 
# senti_word %>%
#   filter(str_detect(word, "네이버")) %>% View()
# # --> 경쟁구도인 네이버 회사를 향한 글이여서 부정적인 반응
# 
# senti_word %>%
#   filter(str_detect(word, "상하다")) %>% View()
# # --> 상한가라는 단어가 상하다로 인식되어서 긍정점수로 수정
# 
# senti_word %>%
#   filter(str_detect(word, "오르다")) %>% View()
# # --> 주가가 올랐을때 작성된 글이 많아서 긍정점수로 수정
# 
# senti_word %>%
#   filter(str_detect(word, "팔다")) %>% View()
# # --> 매도에 대한 작성글 부정점수로 수정
# 
# senti_word %>%
#   filter(str_detect(word, "매수")) %>% View()
# # --> 매수에 대한 작성글 긍정점수로 수정
# 
# senti_word %>%
#   filter(str_detect(word, "진짜")) %>% View()
# # --> 진짜 + 부정적인 의견들이 대다수 
# 
# senti_word %>%
#   filter(str_detect(word, "조정")) %>% View()
# # --> 조정 = 상승세에서 꺽였을때 쓰이는 단어 
# 
# senti_word %>%
#   filter(str_detect(word, "내리다")) %>% View()
# # --> 하락을 예측하는 글
# 
# senti_word %>%
#   filter(str_detect(word, "빠지다")) %>% View()
# # --> 하락을 예측하는 글
# 
# senti_word %>%
#   filter(str_detect(word, "하락")) %>% View()
# # --> 하락을 예측하는 글
# 
# senti_word %>%
#   filter(str_detect(word, "상승")) %>% View()
# # --> 상승을 예측하는 글
# 
# senti_word %>%
#   filter(str_detect(word, "털다")) %>% View()
# # --> 이득만 보고 나오는거
# 
# senti_word %>%
#   filter(str_detect(word, "물리다")) %>% View()
# # --> 고점에서 못팔고 하락세
# 
# senti_word %>%
#   filter(str_detect(word, "올리다")) %>% View()
# # --> 주가 상승
# 
# senti_word %>%
#   filter(str_detect(word, "호재")) %>% View()
# # --> 좋은소식
# 
# senti_word %>%
#   filter(str_detect(word, "폭락")) %>% View()
# # --> 가격 하락
# 
# senti_word %>%
#   filter(str_detect(word, "반등")) %>% View()
# # --> 가격 상승
# 
# senti_word %>%
#   filter(str_detect(word, "던지다")) %>% View()
# # --> 매도
# 
# senti_word %>%
#   filter(str_detect(word, "거품")) %>% View()
# # --> 고평가된 주가가 빠진다는 글이 대다수
# 
# senti_word %>%
#   filter(str_detect(word, "먹다")) %>% View()
# # --> 수익을 봤다
# 
# senti_word %>%
#   filter(str_detect(word, "가즈")) %>% View()
# # --> 오르길 기대하는 글
# 
# senti_word %>%
#   filter(str_detect(word, "바닥")) %>% View()
# # --> 하한가에 대한 얘기들 
# 
# senti_word %>%
#   filter(str_detect(word, "카카오페이")) %>% View()
# # --> 자회사
# 
# senti_word %>%
#   filter(str_detect(word, "끝나다")) %>% View()
# # --> 부정적인 의견들
# 
# senti_word %>%
#   filter(str_detect(word, "악재")) %>% View()
# # --> 안좋은소식
# 
# senti_word %>%
#   filter(str_detect(word, "떨어지")) %>% View()
# # --> 주가하락
# 
# senti_word %>%
#   filter(str_detect(word, "카뱅")) %>% View()
# # --> 자회사
# 
# senti_word %>%
#   filter(str_detect(word, "고점")) %>% View()
# # --> 최고가
# 
# senti_word %>%
#   filter(str_detect(word, "기다리다")) %>% View()
# # --> 매수대기
# 
# senti_word %>%
#   filter(str_detect(word, "규제")) %>% View()
# # --> 악재
# 
# senti_word %>%
#   filter(str_detect(word, "기회")) %>% View()
# # --> 긍정
# 
# senti_word %>%
#   filter(str_detect(word, "빼다")) %>% View()
# # --> 부정
# 
# senti_word %>%
#   filter(str_detect(word, "카카오뱅크")) %>% View()
# # --> 자회사
# 
# senti_word %>%
#   filter(str_detect(word, "단타")) %>% View()
# # --> 주식 보유를 짧게 갖고 파는것
# 
# senti_word %>%
#   filter(str_detect(word, "손절")) %>% View()
# # --> 마이너스를 감안하고 매도
# 
# senti_word %>%
#   filter(str_detect(word, "매도")) %>% View()
# # --> 주식을 판다
# 
# senti_word %>%
#   filter(str_detect(word, "미치다")) %>% View()
# # --> 부정적
# 
# senti_word %>%
#   filter(str_detect(word, "저점")) %>% View()
# # --> 주가흐름 중 가장 낮은 가격
# 
# senti_word %>%
#   filter(str_detect(word, "싸다")) %>% View()
# # --> 주가가 싸다
# 
# senti_word %>%
#   filter(str_detect(word, "버티다")) %>% View()
# # --> 팔지않고 어느정도 오를때까지 버틴다
# 
# senti_word %>%
#   filter(str_detect(word, "뭐하다")) %>% View()
# # --> 뭐하는거냐
# 
# senti_word %>%
#   filter(str_detect(word, "아가리")) %>% View()
# # --> 입을 벌린다
# 
# senti_word %>%
#   filter(str_detect(word, "폭등")) %>% View()
# # --> 가격이 급등
# 
# senti_word %>%
#   filter(str_detect(word, "흐르다")) %>% View()
# # --> 주가가 내려가다
# 
# senti_word %>%
#   filter(str_detect(word, "담다")) %>% View()
# # --> 싼 가격에 매수해라
# 
# senti_word %>%
#   filter(str_detect(word, "액분")) %>% View()
# # --> 호재
# 
# senti_word %>%
#   filter(str_detect(word, "액면분할")) %>% View()
# # --> 호재
# 
# senti_word %>%
#   filter(str_detect(word, "탈출")) %>% View()
# # --> 매도
# 
# senti_word %>%
#   filter(str_detect(word, "돌파")) %>% View()
# # --> 기대
# 
# senti_word %>%
#   filter(str_detect(word, "쓰레기")) %>% View()
# # --> 욕하는글
# 
# senti_word %>%
#   filter(str_detect(word, "정권")) %>% View()
# # --> 정권바뀌면 하락할거라는 심리
# 
# senti_word %>%
#   filter(str_detect(word, "인수")) %>% View()
# # --> 긍정?
# 
# senti_word %>%
#   filter(str_detect(word, "달리다")) %>% View()
# # --> 가격이 올라가자는 긍정심리
# 
# senti_word %>%
#   filter(str_detect(word, "급등")) %>% View()
# # --> 가격급등으로 긍정심리
# 
# senti_word %>%
#   filter(str_detect(word, "반토막")) %>% View()
# # --> 가격이 반으로 떨어졋다는
# 
# senti_word %>%
#   filter(str_detect(word, "완전")) %>% View()
# # --> 완전+ 부정적인 의미로
# 
# senti_word %>%
#   filter(str_detect(word, "살리다")) %>% View()
# # --> 가격이 떨어져서 살려달라는 부정심리
# 
# senti_word %>%
#   filter(str_detect(word, "개잡주")) %>% View()
# # --> 부정적인 심리
# 
# senti_word %>%
#   filter(str_detect(word, "올랐")) %>% View()
# # --> 긍정심리
# 
# senti_word %>%
#   filter(str_detect(word, "힘내다")) %>% View()
# # --> 격려 + 응원 긍정심리
# 
# senti_word %>%
#   filter(str_detect(word, "가즈아")) %>% View()
# # --> 가격이 올라라는 기대심리
# 
# senti_word %>%
#   filter(str_detect(word, "탈세")) %>% View()
# # --> 악재
# 
# senti_word %>%
#   filter(str_detect(word, "회복")) %>% View()
# # --> 가격이 다시 오를것이라는 기대심리
# 
# senti_word %>%
#   filter(str_detect(word, "우상향")) %>% View()
# # --> 가격이 다시 오를것이라는 기대심리
# 
# senti_word %>%
#   filter(str_detect(word, "진심")) %>% View()
# # --> 안좋은상황에 대한 진심
# 
# senti_word %>%
#   filter(str_detect(word, "주도주")) %>% View()
# # --> 카카오에대한 긍정
# 
# senti_word %>%
#   filter(str_detect(word, "나락")) %>% View()
# # --> 추락한다라는의미
# 
# senti_word %>%
#   filter(str_detect(word, "보유")) %>% View()
# # --> 카카오주식 보유중
# 
# senti_word %>%
#   filter(str_detect(word, "팔았")) %>% View()
# # --> 매도
# 
# senti_word %>%
#   filter(str_detect(word, "존버")) %>% View()
# # --> 버틴다
# 
# senti_word %>%
#   filter(str_detect(word, "국민주")) %>% View()
# # --> 타이틀
# 
# senti_word %>%
#   filter(str_detect(word, "매입")) %>% View()
# # --> 자사주매입을 해서 주가안정시키라는 의견
# 
# senti_word %>%
#   filter(str_detect(word, "대장")) %>% View()
# # --> 대장주 카카오
# 
# senti_word %>%
#   filter(str_detect(word, "물적분할")) %>% View()
# # --> 무분별한 물적분할로 인한 부정적
# 
# senti_word %>%
#   filter(str_detect(word, "줍다")) %>% View()
# # --> 싸게 매수
# 
# senti_word %>%
#   filter(str_detect(word, "매수세")) %>% View()
# # --> 매수 거래량이 많다
# 
# senti_word %>%
#   filter(str_detect(word, "문어발")) %>% View()
# # --> 문어발식 확장이라는 부정적
# 
# senti_word %>%
#   filter(str_detect(word, "살리다")) %>% View()
# # --> 손해를 입고있어서 살려다라는
# 
# senti_word %>%
#   filter(str_detect(word, "고평가")) %>% View()
# # --> 현재 가격이 고평가된 가격이다라는 부정적
# 
# senti_word %>%
#   filter(str_detect(word, "대박")) %>% View()
# # --> 긍정적
# 
# senti_word %>%
#   filter(str_detect(word, "이걸")) %>% View()
# # --> 부정적
# 
# senti_word %>%
#   filter(str_detect(word, "담다")) %>% View()
# # --> 매수
# 
# senti_word %>%
#   filter(str_detect(word, "저가매수")) %>% View()
# # --> 매수
# 
# senti_word %>%
#   filter(str_detect(word, "소각")) %>% View()
# # --> 자사주매입 후 소각 2.11 5%증가
# 
# senti_word %>%
#   filter(str_detect(word, "소각해")) %>% View()
# # --> 자사주매입 후 소각 2.11 5%증가
# 
# senti_word %>%
#   filter(str_detect(word, "오늘")) %>% View()
# # --> NA값, 중립으로 분류
# 
# 
# 
# 
# # ----------------------------------------------------
# 
# # 감정사전에 해당 단어 확인
# knu_dic %>% filter(word %in% c("대단하다"))
# knu_dic %>% filter(word %in% c("사다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("가다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("많다")) # 2
# knu_dic %>% filter(word %in% c("네이버")) # 사전에 없음
# knu_dic %>% filter(word %in% c("상하다")) # -2
# knu_dic %>% filter(word %in% c("오르다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("팔다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("매수")) # 사전에 없음
# knu_dic %>% filter(word %in% c("진짜")) # 사전에 없음
# knu_dic %>% filter(word %in% c("조정")) # 사전에 없음
# knu_dic %>% filter(word %in% c("내리다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("빠지다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("하락")) # 사전에 없음
# knu_dic %>% filter(word %in% c("상승")) # 사전에 없음
# knu_dic %>% filter(word %in% c("털다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("물리다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("올리다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("호재")) # 사전에 없음
# knu_dic %>% filter(word %in% c("폭락")) # 사전에 없음
# knu_dic %>% filter(word %in% c("반등")) # 사전에 없음
# knu_dic %>% filter(word %in% c("던지다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("거품")) # 사전에 없음
# knu_dic %>% filter(word %in% c("먹다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("가즈")) # 사전에 없음
# knu_dic %>% filter(word %in% c("바닥")) # 사전에 없음
# knu_dic %>% filter(word %in% c("카카오페이")) # 사전에 없음
# knu_dic %>% filter(word %in% c("끝나다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("악재")) # 사전에 없음
# knu_dic %>% filter(word %in% c("떨어지")) # 사전에 없음
# knu_dic %>% filter(word %in% c("카뱅")) # 사전에 없음
# 
# knu_dic %>% filter(word %in% c("고점")) # 사전에 없음
# knu_dic %>% filter(word %in% c("기다리다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("규제")) # 사전에 없음
# knu_dic %>% filter(word %in% c("기회")) # 사전에 없음
# knu_dic %>% filter(word %in% c("빼다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("카카오뱅크")) # 사전에 없음
# knu_dic %>% filter(word %in% c("단타")) # 사전에 없음
# knu_dic %>% filter(word %in% c("손절")) # 사전에 없음
# knu_dic %>% filter(word %in% c("매도")) # 사전에 없음
# knu_dic %>% filter(word %in% c("미치다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("저점")) # 사전에 없음
# knu_dic %>% filter(word %in% c("싸다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("버티다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("뭐하다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("아가리")) # 사전에 없음
# knu_dic %>% filter(word %in% c("폭등")) # 사전에 없음
# knu_dic %>% filter(word %in% c("흐르다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("담다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("액분")) # 사전에 없음
# knu_dic %>% filter(word %in% c("액변분할")) # 사전에 없음
# knu_dic %>% filter(word %in% c("탈출")) # 사전에 없음
# knu_dic %>% filter(word %in% c("돌파")) # 사전에 없음
# knu_dic %>% filter(word %in% c("쓰레기")) # 사전에 없음
# knu_dic %>% filter(word %in% c("정권")) # 사전에 없음
# knu_dic %>% filter(word %in% c("인수")) # 사전에 없음
# knu_dic %>% filter(word %in% c("달리다")) # 사전에 없음
# 
# knu_dic %>% filter(word %in% c("급등")) # 사전에 없음
# knu_dic %>% filter(word %in% c("반토막")) # 사전에 없음
# knu_dic %>% filter(word %in% c("완전")) # 사전에 없음
# knu_dic %>% filter(word %in% c("터지다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("살리다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("개잡주")) # 사전에 없음
# knu_dic %>% filter(word %in% c("올랐")) # 사전에 없음
# knu_dic %>% filter(word %in% c("힘내다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("가즈아")) # 사전에 없음
# knu_dic %>% filter(word %in% c("탈세")) # 사전에 없음
# knu_dic %>% filter(word %in% c("회복")) # 사전에 없음
# knu_dic %>% filter(word %in% c("우상향")) # 사전에 없음
# knu_dic %>% filter(word %in% c("진심")) # 사전에 없음
# knu_dic %>% filter(word %in% c("주도주")) # 사전에 없음
# knu_dic %>% filter(word %in% c("나락")) # 사전에 없음
# knu_dic %>% filter(word %in% c("보유")) # 사전에 없음
# knu_dic %>% filter(word %in% c("팔았")) # 사전에 없음
# knu_dic %>% filter(word %in% c("존버")) # 사전에 없음
# knu_dic %>% filter(word %in% c("국민주")) # 사전에 없음
# knu_dic %>% filter(word %in% c("매입")) # 사전에 없음
# knu_dic %>% filter(word %in% c("대장")) # 사전에 없음
# knu_dic %>% filter(word %in% c("물적분할")) # 사전에 없음
# knu_dic %>% filter(word %in% c("줍다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("정권")) # 사전에 없음
# knu_dic %>% filter(word %in% c("매수세")) # 사전에 없음
# knu_dic %>% filter(word %in% c("문어발")) # 사전에 없음
# knu_dic %>% filter(word %in% c("살리다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("고평가")) # 사전에 없음
# knu_dic %>% filter(word %in% c("대박")) # 사전에 없음
# knu_dic %>% filter(word %in% c("이걸")) # 사전에 없음
# knu_dic %>% filter(word %in% c("담다")) # 사전에 없음
# knu_dic %>% filter(word %in% c("저가매수")) # 사전에 없음
# knu_dic %>% filter(word %in% c("소각")) # 사전에 없음
# knu_dic %>% filter(word %in% c("소각해")) # 사전에 없음
# 
# 
# # 새로운 감정사전에 수정
# # 해당 단어가 사전에 포함되지않아서 추가
# knu_dic2 <- rbind(knu_dic, data.frame(word=c("대단","사다","가다","네이버","오르다",
#                                              "팔다","매수","진짜", "조정","내리다",
#                                              "빠지다","하락","상승","털다","물리다",
#                                              "올리다","호재","폭락","반등","던지다",
#                                              "거품","먹다","가즈","바닥","카카오페이",
#                                              "끝나다","악재","떨어지","카뱅","고점",
#                                              "기다리다","규제","기회","빼다","카카오뱅크",
#                                              "단타","손절",'매도','미치다',"저점",
#                                              '싸다','버티다','뭐하다','아가리','폭등',
#                                              '흐르다','담다','액분','액면분할','탈출',
#                                              '돌파','쓰레기','정권','인수','달리다',
#                                              '급등','반토막','완전','살리다','개잡주',
#                                              '올랐','힘내다','가즈아','탈세','회복',
#                                              '우상향','진심','주도주','나락','보유',
#                                              '팔았','존버','국민주','매입','대장',
#                                              '물적분할','줍다','매수세','문어발','살리다',
#                                              '고평가','대박','이걸','담다','저가매수',
#                                              '소각','소각해'), 
#                                       polarity=c(-1,1,1,-1,1,
#                                                  -1,1,-1,-1,-1,
#                                                  -1,-1,1,-1,-1,
#                                                  1,1,-1,1,-1,
#                                                  -1,1,1,-1,1,
#                                                  -1,-1,-1,1,1,
#                                                  1,-1,1,-1,1,
#                                                  1,-1,-1,-1,-1,
#                                                  -1,1,-1,1,1,
#                                                  -1,1,1,1,-1,
#                                                  1,-2,-1,1,1,
#                                                  1,-1,-1,-1,-1,
#                                                  1,1,1,-1,1,
#                                                  1,-1,1,-1,1,
#                                                  -1,1,1,1,1,
#                                                  -1,1,1,-1,-1,
#                                                  -1,1,-1,1,1,
#                                                  1,1)))
# 
# # 사전에 있는 단어는 점수만 수정
# knu_dic2 <- knu_dic2 %>%
#   mutate(polarity = ifelse(word %in% c("대단하다","대단한"), -2, polarity))
# knu_dic2 <- knu_dic2 %>%
#   mutate(polarity = ifelse(word %in% c("많다"), 0, polarity))
# knu_dic2 <- knu_dic2 %>%
#   mutate(polarity = ifelse(word %in% c("상하다"), 2, polarity))
# 
# # ----------------------------------------------------
# # 확인
# tail(knu_dic2,20)
# 
# write.csv(knu_dic2, "data/단어합본.csv")


##############################################################
##############################################################
##############################################################



























