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
library(NLTK)


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

sum(duplicated(raw_discuss3$제목)) # 중복내용 4541개 처리
raw_discuss3 <- raw_discuss3[!duplicated(raw_discuss3$제목),]
sum(duplicated(raw_discuss3$제목)) # 중복내용 0개
View(raw_discuss3)

# 데이터 구조 확인
glimpse(raw_discuss3)


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

# 한 글자는 전후 맥락없이 의미를 파악하기 어렵기 때문에 제거
word_discuss_done <- bind_rows(word_discuss_n, word_discuss_p) %>% 
  arrange(id) %>% 
  filter(nchar(word) > 1) %>% 
  select(작성일, 제목, id, word)

View(word_discuss_done)

# 사용된 단어별로 count
word_discuss_done %>% count(word, sort=T) %>% View()

nrow(word_discuss_done) # [1] 489321
# 해당 단어가 포함된 행은 삭제할 것. - 삭제된 게시물
word_discuss_done <- word_discuss_done[-grep("삭제", word_discuss_done$제목),]
word_discuss_done <- word_discuss_done[-grep("게시물", word_discuss_done$제목),]
word_discuss_done <- word_discuss_done[-grep("답글", word_discuss_done$제목),]
nrow(word_discuss_done) # [1] 471441

# ------------------------------------------------------

# 감정사전 활용 예시

knu_dic <- read_csv("data/knu_SentiWord_Dict.csv")
knu_dic

# 긍정단어 필러링 후 정렬
knu_dic %>% 
  filter(polarity == 2) %>% 
  arrange(word)

# 부정단어 필러링 후 정렬
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
# dplyr::left_join() : 감정사전 word 기준 결합
# 없는단어는 polarity NA -> 0 처리
senti_word <- word_discuss_done %>%
  left_join(knu_dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

View(senti_word)

##########################  감정사전 수정 ########################## 
# 단어가 포함된 글 확인
senti_word %>%
  filter(str_detect(word, "대단")) %>% View()
# --> 대단하다 .. 부정어로 사용하는거같음

senti_word %>%
  filter(str_detect(word, "사다")) %>% View()
# --> 가격이 떨어졌으니 매수, 사다 좀더 긍정적인 반응

senti_word %>%
  filter(str_detect(word, "가다")) %>% View()
# --> 가격이 오르길바라는 기대적인 반응

senti_word %>%
  filter(str_detect(word, "많다")) %>% View()
# --> 감정 해석이 안됌 중립으로 변경

senti_word %>%
  filter(str_detect(word, "네이버")) %>% View()
# --> 경쟁구도인 네이버 회사를 향한 글이여서 부정적인 반응

senti_word %>%
  filter(str_detect(word, "상하다")) %>% View()
# --> 상한가라는 단어가 상하다로 인식되어서 긍정점수로 수정

senti_word %>%
  filter(str_detect(word, "오르다")) %>% View()
# --> 주가가 올랐을때 작성된 글이 많아서 긍정점수로 수정

senti_word %>%
  filter(str_detect(word, "팔다")) %>% View()
# --> 매도에 대한 작성글 부정점수로 수정

senti_word %>%
  filter(str_detect(word, "매수")) %>% View()
# --> 매수에 대한 작성글 긍정점수로 수정

senti_word %>%
  filter(str_detect(word, "진짜")) %>% View()
# --> 진짜 + 부정적인 의견들이 대다수 

senti_word %>%
  filter(str_detect(word, "조정")) %>% View()
# --> 조정 = 상승세에서 꺽였을때 쓰이는 단어 

senti_word %>%
  filter(str_detect(word, "내리다")) %>% View()
# --> 하락을 예측하는 글

senti_word %>%
  filter(str_detect(word, "빠지다")) %>% View()
# --> 하락을 예측하는 글

senti_word %>%
  filter(str_detect(word, "하락")) %>% View()
# --> 하락을 예측하는 글

senti_word %>%
  filter(str_detect(word, "상승")) %>% View()
# --> 상승을 예측하는 글

senti_word %>%
  filter(str_detect(word, "털다")) %>% View()
# --> 이득만 보고 나오는거

senti_word %>%
  filter(str_detect(word, "물리다")) %>% View()
# --> 고점에서 못팔고 하락세

senti_word %>%
  filter(str_detect(word, "올리다")) %>% View()
# --> 주가 상승

senti_word %>%
  filter(str_detect(word, "호재")) %>% View()
# --> 좋은소식

senti_word %>%
  filter(str_detect(word, "폭락")) %>% View()
# --> 가격 하락

senti_word %>%
  filter(str_detect(word, "반등")) %>% View()
# --> 가격 상승

senti_word %>%
  filter(str_detect(word, "던지다")) %>% View()
# --> 매도

senti_word %>%
  filter(str_detect(word, "거품")) %>% View()
# --> 고평가된 주가가 빠진다는 글이 대다수

senti_word %>%
  filter(str_detect(word, "먹다")) %>% View()
# --> 수익을 봤다

senti_word %>%
  filter(str_detect(word, "가즈")) %>% View()
# --> 오르길 기대하는 글

senti_word %>%
  filter(str_detect(word, "바닥")) %>% View()
# --> 하한가에 대한 얘기들 

senti_word %>%
  filter(str_detect(word, "카카오페이")) %>% View()
# --> 자회사

senti_word %>%
  filter(str_detect(word, "끝나다")) %>% View()
# --> 부정적인 의견들

senti_word %>%
  filter(str_detect(word, "악재")) %>% View()
# --> 안좋은소식

senti_word %>%
  filter(str_detect(word, "떨어지")) %>% View()
# --> 주가하락

senti_word %>%
  filter(str_detect(word, "카뱅")) %>% View()
# --> 자회사

senti_word %>%
  filter(str_detect(word, "고점")) %>% View()
# --> 최고가

senti_word %>%
  filter(str_detect(word, "기다리다")) %>% View()
# --> 매수대기

senti_word %>%
  filter(str_detect(word, "규제")) %>% View()
# --> 악재

senti_word %>%
  filter(str_detect(word, "기회")) %>% View()
# --> 긍정

senti_word %>%
  filter(str_detect(word, "빼다")) %>% View()
# --> 부정

senti_word %>%
  filter(str_detect(word, "카카오뱅크")) %>% View()
# --> 자회사

senti_word %>%
  filter(str_detect(word, "단타")) %>% View()
# --> 주식 보유를 짧게 갖고 파는것

senti_word %>%
  filter(str_detect(word, "손절")) %>% View()
# --> 마이너스를 감안하고 매도

senti_word %>%
  filter(str_detect(word, "매도")) %>% View()
# --> 주식을 판다

senti_word %>%
  filter(str_detect(word, "미치다")) %>% View()
# --> 부정적

senti_word %>%
  filter(str_detect(word, "저점")) %>% View()
# --> 주가흐름 중 가장 낮은 가격

senti_word %>%
  filter(str_detect(word, "싸다")) %>% View()
# --> 주가가 싸다

senti_word %>%
  filter(str_detect(word, "버티다")) %>% View()
# --> 팔지않고 어느정도 오를때까지 버틴다

senti_word %>%
  filter(str_detect(word, "뭐하다")) %>% View()
# --> 뭐하는거냐

senti_word %>%
  filter(str_detect(word, "아가리")) %>% View()
# --> 입을 벌린다

senti_word %>%
  filter(str_detect(word, "폭등")) %>% View()
# --> 가격이 급등

senti_word %>%
  filter(str_detect(word, "흐르다")) %>% View()
# --> 주가가 내려가다

senti_word %>%
  filter(str_detect(word, "담다")) %>% View()
# --> 싼 가격에 매수해라

senti_word %>%
  filter(str_detect(word, "액분")) %>% View()
# --> 호재

senti_word %>%
  filter(str_detect(word, "액면분할")) %>% View()
# --> 호재

senti_word %>%
  filter(str_detect(word, "탈출")) %>% View()
# --> 매도

senti_word %>%
  filter(str_detect(word, "돌파")) %>% View()
# --> 기대

senti_word %>%
  filter(str_detect(word, "쓰레기")) %>% View()
# --> 욕하는글

senti_word %>%
  filter(str_detect(word, "정권")) %>% View()
# --> 정권바뀌면 하락할거라는 심리

senti_word %>%
  filter(str_detect(word, "인수")) %>% View()
# --> 긍정?

senti_word %>%
  filter(str_detect(word, "달리다")) %>% View()
# --> 가격이 올라가자는 긍정심리

senti_word %>%
  filter(str_detect(word, "급등")) %>% View()
# --> 가격급등으로 긍정심리

senti_word %>%
  filter(str_detect(word, "반토막")) %>% View()
# --> 가격이 반으로 떨어졋다는

senti_word %>%
  filter(str_detect(word, "완전")) %>% View()
# --> 완전+ 부정적인 의미로

senti_word %>%
  filter(str_detect(word, "살리다")) %>% View()
# --> 가격이 떨어져서 살려달라는 부정심리

senti_word %>%
  filter(str_detect(word, "개잡주")) %>% View()
# --> 부정적인 심리

senti_word %>%
  filter(str_detect(word, "올랐")) %>% View()
# --> 긍정심리

senti_word %>%
  filter(str_detect(word, "힘내다")) %>% View()
# --> 격려 + 응원 긍정심리

senti_word %>%
  filter(str_detect(word, "가즈아")) %>% View()
# --> 가격이 올라라는 기대심리

senti_word %>%
  filter(str_detect(word, "탈세")) %>% View()
# --> 악재

senti_word %>%
  filter(str_detect(word, "회복")) %>% View()
# --> 가격이 다시 오를것이라는 기대심리

senti_word %>%
  filter(str_detect(word, "우상향")) %>% View()
# --> 가격이 다시 오를것이라는 기대심리

senti_word %>%
  filter(str_detect(word, "진심")) %>% View()
# --> 안좋은상황에 대한 진심

senti_word %>%
  filter(str_detect(word, "주도주")) %>% View()
# --> 카카오에대한 긍정

senti_word %>%
  filter(str_detect(word, "나락")) %>% View()
# --> 추락한다라는의미

senti_word %>%
  filter(str_detect(word, "보유")) %>% View()
# --> 카카오주식 보유중

senti_word %>%
  filter(str_detect(word, "팔았")) %>% View()
# --> 매도

senti_word %>%
  filter(str_detect(word, "존버")) %>% View()
# --> 버틴다

senti_word %>%
  filter(str_detect(word, "국민주")) %>% View()
# --> 타이틀

senti_word %>%
  filter(str_detect(word, "매입")) %>% View()
# --> 자사주매입을 해서 주가안정시키라는 의견

senti_word %>%
  filter(str_detect(word, "대장")) %>% View()
# --> 대장주 카카오

senti_word %>%
  filter(str_detect(word, "물적분할")) %>% View()
# --> 무분별한 물적분할로 인한 부정적

senti_word %>%
  filter(str_detect(word, "줍다")) %>% View()
# --> 싸게 매수

senti_word %>%
  filter(str_detect(word, "매수세")) %>% View()
# --> 매수 거래량이 많다

senti_word %>%
  filter(str_detect(word, "문어발")) %>% View()
# --> 문어발식 확장이라는 부정적

senti_word %>%
  filter(str_detect(word, "살리다")) %>% View()
# --> 손해를 입고있어서 살려다라는

senti_word %>%
  filter(str_detect(word, "고평가")) %>% View()
# --> 현재 가격이 고평가된 가격이다라는 부정적

senti_word %>%
  filter(str_detect(word, "대박")) %>% View()
# --> 긍정적

senti_word %>%
  filter(str_detect(word, "이걸")) %>% View()
# --> 부정적

senti_word %>%
  filter(str_detect(word, "담다")) %>% View()
# --> 매수

senti_word %>%
  filter(str_detect(word, "저가매수")) %>% View()
# --> 매수

senti_word %>%
  filter(str_detect(word, "소각")) %>% View()
# --> 자사주매입 후 소각 2.11 5%증가

senti_word %>%
  filter(str_detect(word, "소각해")) %>% View()
# --> 자사주매입 후 소각 2.11 5%증가


# ----------------------------------------------------

# 감정사전에 해당 단어 확인
knu_dic %>% filter(word %in% c("대단하다"))
knu_dic %>% filter(word %in% c("사다")) # 사전에 없음
knu_dic %>% filter(word %in% c("가다")) # 사전에 없음
knu_dic %>% filter(word %in% c("많다")) # 2
knu_dic %>% filter(word %in% c("네이버")) # 사전에 없음
knu_dic %>% filter(word %in% c("상하다")) # -2
knu_dic %>% filter(word %in% c("오르다")) # 사전에 없음
knu_dic %>% filter(word %in% c("팔다")) # 사전에 없음
knu_dic %>% filter(word %in% c("매수")) # 사전에 없음
knu_dic %>% filter(word %in% c("진짜")) # 사전에 없음
knu_dic %>% filter(word %in% c("조정")) # 사전에 없음
knu_dic %>% filter(word %in% c("내리다")) # 사전에 없음
knu_dic %>% filter(word %in% c("빠지다")) # 사전에 없음
knu_dic %>% filter(word %in% c("하락")) # 사전에 없음
knu_dic %>% filter(word %in% c("상승")) # 사전에 없음
knu_dic %>% filter(word %in% c("털다")) # 사전에 없음
knu_dic %>% filter(word %in% c("물리다")) # 사전에 없음
knu_dic %>% filter(word %in% c("올리다")) # 사전에 없음
knu_dic %>% filter(word %in% c("호재")) # 사전에 없음
knu_dic %>% filter(word %in% c("폭락")) # 사전에 없음
knu_dic %>% filter(word %in% c("반등")) # 사전에 없음
knu_dic %>% filter(word %in% c("던지다")) # 사전에 없음
knu_dic %>% filter(word %in% c("거품")) # 사전에 없음
knu_dic %>% filter(word %in% c("먹다")) # 사전에 없음
knu_dic %>% filter(word %in% c("가즈")) # 사전에 없음
knu_dic %>% filter(word %in% c("바닥")) # 사전에 없음
knu_dic %>% filter(word %in% c("카카오페이")) # 사전에 없음
knu_dic %>% filter(word %in% c("끝나다")) # 사전에 없음
knu_dic %>% filter(word %in% c("악재")) # 사전에 없음
knu_dic %>% filter(word %in% c("떨어지")) # 사전에 없음
knu_dic %>% filter(word %in% c("카뱅")) # 사전에 없음

knu_dic %>% filter(word %in% c("고점")) # 사전에 없음
knu_dic %>% filter(word %in% c("기다리다")) # 사전에 없음
knu_dic %>% filter(word %in% c("규제")) # 사전에 없음
knu_dic %>% filter(word %in% c("기회")) # 사전에 없음
knu_dic %>% filter(word %in% c("빼다")) # 사전에 없음
knu_dic %>% filter(word %in% c("카카오뱅크")) # 사전에 없음
knu_dic %>% filter(word %in% c("단타")) # 사전에 없음
knu_dic %>% filter(word %in% c("손절")) # 사전에 없음
knu_dic %>% filter(word %in% c("매도")) # 사전에 없음
knu_dic %>% filter(word %in% c("미치다")) # 사전에 없음
knu_dic %>% filter(word %in% c("저점")) # 사전에 없음
knu_dic %>% filter(word %in% c("싸다")) # 사전에 없음
knu_dic %>% filter(word %in% c("버티다")) # 사전에 없음
knu_dic %>% filter(word %in% c("뭐하다")) # 사전에 없음
knu_dic %>% filter(word %in% c("아가리")) # 사전에 없음
knu_dic %>% filter(word %in% c("폭등")) # 사전에 없음
knu_dic %>% filter(word %in% c("흐르다")) # 사전에 없음
knu_dic %>% filter(word %in% c("담다")) # 사전에 없음
knu_dic %>% filter(word %in% c("액분")) # 사전에 없음
knu_dic %>% filter(word %in% c("액변분할")) # 사전에 없음
knu_dic %>% filter(word %in% c("탈출")) # 사전에 없음
knu_dic %>% filter(word %in% c("돌파")) # 사전에 없음
knu_dic %>% filter(word %in% c("쓰레기")) # 사전에 없음
knu_dic %>% filter(word %in% c("정권")) # 사전에 없음
knu_dic %>% filter(word %in% c("인수")) # 사전에 없음
knu_dic %>% filter(word %in% c("달리다")) # 사전에 없음

knu_dic %>% filter(word %in% c("급등")) # 사전에 없음
knu_dic %>% filter(word %in% c("반토막")) # 사전에 없음
knu_dic %>% filter(word %in% c("완전")) # 사전에 없음
knu_dic %>% filter(word %in% c("터지다")) # 사전에 없음
knu_dic %>% filter(word %in% c("살리다")) # 사전에 없음
knu_dic %>% filter(word %in% c("개잡주")) # 사전에 없음
knu_dic %>% filter(word %in% c("올랐")) # 사전에 없음
knu_dic %>% filter(word %in% c("힘내다")) # 사전에 없음
knu_dic %>% filter(word %in% c("가즈아")) # 사전에 없음
knu_dic %>% filter(word %in% c("탈세")) # 사전에 없음
knu_dic %>% filter(word %in% c("회복")) # 사전에 없음
knu_dic %>% filter(word %in% c("우상향")) # 사전에 없음
knu_dic %>% filter(word %in% c("진심")) # 사전에 없음
knu_dic %>% filter(word %in% c("주도주")) # 사전에 없음
knu_dic %>% filter(word %in% c("나락")) # 사전에 없음
knu_dic %>% filter(word %in% c("보유")) # 사전에 없음
knu_dic %>% filter(word %in% c("팔았")) # 사전에 없음
knu_dic %>% filter(word %in% c("존버")) # 사전에 없음
knu_dic %>% filter(word %in% c("국민주")) # 사전에 없음
knu_dic %>% filter(word %in% c("매입")) # 사전에 없음
knu_dic %>% filter(word %in% c("대장")) # 사전에 없음
knu_dic %>% filter(word %in% c("물적분할")) # 사전에 없음
knu_dic %>% filter(word %in% c("줍다")) # 사전에 없음
knu_dic %>% filter(word %in% c("정권")) # 사전에 없음
knu_dic %>% filter(word %in% c("매수세")) # 사전에 없음
knu_dic %>% filter(word %in% c("문어발")) # 사전에 없음
knu_dic %>% filter(word %in% c("살리다")) # 사전에 없음
knu_dic %>% filter(word %in% c("고평가")) # 사전에 없음
knu_dic %>% filter(word %in% c("대박")) # 사전에 없음
knu_dic %>% filter(word %in% c("이걸")) # 사전에 없음
knu_dic %>% filter(word %in% c("담다")) # 사전에 없음
knu_dic %>% filter(word %in% c("저가매수")) # 사전에 없음
knu_dic %>% filter(word %in% c("소각")) # 사전에 없음
knu_dic %>% filter(word %in% c("소각해")) # 사전에 없음


# 새로운 감정사전에 수정
# 해당 단어가 사전에 포함되지않아서 추가
knu_dic2 <- rbind(knu_dic, data.frame(word=c("대단","사다","가다","네이버","오르다",
                                             "팔다","매수","진짜", "조정","내리다",
                                             "빠지다","하락","상승","털다","물리다",
                                             "올리다","호재","폭락","반등","던지다",
                                             "거품","먹다","가즈","바닥","카카오페이",
                                             "끝나다","악재","떨어지","카뱅","고점",
                                             "기다리다","규제","기회","빼다","카카오뱅크",
                                             "단타","손절",'매도','미치다',"저점",
                                             '싸다','버티다','뭐하다','아가리','폭등',
                                             '흐르다','담다','액분','액면분할','탈출',
                                             '돌파','쓰레기','정권','인수','달리다',
                                             '급등','반토막','완전','살리다','개잡주',
                                             '올랐','힘내다','가즈아','탈세','회복',
                                             '우상향','진심','주도주','나락','보유',
                                             '팔았','존버','국민주','매입','대장',
                                             '물적분할','줍다','매수세','문어발','살리다',
                                             '고평가','대박','이걸','담다','저가매수',
                                             '소각','소각해'), 
                                      polarity=c(-1,1,1,-1,1,
                                                 -1,1,-1,-1,-1,
                                                 -1,-1,1,-1,-1,
                                                 1,1,-1,1,-1,
                                                 -1,1,1,-1,1,
                                                 -1,-1,-1,1,1,
                                                 1,-1,1,-1,1,
                                                 1,-1,-1,-1,-1,
                                                 -1,1,-1,1,1,
                                                 -1,1,1,1,-1,
                                                 1,-2,-1,1,1,
                                                 1,-1,-1,-1,-1,
                                                 1,1,1,-1,1,
                                                 1,-1,1,-1,1,
                                                 -1,1,1,1,1,
                                                 -1,1,1,-1,-1,
                                                 -1,1,-1,1,1,
                                                 1,1)))

# 대단하다 점수 +2 -> -2 로 수정
knu_dic2 <- knu_dic2 %>%
  mutate(polarity = ifelse(word %in% c("대단하다","대단한"), -2, polarity))
knu_dic2 <- knu_dic2 %>%
  mutate(polarity = ifelse(word %in% c("많다"), 0, polarity))
knu_dic2 <- knu_dic2 %>%
  mutate(polarity = ifelse(word %in% c("상하다"), 2, polarity))

# ----------------------------------------------------
# 확인
tail(knu_dic2,20)

write.csv(knu_dic2, "data/knu_SentiWord_Dict_2.csv")


######################## 다시 감정점수 분류
senti_word <- word_discuss_done %>%
  left_join(knu_dic2, by = "word") %>% 
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

## polarity 점수별로 긍정/부정/중립 분류
senti_word2 <- senti_word %>% 
  mutate(sentiment = ifelse(polarity >= 1, "pos",
                            ifelse(polarity <= -1, "neg", "neu")))


### 긍정, 부정단어별 가장 빈도가 많은 20개 추출
top10_senti <- senti_word2 %>%
  filter(sentiment != "neu") %>%
  count(sentiment, word) %>% filter(n > 100) %>% 
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
  ggtitle("종목토론실 내 가장 빈도가 많은 긍정,부정 단어 20개") +
  theme(title = element_text(size=15, face='bold'))



########## 댓글별 감정 점수 구하기

# 감정점수 합계 구한 df 따로 생성
score <- aggregate(polarity~id,senti_word2,sum)
names(score) <- c('id', 'score')
head(score)

# 감정점수 df와 score합계 df - inner_join
senti_score <- merge(senti_word2, score,
                     by="id", all=F) %>% select(제목, score) %>% group_by(제목)

# id별로 합쳐서 합계가 중복해서 들어감 -> 정리
sum(duplicated(senti_score$제목)) # 중복내용 299922개 처리
senti_score <- senti_score[!duplicated(senti_score$제목),]
sum(duplicated(senti_score$제목)) # 중복내용 0개

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
  ggtitle('종목토론실 게시글 감정 빈도 비율그래프') +
  theme(title = element_text(size=15, face='bold'))

##################################################################
# 긍정 7.3% / 중립 87.6% / 부정 5.1%
# -> 수정 후 15% / 70.3% / 14.6%
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

# 날짜변환 ㅠ 실패
glimpse(date_discuss)
gsub(".","-",date_discuss$작성일)
date_discuss$작성일[1]
as.Date("2020.01.01", format="%Y-%m-%d")
as.Date(date_discuss$작성일)

# 위치찾기
which(date_discuss$n==1263)
date_discuss[190:200,]


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
               color='orange',size=1.2)+
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





