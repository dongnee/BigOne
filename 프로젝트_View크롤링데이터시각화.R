# 파이썬에서 크롤링하여 수집한 데이터 분석 시각화작업

library(KoNLP)
library(wordcloud2)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)

useSejongDic()

setwd('C:/PythonStudy/데이터분석팀프로젝트')
getwd()

# 데이터 가져오기
crwal_data <- read_csv(file='crawl_data/view_info_df.csv')
nrow(crwal_data) # 105
View(crwal_data)

# 명사 분석
crwal_data2 <- sapply(crwal_data$content, extractNoun, USE.NAMES = F)
crwal_data2

# 벡터 변환
crwal_data3 <- unlist(crwal_data2)
crwal_data3

#-------- 필요없는 문자 제거

# 2자 이상만 추출
crwal_data4 <- Filter(function(x){nchar(x) >=2}, crwal_data3)
crwal_data4

# 특수문자 제거
crwal_data4 <- str_replace_all(crwal_data4,"[^[:alpha:]]","")
crwal_data4

# 제거할 단어 리스트
gsub_list <- readLines('data/view_info_content_gsub.txt', encoding='UTF-8')
gsub_list

for(i in 1:length(gsub_list)){
  crwal_data4 <- gsub(gsub_list[i],"",crwal_data4)
}

# 공백 제거 후 데이터 프레임 생성
write(crwal_data4,"data/view_info_content.txt")
crwal_data5 <- read.table("data/view_info_content.txt")
crwal_data5

# 빈도 저장
wordcount <- table(crwal_data5)

# 정렬
View(sort(wordcount, decreasing = T))

# 빈도수 10개이상만 출력
freq10 <- wordcount %>% sort(wordcount, decreasing = T) %>% head(395)
freq10
head(as.data.frame(freq10),100)

# ------------------------------------------------------------------------

# 워드크라우드 시각화
wordcloud2(freq10, color = "random-light",
           backgroundColor = 'white',
           size=1.5,
           rotateRatio = 0)


# ggplot2 : 바 그래프
ggplot(head(as.data.frame(freq10),50), aes(x=reorder(crwal_data5, Freq), y=Freq)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("word") + ylab("frep") +
  ggtitle('"카카오"검색 후 크롤링된 형태소 분석')


# ------------------------------------------------------------------------
# NLP4kec 패키지 이용하여 N-Gram 생성

# 데이터 가져오기
crwal_data <- read_csv(file='crawl_data/view_info_df.csv')
nrow(crwal_data) # 105행
sum(is.na(crwal_data$content)) # NA값 2개
# NA값 처리
crwal_data$content[is.na(crwal_data$content)] <- ""
sum(is.na(crwal_data$content)) # NA값 0개

# 한글을 제외한 모든 글자 삭제처리
crwal_data$content <- crwal_data$content %>% 
  stringr::str_remove_all(pattern = "[^가-힣]")

# 데이터 변경확인
crwal_data$content[1]

# 패키지설치
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_141')

install.packages('rJava')
install.packages('C:/Users/admin/Downloads/NLP4kec_1.4.0.zip', repos=NULL)
library(NLP4kec)
library(rJava)
library(multilinguer)
install_jdk()

install.packages('RWeka')
library(tm) # 말뭉치 생성, 말뭉치 전처리, DTM 생성
library(RWeka) # N-gram 생성
library(RColorBrewer) # 그래프 색상

# 자연어처리
data <- NLP4kec::r_parser_r(contentVector=crwal_data$content, language = "ko")
# 자연어 처리를 하면 데이터형식은 문자형이 된다
class(data)
# 자연어별로 단어 분리 확인
data[1]

# tm패키지로 말뭉치 만들기
data <- data %>% tm::VectorSource() %>% tm::VCorpus()

# 아래와 같이 데이터 형식이 바뀌어야 한다.
class(data)
# [1] "VCorpus" "Corpus" 
class(data[[1]])
# [1] "PlainTextDocument" "TextDocument"

# 다운로드 받은 한국 불용어 파일 불러오기
dic <- read.table(file = "data/한국어불용어100.txt",
                  sep = "\t",
                  fileEncoding = "UTF-8")

# 불러온 한국 불용어 파일에서 첫 번째 열(단어들)만 남기고 문자형으로 변환
dic <- dic[ , 1] %>% as.character()

# 그 외 여러 용도로 제외할 문자 모음 만들기
# 위 불용어 파일에 없는 글자들을 수동으로 입력
dic2 <- c("가지다", "걸리다", "그렇다", "나가다", "나오다", "나타나다", "늘어나다", "다녀오다", "다르다", "다양하다", "대하다", "돌보다", "드리다", "들어가다", "따르다", "떨어지다", "마시다", "못하다", "바라다", "보내다", "버리다", "밝히다", "보이다", "부탁드리다", "불가피하다", "알리다", "알아보다", "이기다", "위하다", "전하다", "지키다", "최대한", "지나다", "여러분", "인하다", "통하다", "있다", "하다", "되다", "주다", "만들다")

# N-gram => 한 단어에서만 제외할 문자
# 키워드가 카카오이므로 가장 많기때문에 제외,
# 카카오 기업과 관련없는 단어 제외
# 블로그 네이버는 해당 페이지에서 가져왔기때문에 제외
dic3 <- c("카카오", "콜라겐", "누르다", "그러다", "블로그", "네이버", "포스팅", "바르다", "에센스", "느끼다", "마사지","티스토리", "모르다", "촉촉하다")

# N-gram => 두, 세 단어에서만 제외할 문자
dic4 <- c("립 밤", "플레이 립 밤", "글 플레이 립", "맥 글 플레이", "세다 렉스 콜라겐", "디 뷰티 카카오톡", "느끼다 지다", "받다 보다", "미스 디 드", "디 드 퍼퓸")

# 위 제외할 문자를 사용하여 두 개의 말뭉치를 생성
# N-gram => 한 단어에서 사용할 data1
data1 <- tm::tm_map(data, FUN = stripWhitespace) %>%
  tm::tm_map(FUN = removeNumbers) %>% 
  tm::tm_map(FUN = removePunctuation) %>% 
  tm::tm_map(FUN = removeWords, words = c(dic, dic2, dic3))

# N-gram => 두 단어에서 사용할 data2
data2 <- tm::tm_map(data, FUN = stripWhitespace) %>%
  tm::tm_map(FUN = removeNumbers) %>%
  tm::tm_map(FUN = removePunctuation) %>%
  tm::tm_map(FUN = removeWords, words = c(dic, dic2, dic4))

# 이후 TDM 생성에서 활용할 N-Gram-Tokenizer를 만들어 주었다
NGramTokenizer2 <- function(x) unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse=" "), use.names=FALSE)
NGramTokenizer3 <- function(x) unlist(lapply(NLP::ngrams(words(x), 3), paste, collapse=" "), use.names=FALSE)

# 위 N-Gram-Tokenizer를 활용하여 TDM을 만들어 준다
# 이 TDM을 통해 빈도수가 높은 단어를 파악하고, 불용어가 있는지 점검해서 다시 말뭉치 전처리 과정을 반복했다
ng1 <- tm::TermDocumentMatrix(data1)
ng2 <- tm::TermDocumentMatrix(data2, control=list(tokenize=NGramTokenizer2))
ng3 <- tm::TermDocumentMatrix(data2, control=list(tokenize=NGramTokenizer3))

# 단어별 빈도수 확인
#최소 빈도수는 N-Gram 단어 개수에 따라 임의로 다르게 설정해주었다
w <- tm::findFreqTerms(ng1, lowfreq = 10)
w2 <- tm::findFreqTerms(ng2, lowfreq = 10)
w3 <- tm::findFreqTerms(ng3, lowfreq = 5)

# 아래와 같이 매트릭스 형태로 변경하고 내림차순 정렬을 하면
# 각 단어와 빈도수를 편하게 확인할 수 있다
wf <- rowSums(as.matrix(ng1[w,])) %>% sort(decreasing = TRUE)
wf
# 이 후 그래프와 워드 클라우드 생성을 위해 데이터 프레임으로 변환
wf <- data.frame(words = names(wf), frequency = wf)
wf

# 글자수 2개와 3개도 동일하게 해준다
wf2 <- rowSums(as.matrix(ng2[w2,])) %>% sort(decreasing = TRUE)
wf2 <- data.frame(words = names(wf2), frequency = wf2)
wf3 <- rowSums(as.matrix(ng3[w3,])) %>% sort(decreasing = TRUE)
wf3 <- data.frame(words = names(wf3), frequency = wf3)


# 바그래프로 시각화
bar <- function(x){
  x %>% 
    dplyr::slice(1:20) %>% 
    ggplot2::ggplot(mapping = aes(x = reorder(words, frequency), y = frequency)) +
    ggplot2::geom_bar(stat = "identity", fill = "gray") +
    ggplot2::theme_light() +
    ggplot2::labs(x = "Words", y = "Frequency") +
    ggplot2::coord_flip()
  }

bar(wf)
bar(wf2)
bar(wf3)


# 워드 클라우드를 생성
word <- function(x){
  wordcloud2::wordcloud2(x, color = "random-light", fontFamily = "NanumGothic",
                          size=1, rotateRatio = 0)
  }

word(wf)
word(wf2)
word(wf3)


########################################################################
########################################################################
# 두번째. view_카카오035720_info_df

# 파이썬에서 크롤링하여 수집한 데이터 분석 시각화작업

library(KoNLP)
library(wordcloud2)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)

useSejongDic()

setwd('C:/PythonStudy/데이터분석팀프로젝트')
getwd()

# 데이터 가져오기
crwal_data <- read.csv('crawl_data/view_카카오035720_info_df.csv', fileEncoding='utf-8')
nrow(crwal_data) # 92
View(crwal_data)

# 명사 분석
crwal_data2 <- sapply(crwal_data$content, extractNoun, USE.NAMES = F)
crwal_data2

# 벡터 변환
crwal_data3 <- unlist(crwal_data2)
crwal_data3

#-------- 필요없는 문자 제거

# 2자 이상만 추출
crwal_data4 <- Filter(function(x){nchar(x) >=2}, crwal_data3)
crwal_data4

# 특수문자 제거
crwal_data4 <- str_replace_all(crwal_data4,"[^[:alpha:]]","")
crwal_data4

### 제거할 단어 리스트 ###
gsub_list <- readLines('data/view_info_content_gsub_2.txt', encoding='UTF-8')
gsub_list

for(i in 1:length(gsub_list)){
  crwal_data4 <- gsub(gsub_list[i],"",crwal_data4)
} ###

# 공백 제거 후 데이터 프레임 생성
write(crwal_data4,"data/view_info_content_2.txt")
crwal_data5 <- read.table("data/view_info_content_2.txt")
crwal_data5

# 빈도 저장
wordcount <- table(crwal_data5)

# 정렬
View(sort(wordcount, decreasing = T))

# 빈도수 10개이상만 출력
freq10 <- wordcount %>% sort(wordcount, decreasing = T) %>% head(291)
freq10
head(as.data.frame(freq10),100)

# ------------------------------------------------------------------------

# 워드크라우드 시각화
wordcloud2(freq10, color = "random-light",
           backgroundColor = 'white',
           size=2.8,
           rotateRatio = 0)


# ggplot2 : 바 그래프
ggplot(head(as.data.frame(freq10),50), aes(x=reorder(crwal_data5, Freq), y=Freq)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("word") + ylab("frep") +
  ggtitle('"카카오 035720"검색 후 크롤링된 형태소 분석')


# ------------------------------------------------------------------------
# NLP4kec 패키지 이용하여 N-Gram 생성

# 데이터 가져오기
crwal_data <- read.csv('crawl_data/view_카카오035720_info_df.csv', fileEncoding='utf-8')
nrow(crwal_data) # 92행
sum(is.na(crwal_data$content)) # NA값 0개

# 한글을 제외한 모든 글자 삭제처리
crwal_data$content <- crwal_data$content %>% 
  stringr::str_remove_all(pattern = "[^가-힣]")

# 데이터 변경확인
crwal_data$content[1]

# 패키지설치
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_141')

install.packages('rJava')
install.packages('C:/Users/admin/Downloads/NLP4kec_1.4.0.zip', repos=NULL)
library(NLP4kec)
library(rJava)
library(multilinguer)
install_jdk()

install.packages('RWeka')
library(tm) # 말뭉치 생성, 말뭉치 전처리, DTM 생성
library(RWeka) # N-gram 생성
library(RColorBrewer) # 그래프 색상

# 자연어처리
data <- NLP4kec::r_parser_r(contentVector=crwal_data$content, language = "ko")
# 자연어 처리를 하면 데이터형식은 문자형이 된다
class(data)
# 자연어별로 단어 분리 확인
data[1]

# tm패키지로 말뭉치 만들기
data <- data %>% tm::VectorSource() %>% tm::VCorpus()

# 아래와 같이 데이터 형식이 바뀌어야 한다.
class(data)
# [1] "VCorpus" "Corpus" 
class(data[[1]])
# [1] "PlainTextDocument" "TextDocument"

# 다운로드 받은 한국 불용어 파일 불러오기
dic <- read.table(file = "data/한국어불용어100.txt",
                  sep = "\t",
                  fileEncoding = "UTF-8")

# 불러온 한국 불용어 파일에서 첫 번째 열(단어들)만 남기고 문자형으로 변환
dic <- dic[ , 1] %>% as.character()

# 그 외 여러 용도로 제외할 문자 모음 만들기
# 위 불용어 파일에 없는 글자들을 수동으로 입력
dic2 <- c("가지다", "걸리다", "그렇다", "나가다", "나오다", "나타나다", "늘어나다", "다녀오다", "다르다", "다양하다", "대하다", "돌보다", "드리다", "들어가다", "따르다", "떨어지다", "마시다", "못하다", "바라다", "보내다", "버리다", "밝히다", "보이다", "부탁드리다", "불가피하다", "알리다", "알아보다", "이기다", "위하다", "전하다", "지키다", "최대한", "지나다", "여러분", "인하다", "통하다", "있다", "하다", "되다", "주다", "만들다")

# N-gram => 한 단어에서만 제외할 문자
# 키워드가 카카오이므로 가장 많기때문에 제외,
# 카카오 기업과 관련없는 단어 제외
# 블로그 네이버는 해당 페이지에서 가져왔기때문에 제외
dic3 <- c("카카오", "누르다", "그러다", "블로그", "네이버", "포스팅", "바르다", "느끼다", "모르다", "촉촉하다",
          "삼성전자")

# N-gram => 두, 세 단어에서만 제외할 문자
dic4 <- c("톡 비즈", "멀다 트", "엔터 테", "테 멀다", "테 멀다 트", "엔터 테 멀다", "카카오 엔터 테", "스템 임 플란트",
          "모 메다 텀", "출처 네이버 금융")

# 위 제외할 문자를 사용하여 두 개의 말뭉치를 생성
# N-gram => 한 단어에서 사용할 data1
data1 <- tm::tm_map(data, FUN = stripWhitespace) %>%
  tm::tm_map(FUN = removeNumbers) %>% 
  tm::tm_map(FUN = removePunctuation) %>% 
  tm::tm_map(FUN = removeWords, words = c(dic, dic2, dic3))

# N-gram => 두 단어에서 사용할 data2
data2 <- tm::tm_map(data, FUN = stripWhitespace) %>%
  tm::tm_map(FUN = removeNumbers) %>%
  tm::tm_map(FUN = removePunctuation) %>%
  tm::tm_map(FUN = removeWords, words = c(dic, dic2, dic4))

# 이후 TDM 생성에서 활용할 N-Gram-Tokenizer를 만들어 주었다
NGramTokenizer2 <- function(x) unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse=" "), use.names=FALSE)
NGramTokenizer3 <- function(x) unlist(lapply(NLP::ngrams(words(x), 3), paste, collapse=" "), use.names=FALSE)

# 위 N-Gram-Tokenizer를 활용하여 TDM을 만들어 준다
# 이 TDM을 통해 빈도수가 높은 단어를 파악하고, 불용어가 있는지 점검해서 다시 말뭉치 전처리 과정을 반복했다
ng1 <- tm::TermDocumentMatrix(data1)
ng2 <- tm::TermDocumentMatrix(data2, control=list(tokenize=NGramTokenizer2))
ng3 <- tm::TermDocumentMatrix(data2, control=list(tokenize=NGramTokenizer3))

# 단어별 빈도수 확인
#최소 빈도수는 N-Gram 단어 개수에 따라 임의로 다르게 설정해주었다
w <- tm::findFreqTerms(ng1, lowfreq = 10)
w2 <- tm::findFreqTerms(ng2, lowfreq = 10)
w3 <- tm::findFreqTerms(ng3, lowfreq = 5)

# 아래와 같이 매트릭스 형태로 변경하고 내림차순 정렬을 하면
# 각 단어와 빈도수를 편하게 확인할 수 있다
wf <- rowSums(as.matrix(ng1[w,])) %>% sort(decreasing = TRUE)
wf
# 이 후 그래프와 워드 클라우드 생성을 위해 데이터 프레임으로 변환
wf <- data.frame(words = names(wf), frequency = wf)
wf

# 글자수 2개와 3개도 동일하게 해준다
wf2 <- rowSums(as.matrix(ng2[w2,])) %>% sort(decreasing = TRUE)
wf2 <- data.frame(words = names(wf2), frequency = wf2)
wf3 <- rowSums(as.matrix(ng3[w3,])) %>% sort(decreasing = TRUE)
wf3 <- data.frame(words = names(wf3), frequency = wf3)


# 바그래프로 시각화
bar <- function(x){
  x %>% 
    dplyr::slice(1:20) %>% 
    ggplot2::ggplot(mapping = aes(x = reorder(words, frequency), y = frequency)) +
    ggplot2::geom_bar(stat = "identity", fill = "gray") +
    ggplot2::theme_light() +
    ggplot2::labs(x = "Words", y = "Frequency") +
    ggplot2::coord_flip()
}

bar(wf)
bar(wf2)
bar(wf3)


# 워드 클라우드를 생성
word <- function(x){
  wordcloud2::wordcloud2(x, color = "random-light", fontFamily = "NanumGothic",
                         size=1, rotateRatio = 0)
}

word(wf)
word(wf2)
word(wf3)


########################################################################
########################################################################
# 세번째. discuss_2022_2020_df

# 파이썬에서 크롤링하여 수집한 데이터 분석 시각화작업

library(KoNLP)
library(wordcloud2)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)

useSejongDic()

setwd('C:/PythonStudy/데이터분석팀프로젝트')
getwd()

# 데이터 가져오기
crwal_data <- read.csv('crawl_data/discuss_2022_2020_df.csv', fileEncoding='utf-8')
nrow(crwal_data) # 92836
View(crwal_data)

## 도배글 찾기 n_distinct은 고유값찾기
sum(duplicated(crwal_data$제목)) # [1] 12942
sum(!duplicated(crwal_data$제목)) # [1] 79894

crwal_data[!duplicated(crwal_data$제목),]$제목
crwal_data2 <- crwal_data[!duplicated(crwal_data$제목),]$제목
View(crwal_data2)
typeof(crwal_data2) # [1] "list"
length(crwal_data2)


# 명사 분석
crwal_data3 <- sapply(crwal_data2, extractNoun, USE.NAMES = F)
crwal_data3

# 벡터 변환
crwal_data4 <- unlist(crwal_data3)
crwal_data4

#-------- 필요없는 문자 제거

# 2자 이상만 추출
crwal_data5 <- Filter(function(x){nchar(x) >=2}, crwal_data4)
crwal_data5

# 특수문자 제거
crwal_data5 <- str_replace_all(crwal_data5,"[^[:alpha:]]","")
crwal_data5

### 제거할 단어 리스트 ###
gsub_list <- readLines('data/discuss_2022_2020_gsub.txt', encoding='UTF-8')
gsub_list

for(i in 1:length(gsub_list)){
  crwal_data5 <- gsub(gsub_list[i],"",crwal_data5)
} ###

# 공백 제거 후 데이터 프레임 생성
write(crwal_data5,"data/discuss_2022_2020.txt")
crwal_data6 <- read.table("data/discuss_2022_2020.txt")
crwal_data6

# 빈도 저장
wordcount <- table(crwal_data6)

# 정렬
View(sort(wordcount, decreasing = T))

# 빈도수 100개이상만 출력
freq10 <- wordcount %>% sort(wordcount, decreasing = T) %>% head(250)
freq10
head(as.data.frame(freq10),100)

# ------------------------------------------------------------------------

# 워드크라우드 시각화
wordcloud2(freq10, color = "random-light",
           backgroundColor = 'white',
           size=2,
           rotateRatio = 0)


# ggplot2 : 바 그래프
ggplot(head(as.data.frame(freq10),50), aes(x=reorder(crwal_data5, Freq), y=Freq)) +
  geom_bar(stat = "identity") + coord_flip() +
  xlab("word") + ylab("frep") +
  ggtitle('"카카오 035720"검색 후 크롤링된 형태소 분석')


# ------------------------------------------------------------------------
# NLP4kec 패키지 이용하여 N-Gram 생성

# 데이터 가져오기
crwal_data <- read.csv('crawl_data/discuss_2022_2020_df.csv', fileEncoding='utf-8')
nrow(crwal_data) # 92836
sum(is.na(crwal_data$제목)) # NA값 0개

# 한글을 제외한 모든 글자 삭제처리
crwal_data2 <- crwal_data2 %>% 
  stringr::str_remove_all(pattern = "[^가-힣]")

# 데이터 변경확인
crwal_data2[1]

# 패키지설치
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_141')
install_jdk()
install.packages('rJava')
install.packages('C:/Users/admin/Downloads/NLP4kec_1.4.0.zip', repos=NULL)
install.packages('RWeka')
library(NLP4kec)
library(rJava)
library(multilinguer)
library(tm) # 말뭉치 생성, 말뭉치 전처리, DTM 생성
library(RWeka) # N-gram 생성
library(RColorBrewer) # 그래프 색상

# 자연어처리
data <- NLP4kec::r_parser_r(contentVector=crwal_data2, language = "ko")
# 자연어 처리를 하면 데이터형식은 문자형이 된다
class(data)
# 자연어별로 단어 분리 확인
data[1]

# tm패키지로 말뭉치 만들기
data <- data %>% tm::VectorSource() %>% tm::VCorpus()

# 아래와 같이 데이터 형식이 바뀌어야 한다.
class(data)
# [1] "VCorpus" "Corpus" 
class(data[[1]])
# [1] "PlainTextDocument" "TextDocument"

# 다운로드 받은 한국 불용어 파일 불러오기
dic <- read.table(file = "data/한국어불용어100.txt",
                  sep = "\t",
                  fileEncoding = "UTF-8")

# 불러온 한국 불용어 파일에서 첫 번째 열(단어들)만 남기고 문자형으로 변환
dic <- dic[ , 1] %>% as.character()

# 그 외 여러 용도로 제외할 문자 모음 만들기
# 위 불용어 파일에 없는 글자들을 수동으로 입력
dic2 <- c("가지다", "걸리다", "그렇다", "나가다", "나오다", "나타나다", "늘어나다", "다녀오다", "다르다", "다양하다", "대하다", "돌보다", "드리다", "들어가다", "따르다", "떨어지다", "마시다", "못하다", "바라다", "보내다", "버리다", "밝히다", "보이다", "부탁드리다", "불가피하다", "알리다", "알아보다", "이기다", "위하다", "전하다", "지키다", "최대한", "지나다", "여러분", "인하다", "통하다", "있다", "하다", "되다", "주다", "만들다")

# N-gram => 한 단어에서만 제외할 문자
# 키워드가 카카오, 주식이므로 가장 많기때문에 제외,
# 카카오 기업과 관련없는 단어 제외
dic3 <- c("카카오", "주식", "이재명")

# N-gram => 두, 세 단어에서만 제외할 문자
dic4 <- c("삭제 시물 답글", "시물 답글 카카오", "안티 알다 바", "성탄절 토요일 일요일", "일요일 월요일 공휴일",
          "월요일 공휴일 지정", "답글 삭제 시물", "시물 답글 삭제", "삭제 시물 답글", "기쁘다 감사 말씀", 
          "유영두 옆 작두", "순종 기쁘다 감사", "민주 단망 일기도", "삭제 시물", "시물 답글", "깨다 지다", 
          "말씀 감사", "시황 속보", "깨다 지다", "차다 티", "관계자 아가 분유", "끝 나다", "줄 알다",
          "훠 훠 훠", "민주 당망 일기도", "없다 안티 알바", "안티 알바 충")

# 위 제외할 문자를 사용하여 두 개의 말뭉치를 생성
# N-gram => 한 단어에서 사용할 data1
data1 <- tm::tm_map(data, FUN = stripWhitespace) %>%
  tm::tm_map(FUN = removeNumbers) %>%
  tm::tm_map(FUN = removePunctuation) %>%
  tm::tm_map(FUN = removeWords, words = c(dic, dic2, dic3))

# N-gram => 두 단어에서 사용할 data2
data2 <- tm::tm_map(data, FUN = stripWhitespace) %>%
  tm::tm_map(FUN = removeNumbers) %>%
  tm::tm_map(FUN = removePunctuation) %>%
  tm::tm_map(FUN = removeWords, words = c(dic, dic2, dic4))

# 이후 TDM 생성에서 활용할 N-Gram-Tokenizer를 만들어 주었다
NGramTokenizer2 <- function(x) unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse=" "), use.names=FALSE)
NGramTokenizer3 <- function(x) unlist(lapply(NLP::ngrams(words(x), 3), paste, collapse=" "), use.names=FALSE)

# 위 N-Gram-Tokenizer를 활용하여 TDM을 만들어 준다
# 이 TDM을 통해 빈도수가 높은 단어를 파악하고, 불용어가 있는지 점검해서 다시 말뭉치 전처리 과정을 반복했다
ng1 <- tm::TermDocumentMatrix(data1)
ng2 <- tm::TermDocumentMatrix(data2, control=list(tokenize=NGramTokenizer2))
ng3 <- tm::TermDocumentMatrix(data2, control=list(tokenize=NGramTokenizer3))

# 단어별 빈도수 확인
#최소 빈도수는 N-Gram 단어 개수에 따라 임의로 다르게 설정해주었다
w <- tm::findFreqTerms(ng1, lowfreq = 10)
w2 <- tm::findFreqTerms(ng2, lowfreq = 10)
w3 <- tm::findFreqTerms(ng3, lowfreq = 5)

# 아래와 같이 매트릭스 형태로 변경하고 내림차순 정렬을 하면
# 각 단어와 빈도수를 편하게 확인할 수 있다
wf <- rowSums(as.matrix(ng1[w,])) %>% sort(decreasing = TRUE)
# 이 후 그래프와 워드 클라우드 생성을 위해 데이터 프레임으로 변환
wf <- data.frame(words = names(wf), frequency = wf)

# 글자수 2개와 3개도 동일하게 해준다
wf2 <- rowSums(as.matrix(ng2[w2,])) %>% sort(decreasing = TRUE)
wf2 <- data.frame(words = names(wf2), frequency = wf2)
wf3 <- rowSums(as.matrix(ng3[w3,])) %>% sort(decreasing = TRUE)
wf3 <- data.frame(words = names(wf3), frequency = wf3)


# 바그래프로 시각화
bar <- function(x){
  x %>% 
    dplyr::slice(1:20) %>% 
    ggplot2::ggplot(mapping = aes(x = reorder(words, frequency), y = frequency)) +
    ggplot2::geom_bar(stat = "identity", fill = "gray") +
    ggplot2::theme_light() +
    ggplot2::labs(x = "Words", y = "Frequency") +
    ggplot2::coord_flip()
}

bar(wf)
bar(wf2)
bar(wf3)


# 워드 클라우드를 생성
word <- function(x){
  wordcloud2::wordcloud2(x, color = "random-light", fontFamily = "NanumGothic",
                         size=1, rotateRatio = 0)
}

word(wf)
word(wf2)
word(wf3)