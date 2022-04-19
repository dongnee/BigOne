getwd()
setwd('C:/Rstudy/data')

library(KoNLP)
library(wordcloud)
library(wordcloud2)
library(stringr)
library(RColorBrewer)
library(readxl)
library(xlsx)
library(tidyverse)
library(ggplot2)

useSejongDic()

# 뉴스후반부 파일 불러오기
news_second <- read.csv('뉴스후반부.txt', encoding = 'UTF-8')
View(news_second)

class(news_second)

# 엑셀 파일로 저장
write.xlsx(news_second,"news_second.xlsx", sheetName = 'Sheet1')

# 엑셀 파일 불러와서 그중 제목만 뽑아내기
news_second <- read_excel('news_second.xlsx')
View(news_second)

class(news_second)

titles <- news_second[,2]
class(titles)
names(titles)
names(titles) <- ('titledate')

write.csv(titles, 'titles.txt')

# 타이틀 파일 data1으로 불러오기
data1 <- readLines('titles.txt')
data1

# 그중 명사만 추출
data2 <- sapply(data1, extractNoun, USE.NAMES=F)
data2

class(data)

# list 형태 -> vector로 변환
data3 <- unlist(data2)
data3

class(data3)

# 데이터 정제 -> 문자 제외 모두 삭제
data4 <- str_replace_all(data3, '[^[:alpha:]]', "")
data4 <- gsub(' ','',data4)
data4 <- Filter(function(x){nchar(x)>=2}, data4)
data4

class(data4)


gsub_list <- readLines('gsub_news_list.txt', encoding='UTF-8')
gsub_list

for(i in 1:length(gsub_list)){
  data4 <- gsub(gsub_list[i],'',data4)
}

# 파일로 저장 후 읽어오기
write(data4, 'news_wordcloud.txt')

# 빈도 확인
wordcount <- table(data4)
wordcount

# 정렬 후 확인
View(sort(wordcount, decreasing = T))

wordcount <- sort(wordcount,decreasing=T)
wordcount

wordcount[1:300]

write(wordcount, 'wordcount.txt')

wordcloud2(data = wordcount[1:300] , 
           color = "random-dark" ,
           shape = "cloud" , 
           size = 4.5, 
           fontFamily = "나눔고딕")

wordcount <- head(wordcount,21)
class(wordcount)
wordtable <- as.data.frame(wordcount)
wordtable <- wordtable[-1,]

ggplot(wordtable, aes(x = data4, y = Freq)) + 
  geom_col() + coord_flip() +
  labs(title='네이버 뉴스에서 자주 언급되는 상위 20개 단어(카카오제외)')



