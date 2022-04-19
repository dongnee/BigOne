##### 뉴스 파일 합치기 #####

# 20.1.1 ~ 21.2.28 앞부분 불러오기
news_front_part <- read_csv('news_front.txt')
View(news_front_part)

# 21.3.1 ~ 22.4.13 뒷부분도 정제 및 정렬
news_back_part <- read_csv('back_sort.csv')
View(news_back_part)
class(news_back_part)

library(lubridate)

news_back_part['날짜']

news_back <- news_back_part
news_back <- news_back[order(news_back$날짜),]


news_back$날짜[1:2110] <- 0
news_back$날짜[8786:13490] <- 0
news_back <- news_back[order(news_back$날짜),]
View(news_back)

news_data <- rbind(news_front_part, news_back)
View(news_data)

news_data <- news_data[order(news_data$날짜),]
View(news_data)

write.csv(news_data, file='news_data.csv')


# 뉴스 제목만 뽑아 파일로 저장후 불러오기
title <- news_data[,2]
write.csv(title, 'title.txt')

title <- readLines('title.txt')
title

# 그중 명사만 추출
data2 <- sapply(title, extractNoun, USE.NAMES=F)
data2

# list 형태 -> vector로 변환
data3 <- unlist(data2)
data3

# 데이터 정제 -> 문자 제외 모두 삭제
data4 <- str_replace_all(data3, '[^[:alpha:]]', "")
data4 <- gsub(' ','',data4)
data4 <- Filter(function(x){nchar(x)>=2}, data4)

gsub_list <- readLines('gsub_news_list.txt', encoding='UTF-8')
gsub_list

for(i in 1:length(gsub_list)){
  data4 <- gsub(gsub_list[i],'',data4)
}

# 빈도 확인
wordcount <- table(data4)
wordcount

# 파일로 저장 후 읽어오기
write(data4, 'news_all_wordcloud.txt')

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









