
<br>

# 뉴스와 주가의 연관성

<br>

### 프로젝트 기간 
2022.04.11 ~ 2022.4.29 (주말제외 15일)

팀원 : 김도연, 김세윤, 박균탁, 장은영

- 발표자료  
	- https://github.com/somijjjjj/BigOne/blob/eunyoung/1%EC%A1%B0%20%EB%B0%9C%ED%91%9C%EC%9E%90%EB%A3%8C.pdf

<br>

### 주제 선정
- 코로나 이후 더 활발해진 주식 시장에
뉴스가 미치는 영향이 있는지
파악해보고자 주제 선정
- 코로나 이후 주가의 변동폭이 큰 기업 중 하나이고, 이슈가 많은 기업이라는 이유로 **"카카오"** 으로 선정함


<br>

### 배경

![](https://velog.velcdn.com/images/silver0/post/dc0c43a1-d0bb-4fa6-908f-977b203c62c9/image.png)

많은 사람들이 뉴스를 통해 주식의 매도/매수를 결정하는 자료를 바탕으로 뉴스가 주가에 미치는 영향에 대해 파악하기로 했다.

![](https://velog.velcdn.com/images/silver0/post/c7fa9585-828e-4aca-857b-e58c393f51b7/image.png)

주식시장이 코로나가 창궐한 시기에 거래가 개인 주식 순매수 100조에 육박할 정도로 활발했었다.

![](https://velog.velcdn.com/images/silver0/post/982f311e-5002-4793-818b-9a348b18d501/image.png)

이 시기에 주가 변동폭이 큰 기업 중 하나인 '카카오'로 선정하였고, 이슈가 많은 기업이기에 유의미한 결과가 있으리라고 생각했다.

---

<br>

#### 종목토론실에서 나타난 단어 빈도 워드크라우드

![](https://velog.velcdn.com/images/silver0/post/b7be741f-d8b6-4fcc-8a73-a611589ead8a/image.png)

#### n-gram으로 자연언어처리을 통해 표현된 단어 빈도 분석 
![](https://velog.velcdn.com/images/silver0/post/e9752bf4-26db-4a1e-ae24-d1e765a5bfbc/image.png)





<br>

### 프로젝트 계획

> 
1. 참고자료 및 데이터 수집
	- 뉴스 크롤링 및 전처리
    - 트위터 크롤링
    - 종목토론실, 네이버view 크롤링 
    - 카카오 재무제표 수집 및 주가 시각화
    - 크롤링 텍스트마이닝 시각화 
2. 데이터 가공 및 단어사전 구축, 감성분석
	- 수집데이터 형태소 분석 및 단어별 감정분류 
	- KNU 감성사전 감정단어 추가 
3. 예측 모델 및 선형회귀 모델 생성 
	- GRU 모델링
    - 다중 회귀분석 모델링
    - ARIMA 모델링
    - LSTM 모델링 
4. 모델 비교 및 평가 


<br>

### 기술스택

> 개발언어 : Python, R  
개발도구 : jupyter notebook, R studio  
협업툴 : Git

<br>
<br>

# 구현

### 데이터 분석 기간 
 2020.1.1 ~ 2022.4.13 

<br>

### **데이터 수집에 이용한 라이브러리**  

> 크롤링 : selenium, BeautifulSoup  
트위터 오픈소스 : [snscreape](https://github.com/JustAnotherArchivist/snscrape) 

<br>

### **수집된 데이터크기** 
> 뉴스 : (14,890＊4)  
종목토론실 : (223,164＊5)  
네이버 view : (296＊3)  
트위터 : (3802＊4)  

<br>


## 모델링

![](https://velog.velcdn.com/images/silver0/post/ebde9eb1-d0aa-4d88-aedc-630ceeccaef6/image.png)

## 평가

![](https://velog.velcdn.com/images/silver0/post/c82f3cf4-16b7-4929-998d-677858401181/image.png)

R2(결정계수)가 모델의 대한 설명력이므로
> LSTM  >  GRU  >  다중회귀분석 > ARIMA

모델의 설명력은 LSTM이 가장 높았고,
뉴스의 긍정/부정만 가지고는 주가를 예측하기엔 힘들다는 걸 알 수 있었다.


<br>
<br>

# 💭Project Review

### 잘한 점
선정한 주제 자체가 수업에 배우지 않았던 내용이라 방향을 잡기 어려웠지만  
팀원 모두가 주제에 대해 머리를 맞대어 해결해야 할 문제와 개념들을 쪼개어 좁혀나감으로써  
프로젝트를 완성 시키고 마무리할 수 있었다고 생각합니다.

강의에서 배운 분석 기법 이외의 것도 학습함으로써, 주제에 적용 시켜 볼 수 있었습니다.

팀원 모두가 각자 맡은 역할을 충실히 수행하여 일정을 지키며 진행할 수 있었습니다.

### 아쉬운 점
딥러닝 기반 모델에 대해 이해도가 낮아 충분한 모델 설명이 안된듯하여 아쉽지만 이번 프로젝트 경험을 통해 머신러닝과 그나마 가까워진 듯 하여 만족스럽습니다.

직접 수집하고 가공한 긍정/부정 단어를 분석에 더욱 활용하지 못한 것이 아쉽습니다.

예측 모델에 인풋 데이터로 지난 sns언급량 등을 추가하여 여러 데이터로 모델을 구성을 해보았으면 좋았을 거 같습니다.



<br>
<br>

### Reference

- 주가 예측 관련 글

https://ohshinyeop.tistory.com/13 

[[LSTM/GRU] 주식가격 예측 모델 구현](https://data-analysis-expertise.tistory.com/67)  

[주식, 비트코인 예측 : 머신러닝 분류모델 : 개념, 종류, 특징](https://jjeongil.tistory.com/673)  

[[파이썬]딥러닝(LSTM)을 이용한 'Apple' 주가 예측하기](https://aalto.tistory.com/6)   


[[파이썬] 예측모델 (LSTM 모델 주가예측)](https://post.naver.com/viewer/postView.nhn?volumeNo=29132930&memberNo=18071586)    


[주가 예측 모델 생성시 주의사항](https://codingapple.com/unit/deep-learning-stock-price-ai/)  


[주가 예측모델 예시(RNN, LSTM)](https://diane-space.tistory.com/331)  


[비트코인 예측](https://github.com/Maker-Kim/Study/blob/master/python/Coin_Ai.ipynb)  


[주가 데이터 불러오는 법](https://dacon.io/codeshare/3710?dtype=recent)  


[20년 이후로 주식을 시작한 사람들의 수익 분석 및 주식 투자 예측](https://dacon.io/codeshare/2150?dtype=recent)  


[주가예측 그리기](https://muscleking3426.tistory.com/48)  


[LSTM을활용한주가예측모델](https://colab.research.google.com/github/teddylee777/machine-learning/blob/master/04-TensorFlow2.0/01-%EC%82%BC%EC%84%B1%EC%A0%84%EC%9E%90-%EC%A3%BC%EA%B0%80%EC%98%88%EC%B8%A1/02-LSTM-stock-forecasting-with-LSTM-financedatareader.ipynb#scrollTo=e1O7vDdihUs4)  


[ARIMA모델](https://rdmkyg.blogspot.com/2021/08/autoarima-nasdaq-100.html)


<br>

- 감성분석 관련 글

[[NLP] 텍스트 분류와 감성(Sentiment)분석 구현하기](https://techblog-history-younghunjo1.tistory.com/111)  


[KNU 한국어 감성사전 - 긍/부정 단어](https://github.com/park1200656/KnuSentiLex)   


[긍/부정 분류](https://projectlog-eraser.tistory.com/21) 
→ 주가 특성상 수치로 표현된 기사 제목들도 많으므로, 수치에 대한 기준도 필요할 것

[R. 텍스트 마이닝 _ 감정 분석](https://velog.io/@yuhayung7296/R.-%ED%85%8D%EC%8A%A4%ED%8A%B8-%EB%A7%88%EC%9D%B4%EB%8B%9D-%EA%B0%90%EC%A0%95-%EB%B6%84%EC%84%9D) 


<br>

- 자연어 처리

https://blog.naver.com/dalgoon02121/222051184805  

https://wonhwa.tistory.com/35

<br>

- 기타

https://right1203.github.io/study/2018/09/12/sentiment-analysis-1/





