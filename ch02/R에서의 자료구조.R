# [빅데이터를 활용한 스마트 데이터 전문가 양성과정]
# [제대로 알고 쓰는 통계분석 R]
# 2019-06-18-(화) / 368page


#  벡터(Vector) : 단일 값들이 한 군데 모여 있는 자료구조
# 위치와 키워드
seq(from=-3,to=3,by=1)
seq(-3,3,length.out=61) # 61개
seq(-3,3,0.1)           # 0.1간격


#  팩터(Factor) : 질적 자료를 저장하는 자료구조

# factor(x=character(),levels,labels=levels,ordered=F)
# levels  : 주어진 데이터 중 FACTOR의 각 값(수준)으로 할 값을 벡터 형태로 지정 (빠진값은 NA처리)
# labels  : 실제 값 외에 사용할 각 수준의 이름(벡터), 예를 들어 데이터에서 1 = 남자
# ordered : 순위형 자료여부 T/F

x<-c(1:5)
factor(x,levels=c(1:4))
factor(x,levels=c(1:4),labels=c('a','b','c','d'))
factor(x,levels=c(1:4),labels=c('a','b','c','d'),ordered=T)

weekend <- c(1:7)
factor(weekend,levels=c(1:length(weekend)))
factor(weekend,levels=c(1:length(weekend)),
       labels=c('일','월','화','수','목','금','토'),
       ordered=T)


#  데이터 프레임(data.frame) : 형태가 서로다른 벡터가 열로 구성된 자료구조.
name   <- c('철수','영희','길동')
age    <- c(21,20,31)
gender <- factor(c('M','F','M'))
# factor를 지정해 줄 수 있으나 굳이 factor처리하지 않아도 구분(levels)될 수 있다

person <- data.frame(name, age, gender)
person$name
person$age
person$gender

person[3,1]


# https://mdis.kostat.go.kr/index.do

#  외부로부터 자료 가져오기 37page

setwd('D:/dudwlsrla92/R_Statistics/ch02')
getwd()

data <- read.csv("ch02.csv",header=F,na.strings=c("."))
str(data)
data$V1 <- factor(data$V1, levels=c(1,2),
                  labels=c('남자','여자'))
data$V3 <- factor(data$V3, levels=1:14,
                  labels=c('가구주','가구주의 배우자','자녀',
                           '자녀의 배우자','가구주의 부모',
                           '배우자의 부모','손자녀, 그 배우자',
                           '증손자녀, 그 배우자','조부모',
                           '형제자매, 그 배우자',
                           '형제자매의 자녀, 그 배우자',
                           '부모의 형제자매, 그 배우자','기타 친인척',
                           '그외같이사는사람'))
data$V4 <- factor(data$V4, levels=1:8,
                  labels=c('안 받았음','초등학교','중학교',
                           '고등학교','대학-4년제 미만','대학-4년제 이상',
                           '석사과정','박사과정'))
str(data)
save.image("data.rda")
