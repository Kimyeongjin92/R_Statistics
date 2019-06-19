## [빅데이터를 활용한 스마트 데이터 전문가 양성과정]
## [제대로 알고 쓰는 통계분석 R]
## 2019-06-18-(화) / 368page


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


# ================================================================================
##  외부로부터 자료 가져오기 / 37page   ========================================
##  https://mdis.kostat.go.kr/index.do  ========================================
# ================================================================================

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
View(data)


## ===================================================================================
## [빅데이터를 활용한 스마트 데이터 전문가 양성과정]  ================================
## [제대로 알고 쓰는 통계분석 R]                      ================================
## 2019-06-19-(수) / 50page                           ================================
## ===================================================================================


### ===================================================================================
###  1. 기초통계량 50Page  ===========================================================
### ===================================================================================

library(ggplot2)


## 1. 산점도: cars
cars
str(cars)

par(mfrow=c(1,2))
plot(cars$speed, cars$dist,
     main="속도와 제동거리",xlab="속도(mph)",ylab="제동거리(ft)",
     pch=1, col="red",)
plot(jitter(cars$speed), jitter(cars$dist),
     main="속도와 제동거리",xlab="속도(mph)",ylab="제동거리(ft)",
     pch=1, col="red",) 
# ggplot() + geom_point(position="jitter") 혹은 geom_jitter()


## 2. 시계열: Nile(1871~1970 연도별 나일강 유량)
Nile
str(Nile)
length(Nile)
Nile

# 1-1) Nile을 Dataframe으로 변환 (1)
Nile<-as.data.frame(Nile)
Nile$Year  <- c(1871:1970)

# 1-2) Nile을 Dataframe으로 변환 (2)
df_Nile <- data.frame(year =time(Nile),
                      flood=as.matrix(Nile))
head(df_Nile)

# 2) ggplot 
ggplot(Nile,aes(x=Year,y=x))+
        geom_line(color="steel blue",size=1)+
        ggtitle('연도별 나일강의 유량 변화') +
        theme_bw(base_family="baedal",base_size = 20)


## 3. 막대그래프: data.rda 높이
load("data.rda")
tableV5 <- table(data$V5)
barplot(tableV5,main="출생아(남자)별 빈도",xlab="출생아수",ylab="빈도")
# height :막대의 높이, width : 넓이

tableV1.V4 <- table(data$V1,data$V4)
barplot(tableV1.V4,main="학력에 따른 성별 인원수",xlab="학력",ylab="빈도")


## 4. 히스토그램: data.rda 폭
hist(data$V2, main="연령별 분포", xlab="연령", ylab="빈도")
hist(data$V2, breaks=c(seq(0,90,10)),right=F,                 # 밀도(개수)
     main="연령별 분포", xlab="연령", ylab="빈도")
hist(data$V2, probability=T, breaks=c(seq(0,90,10)),right=F,  # probability 빈도(전체의 합이 1)
     main="연령별 분포", xlab="연령", ylab="빈도")            # right=F는 0 <= age <10 처럼 오른쪽은 포함하지 않겠다는 뜻.


## 5. 원 그래프: data.rda 폭
pie(table(data$V4),main="학력수준별 비중",cex=0.9)
