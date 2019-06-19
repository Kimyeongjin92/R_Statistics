## ===================================================================================
## [빅데이터를 활용한 스마트 데이터 전문가 양성과정]  ================================
## [제대로 알고 쓰는 통계분석 R]                      ================================
## 2019-06-19-(수) / 63page                           ================================
## ===================================================================================

### ===================================================================================
###  2. 모수와 통계량   ==============================================================
### ===================================================================================

setwd('D:/dudwlsrla92/R_Statistics/ch02')
getwd()

ranicafe <- read.csv('cafedata.csv',stringsAsFactors = F)
str(ranicafe)
dim(ranicafe)

## 1. 최댓값과 최솟값: ranicafe 커피판매량
ranicafe$Coffees<-as.numeric(ranicafe$Coffees)
sort(ranicafe$Coffees)
sort(ranicafe$Coffees)[1]
sort(ranicafe$Coffees,decreasing=T)
sort(ranicafe$Coffees,decreasing=T)[1]
min(ranicafe$Coffees,na.rm=T)
max(ranicafe$Coffees,na.rm=T)


## 2. 최빈값: ranicafe
stem(ranicafe$Coffees)
hist(ranicafe$Coffees)
# 줄기-잎 그림을 통해 최빈값은 4라는 것을 알 수 있다.


## 2. 평균과 중앙값: ranicafe
### 대푯값 : 전체자료를 대표하는 값으로 중심 위치를 나타내는 특성을 사용합니다.
### 자료의 중심은 무게중심:(산술)평균, 순서상 중간:중앙값
### 평균은 이상치에 약하다.

# 1-1) 평균
rc <- ranicafe$Coffees
weight <-1/(length(rc)-1)   # NA 개수를 빼주어야 해서 -1 해줌.
sum(rc*weight,na.rm=T)      # 평균
mean(rc, na.rm=T)

# 1-2) 이상치가 있는 평균
rc[rc == max(rc,na.rm=T)] <- 480 # 이상치를 넣은 상태.
mean(rc, na.rm=T)

# 2-1) 중앙값
median.idx <- (1+length(rc)-1)/2 # ex) 1,2,3,4,5에서 3은 (1+5)/2로 구할 수 있다.
sort(rc)[median.idx]

median(rc,na.rm=T)

