## ===================================================================================
## [빅데이터를 활용한 스마트 데이터 전문가 양성과정]  ================================
## [제대로 알고 쓰는 통계분석 R]                      ================================
## 2019-06-26(수) / 여러 모집단의 평균 비교 검정 262page  ============================
## ===================================================================================


### ================================================================================
###  0. 데이터 프레임 다루기 (243Page) ============================================
### ================================================================================

data <- read.table("http://www.amstat.org/publications/jse/datasets/babyboom.dat.txt",header=F)

# 1) 구조확인하기
nrow(data)
ncol(data)
dim(data)
str(data)      # int(숫자형)
summary(data)  # 요약통계
head(data,5)
tail(data,5)
table(data$gender)

# 2) 변수명 확인 및 변경
names(data)
row.names(data)
col.names(data)
names(data)    <- c("time","gender","weight","minutes")
names(data)[1] <- "time.24Hrs"

# 3) 열 추출하기.
g1 <- data$time.24Hrs
g2 <- data$gender
g3 <- data$weight
g4 <- data$minutes

# 4) 행 추출하기.
data[data$gender==2 & data$weight > mean(data$weight), ]
subset(data,gender==2 & data$weight > mean(data$weight))

# 5) 행열 추출하기.
data[data$gender==2 & data$weight > mean(data$weight), c(2,4)]

# 6) 데이터 정제하기
data$minutes <- ifelse(data$minutes >= 1000, NA, data$minutes)

# 7) 데이터 프레임 저장하기.
chapter7 <- data[,c(2,3)]
write.table(chapter7, "chapter7.txt")
write.table(chapter7, "chapter7(2).txt",row.names=F) # 행번호 제거

# 8) 데이터 프레임 불러오기.
write.csv("data.txt",header=T,na.string=c('99'))     # 99에 해당하는 데이터를 NA처리

# 9) 결측 판별 함수(is.na)
is.na(c(1, NA, 3, NA, 5)) # NA = TURE로 표시된다.
nonNA.sum <- sum(data$minutes[!is.na(data$minutes)])

# 10) factor형 자료 처리
data$gender<-factor(data$gender)
summary(data)
str(data)

# 11) summaryBy를 이용한 통계
install.packages("doBy")
library(doBy)
summaryBy(weight~gender, data=data, FUN=c(mean, sd), na.rm=T)
# 통계를 구할 변수 ~ 집단을 구분할 변수, 데이터, FUN=수식(length, mean, sd 등)


### ================================================================================
###  1. 모집단이 두개(서로 독립인 두 집단, 서로 대응인 두 집단) (262Page) =========
### ================================================================================