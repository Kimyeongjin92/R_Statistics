## ===================================================================================
## [빅데이터를 활용한 스마트 데이터 전문가 양성과정]  ================================
## [제대로 알고 쓰는 통계분석 R]                      ================================
## 2019-06-21-(금) / 표본분포 151page                          =======================
## ===================================================================================


### ================================================================================
###  1. 표본분포 (152Page)  =======================================================
### ================================================================================

m10 <- rep(NA,1000)
m40 <- rep(NA,1000)

set.seed(9)
for(i in 1:1000){
  m10[i] <- mean(rnorm(10)) # 0과 1을 안붙이면 표준 정규분포가 된다.
  m40[i] <- mean(rnorm(40))
}

# 표본평균 분포의 평균과 표준편차
option(digits=4)
c(mean(m10),sd(m10))
c(mean(m40),sd(m40))

# 그래프 (표본 크기가 클수록 기댓값(모집단 평균)주변에 많이 몰려있으며 자료가 분포하는 전체 폭이 줄어듦)
par(mfrow=c(1,2))
hist(m10,col="cyan",border="blue",main="n=10")
hist(m40,col="red",border="blue",main="n=40")


### ================================================================================
###  2. 중심극한정리 (160Page)  ===================================================
### ================================================================================

# 모집단이 정규분포일 때
set.seed(9)
n <- 1000
r.1.mean <- rep(NA,n)
r.2.mean <- rep(NA,n)

for (i in 1:n){
  r.1.mean[i] <- mean(rnorm(4,mean=3  , sd=1))
  r.2.mean[i] <- mean(rnorm(4,mean=170, sd=6))
}

# 표본평균 분포의 평균과 표준편차
options(digits=4)
c(mean(r.1.mean),sd(r.1.mean))
c(mean(r.2.mean),sd(r.2.mean))

# 그래프
hist(r.1.mean,prob=T,col="blue",border="cyan",main="",xlab="표본평균",ylab="밀도")
x1 <- seq(min(r.1.mean),max(r.1.mean),length=1000)
y1 <- dnorm(x=x1,mean=3,sd=(1/sqrt(4)))
lines(x1,y1,lty=2,lwd=2,col="blue")


hist(r.2.mean,prob=T,col="red",border="cyan",main="",xlab="표본평균",ylab="밀도")
x2 <- seq(min(r.2.mean),max(r.2.mean),length=1000)
y2 <- dnorm(x=x2,mean=170,sd=(6/sqrt(4)))
lines(x2,y2,lty=2,lwd=2,col="blue")

# 모집단이 임의의 분포일 때.
set.seed(9)
t <- 10
p <- 0.1
x <- 0:10
n <- 1000
b.2.mean  <- rep(NA,n)
b.4.mean  <- rep(NA,n)
b.32.mean <- rep(NA,n)
b.64.mean <- rep(NA,n)

for(i in 1:n){
  b.2.mean[i]  <- mean(rbinom(2 , size=t, prob=p))
  b.4.mean[i]  <- mean(rbinom(4 , size=t, prob=p))
  b.32.mean[i] <- mean(rbinom(32, size=t, prob=p))
  b.64.mean[i] <- mean(rbinom(64, size=t, prob=p))
}

# 표본평균들의 분포에서 평균과 표준편차
options(digits=4)
c(mean(b.2.mean),sd(b.2.mean))
c(mean(b.4.mean),sd(b.4.mean))
c(mean(b.32.mean),sd(b.32.mean))
c(mean(b.64.mean),sd(b.64.mean))


# B(10, 0.1)의 분산과 표준편차
c(10*0.1*0.9, sqrt(10*0.1*0.9))


par(mfrow=c(2,2))
hist(b.2.mean,prob=T, xlim=c(0,4),main="표본 크기 :2",
     ylab="",xlab="",col="red",border="cyan",)
x1 <- seq(min(b.2.mean),max(b.2.mean),length=1000)
y1 <- dnorm(x=x1,mean=1,sd=sqrt(0.9)/sqrt(2))     
# 평균이 1 분산이 0.9 표준편차가 sqrt(0.9) 정규분포(1,sqrt(0.9)/sqrt(2))
lines(x1,y1,lty=2,lwd=2,col="blue")

hist(b.4.mean,prob=T, xlim=c(0,4),main="표본 크기 :4",
     ylab="",xlab="",col="red",border="cyan",)
x2 <- seq(min(b.4.mean),max(b.2.mean),length=1000)
y2 <- dnorm(x=x2,mean=1,sd=sqrt(0.9)/sqrt(2))     
lines(x2,y2,lty=2,lwd=2,col="blue")

hist(b.32.mean,prob=T, xlim=c(0,4),main="표본 크기 :32",
     ylab="",xlab="",col="red",border="cyan",)
x3 <- seq(min(b.32.mean),max(b.2.mean),length=1000)
y3 <- dnorm(x=x3,mean=1,sd=sqrt(0.9)/sqrt(38))     
lines(x3,y3,lty=2,lwd=2,col="blue")

hist(b.64.mean,prob=T, xlim=c(0.5,1.5),main="표본 크기 :32",
     ylab="",xlab="",col="red",border="cyan",)
x4 <- seq(min(b.64.mean),max(b.2.mean),length=1000)
y4 <- dnorm(x=x4,mean=1,sd=sqrt(0.9)/sqrt(64))     
lines(x4,y4,lty=2,lwd=2,col="blue")
