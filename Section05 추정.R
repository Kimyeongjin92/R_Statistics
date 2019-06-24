## ===================================================================================
## [빅데이터를 활용한 스마트 데이터 전문가 양성과정]  ================================
## [제대로 알고 쓰는 통계분석 R]                      ================================
## 2019-06-24(월) / 추정 151page                      ================================
## ===================================================================================


### ================================================================================
###  1. 점추정 (186Page)    =======================================================
### ================================================================================

#  1) 
x   <- seq(-3,3,by=0.01)
y   <- dnorm(x)
y.1 <- dnorm(x, sd=sqrt(1/13))
y.2 <- dnorm(x, sd=sqrt(7/18))
pnorm(0.1, sd=sqrt(1/3))-pnorm(-0.1, sd=sqrt(1/3))
pnorm(0.1, sd=sqrt(7/18))-pnorm(-0.1, sd=sqrt(7/18))
plot(x, y, type="l",ylim=c(0,0.8), axes=F,ylab="",
     lwd=3,col="yellow")
lines(x, y.1, col="red", lwd=3)
lines(y, y.2, col="green",lty=2,lwd=3)
axis(1)

# y2bar의 기대값을 구하기 위한 계산식
options(digits=3)
set.seed(1)
mean.seq <- function(x){
  n   <- length(x)
  sum <- 0
  n2  <- 0
  for(i in 1:n){
    newx <- i * x[i]
    sum  <- sum + newx
    n2   <- n2 + i
  }
  return(sum/n2)
}

y1 <- rep(NA,1000)
y2 <- rep(NA,1000)

for(i in 1:1000){
  smp   <- rnorm(3)
  y1[i] <- mean(smp)
  y2[i] <- mean.seq(smp)
}

n1 <- length(y1[(y1>-0.1) & (y1 < 0.1)])
n2 <- length(y1[(y1>-0.1) & (y1 < 0.1)])
data.frame(mean=mean(y1),var=var(y1),n=n1)
data.frame(mean=mean(y2),var=var(y2),n=n2)

par(mfrow=c(1,2))
hist(y1, probability = T, xlim=c(-2,2),ylim=c(0,0.65),
     main="(x1+x2+x3)/3",xlab="",col="orange",border="red")
hist(y2, probability = T, xlim=c(-2,2),ylim=c(0,0.65),
     main="(1*x1+2*x2+3*x3)/6",xlab="",col="orange",border="red")


#  2) 모비율에 대한 점추정

library(prob)
n <- 3
smps.all <- rolldie(n)
str(smps.all)
head(smps.all, n=3)

is.even <- function(x) return(!x%%2)  # !의 경우 반대가 되어 0=T 1=F
var.p   <- function(x){
  return(sum((x-mean(x))^2/length(x))  )
}
p.even <- function(x,s.size=3){
  return(sum(is.even(x))/s.size)  # sum(T,T,T) = 3
}

phat <- apply(smps.all, 1, p.even)

mean(phat)
(p.p <- 0.5)
var.p(phat)
(p.p*(1-p.p)/3)
sqrt(var.p(phat))


### ================================================================================
###  2. 구간 추정 (186Page)    ====================================================
### ================================================================================


#  1) 모집단의 분산을 알 때 모평균의 구간추정 

set.seed(9)
n <- 10
x <- 1:100
y <- seq(-3,3,by=0.01)

smps <- matrix(rnorm(n*length(x)),ncol=n)

xbar <- apply(smps, 1, mean)
se   <- 1/sqrt(10)
alpha<- 0.05
z    <- qnorm(1 - alpha/2)
ll   <- xbar - z * se
ul   <- xbar + z * se

plot(y, type="n",xlab="trial",ylab="z",
     main="95% Confidence Interval for Population mean",
     xlim=c(1,100),ylim=c(-1.5,1.5),cex.lab=1.8)
abline(h=0)


#  2) 모집단의 분산을 모를 때 모평균의 구간추정 

ci.t <- function(x, alpha=0.05){
   n <- length(smp)
   m <- mean(x)
   s <- sd(x)
   t <- qt(1-(alpha/2), df=n-1)
   ll<- m - t * (s/sqrt(n))
   ul<- m + t * (s/sqrt(n))
   ci<- c(1-alpha, ll, m, ul)
   names(ci) <- c("Confidence Level","Lower limit","Mean","Upper limit")
   return(ci)
}

smp <- c(520,498,481,512,515,542,520,518,527,526)
ci.t(smp)
ci.t(smp,0.1)




