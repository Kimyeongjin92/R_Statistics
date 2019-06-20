## ===================================================================================
## [빅데이터를 활용한 스마트 데이터 전문가 양성과정]  ================================
## [제대로 알고 쓰는 통계분석 R]                      ================================
## 2019-06-20-(목) / 104page                          ================================
## ===================================================================================

#http://lenkiefer.com/2018/05/21/pomological-plots/
### ================================================================================
###  확률과 확률분포 (115Page)  ===================================================
### ================================================================================


#  1. prob 패키지를 이용한 확률계산 115page
install.packages('prob')
library(prob)

rolldie(1)    # 주사위 1개일 경우의 수 
rolldie(2)    # 주사위 2개일 경우      6*6

tosscoin(1)   # 동전던지기 H 앞면 T 뒷면
tosscoin(2)
tosscoin(2,makespace=T) #확률계산

urnsamples(1:3,size=2)            # 1~3 2개 묶음으로 나울 수 있는 경우의 수.
urnsamples(1:3,size=2,replace=T)  # 복원 추출일 경우
urnsamples(c(rep("R",3),rep("B",2)),size=2)


#  2. 확률변수의 평균(기댓값)과 분산. 121page
x   <- c(0,1,2)
px  <- c(1/4,2/4,1/4)  # 확률변수가 나타날 확률
EX  <- sum(x*px)       # 기댓값

x2   <- x^2
EX2  <- sum(x2*px)  # x제곱의 기댓값
VARX <- EX2 - EX^2


#  3-1. [분포함수] dbinom 이항분포_123page
# dbinom(x,size,prob)
# x    :이항분포의 성공의 횟수의 벡터
# size :시행의 횟수
# prob :성공의 확률

n <- 6
p <- 1/3
x <- 0:n

(dbinom(2,size=n,prob=p))
(dbinom(4,size=n,prob=p))
(dbinom(12,size=12,prob=0.5))
(1/2)^12
(px <- dbinom(x,size=n,prob=p)) 

plot(x,px,type="s",xlab="성공 횟수(x)",ylab="확률(P[X=x])",main="B(6,1/3)")
plot(x,px,type="h",xlab="성공 횟수(x)",ylab="확률(P[X=x])",main="B(6,1/3)",lwd=100,col='red')

ggplot()


#  3-2. [분포함수] pbinom 누적함수
# 연속형일 때 누적에서 빼는 것으로 특정 구간을 구할 수 있다.
# 어떤 값이 누적될 때 까지의 확률.
pbinom(2,n,p) # 0,1,2     세개의 값을 모두 더한 값.
pbinom(4,n,p) # 0,1,2,3,4 그래프를 보면서 확인하면 이해하기 쉽다.

pbinom(4,n,p) - pbinom(2,n,p) # 누적에서 뺀 값은
dbinom(3,n,p) + dbinom(4,n,p) # 일반 이항분포의 각각 값과 같다.


#  3-3. [분포함수] qbinom
qbinom(0.1,n,p) # 누적확률이 0.1이 될 때의 x값
qbinom(0.5,n,p) # 누적확률이 0.5가 될 때의 x값.


#  3-4. [분포함수] rbinom
rbinom(10,n,p) # n과 p를 따르는 것 중에 random으로 10개 뽑아라
set.seed(1234) # 랜덤을 고정하여 같은 값이 출력되게 할때.


#  4. R의 분포함수를 이용한 기댓값과 분산. 133page

n  <- 6
p  <- 1/3
x  <- 0:n
px <- dbinom(x,size=n,prob=p)

ex   =  sum(x*px)  # 기댓값
ex2  =  sum(x^2 * px)
varx <- ex2 - ex^2

n*p
n*p*(1-p)


#  5. 정규분포_138page

options(digits=3)
mu    <- 170
sigma <- 6
ll <- mu - 3*sigma # lower -무한대부터 
ul <- mu + 3*sigma # upper +무한대까지는 너무 길기 때문에 한정.

x  <- seq(ll,ul,by=0.01) 
nd <- dnorm(x, mean=mu, sd=sigma)

plot(x, nd, type="l", xlab="x", ylab="P(X=x)", lwd=2, col="red")

pnorm(mu,  mean=mu, sd=sigma)
pnorm(158, mean=mu, sd=sigma)
pnorm(180, mean=mu, sd=sigma) -
  pnorm(160, mean=mu, sd=sigma)  # 키가 160~180


qnorm(0.25, mean=mu, sd=sigma)   # 면적이 0.25가 되는 때의 x값(1분위수)
qnorm(0.5 , mean=mu, sd=sigma)
qnorm(0.75, mean=mu, sd=sigma)


options(digits=5)
set.seed(5)
smp <- rnorm(400,mean=mu,sd=sigma)
c(mean(smp),sd(smp))


hist(smp, prob=T,
     main="",
     xlab="",ylab="",col="white",border="black")
lines(x,nd,lty=2)

smp1<-as.data.frame(smp)
ggplot(smp1,aes(x=smp,fill=cut(..x..,9)))+
  geom_histogram(show.legend=F,
                 breaks=seq(150,190,5),
                 col="black",
                 size=1)

#  6. 표준정규분포_142page
options(digits=4)
mu     <- 0
sigma  <- 1

p0.05  <- qnorm(0.05,  mean=mu, sd=sigma) # 면적이 0.05일 때의 x값은 -1.645(+1.645) 90% 신뢰수준
p0.025 <- qnorm(0.025, mean=mu, sd=sigma)
p0.05 ; p0.025

pnorm(1.645, mu, sigma) - pnorm(-1.645, mu, sigma) # 90%
pnorm(1.96, mu, sigma) - pnorm(-1.96, mu, sigma)   # 95%

# 그림 3-17
aa <-rnorm(10000000,mean=mu,sd=sigma)
aa1<-as.data.frame(aa)
ggplot(aa1,aes(x=aa))+                      # fill= cut(..변수없을때 기준(count,x)..,막대 개수) 
  geom_density(fill="red",alpha=0.2,size=2) +
  scale_x_continuous(breaks=seq(0,90,10)) +              # 숫자 나누는 기준
  scale_fill_discrete(h = c(180, 360), c = 150, l = 80) +
  theme_bw(base_family="baedal",base_size=20) +
  labs(title='표준정규분포(95%)',x='x',y='') +
  theme(plot.title = element_text(family="baedal",
                                  face = "bold",
                                  hjust = 0.5, 
                                  size = 40, 
                                  color = "black")) 

#                                               
install.packages('tidyverse')
library(tidyverse)
z <-  rnorm(1000, mean = 0, sd = 1)

dens <- density(z)

data <- tibble(x = dens$x, y = dens$y) %>% 
  mutate(variable = case_when(
    (x >= -2 & x <= 0) ~ "On",
    (x >= 0.2 & x <= 1) ~ "Off"))
#> Warning: package 'bindrcpp' was built under R version 3.4.4

ggplot(data, aes(x, y)) + geom_line() +
  geom_area(data = filter(data, variable == 'On'), fill = 'grey') + 
  geom_area(data = filter(data, variable == 'Off'), fill = 'light blue')


#                                              


#  7. 연습문제. =============================================================================
# 이항분포

# 7-1) 다음의 문제가 베르누이 시행인지 판단하시오. (P 확률)
#1) 영화관에서 줄을 기다리는 시간을 측정한다.               ( X )
#2) 전화가 왔을 때, 전화를 한 사람이 여자인지를 츨정한다.   ( O ) 
#3) 주사위를 한 번 던졌을 때, 나오는 숫자를 체크한다.       ( X )
#4) 주사위를 한 번 던졌을 때, 숫자 2가 나오는지를 체크한다. ( O )


# 7-2) 한 축구 선수가 페널티 킥을 차면 5번 중 4번을 성공한다고한다.
#      이 선수가 10번의 페널티킥을 차서 7번 성공할 확률을 구하시오.
dbinom(7, size=10, prob=4/5)

# 7-3) A라는 회사는 스마트 폰의 한 부품을 만드는 회사로, 
#      이 A사의 불량률은 5%(0.05)로 알려져 있다 이 회사의 제품 20개를 조사했을 때,
#      불량이 2개 이하로 나올 확률을 구하시오.
pbinom(2, size=20, prob=0.05)

# 7-4) 어떤 희귀 바이러스에 감염되었을 때, 회복할 수 있는 치료율은 20%라고 한다.
#      이 바이러스에 감염된 환자 20명을 치료했을 때, 적어도 2명 이상은 회복될 확률.
1-pbinom(1,size=20,prob=0.2)

#*** 7-5) 주사위 두개를 던졌을 때, 눈금의 합이 6이 될 확률을 구하시오.
# dbinom(5, size=36, prob=5/36)
5/36

# 이항분포

# 7-1) A라는 전구회사에서 생산하는 전구의 수명은 800일이고 
#      표준편차는 40일인 정규분포를 따른다.
#      이때 전구의 수명이 750이하일 확률을 구하시오.
pnorm(750, mean=800, sd=40)


# 7-2) 근무기간 평균이 11년, 분산이 16년인 정규분포 

# 20년 이상 근무한 종업원의 비율
1-pnorm(19, mean=11, sd=4)

# 근무연수가 가장 오래된 10%의 종업원은 
# 이 회사에서 몇 년 이상 근무했다고 볼 수 있는가. 
qnorm(0.9, mean=11, sd=4) # 5년 이상.


# 7-3) 수학성적 평균 70, 표준편차 8 (점수가 80이상 90이하)
pnorm(90, mean=70, sd=8) - pnorm(80, mean=70, sd=8)

# 7-4) 확률변수 X가 평균 1.5, 표준편차 2인 정규분포.
#      실수 전체의 집합에서 정의된 함수 H(t) = P(t<=X<=t+1)이다
#      H(0)+H(2)의 값을 구하시오.
pnorm(1,mean=1.5,sd=2) - pnorm(0,mean=1.5,sd=2) +
pnorm(3,mean=1.5,sd=2) - pnorm(2,mean=1.5,sd=2)

# ===================================================================================

