## ===================================================================================
## [빅데이터를 활용한 스마트 데이터 전문가 양성과정]  ================================
## [제대로 알고 쓰는 통계분석 R]                      ================================
## 2019-06-26(수) / 모집단이 두개인 경우              ================================
## ===================================================================================


### ================================================================================
###  1. 서로 독립인 두 집단 (262Page) =============================================
### ================================================================================

data <- read.table("chapter7.txt",header=T)
boy  <- subset(data,gender==1)
girl <- subset(data,gender==2)

var.test(data$weight~data$gender)

# 1) 정규성 테스트
shapiro.test(girl$weight) # P>0.05 정규성을 이룬다.
qqnorm(girl$weight)       # 정규성 = 직선근처에 점들이 모여있다
qqline(girl$weight)

shapiro.test(boy$weight)  # P<0.01798 정규성 NONO
qqnorm(boy$weight)      
qqline(boy$weight)

iriss <- subset(iris, Species=='setosa')
shapiro.test(iriss$Sepal.Length) # P>0.05, 정규성 있음
qqnorm(iriss$Sepal.Length)
qqline(iriss$Sepal.Length)

shapiro.test(iriss$Petal.Width) # P 굉장히 작다. 정규성 없음
qqnorm(iriss$Petal.Width)
qqline(iriss$Petal.Width)

# 2) 등분산성 ( 그래프가 홀쭉한지 펑퍼짐한건지) 
# 분산을 비교하기 위해서 F-test를 실시. 
# var.test(y축 ~ x축)
var.test(data$weight ~ data$gender) 
# P>0.05 두개가 같다. 그러나 0.07의 숫자인만큼 언뜻 보기에는 달라 보일 수 있다.


# 3) 평균이 같은지 다른지 알아본다.
# ex) H0 두개가 같다 
#     H1 다르다(한쪽 검정 : 혹은 크거나 작다)
#    Two Sample T

t.test(data$weight ~ data$gender,
       mu=0, 
       alternative="less", 
       var.equal=T)            # 분산이 같다는 가정하에서.
# P(0.067)>0.05 같다.
# 그러나 남자가 정규성을 띄지 않기 때문에 정확한 결론은 아니다. 


### ================================================================================
###  2. 서로 대응인 두 집단 (271Page) =============================================
### ================================================================================


# (좌측검정) 전 - 후 = 음수(가 나와야 효과가 있다) (대안가설 : 약을 먹고 몸무게가 늘어났다.)
# (우측검정) 전 - 후 = 양수(가 나와야 효과가 있다) (대안가설 : 약을 먹고 몸무게가 늘어났다.)

install.packages('PairedData')
library(PairedData)
data <- read.csv('01.anorexia.csv')
str(data)

install.packages('psych')
library(psych)

summary(data)
describe(data)

n <- length(data$Prior - data$Post)
m <-   mean(data$Prior - data$Post)
s <-     sd(data$Prior - data$Post)
(t.t <- m/(s/sqrt(n)))

t.test(data$Prior, data$Post, paired=T, alternative = "less")
  
alpha <- 0.05
qt(alpha,df=16)
pt(t.t,df=16)


### ================================================================================
###  3. 연습문제 2-Samplt T 테스트 =============================================
### ================================================================================

#  1) mtcars 기어종류 am(오토/수동)에 따른 mpg의 차이가 통계적으로 유의한가.
# H0 수동과 오토의 mpg는 차이가 없다(같다)
# H1 수동과 오토의 mpg는 차이가 있다(다르다)
am1 <- mtcars[mtcars$am == 1,] 
shapiro.test(am1$mpg)            # P(0.5363)>0.05
qqnorm(am1$mpg)                  # 정규성을 이룬다.
qqline(am1$mpg)

am0 <- mtcars[mtcars$am == 0,] 
shapiro.test(am0$mpg)            # P(0.8987)>0.05
qqnorm(am0$mpg)                  # 정규성을 이룬다.
qqline(am0$mpg)

var.test(mtcars$mpg ~ mtcars$am) # P(0.06691)>0.05 #분산이 서로 동일하다.

t.test(mtcars$mpg ~ mtcars$am,   # P(0.000285)<0.05 오토와 수동의 mpg는 다르다(차이가 있다.).
       mu = 0,
       var.equal=T)


#  2) MASS의 Cars93 생산국(Origin) USA vs non-USA 차가격(price)의 평균이 차이가 있는지.
# H0 USA vs non-USA 차가격 평균의 차이가 없다 (같다)
# H1 USA vs non-USA 차가격 평군의 차이가 있다 (다르다)

install.packages('MASS')
library(MASS)

str(Cars93)
USA<-subset(Cars93, Origin=='USA')
shapiro.test(USA$Price)            # P(0.0002)<0.05 
qqnorm(USA$Price)                  # 정규성을 이루지 않는다.
qqline(USA$Price)

non<-subset(Cars93, Origin=='non-USA')
shapiro.test(non$Price)            # P(0.0002)<0.05 
qqnorm(non$Price)                  # 정규성을 이루지 않는다.
qqline(non$Price)

var.test(Cars93$Price ~ Cars93$Origin) # P(0.01387)<0.05 분산이 서로 동일하지 않다.

t.test(Cars93$Price ~ Cars93$Origin,   # P(0.34)>0.05 평균의 차이가 없다.
       mu = 0,
       var.equal=F)

#  3-1) subcompact 자동차와 midsize 자동차의 고속도로 연비 검정
# H0 subcompact와 midsize의 고속도로 연비는 차이가 없다(같다)
# H1 subcompact와 midsize의 고속도로 연비는 차이가 있다(다르다)
library(ggplot2)
str(mpg)
table(mpg$class)

subcompact <- subset(mpg, mpg$class=='subcompact')
shapiro.test(subcompact$hwy)         # P(0.01036)<0.05 
qqnorm(subcompact$hwy)               # 정규성을 이루지 않는다.
qqline(subcompact$hwy)

midsize    <- subset(mpg, mpg$class=='midsize')
shapiro.test(midsize$hwy)            # P(0.01311)<0.05 
qqnorm(midsize$hwy)                  # 정규성을 이루지 않는다.
qqline(midsize$hwy)

sm<-rbind(subcompact,midsize)
table(sm$class)
str(sm)

var.test(sm$hwy ~ sm$class)          # P(8.825e-08)<0.05 분산이 서로 동일하지 않다.

t.test(sm$hwy ~ sm$class,            # P(0.38)>0.05 고속도로 연비의 차이가 없다.
       mu = 0,
       var.equal=F)

#  3-2) 일반 휘발유(r)와 고급 휘발유(p)의 도시 연비(cty) 검정
# H0 일반 휘발유(r)와 고급 휘발유(p)의 도시 연비(cty)는 차이가 없다(같다)
# H1 일반 휘발유(r)와 고급 휘발유(p)의 도시 연비(cty)는 차이가 있다(다르다)
r <- subset(mpg, fl=='r')
shapiro.test(r$cty)            # P(2.7e-05)<0.05 
qqnorm(r$cty)                  # 정규성을 이루지 않는다.
qqline(r$cty)

options(digits=6)
p <- subset(mpg, fl=='p')
shapiro.test(p$cty)            # P(0.498)<0.05 
qqnorm(p$cty)                  # 정규성을 이루지 않으나 근접하다..
qqline(p$cty)

table(mpg$fl)
rp<-rbind(r,p)

var.test(rp$cty ~ rp$fl)        # P(0.0428)<0.05 분산이 서로 동일하지 않다. 

t.test(rp$cty ~ rp$fl,          # P(0.228)>0.05 도시 연비는 차이가 없다(같다)
       mu = 0,
       var.equal=F)


#  3-3) subcompact 자동차의 전륜구동(f)이냐 후륜구동(r)이냐에 따른 도시연비 검정
# H0 전륜 구동(f)와 후륜 구동(r)의 차이가 없다(같다)
# H0 전륜 구동(f)와 후륜 구동(r)의 차이가 있다(다르다)