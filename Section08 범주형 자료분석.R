## ===================================================================================
## [빅데이터를 활용한 스마트 데이터 전문가 양성과정]  ================================
## [제대로 알고 쓰는 통계분석 R]                      ================================
## 2019-06-27(목) / 범주형 자료분석                   ================================
## ===================================================================================


# 지금까지 배운것에서 문제를 주면 알아서 찾아야한다.
# (양적 자료)
# 1 - sample T
# 1 - smaple prop
# 2 - sample T
# Paired T
# ANOVA (f분포가 쓰임)
# 등분산성, 정규성


### ================================================================================
###  1. 적합도 검정 (302Page) =====================================================
### ================================================================================

# 이제부터는 값으로부터 직접 계산을 할 수 없는 질적자료
# 명목형 변수(카이제곱 분포가 쓰임)와 각 범주의 개수(count)
# 여기서 검정통계량은 (범주의 개수-1)을 자유도로 갖는 카이제곱 분포를 따릅니다.
# 지금부터 범주의 개수가 4개(자유도 3)인 멘델의 유전법칙을 검정해보자.

x <- c(315,101,108,32)         # 범주형 자료.
chisq.test(x,p=c(9,3,3,1)/16)

# 그래프 그리기
x  <- seq(0, 15, by=0.01)
dc <- dchisq(x,df=3)

alpha  <- 0.05
tol    <- qchisq(0.95,df=3)

alpha2 <- 0.9254
tol2   <- qchisq(1-alpha2,df=3)

par(mar=c(0,1,1,1))
plot(x,dc, type="l",axes=F,ylim=c(-0.03,0.25),xla="",ylab="")
abline(h=0)
tol.g <- round(tol,2)
polygon(c(tol.g, x[x>tol.g], 15), c(0,dc[x>tol.g], 0), col= "red")
polygon(c(tol2,  x[x>tol2],15), c(0,dc[x>tol2],0), density=20, angle=45)
text(0, -0.03, "0",cex=0.8)
text(tol, -0.03, expression(chi[0.05]^{2}==7.81),cex=0.8)
text(tol2, -0.03, expression(chi[0.92]^{2}==0.47),cex=0.8)


### ================================================================================
###  2. 동질성 검정 (308Page) =====================================================
### ================================================================================

sns.c <- read.csv('snsbyage.csv',header=T,stringsAsFactors = F)

sns.c <- transform(sns.c, age.c = 
                     factor(age, levels=c(1,2,3),
                            labels=c("20대","30대","40대")))
sns.c <- transform(sns.c, service.c = 
                     factor(service, levels=c("F","T","K","C","E"),
                            ordered=T))
c.tab <- table(sns.c$age.c, sns.c$service.c)
c.tab
a.n   <- margin.table(c.tab, margin = 1)
s.n   <- margin.table(c.tab, margin = 2)
s.p   <- s.n/ margin.table(c.tab)
expected <- a.n %*% t(s.p)

o.e   <- c.tab-expected
t.t   <- sum( (o.e)^2/expected ) # 검정 통계량

qchisq(0.95, df=8)               # 

1-pchisq(t.t,df=8)

# chisq.test 이용하기.
chisq.test(c.tab)

# 이런식으로 저장하면 다양한 변수를 볼 수 있다.
result <- chisq.test(c.tab)
result$expected
str(result)
result$p.value

# 기대도수는 expected에 저장되어 있다.
# 기대 도수의 합을 구할 수 있다.
addmargins(result$expected)


### ================================================================================
###  3. 독립성 검정 (316Page) =====================================================
### ================================================================================

# 1) 가설 설정하기
data(UCBAdmissions)
UCBAdmissions
ucba.tab <- apply(UCBAdmissions, c(1,2), sum)  # 6개의 테이블을 한번에 더한다. 
round(prop.table(ucba.tab, margin=2)*100,1)

# 2) 독립성 검정
a.n <-margin.table(ucba.tab, margin=1)
g.n <-margin.table(ucba.tab, margin=2)

a.p <- a.n /margin.table(ucba.tab)
g.p <- g.n /margin.table(ucba.tab)

expected <- margin.table(ucba.tab)*(a.p %*% t(g.p)) 
addmargins(expected)

# 3) 카이 제곱 통계
o.e <- (ucba.tab - expected)^2 / expected
addmargins(o.e)

chisq.t <- sum(o.e)  # 검정 통계량
chisq.t

qchisq(0.95,df=1)
1-pchisq(chisq.t,df=1)

# 322PAGE 설명...읽자.
chisq.test(ucba.tab, correct=F)



