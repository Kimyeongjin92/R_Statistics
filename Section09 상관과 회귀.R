## ===================================================================================
## [빅데이터를 활용한 스마트 데이터 전문가 양성과정]  ================================
## [제대로 알고 쓰는 통계분석 R]                      ================================
## 2019-06-28(금) / 상관과 회귀                       ================================
## ===================================================================================

### ================================================================================
###  1. 상관계수 (332Page)    =====================================================
### ================================================================================

hf <- read.table("http://www.randomservices.org/random/data/Galton.txt",
                 header=T, stringsAsFactors = F)
str(hf)

# 데이터 전처리 (factor로 묶어준다.)
hf$Gender <- factor(hf$Gender, levels=c('M','F'))
hf.son    <- subset(hf,Gender=='M')
hf.son    <- hf.son[c("Father","Height")]

f.mean    <- mean(hf.son$Father)
s.mean    <- mean(hf.son$Height)
cov.num   <- sum((hf.son$Father-f.mean)*(hf.son$Height-s.mean))

# 표본 공분산.
cov.xy <- cov.num / (nrow(hf.son)-1)  

# R 함수를 이용한 공분산(표본)
cov(hf.son$Father, hf.son$Height)

# 상관계수를 구하기 위해 표준편차로 나눈다.
r.xy <- cov.xy / (sd(hf.son$Father)* sd(hf.son$Height)) ; r.xy

# R 함수를 이용한 상관계수
cor(hf.son$Father, hf.son$Height) # 0.39정도의 관계를 갖고있다.

# 그래프
plot(hf.son$Height,hf.son$Father)

### ================================================================================
###  2. 회귀분석 (335Page)    =====================================================
### ================================================================================

# 회귀계수의 추정
mean.x <- mean(hf.son$Father)
mean.y <- mean(hf.son$Height)

sxy    <- sum((hf.son$Father - mean.x)*(hf.son$Height - mean.y))
sxx    <- sum((hf.son$Father - mean.x)^2)

b1     <- sxy / sxx             # 0.448
b0     <- mean.y - b1 * mean.x  # 38.259

# 아들의 키 = [0.448(b1: 계수)*아버지의 키(father) + 38.259(b0: 절편)] (절편과 계수 y=ax+b)


# lm() 함수 이용
out    <- lm(Height ~ Father, data=hf.son)
summary(out) # *** 굉장히 유의하다. 둘다 전부 유의하다.
# Residuals          : 회귀선형을 기준으로 각 데이터마다의 오차들의 거리를 나타낸 상자그림 (Error)
# Coefficients       : 절편과 계수
# adjusted R-squared : 내가 만든 회귀식이 이 전체 모형의 15.13%를 설명한다는 뜻


# 좋은 