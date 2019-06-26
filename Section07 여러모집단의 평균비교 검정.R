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




