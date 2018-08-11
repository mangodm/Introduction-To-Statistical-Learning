# 패키지 로드

library(MASS)
library(ISLR)

## --- 단순선형회귀 분석

# 변수명 확인
names(Boston)

# ? 사용하여 변수에 대한 설명 확인
?Boston

# plot some variables
plot(medv~lstat, data = Boston) # ~ : formula format

# 모형 적합 및 결과 확인 
fit1=lm(medv~lstat, data = Boston) 
fit1
summary(fit1)
abline(fit1,col="red") # 적합된 회귀직선 그리기
names(fit1)
confint(fit1) # confidence level for the coefficients
predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence") # 새로운 lstat 데이터(5, 10, 15)에 대한 예측

### --------------------------------------------------

## --- 다중선형회귀 분석

# 모형 적합 및 결과 확인
fit2=lm(medv~lstat+age,data=Boston)
summary(fit2)

fit3=lm(medv~.,Boston) # ~. : response를 제외한 모든 변수를 사용
summary(fit3)
par(mfrow=c(2,2)) # 2x2 layout 설정
plot(fit3) # linear model에 대한 various views 제공(non-linearity, noramlity 등)

fit4=update(fit3,~.-age-indus) # update(): 기존에 적합시킨 모형에서 빼고 싶은 변수 생겼을 때 유용하게 사용 
summary(fit4)

### --------------------------------------------------

## --- Nonlinear terms and Interactions

# 상호작용항 추가하여 모형 적합 및 결과 확인
fit5=lm(medv~lstat*age, data = Boston) # *로 상호작용항 표시
summary(fit5)

# quadratic term 추가하여 모형 적합 및 결과 확인
fit6=lm(medv~lstat +I(lstat^2), data = Boston); summary(fit6) # I(): identity function / ; one-line에 여러 명령어 추가
attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat,fitted(fit6),col="red",pch=20) # plot에 fitted value 추가 cf) abline은 straight line fit만 그려준다! 

# poly()를 활용한 모형 적합
fit7=lm(medv~poly(lstat,4))
points(lstat,fitted(fit7),col="blue",pch=20) # 4차 vs. 2차 비교

# cf) pch options 확인
plot(1:20,1:20,pch=1:20,cex=2)

### --------------------------------------------------

## --- Qualitative predictors

# 데이터 로드 및 모형 적합
Carseats <- ISLR::Carseats
names(Carseats)
summary(Carseats) # ShelveLoc, Urban, US 등: qualitative variable
str(Carseats)

fit1=lm(Sales~.+Income:Advertising+Age:Price,Carseats) # 상호작용항 추가
summary(fit1)
contrasts(Carseats$ShelveLoc) # linear model에 어떻게 qualitative variable을 넣을 것인지(3 수준이니 dummy variable은 두 개로)

# x, y 두 변수의 산점도 + 적합된 회귀 직선 plotting하는 함수 생성 
regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}
attach(Carseats)
regplot(Price,Sales)

# option 추가 가능한 함수로 만들기
regplot=function(x,y,...){ # (...): unnamed arguments: extra arguments를 허용하여 함수 실행
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}
regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)
