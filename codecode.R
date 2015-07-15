math2=read.csv("C:\\Users\\User\\Desktop\\math2.csv", header = T)
math2
attach(math2)

install.packages("leaps")
library(leaps)
install.packages("ISLR")
library(ISLR)
install.packages("ggplot2")
library(ggplot2)


fit = glm(up75~., data=math2, family=binomial)
summary(fit)
fit2 = glm(up75~sex+failures+paid, data=math2, family=binomial)
summary(fit2)


plot(fit2)
lines(sex, fit2$fitted.values, type="l", col="red")
lines(failures, fit2$fitted.values, type="l", col="red")
title(main="Menarche Data with Fitted Logistic Regression Line")

plot(fit2)

plot(predict(fit2),residuals(fit2))
abline(h=0,lty=2,col="grey")
plot(predict(fit2),residuals(fit2),col=c("blue","red")[1+up75])
abline(h=0,lty=2,col="grey")


#clean variables
rm(list=ls(all=TRUE))
detach(por2)

por2=read.csv("C:\\Users\\User\\Desktop\\por2.csv", header = T)
por2
attach(por2)
fit3 = glm(up75~., data=por2, family=binomial)
summary(fit3)
fit4 = glm(up75~failures+higher, data=por2, family=binomial)
summary(fit4)

fit5 = glm(up75~failures, data=por2, family=binomial)
summary(fit5)
pfit5 <-predict(fit5, type="response")
plot(pfit5)



