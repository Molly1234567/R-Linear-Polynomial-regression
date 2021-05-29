datawine=read.csv("winequality-red.csv",header=TRUE,sep=";")
str(datawine)
View(datawine)
#data range
summary(datawine)
pairs(datawine)
#show the quality how to distribute
summary(datawine$quality)
hist(datawine$quality)
hist(datawine$fixed.acidity)
cor(datawine)
pairs(datawine)
install.packages("psych")
library(psych)
pairs.panels(datawine)
#train linear model (0.36/0.35)  it is bad
model=lm(quality~.,data=datawine)
summary(model)
#fixed acidity(0.748/0.747) it is good
model1=lm(fixed.acidity~citric.acid+density+pH,data=datawine)
summary(model1)
model1$coefficients
#relation=lm(fixed.acidity~citric.acid+density+pH,data=datawine)
#relation
#predict
newdata1=data.frame(citric.acid=0.5,density=0.9959,pH=3.2)
predict(model1,newdata1,interval = "confidence")
predict(model1,newdata1,interval = "predict")
#train polynomial model(1/1) overfitting
model2=lm(quality ~ polym(alcohol,volatile.acidity,sulphates,citric.acid,total.sulfur.dioxide,density,chlorides,fixed.acidity,pH,free.sulfur.dioxide,residual.sugar,degree=4), data=datawine)
summary(model2)
#train polynomial model(0.763/0.5715) it is good
model3=lm(quality~polym(fixed.acidity,volatile.acidity,chlorides,free.sulfur.dioxide,total.sulfur.dioxide,density,pH,sulphates,alcohol,degree=4),data=datawine)
summary(model3)
model3$coefficients
#train polynomial model(0.3067/0.305)
model4=lm(quality~I(volatile.acidity^3)+free.sulfur.dioxide+total.sulfur.dioxide+I(alcohol^3),data=datawine)
summary(model4)
#train polynomial model(0.39/0.385)
model5=lm(quality~polym(alcohol,volatile.acidity,sulphates,degree=4),data=datawine)
summary(model5)
#train polynomial model(0.8758/0.6681)
model6=lm(quality~polym(fixed.acidity,volatile.acidity,chlorides,free.sulfur.dioxide,total.sulfur.dioxide,density,pH,sulphates,alcohol,residual.sugar,degree=4),data=datawine)
summary(model6)
#train polynomial model(0.624/0.513)
model7=lm(quality ~ polym(alcohol,volatile.acidity,sulphates,citric.acid,total.sulfur.dioxide,density,chlorides,fixed.acidity,pH,free.sulfur.dioxide,residual.sugar,degree=3), data=datawine)
summary(model7)
