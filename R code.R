data=read.csv("D:/Pro ML book/linear_reg_example.csv")
data$correlated_age = data$Age*0.5 + rnorm(nrow(data))*0.1
cor(data$Age,data$correlated_age)
lm=glm(Weight~Age+correlated_age,data=data)
summary(lm)

test$prediction=predict(lm,test)
sum((test$prediction-test$Weight)^2)


samp_coef=c()
for(i in 1:100){
  samp=sample(nrow(data),0.5*nrow(data))
  data2=data[samp,]
  lm=lm(Weight~Age,data=data2)
  samp_coef=c(samp_coef,lm$coefficients['Age'])
}
sqrt(var(samp_coef)/(nrow(data)-1))

data$prediction=mean(data$Weight)
sum((data$prediction-data$Weight)^2)


test$prediction=predict(lm,test)
test$abs_error=abs(test$prediction-test$Weight)

sum(test$abs_error)/sum(test$Weight)

lm=glm(Weight~Age+row_id,data=train)
summary(lm)

test$prediction=predict(lm,test)
test$abs_error=abs(test$prediction-test$Weight)

sum(test$abs_error)/sum(test$Weight)



data$row_id=seq(1:nrow(data))
lm=glm(Weight~Age+row_id,data=data)
summary(lm)


prediction=predict(lm,data)

data$prediction=predict(lm,data)
data$error=(data$Weight-data$prediction)


samp=sample(1:nrow(data),115)
data2=data[samp,]
dim(data2)

lm=glm(Weight~Age,data=data2)
summary(lm)

lm$coefficients[2]

age_coef=c()

for(i in 1:100){
  samp=sample(1:nrow(data),65)
  data2=data[samp,]
  lm=glm(Weight~Age+row_id,data=data2)
  age_coef=c(age_coef,lm$coefficients[3])
}
sd(age_coef)