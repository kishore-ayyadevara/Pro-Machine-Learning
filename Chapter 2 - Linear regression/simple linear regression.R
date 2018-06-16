# import file
data=read.csv("D:/Pro ML book/linear_reg_example.csv")
# Build model
lm=glm(Weight~Age,data=data)
# summarize model
summary(lm)

#Extracting prediction
data$prediction=predict(lm,data)
# Extracting residuals
data$residual = data$Weight - data$prediction
# summarizing the residuals
summary(data$residual)


# Extracting the standard deviation of coefficients
# Initialize an object that stores the various coefficient values
samp_coef=c()
# Repeat the experiment 100 times
for(i in 1:100){
  # sample 50% of total data
  samp=sample(nrow(data),0.5*nrow(data))
  data2=data[samp,]
  # fit a model on the sampled data
  lm=lm(Weight~Age,data=data2)
  # extract the coefficient of independent variable and store it
  samp_coef=c(samp_coef,lm$coefficients['Age'])
}
sd(samp_coef)

# SSE of residuals
data$prediction = predict(lm,data)
sum((data$prediction-data$Weight)^2)

#Null deviance
data$prediction = mean(data$Weight)
sum((data$prediction-data$Weight)^2)
