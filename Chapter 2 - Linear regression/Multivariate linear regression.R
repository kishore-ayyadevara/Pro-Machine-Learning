# import file
data=read.csv("D:/Pro ML book/Linear regression/linear_multi_reg_example.csv")
# Build model
lm=glm(Weight~Age+New,data=data)
# summarize model
summary(lm)