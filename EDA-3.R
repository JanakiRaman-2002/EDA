# QUESTION - 1
x <- c(2,3,4,5,6)
y <- c(12,17,23,28,32)
df = data.frame(x,y)

#a
regression_a = lm(y~x)
regression_a
a = regression_a$coefficients[2]
b = regression_a$coefficients[1]

#b
plot(x, y, pch=16, col='red', cex=1.2)
abline(lm(y ~ x), col='blue' , lty='dashed')

#c
n=5
predictors = 1

ypp = predict(regression, df)
RSS = sum((ypp - y)^2)
RSS

TSS = sum((y-mean(y))^2)
r_sq = 1-(RSS/TSS)
r_sq

adj_r_sq = 1 - ((1-r_sq)*(n-1))/(n-predictors-1)
adj_r_sq

#d
mean_x = mean(x)
mean_y = mean(y)
var_x = sum((x-mean_x)^2)/n-1
var_y = sum((y-mean_y)^2)/n-1

SSM = sum((ypp-mean_y)^2)
DFM = predictors
DFE = n-predictors-1
SSE = RSS

MSM = SSM/DFM
MSE = SSE/DFE

F_static = MSM/MSE
F_critical = qf(0.05, predictors, n-predictors-1, FALSE)
summary(regression_a)
pvalue = (summary(regression_a)$coefficients[,4]["x"])
if(F_static > F_critical && 0.05>pvalue){
  print("Reject Null Hypothesis - Model can be accepted and has atleast one dependence")
}else{
  print("Accept Null Hypothesis")
}


# QUESTION - 2
x1 <- c(2,3,4,5,6)
x2 <- c(14,15,26,27,28)
x3 <- c(45, 89, 100, 290, 478)
y1 <- c(12, 17, 23, 28, 32)
df1 = data.frame(x1,x2,x3,y1)
df1

#a
regression_df1 = lm(y1~x1+x2+x3)
regression_df1

summary(regression_df1)

regression_df1$coefficients
r_sq_df1 = regression_df1$r.squared
adj_r_sq_df1 = regression_df1$adj.r.squared    

X1 <- c(2,3,4,5,6)
X2 <- c(14,15,26,27,28)
X3 <- c(4, 6, 8, 10, 12)
y2 <- c(12, 17, 23, 28, 32)
df2 = data.frame(X1,X2,X3,y2)
df2
