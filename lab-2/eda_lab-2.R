#Find the best linear fit(Y=aX+b)

x = c(2,3,4,5,6)
y = c(12,17,23,28,32)
n=5
df = data.frame(x,y)
regression = lm(y~x)
a = regression$coefficients[2]
b = regression$coefficients[1]
a
b

#Plot the graph of the model

plot(x, y, pch=16, col='red', cex=1.2)
abline(lm(y ~ x), col='blue' , lty='dashed')

#Determine the minimum RSS 

ypp = predict(regression, df)
RSS = sum((ypp - y)^2)
RSS

#Draw the residual plot for the best linear fit and comment
#on the suitability of the linear model to this training data.

plot(regression)
abline(y~ypp)

#Evaluate the standard errors associated with a and b.

RSE = sqrt(RSS/(n-2))
denom = sum((x-mean(x))^2)
Std_err_b = RSE*sqrt((1/n)+((mean(x)^2)/denom))
Std_err_a = RSE*sqrt(1/denom)
Std_err_a
Std_err_b
summary(regression)

#Determine the 95% confidence interval for a and b

confidence_level_b = c((b-2*Std_err_b), (b+2*Std_err_b))
confidence_level_b

confidence_level_a = c((a-2*Std_err_a), (a+2*Std_err_a))
confidence_level_a

#Compute R2 statistic

TSS = sum((y-mean(y))^2)
r_sq = 1-(RSS/TSS)
r_sq


#Predict the value of a test instance from the dataset

df=data.frame(x)
prediction = predict(regression,df)
prediction

#2) Apply linear regression analysis on any prominent dataset and state the inferences

phone_prediction <- read.csv('c:/Users/nehaal/Downloads/train (2).csv')

#Find the best linear fit(Y=aX+b)
regression = lm(phone_prediction$clock_speed~phone_prediction$battery_power)
a = regression$coefficients[2]
b = regression$coefficients[1]
a
b

#Plot the graph of the model

plot(phone_prediction$battery_power, phone_prediction$clock_speed, pch=16, col='red', cex=1.2)
abline(lm(phone_prediction$clock_speed~phone_prediction$battery_power), col='blue' , lty='dashed')

#Determine the minimum RSS 

df<- phone_prediction[c('battery_power','clock_speed')]
ypp = predict(regression, df)
RSS = sum((ypp - y)^2)
RSS

#Draw the residual plot for the best linear fit and comment
#on the suitability of the linear model to this training data.

plot(regression)
abline(phone_prediction$clock_speed~ypp)

#Evaluate the standard errors associated with a and b.

RSE = sqrt(RSS/(n-2))
denom = sum((x-mean(x))^2)
Std_err_b = RSE*sqrt((1/n)+((mean(x)^2)/denom))
Std_err_a = RSE*sqrt(1/denom)
Std_err_a
Std_err_b
summary(regression)

#Determine the 95% confidence interval for a and b

confidence_level_b = c((b-2*Std_err_b), (b+2*Std_err_b))
confidence_level_b

confidence_level_a = c((a-2*Std_err_a), (a+2*Std_err_a))
confidence_level_a

#Compute R2 statistic

TSS = sum((y-mean(y))^2)
r_sq = 1-(RSS/TSS)
r_sq


#Predict the value of a test instance from the dataset

df=data.frame(df$battery_power)
prediction = predict(regression,df)
prediction


Sys.time()
Sys.info()

