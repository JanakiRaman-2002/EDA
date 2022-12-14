#a
x = c(2,3,4,5,6)
y = c(12,17,23,28,32)
n=5
df = data.frame(x,y)
regression = lm(y~x)
a = regression$coefficients[2]
b = regression$coefficients[1]

#b
plot(x, y, pch=16, col='red', cex=1.2)
abline(lm(y ~ x), col='blue' , lty='dashed')

#c
ypp = predict(regression, df)
RSS = sum((ypp - y)^2)
RSS

#d
plot(regression)
abline(y-ypp)

#e
RSE = sqrt(RSS/(n-2))
denom = sum((x-mean(x))^2)
Std_err_b = RSE*sqrt((1/n)+((mean(x)^2)/denom))
Std_err_a = RSE*sqrt(1/denom)
Std_err_a
Std_err_b
summary(regression)

#f
confidence_level_b = c((b-2*Std_err_b), (b+2*Std_err_b))
confidence_level_b

confidence_level_a = c((a-2*Std_err_a), (a+2*Std_err_a))
confidence_level_a

#g
TSS = sum((y-mean(y))^2)
r_sq = 1-(RSS/TSS)
r_sq
