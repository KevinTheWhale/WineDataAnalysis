setwd("C:/Users/tianw/OneDrive/Desktop/r practice/Math 261A/project")
wine = read.csv("winequality-red.csv")

#create variables for the full dataset
y_f = wine$quality
x1_f = wine$fixed.acidity
x2_f = wine$volatile.acidity
x3_f = wine$citric.acid
x4_f = wine$residual.sugar
x5_f = wine$chlorides
x6_f = wine$free.sulfur.dioxide
x7_f = wine$total.sulfur.dioxide
x8_f = wine$density
x9_f = wine$pH
x10_f = wine$sulphates
x11_f = wine$alcohol

n = length(y_f)

#split the data into 80:20 80% for training and 20% for testing
library(sampling)
tr = srswor(1279, n)

#training dataset
y = y_f[tr == 1]
x1 = x1_f[tr == 1]
x2 = x2_f[tr == 1] 
x3 = x3_f[tr == 1] 
x4 = x4_f[tr == 1] 
x5 = x5_f[tr == 1] 
x6 = x6_f[tr == 1] 
x7 = x7_f[tr == 1] 
x8 = x8_f[tr == 1] 
x9 = x9_f[tr == 1] 
x10 = x10_f[tr == 1] 
x11 = x11_f[tr == 1] 

#testing dataset
y_t = y_f[tr == 0]
x1_t = x1_f[tr == 0]
x2_t = x2_f[tr == 0] 
x3_t = x3_f[tr == 0] 
x4_t = x4_f[tr == 0] 
x5_t = x5_f[tr == 0] 
x6_t = x6_f[tr == 0] 
x7_t = x7_f[tr == 0] 
x8_t = x8_f[tr == 0] 
x9_t = x9_f[tr == 0] 
x10_t = x10_f[tr == 0] 
x11_t = x11_f[tr == 0]

#we fit using linear regression
res_l = lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11)
anova(res_l)
summary(res_l)

par(mfrow = c(4,4))
stud_l = rstudent(res_l)
plot(res_l$fitted.values, stud_l)
plot(x1, stud_l, xlab="fixed.acidity")
plot(x2, stud_l, xlab="volatile.acidity")
plot(x3, stud_l, xlab="citric.acid")
plot(x4, stud_l, xlab="residual.sugar")
plot(x5, stud_l, xlab="chlorides")
plot(x6, stud_l, xlab="free.sulfur.dioxide")
plot(x7, stud_l, xlab="total.sulfur.dioxide")
plot(x8, stud_l, xlab="density")
plot(x9, stud_l, xlab="pH")
plot(x10, stud_l, xlab="sulphates")
plot(x11, stud_l, xlab="alcohol")
plot(stud_l)
qqnorm(stud_l)
qqline(stud_l)


library(MASS)
bc = boxcox(res_l)

#find the best lambda for transformation 
v = bc$x[which.max(bc$y)]
res_t = lm(y^v~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11)
anova(res_t)
summary(res_t)
stud_t = rstudent(res_t)
plot(res_l$fitted.values, stud_t)
plot(x1, stud_t, xlab="fixed.acidity")
plot(x2, stud_t, xlab="volatile.acidity")
plot(x3, stud_t, xlab="citric.acid")
plot(x4, stud_t, xlab="residual.sugar")
plot(x5, stud_t, xlab="chlorides")
plot(x6, stud_t, xlab="free.sulfur.dioxide")
plot(x7, stud_t, xlab="total.sulfur.dioxide")
plot(x8, stud_t, xlab="density")
plot(x9, stud_t, xlab="pH")
plot(x10, stud_t, xlab="sulphates")
plot(x11, stud_t, xlab="alcohol")
qqnorm(stud_t)
qqline(stud_t)

x1sq = x1^2
x2sq = x2^2
x3sq = x3^2
x4sq = x4^2
x5sq = x5^2
x6sq = x6^2
x7sq = x7^2
x8sq = x8^2
x9sq = x9^2
x10sq = x10^2
x11sq = x11^2

res_p = lm(y~x1sq+x2sq+x3sq+x4sq+x5sq+x6sq+x7sq+x8sq+x9sq+x10sq+x11sq+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x1:x2+x1:x3+x1:x4+x1:x5+x1:x6+x1:x7+x1:x9+x1:x9+x1:x10+x1:x11+x2:x3+x2:x4+x2:x5+x2:x6+x2:x7+x2:x8+x2:x9+x2:x10+x2:x11+x3:x4+x3:x5+x3:x6+x3:x7+x3:x8+x3:x9+x3:x10+x3:x11+x4:x5+x4:x6+x4:x7+x4:x8+x4:x9+x4:x10+x4:x11+x5:x6+x5:x7+x5:x8+x5:x9+x5:x10+x5:x11+x6:x7+x6:x8+x6:x9+x6:x10+x6:x11+x7:x8+x7:x9+x7:x10+x7:x11+x8:x9+x8:x10+x8:x11+x9:x10+x9:x11+x10:x11)
anova(res_p)
summary(res_p)

stud_p = rstudent(res_p)
plot(res_p$fitted.values, stud_p)
plot(x1, stud_p, xlab="fixed.acidity")
plot(x2, stud_p, xlab="volatile.acidity")
plot(x3, stud_p, xlab="citric.acid")
plot(x4, stud_p, xlab="residual.sugar")
plot(x5, stud_p, xlab="chlorides")
plot(x6, stud_p, xlab="free.sulfur.dioxide")
plot(x7, stud_p, xlab="total.sulfur.dioxide")
plot(x8, stud_p, xlab="density")
plot(x9, stud_p, xlab="pH")
plot(x10, stud_p, xlab="sulphates")
plot(x11, stud_p, xlab="alcohol")
plot(stud_p)
qqnorm(stud_p)
qqline(stud_p)


x1cu = x1^3
x2cu = x2^3
x3cu = x3^3
x4cu = x4^3
x5cu = x5^3
x6cu = x6^3
x7cu = x7^3
x8cu = x8^3
x9cu = x9^3
x10cu = x10^3
x11cu = x11^3

res_cubic = lm(y~x1+x1sq+x1cu+x2+x2sq+x2cu+x3+x3sq+x3cu+x4+x4sq+x4cu+x5+x5sq+x5cu+x6+x6sq+x6cu+x7+x7sq+x7cu+x8+x8sq+x8cu+x9+x9sq+x9cu+x10+x10sq+x10cu+x11+x11sq+x11cu+x1:x2sq+x1sq:x2+x1:x3sq+x1sq:x3+x1:x4sq+x1sq:x4+x1:x5sq+x1sq:x5+x1:x6sq+x1sq:x6+x1:x7sq+x1sq:x7+x1:x8sq+x1sq:x8+x1:x9sq+x1sq:x9+x1:x10sq+x1sq:x10+x1:x11sq+x1sq:x11+x2:x3sq+x2sq:x3+x2:x4sq+x2sq:x4+x2:x5sq+x2sq:x5+x2:x6sq+x2sq:x6+x2:x7sq+x2sq:x7+x2:x8sq+x2sq:x8+x2:x9sq+x2sq:x9+x2:x10sq+x2sq:x10+x2:x11sq+x2sq:x11+x3:x4sq+x3sq:x4+x3:x5sq+x3sq:x5+x3:x6sq+x3sq:x6+x3:x7sq+x3sq:x7+x3:x8sq+x3sq:x8+x3:x9sq+x3sq:x9+x3:x10sq+x3sq:x10+x3:x11sq+x3sq:x11+x4:x5sq+x4sq:x5+x4:x6sq+x4sq:x6+x4:x7sq+x4sq:x7+x4:x8sq+x4sq:x8+x4:x9sq+x4sq:x9+x4:x10sq+x4sq:x10+x4:x11sq+x4sq:x11+x5:x6sq+x5sq:x6+x5:x7sq+x5sq:x7+x5:x8sq+x5sq:x8+x5:x9sq+x5sq:x9+x5:x10sq+x5sq:x10+x5:x11sq+x5sq:x11+x6:x7sq+x6sq:x7+x6:x8sq+x6sq:x8+x6:x9sq+x6sq:x9+x6:x10sq+x6sq:x10+x6:x11sq+x6sq:x11+x7:x8sq+x7sq:x8+x7:x9sq+x7sq:x9+x7:x10sq+x7sq:x10+x7:x11sq+x7sq:x11+x8:x9sq+x8sq:x9+x8:x10sq+x8sq:x10+x8:x11sq+x8sq:x11+x9:x10sq+x9sq:x10+x9:x11sq+x9sq:x11+x10:x11sq+x10sq:x11+x1:x2:x3+x1:x2:x4+x1:x2:x5+x1:x2:x6+x1:x2:x7+x1:x2:x8+x1:x2:x9+x1:x2:x10+x1:x2:x11+x1:x3:x4+x1:x3:x5+x1:x3:x6+x1:x3:x7+x1:x3:x8+x1:x3:x9+x1:x3:x10+x1:x3:x11+x1:x4:x5+x1:x4:x6+x1:x4:x7+x1:x4:x8+x1:x4:x9+x1:x4:x10+x1:x4:x11+x1:x5:x6+x1:x5:x7+x1:x5:x8+x1:x5:x9+x1:x5:x10+x1:x5:x11+x1:x6:x7+x1:x6:x8+x1:x6:x9+x1:x6:x10+x1:x6:x11+x1:x7:x8+x1:x7:x9+x1:x7:x10+x1:x7:x11+x1:x8:x9+x1:x8:x10+x1:x8:x11+x1:x9:x10+x1:x9:x11+x1:x10:x11+x2:x3:x4+x2:x3:x5+x2:x3:x6+x2:x3:x7+x2:x3:x8+x2:x3:x9+x2:x3:x10+x2:x3:x11+x2:x4:x5+x2:x4:x6+x2:x4:x7+x2:x4:x8+x2:x4:x9+x2:x4:x10+x2:x4:x11+x2:x5:x6+x2:x5:x7+x2:x5:x8+x2:x5:x9+x2:x5:x10+x2:x5:x11+x2:x6:x7+x2:x6:x8+x2:x6:x9+x2:x6:x10+x2:x6:x11+x2:x7:x8+x2:x7:x9+x2:x7:x10+x2:x7:x11+x2:x8:x9+x2:x8:x10+x2:x8:x11+x2:x9:x10+x2:x9:x11+x2:x10:x11+x3:x4:x5+x3:x4:x6+x3:x4:x7+x3:x4:x8+x3:x4:x9+x3:x4:x10+x3:x4:x11+x3:x5:x6+x3:x5:x7+x3:x5:x8+x3:x5:x9+x3:x5:x10+x3:x5:x11+x3:x6:x7+x3:x6:x8+x3:x6:x9+x3:x6:x10+x3:x6:x11+x3:x7:x8+x3:x7:x9+x3:x7:x10+x3:x7:x11+x3:x8:x9+x3:x8:x10+x3:x8:x11+x3:x9:x10+x3:x9:x11+x3:x10:x11+x4:x5:x6+x4:x5:x7+x4:x5:x8+x4:x5:x9+x4:x5:x10+x4:x5:x11+x4:x6:x7+x4:x6:x8+x4:x6:x9+x4:x6:x10+x4:x6:x11+x4:x7:x8+x4:x7:x9+x4:x7:x10+x4:x7:x11+x4:x8:x9+x4:x8:x10+x4:x8:x11+x4:x9:x10+x4:x9:x11+x4:x10:x11+x5:x6:x7+x5:x6:x8+x5:x6:x9+x5:x6:x10+x5:x6:x11+x5:x7:x8+x5:x7:x9+x5:x7:x10+x5:x7:x11+x5:x8:x9+x5:x8:x10+x5:x8:x11+x5:x9:x10+x5:x9:x11+x5:x10:x11+x6:x7:x8+x6:x7:x9+x6:x7:x10+x6:x7:x11+x6:x8:x9+x6:x8:x10+x6:x8:x11+x6:x9:x10+x6:x9:x11+x6:x10:x11+x7:x8:x9+x7:x8:x10+x7:x8:x11+x7:x9:x10+x7:x9:x11+x7:x10:x11+x8:x9:x10+x8:x9:x11+x8:x10:x11+x9:x10:x11)
summary(res_cubic)

stud_c = rstudent(res_cubic)
plot(res_cubic$fitted.values, stud_c)
plot(x1, stud_c, xlab="fixed.acidity")
plot(x2, stud_c, xlab="volatile.acidity")
plot(x3, stud_c, xlab="citric.acid")
plot(x4, stud_c, xlab="residual.sugar")
plot(x5, stud_c, xlab="chlorides")
plot(x6, stud_c, xlab="free.sulfur.dioxide")
plot(x7, stud_c, xlab="total.sulfur.dioxide")
plot(x8, stud_c, xlab="density")
plot(x9, stud_c, xlab="pH")
plot(x10, stud_c, xlab="sulphates")
plot(x11, stud_c, xlab="alcohol")
plot(stud_c)
qqnorm(stud_c)
qqline(stud_c)

# we pick quadratic and find the outliers
D=cooks.distance(res_p)
which(D>1)

n1 = length(y)
DFB=dfbetas(res_p)
c1 = which((abs(DFB)>2/sqrt(n1))[,1])
c2 = which((abs(DFB)>2/sqrt(n1))[,2])
c3 = which((abs(DFB)>2/sqrt(n1))[,3])
c4 = which((abs(DFB)>2/sqrt(n1))[,4])
c5 = which((abs(DFB)>2/sqrt(n1))[,5])
c6 = which((abs(DFB)>2/sqrt(n1))[,6])
c7 = which((abs(DFB)>2/sqrt(n1))[,7])
c8 = which((abs(DFB)>2/sqrt(n1))[,8])
c9 = which((abs(DFB)>2/sqrt(n1))[,9])
c10 = which((abs(DFB)>2/sqrt(n1))[,10])
c11 = which((abs(DFB)>2/sqrt(n1))[,11])

#find the top 20 most common
vec = c(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11)
out = sort(table(vec), decreasing = TRUE)[1:20]

# combination of all the outliers(probably not goning to use it)
outl = unique(c(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11))

FIT_p=dffits(res_p)
#we found around 366 points outliers that might affect prediction
which(abs(FIT_p)>2*sqrt(12/n1))

#since our goal is to predict the testing set, we get rid of top 50 pts with highest dffits
out3 = sort(abs(FIT_p),decreasing = TRUE)[1:30]

y = y[-out3]
x1 = x1[-out3]
x2 = x2[-out3]
x3 = x3[-out3]
x4 = x4[-out3]
x5 = x5[-out3]
x6 = x6[-out3]
x7 = x7[-out3]
x8 = x8[-out3]
x9 = x9[-out3]
x10 = x10[-out3]
x11 = x11[-out3]

x1sq = x1^2
x2sq = x2^2
x3sq = x3^2
x4sq = x4^2
x5sq = x5^2
x6sq = x6^2
x7sq = x7^2
x8sq = x8^2
x9sq = x9^2
x10sq = x10^2
x11sq = x11^2

res_p = lm(y~x1sq+x2sq+x3sq+x4sq+x5sq+x6sq+x7sq+x8sq+x9sq+x10sq+x11sq+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x1:x2+x1:x3+x1:x4+x1:x5+x1:x6+x1:x7+x1:x9+x1:x9+x1:x10+x1:x11+x2:x3+x2:x4+x2:x5+x2:x6+x2:x7+x2:x8+x2:x9+x2:x10+x2:x11+x3:x4+x3:x5+x3:x6+x3:x7+x3:x8+x3:x9+x3:x10+x3:x11+x4:x5+x4:x6+x4:x7+x4:x8+x4:x9+x4:x10+x4:x11+x5:x6+x5:x7+x5:x8+x5:x9+x5:x10+x5:x11+x6:x7+x6:x8+x6:x9+x6:x10+x6:x11+x7:x8+x7:x9+x7:x10+x7:x11+x8:x9+x8:x10+x8:x11+x9:x10+x9:x11+x10:x11)
summary(res_p)

stud_p = rstudent(res_p)
plot(res_p$fitted.values, stud_p)
plot(x1, stud_p, xlab="fixed.acidity")
plot(x2, stud_p, xlab="volatile.acidity")
plot(x3, stud_p, xlab="citric.acid")
plot(x4, stud_p, xlab="residual.sugar")
plot(x5, stud_p, xlab="chlorides")
plot(x6, stud_p, xlab="free.sulfur.dioxide")
plot(x7, stud_p, xlab="total.sulfur.dioxide")
plot(x8, stud_p, xlab="density")
plot(x9, stud_p, xlab="pH")
plot(x10, stud_p, xlab="sulphates")
plot(x11, stud_p, xlab="alcohol")
plot(stud_p)
qqnorm(stud_p)
qqline(stud_p)

#forward selection
model = y~x1sq+x2sq+x3sq+x4sq+x5sq+x6sq+x7sq+x8sq+x9sq+x10sq+x11sq+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x1:x2+x1:x3+x1:x4+x1:x5+x1:x6+x1:x7+x1:x9+x1:x9+x1:x10+x1:x11+x2:x3+x2:x4+x2:x5+x2:x6+x2:x7+x2:x8+x2:x9+x2:x10+x2:x11+x3:x4+x3:x5+x3:x6+x3:x7+x3:x8+x3:x9+x3:x10+x3:x11+x4:x5+x4:x6+x4:x7+x4:x8+x4:x9+x4:x10+x4:x11+x5:x6+x5:x7+x5:x8+x5:x9+x5:x10+x5:x11+x6:x7+x6:x8+x6:x9+x6:x10+x6:x11+x7:x8+x7:x9+x7:x10+x7:x11+x8:x9+x8:x10+x8:x11+x9:x10+x9:x11+x10:x11

res0 = lm(y~1)
add1(res0, model, test = "F")

res1 = lm(y~x11)
add1(res1, model, test = "F")

res2 = lm(y~x2+x11)
add1(res2, model, test = "F")

res3 = lm(y~x2+x10+x11)
add1(res3, model, test = "F")

res4 = lm(y~x10sq+x2+x10+x11)
add1(res4, model, test = "F")

res5 = lm(y~x10sq+x2+x10+x11+x10:x11)
add1(res5, model, test = "F")

res6 = lm(y~x10sq+x2+x7+x10+x11+x10:x11)
add1(res6, model, test = "F")

res7 = lm(y~x10sq+x2+x7+x10+x11+x7:x10+x10:x11)
add1(res7, model, test = "F")

res8 = lm(y~x10sq+x2+x5+x7+x10+x11+x7:x10+x10:x11)
add1(res8, model, test = "F")

res9 = lm(y~x9sq+x10sq+x2+x5+x7+x10+x11+x7:x10+x10:x11)
add1(res9, model, test = "F")

res10 = lm(y~x9sq+x10sq+x2+x5+x7+x10+x11+x2:x7+x7:x10+x10:x11)
add1(res10, model, test = "F")

res11 = lm(y~x9sq+x10sq+x2+x3+x5+x7+x10+x11+x2:x7+x7:x10+x10:x11)
add1(res11, model, test = "F")

res12 = lm(y~x9sq+x10sq+x2+x3+x5+x6+x7+x10+x11+x2:x7+x7:x10+x10:x11)
add1(res12, model, test = "F")

res13 = lm(y~x7sq+x9sq+x10sq+x2+x3+x5+x6+x7+x10+x11+x2:x7+x7:x10+x10:x11)
add1(res13, model, test = "F")

res14 = lm(y~x7sq+x9sq+x10sq+x2+x3+x5+x6+x7+x10+x11+x2:x7+x6:x11+x7:x10+x10:x11)
add1(res14, model, test = "F")

#term x6:x10 not very significant but still try to add anyways
res15 = lm(y~x7sq+x9sq+x10sq+x2+x3+x5+x6+x7+x10+x11+x2:x7+x6:x10+x6:x11+x7:x10+x10:x11)
add1(res15, model, test = "F")

#compare the result, r-sq doesn't increase a lot
summary(res14)
summary(res15)

#stepwise selection
res_step0 = lm(y~1)
add1(res_step0, model, test = "F")

res_step1 = lm(y~x11)
drop1(res_step1, y~x11, test = "F")
add1(res_step1, model, test = "F")

res_step2 = lm(y~x2+x11)
drop1(res_step2, y~x2+x11, test = "F")
add1(res_step2, model, test = "F")

res_step3 = lm(y~x2+x10+x11)
drop1(res_step3, y~x2+x10+x11, test = "F")
add1(res_step3, model, test = "F")

res_step4 = lm(y~x10sq+x2+x10+x11)
drop1(res_step4, y~x10sq+x2+x10+x11, test = "F")
add1(res_step4, model, test = "F")

res_step5 = lm(y~x10sq+x2+x10+x11+x10:x11)
drop1(res_step5, y~x10sq+x2+x10+x11+x10:x11, test = "F")

res_step6 = lm(y~x10sq+x2+x10+x10:x11)
add1(res_step6, model, test = "F")

res_step7 = lm(y~x10sq+x2+x7+x10+x10:x11)
drop1(res_step7, y~x10sq+x2+x7+x10+x10:x11, test = "F")
add1(res_step7, model, test = "F")

res_step8 = lm(y~x10sq+x2+x7+x10+x7:x10+x10:x11)
drop1(res_step8, y~x10sq+x2+x7+x10+x7:x10+x10:x11, test = "F")

res_step9 = lm(y~x2+x7+x10+x7:x10+x10:x11)
add1(res_step9, model, test = "F")

res_step10 = lm(y~x2+x5+x7+x10+x7:x10+x10:x11)
drop1(res_step10, y~x2+x5+x7+x10+x7:x10+x10:x11, test = "F")
add1(res_step10, model, test = "F")

res_step11 = lm(y~x9sq+x2+x5+x7+x10+x7:x10+x10:x11)
drop1(res_step11, y~x9sq+x2+x5+x7+x10+x7:x10+x10:x11, test = "F")
add1(res_step11, model, test = "F")

res_step12 = lm(y~x9sq+x2+x5+x7+x10+x2:x7+x7:x10+x10:x11)
drop1(res_step12, y~x9sq+x2+x5+x7+x10+x2:x7+x7:x10+x10:x11, test = "F")

res_step13 = lm(y~x9sq+x2+x5+x10+x2:x7+x7:x10+x10:x11)
add1(res_step13, model, test = "F")

res_step14 = lm(y~x9sq+x2+x3+x5+x10+x2:x7+x7:x10+x10:x11)
drop1(res_step14, y~x9sq+x2+x3+x5+x10+x2:x7+x7:x10+x10:x11, test = "F")
add1(res_step14, model, test = "F")

res_step15 = lm(y~x9sq+x2+x3+x5+x6+x10+x2:x7+x7:x10+x10:x11)
drop1(res_step15, y~x9sq+x2+x3+x5+x6+x10+x2:x7+x7:x10+x10:x11, test = "F")
add1(res_step15, model, test = "F")

#x7sq really close to being significant, add it and try out the model
res_step16 = lm(y~x7sq+x9sq+x2+x3+x5+x6+x10+x2:x7+x7:x10+x10:x11)
drop1(res_step16, y~x7sq+x9sq+x2+x3+x5+x6+x10+x2:x7+x7:x10+x10:x11, test = "F")
add1(res_step16, model, test = "F")

#compare res_step15 and 16
summary(res_step15)
summary(res_step16)
sum(anova(res_step16)[1])
anova(res_step16)
#I decide to pick res_step16, although compare to res15/res14 it's r-sq is lower, but all the terms are significant. could also pick res_step15


#Validation/Prediction on the testing dataset
#The rsquare is pretty bad as expected
 
library(qpcR)
pres <- PRESS(res_step16)
pre = as.numeric(pres[1])
r2_pred = 1 - (pre/sum(anova(res_step16)[2])) 
r2_pred

yhat = res_step16$coefficients[1]+res_step16$coefficients[2]*x7_t^2+res_step16$coefficients[3]*x9_t^2+res_step16$coefficients[4]*x2_t+res_step16$coefficients[5]*x3_t+res_step16$coefficients[6]*x5_t+res_step16$coefficients[7]*x6_t+res_step16$coefficients[8]*x10_t+res_step16$coefficients[9]*x2_t*x7_t+res_step16$coefficients[10]*x7_t*x10_t+res_step16$coefficients[11]*x10_t*x11_t
r2 = 1 -sum((y_t-yhat)^2)/sum((y_t-mean(y_t))^2)
e = sum((y_t - yhat)^2/length(y_t))

x = as.data.frame(cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11))
colnames(x)=c('FA','VA','CA','RS','C','FS','TS','D','PH','S','A')
x = as.data.frame(cbind(x$VA,x$CA,x$C,x$FS,x$S,(x$TS^2),(x$PH^2),x$VA*x$TS,x$TS*x$S,x$S*x$A))
colnames(x)=c('VA','CA','C','FS','S','TS2','PH2','VATS','TSS','SA')
VIF=diag(solve(cor(x)))
VIF



x=as.data.frame(sweep(cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11),2, FUN='-',apply(cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11),2,mean)))
colnames(x)=c('FA','VA','CA','RS','C','FS','TS','D','PH','S','A')
x = as.data.frame(cbind(x$VA,x$CA,x$C,x$FS,x$S,(x$TS^2),(x$PH^2),x$VA*x$TS,x$TS*x$S,x$S*x$A))
colnames(x)=c('VA','CA','C','FS','S','TS2','PH2','VATS','TSS','SA')
VIF=diag(solve(cor(x)))
VIF


plot(lm.ridge(y~VA+CA+C+FS+S+TS2+PH2+VATS+TSS+SA,data=x,lambda=seq(0,50,by=5)))

rid=lm.ridge(y~VA+CA+C+FS+S+TS2+PH2+VATS+TSS+SA,data=x,lambda=20)
coeff = coefficients(rid)

yhat1 = coeff[1]+coeff[2]*x2_t+coeff[3]*x3_t+coeff[4]*x5_t+coeff[5]*x6_t+coeff[6]*x10_t+coeff[7]*x7_t^2+coeff[8]*x9_t^2+coeff[9]*x2_t*x7_t+coeff[10]*x7_t*x10_t+coeff[11]*x10_t*x11_t
r2_1 = 1 -sum((y_t-yhat1)^2)/sum((y_t-mean(y_t))^2)
e_1 = sum((y_t - yhat1)^2/length(y_t))

x=as.data.frame(cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11))
colnames(x)=c('FA','VA','CA','RS','C','FS','TS','D','PH','S','A')
x=cbind(x,x^2,x$FA*x$VA,x$FA*x$CA,x$FA*x$RS,x$FA*x$C,x$FA*x$FS,x$FA*x$TS,x$FA*x$D,x$FA*x$PH,x$FA*x$S,x$FA*x$A,x$VA*x$CA,x$VA*x$RS,x$VA*x$C,x$VA*x$FS,x$VA*x$TS,x$VA*x$D,x$VA*x$PH,x$VA*x$S,x$VA*x$A,x$CA*x$RS,x$CA*x$C,x$CA*x$FS,x$CA*x$TS,x$CA*x$D,x$CA*x$PH,x$CA*x$S,x$CA*x$A,x$RS*x$C,x$RS*x$FS,x$RS*x$TS,x$RS*x$D,x$RS*x$PH,x$RS*x$S,x$RS*x$A,x$C*x$FS,x$C*x$TS,x$C*x$D,x$C*x$PH,x$C*x$S,x$C*x$A,x$FS*x$TS,x$FS*x$D,x$FS*x$PH,x$FS*x$S,x$FS*x$A,x$TS*x$D,x$TS*x$PH,x$TS*x$S,x$TS*x$A,x$D*x$PH,x$D*x$S,x$D*x$A,x$PH*x$S,x$PH*x$A,x$S*x$A)
colnames(x)=c('FA','VA','CA','RS','C','FS','TS','D','PH','S','A','FA^2','VA^2','CA^2','RS^2','C^2','FS^2','TS^2','D^2','PH^2','S^2','A^2','FA:VA','FA:CA','FA:RS','FA:C','FA:FS','FA:TS','FA:D','FA:PH','FA:S','FA:A','VA:CA','VA:RS','VA:C','VA:FS','VA:TS','VA:D','VA:PH','VA:S','VA:A','CA:RS','CA:C','CA:FS','CA:TS','CA:D','CA:PH','CA:S','CA:A','RS:C','RS:FS','RS:TS','RS:D','RS:PH','RS:S','RS:A','C:FS','C:TS','C:D','C:PH','C:S','C:A','FS:TS','FS:D','FS:PH','FS:S','FS:A','TS:D','TS:PH','TS:S','TS:A','D:PH','D:S','D:A','PH:S','PH:A','S:A')


library(knitr) 
library(tidyverse, warn.conflict=F)
mtcars_cor <- Hmisc::rcorr(as.matrix(x))
cors <- function(df) { 
  # turn all three matrices (r, n, and P into a data frame)
  M <- Hmisc::rcorr(as.matrix(df))
  # return the three data frames in a list return(Mdf)
  Mdf <- map(M, ~data.frame(.x))
}

cors(x) %>%
  map(~rownames_to_column(.x, var="measure1")) %>%
  # format each data set (r,P,n) long 
  map(~pivot_longer(.x, -measure1, "measure2")) %>%
  # merge our three list elements by binding the rows
  bind_rows(.id = "id") %>%
  pivot_wider(names_from = id, values_from = value) %>%
  mutate(sig_p = ifelse(P < .05, T, F), p_if_sig = ifelse(P <.05, P, NA), r_if_sig = ifelse(r <.05, r, NA)) %>% 
  head() %>%
  kable()
formatted_cors <- function(df){
  cors(df) %>%
    map(~rownames_to_column(.x, var="measure1")) %>%
    map(~pivot_longer(.x, -measure1, "measure2")) %>% 
    bind_rows(.id = "id") %>%
    pivot_wider(names_from = id, values_from = value) %>%
    mutate(sig_p = ifelse(P < .05, T, F), p_if_sig = ifelse(P <.05, P, NA), r_if_sig = ifelse(P <.05, r, NA)) 
}
formatted_cors(x) %>% 
  ggplot(aes(measure1, measure2, fill=r, label=round(r_if_sig,2))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Pearson's\nCorrelation", title="Correlations in Mtcars", subtitle="Only significant Pearson's correlation coefficients shown") + scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(-1,1)) +
  geom_text() +
  theme_classic() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  theme(text=element_text(family="Roboto"))
