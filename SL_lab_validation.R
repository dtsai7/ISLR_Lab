require(ISLR)
require(boot)
?cv.glm
plot(mpg~horsepower, data = Auto)


## leave-one-out cross-validation (LOOCV)
glm.fit = glm(mpg~horsepower, data = Auto)
cv.glm(Auto, glm.fit)$delta
# result: [1] 24.23151 24.23114
# time consuming if n is large
# therefore, we need the formula to speed up the process


## write simple function for LOOCV formula
loocv = function(fit){
  h = lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
  
}


loocv(glm.fit)
# much faster


cv.error = rep(0, 5)
degree = 1:5
for (d in degree){
  glm.fit = glm(mpg~poly(horsepower, d), data = Auto)
  cv.error[d] = loocv(glm.fit)
}


plot(degree, cv.error, type = "b")



## 10-fold CV
cv.error10 = rep(0,5)
for (d in degree){
  glm.fit = glm(mpg~poly(horsepower, d), data = Auto)
  cv.error10[d] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

lines(degree, cv.error10, type = "b", col = "red")



