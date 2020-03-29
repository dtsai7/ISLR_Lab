require(ISLR)
require(MASS)

## linear discriminant analysis
lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
lda.fit
plot(lda.fit)

Smarket.2005 = subset(Smarket, Year=2005)
lda.pred = predict(lda.fit, Smarket.2005)
## check data type
class(lda.pred)
data.frame(lda.pred)[1:5,]

#confusion matrix
table(lda.pred$class, Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

