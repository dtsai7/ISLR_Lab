require(ISLR)
names(Smarket)
summary(Smarket)
?Smarket
pairs(Smarket, col=Smarket$Direction)

#logistic regression
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
              data=Smarket, 
              family=binomial)

summary(glm.fit)
glm.probs = predict(glm.fit, type="response")
glm.probs[1:5]
#classification based on threshold 0.5
glm.pred = ifelse(glm.probs>0.5, "Up", "Down")
glm.pred[1:10]

attach(Smarket)
#confusion matrix
table(glm.pred, Direction)
mean(glm.pred==Direction)


#--------------make training and test data-------------------
train = Year<2005
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
              data=Smarket, 
              family=binomial,
              subset=train)
glm.prob = predict(glm.fit, newdata=Smarket[!train,], type="response")
glm.pred = ifelse(glm.probs>0.5, "Up", "Down")

Direction.2005 = Smarket$Direction[!train]
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

# simplify model to avoid overfitting
glm.fit = glm(Direction~Lag1+Lag2, 
                    data=Smarket, 
                    family=binomial,
                    subset=train)

