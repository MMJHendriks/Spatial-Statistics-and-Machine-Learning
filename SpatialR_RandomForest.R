install.packages("randomForest")
install.packages("devtools")
install.packages("gbm")

library(randomForest)
library(devtools)
library(MASS)
library(gbm)

Boston
names(Boston)

set.seed(1)
train=sample(1:nrow(Boston), nrow(Boston)/2)
######################
bag.boston=randomForest(medv~., data=Boston, subset=train, mtry=13, importance=TRUE, ntree=1000)
bag.boston
#mtry = 13 IV 
#nr of tree by default 500, can change using ntree
#classification or regression backing? type regression, mean of squared residual
# continuous variable go for regression, classes use classification

###################
yhat.bag=predict(bag.boston, newdata = Boston[-train,])
boston.test=Boston[-train,"medv"]

plot(yhat.bag, boston.test)
abline(0,1)
#y value of test dat, x prediction data 
#cannot predict these top points
#abline seems to go throught the points 
#taking care of some of the variances, but see some outliers
round(mean((yhat.bag-boston.test)^2),2) #calculate MSE
# how do you know if MSE is okay?
#play around with number of variables nr of draws and see if MSE decreases

#change the number of trees grown by randomForest
bag.boston=randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=25)
yhat.bag=predict(bag.boston, newdata=Boston[-train,])

mean((yhat.bag-boston.test)^2)
#change less or more than 10 %
# so 500 is better, what about 10000? where do you stop?

bag.boston=randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=10000)
yhat.bag=predict(bag.boston, newdata=Boston[-train,])

mean((yhat.bag-boston.test)^2)
# way more computation time, play around with the numbers 

##############################
## random forest
set.seed(1)
rf.boston=randomForest(medv~.,data = Boston, subset=train, mtry=4, importance=TRUE)
yhat.rf=predict(rf.boston, newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
#similar performance
#RF better

importance (rf.boston) #%incMSE mean decrease of accuracy in prediction
                      # when a given variable is excluded from the model ... 
varImpPlot(rf.boston)

######################### 
#Boosting

boost.boston1=gbm(medv~.,data=Boston[train,],distribution="gaussian",
                  n.trees=5000, interaction.depth = 4)
summary(boost.boston1)

##### partial dependence plots based on marginal effect of select variable
par(mfrow=c(1,2))
plot(boost.boston1,i="rm")
plot(boost.boston1,i="lstat")

######
yhat.boost1=predict(boost.boston1, newdata = Boston[-train,],
                    n.trees=5000)
                    
mean((yhat.boost1-boston.test)^2)                    
#better, is it worth it? it changed more than 10% so yes

###### change lambda
boost.boston2=gbm(medv~., data=Boston[train,],distribution = "gaussian",
                  n.trees = 5000, interaction.depth = 4, shrinkage=0.1, verbose=F)
#don't change interacion and shrinkage most of time
yhat.boost2=predict(boost.boston2, newdata=Boston[-train,],
                    n.trees=5000)
mean((yhat.boost2-boston.test)^2)
# would say it is worth it 






                    
                    
                    