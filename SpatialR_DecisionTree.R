#install.packages("MASS")
#install.packages("party")
#install.packages("tree")

library(MASS)
library(party)
library(tree)

Boston
?Boston
#median house value

set.seed(1)
train=sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train) #fit the tree to the training data
summary(tree.boston) 
#deviance; how good is the decision tree

plot(tree.boston)
text(tree.boston, pretty=0)
#rm avergae nr rooms per dwelling
#lstat ; lower tatus of the population (percent)
#crim per capita crime rate by t
#7 nodes

cv.boston=cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type='b')

prune.boston=prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston,pretty=0)

yhat=predict(tree.boston, newdata=Boston[-train,]) #use the unpruned tree to make predictgions on the test set
boston.test=Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2)
#more branches you MSE would get worse

