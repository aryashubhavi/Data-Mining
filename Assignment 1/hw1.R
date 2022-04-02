data(trees)
X=as.matrix(trees)
X=scale(X,center=TRUE,scale=FALSE)

girth <- X[, 1]
height <-X[, 2]
volume <-X[, 3]
mydata <-data.frame(girth,height,volume)

install.packages("ggplot2")  
install.packages("GGally")

library("ggplot2")                     # Load ggplot2 package
library("GGally")                      # Load GGally package

ggpairs(mydata) 

#######

modelGirth <- lm(girth~height + volume, data = mydata)
summary(modelGirth)
summary(modelGirth)$coefficient

modelGirthremVol <- lm(girth~height, data = mydata)
summary(modelGirthremVol)
summary(modelGirthremVol)$coefficient

####

cov(mydata)
cov(trees)
