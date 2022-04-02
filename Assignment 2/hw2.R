##Q1

x = as.matrix(read.csv("rachmaninov_pc2_onset.csv"))
matplot(t(x[, -c(1,2)]), type="l")

## Covariance matrix proof
c <- ncol(x) #number of variables
r <- nrow(x) #number of elements

#means for each column
x_mean <-matrix(data=1, nrow=r) %*% cbind(mean(x[,1]),mean(x[,2]),mean(x[,3]),mean(x[,4]),mean(x[,5]),mean(x[,6]),mean(x[,7]),mean(x[,8]),mean(x[,9]),mean(x[,10]),mean(x[,11]),mean(x[,12]))

#difference matrix

v1 <- c(x[,1])
v2 <- c(x[,2])
v3 <- c(x[,3])
v4 <- c(x[,4])
v5 <- c(x[,5])
v6 <- c(x[,6])
v7 <- c(x[,7])
v8 <- c(x[,8])
v9 <- c(x[,9])
v10 <- c(x[,10])
v11 <- c(x[,11])
v12 <- c(x[,12])

new_x <- cbind(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12)
D <- new_x - x_mean

#covariance matrix creation
cova_x <- (r-1)^-1*t(D) %*% D


#with R function
auto_cov <- cov(x)


#Part c
install.packages("MASS")                            # Install MASS package
library("MASS")                                     # Load MASS package
set.seed(98989)                                     # Set seed for reproducibility
my_n <- 1000                                       # Specify sample size
my_mu <- colMeans(x, dims = 1)                             # Specify the means of the variables
my_Sigma <- cov(x)  # Specify the covariance matrix of the variables
mynormal <- mvrnorm(n = my_n, mu = my_mu, Sigma = my_Sigma)  # Random sample from multivariate normal distribution

library("dplyr")

# creating a data frame
data_frame <- data.frame(mynormal)

# eliminating NA values
data_without_na <- data_frame %>%                      
  replace(is.na(.), 0) 
print("Row Wise Sum")

data_mod <- data_without_na%>%
  mutate(sum_of_rows = rowSums(.))
print(data_mod)


#######Q3#######
mystery <- as.matrix(read.csv("mystery.csv"))
cov_mystery <- cov(mystery) #covariance matrix
mystery.svd <- svd(mystery) #singular value decomposition
mystery.svd

qr(mystery)$rank
Null(mystery)


####Q3 - part c #####

# Installing required package
install.packages("dplyr")

# Loading the package
library(dplyr)

mystery <- as.matrix(read.csv("mystery.csv"))

# Importing excel file
str(mystery)


data <- as.matrix(read.csv("mystery.csv"))

# Apply PCA using prcomp function
# Need to scale / Normalize as
# PCA depends on distance measure
my_pca <- prcomp(mystery, scale = TRUE,
                 center = TRUE, retx = T)
names(my_pca)

# Summary
summary(my_pca)
my_pca


# View the principal component loading
# my_pca$rotation[1:5, 1:4]
my_pca$rotation

# See the principal components
dim(my_pca$x)
my_pca$x

# Plotting the resultant principal components
# The parameter scale = 0 ensures that arrows
# are scaled to represent the loadings
biplot(my_pca, main = "Biplot", scale = 0)

# Compute standard deviation
my_pca$sdev

# Compute variance
my_pca.var <- my_pca$sdev ^ 2
my_pca.var

# Proportion of variance for a scree plot
propve <- my_pca.var / sum(my_pca.var)
propve

# Plot variance explained for each principal component
plot(propve, xlab = "principal component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b",
     main = "Scree Plot")
# Plot the cumulative proportion of variance explained
plot(cumsum(propve),
     xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# Find Top n principal component
# which will atleast cover 90 % variance of dimension
which(cumsum(propve) >= 0.9)[1]


# Predict mpg using first 4 new Principal Components
# Add a training set with principal components
train.data <- data.frame(mystery[,16], my_pca$x[, 1:4])

# Running a Decision tree algporithm
## Installing and loading packages
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

rpart.model <- rpart( mystery[,16] ~ .,
                     data = train.data, method = "anova")

rpart.plot(rpart.model)

######## Q4 ########
install.packages("mlbench")
library("mlbench")
data("Ionosphere")
ion=as.matrix(Ionosphere)
library(dplyr)

good <- ion[which(ion[,35] == "good"), ]
bad <- ion[which(ion[,35] == "bad"), ]

goodmat <- data.matrix(good)
badmat <- data.matrix(bad)

goodmat<- goodmat[,-c(35)]
goodmat<- goodmat[,-c(1,2)]
badmat<- badmat[,-c(35)]
badmat<- badmat[,-c(1,2)]
goodmat<- matrix(as.numeric(goodmat),    # Convert to numeric matrix
                  ncol = ncol(goodmat))
badmat<- matrix(as.numeric(badmat),    # Convert to numeric matrix
                 ncol = ncol(badmat))

covGood <- cov(goodmat)
covBad <- cov(badmat)



mahGood <- data.matrix(mahalanobis(goodmat, colMeans(goodmat), covGood))
mahBad <- data.matrix(mahalanobis(badmat, colMeans(badmat), covBad))

##Confusion matrix
install.packages("caret")

library(caret)
library(InformationValue)
library(ISLR)

set.seed(1)
mydata <- ion
sample <- sample(c(TRUE, FALSE), nrow(mydata), replace=TRUE, prob=c(0.7,0.3))
train <- mydata[sample, ]
test <- mydata[!sample, ]

test <- ifelse(mahGood < mahBad[,1], 1, 0) #1 if bad, 0 if good
predicted <- (mydata=train)
optimal <- optimalCutoff(test[,36], predicted)[1]
confusionMatrix(test$default, predicted)

