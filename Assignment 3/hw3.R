
###Q2

arrests = as.matrix(read.csv("Arrests.csv"))

###Part a
white <- arrests[which(arrests[,3] == "White"), ]
wmat <- data.matrix(white)
wh_released<- wmat[,c(2)]

black <- arrests[which(arrests[,3] == "Black"), ]
bmat <- data.matrix(black)
bl_released<- bmat[,c(2)]

White_Yes <- which(wh_released== "Yes")
Black_Yes <- which(bl_released== "Yes")

White_prob = length(White_Yes)/length(wh_released)
Black_prob = length(Black_Yes)/length(bl_released)

arrests_mos = read.csv("Arrests.csv")
table_prob <- table(arrests_mos$colour, arrests_mos$released)
mosaicplot(table_prob, main="Mosaic Plot of Black and White Released Probability")

print(White_prob)

print(Black_prob)


#### Part b

table_col_rel_emp <- table(arrests_mos$employed, arrests_mos$colour, arrests_mos$released )
mosaicplot(table_col_rel_emp, shade = TRUE, margin = list(1:2, 3), main="Mosaic Plot of Colour, Employed and Released")
fm <- loglin(table_col_rel_emp, list(1:2, 3))
pchisq(fm$pearson, fm$df, lower.tail = FALSE)

mosaicplot(table_prob, main="Mosaic Plot of Color and Released")


### part c
table_col_rel_check <- table(arrests_mos$checks, arrests_mos$colour, arrests_mos$released )
mosaicplot(table_col_rel_check, shade = TRUE, margin = list(1:2, 3), main="Mosaic Plot of Colour, Checks and Released")
fmb <- loglin(table_col_rel_check, list(1:2, 3))
pchisq(fmb$pearson, fmb$df, lower.tail = FALSE)


#### Q3

##Part a
samp <- rexp(1000)
plot(ecdf(samp))

##Part b

ecdf_func <- ecdf(samp)
ecdf_samp <- data.frame(lapply(samp,ecdf_func))

ecdf_samp <- as.vector(ecdf_samp)
mynewcdf <-ecdf(ecdf_samp)
plot(mynewcdf)

#Part c
rand <- rnorm(1000)
plot(ecdf(rand))

ecdf_func_rand <- ecdf(rand)
ecdf_rand_samp <- data.frame(lapply(rand,ecdf_func_rand))

ecdf_rand_samp <- as.matrix(ecdf_rand_samp)
ecdf_rand_samp <- ecdf_rand_samp[1,]
ecdf_rand_samp <- as.numeric(ecdf_rand_samp)
mynewcdf_rand <- ecdf(ecdf_rand_samp)
plot(mynewcdf_rand)



################################################
#######Q4
time_series <- read.csv("time_series.csv")
# Libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

# Load dataset
data <- time_series
date <- (1:200)
z <- sample(date,1)
var_names <- names(data)
a<- get(var_names[z], data)

      
# Usual area chart
data <- time_series
p <- data %>%
  ggplot( aes(x=date, y=a)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("time series values")+
  theme_ipsum()

# Turn it interactive with ggplotly
p <- ggplotly(p)
p


#### Part b

####Seasonality (12 months)
library(gridExtra)

#V30
ndata <- time_series[1:12,]
newdate <- (1:12)
py<- ndata$V30
p1 <- ndata %>%
  ggplot( aes(x=newdate, y=py)) +
  geom_point(color="red") + geom_line(color="#69b3a2")+
  ylab("time series values")+xlab("Model V30")+
  theme_ipsum()
p1

##V72
newdate <- (1:12)
py4<- ndata$V72
p4 <- ndata %>%
  ggplot( aes(x=newdate, y=py4)) +
  geom_point(color="green") + geom_line(color="#69b3a2")+
  ylab("time series values")+xlab("Model V72")+
  theme_ipsum()
p4

##V108
newdate <- (1:12)
py5<- ndata$V108
p5 <- ndata %>%
  ggplot( aes(x=newdate, y=py5)) +
  geom_point(color="green") + geom_line(color="#69b3a2")+
  ylab("time series values")+xlab("Model V108")+
  theme_ipsum()
p5

##V108
newdate <- (1:12)
py6<- ndata$V59
p6 <- ndata %>%
  ggplot( aes(x=newdate, y=py6)) +
  geom_point(color="green") + geom_line(color="#69b3a2")+
  ylab("time series values")+xlab("Model V59")+
  theme_ipsum()
p6

grid.arrange(p1,p4,p5,p6, nrow = 3)

###
library(ggfortify)
library(zoo)
library(tseries)
library(astsa)
library(forecast)
library(ggplot2)

data <- time_series
myts <- ts (time_series, start=c(2009, 1), end=c(2014, 12), frequency=200)
ggseasonplot(myts)
