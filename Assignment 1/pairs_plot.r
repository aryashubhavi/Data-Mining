# first example of plotting in R: pairs plot

data(iris);  # include the famous iris data
iris	     # type out the data
plot(iris);  #  stop here to get simple pairs plot.

stopp;

#--------------------------------
# some simple matrix manipulations in R

ex1 = iris[1,]     # the first row (example) of data matrix
seplen = iris[,1]   # the first column (feature) of data matrix (sepal length)
sepwid = iris[,2];  # sepal width
n = length(seplen);  # number of examples/vectors/data points/observations/
type = rep(0,n);
type[iris[,5] == "virginica"] = 1;
type[iris[,5] == "versicolor"] = 2;
type[iris[,5] == "setosa"] = 3;  # type is now a vector of 1-3 for 3 types
#plot(seplen,sepwid,pch=type);   # scatter plot with different symbols for each iris type.  





