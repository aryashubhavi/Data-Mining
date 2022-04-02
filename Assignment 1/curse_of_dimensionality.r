# simple example demonstrating the "curse of dimensionality"
# create random points in a p-dimensional hypercube for different values of p  
# The bigger the dimension, p,  the more the mass of points is clustered around the edges
#
# This example also demonstrates some basic issues of matrices in R

p = 30     # the dimension (number of features) in the data
n = 100   # number of data points
X = matrix(runif(p*n),nrow=n,ncol=p);  # create a nxp random data matrix.  each coord of each observation is unif(0,1)
pairs(X);  # pairs plot to check the data behaves as we expect
dist_to_edge = rep(0,n);  # this will hold the distance to the hypercube edge for each data point.
for (i in 1:n) {
   dist_to_edge[i] = min(.5 - abs(.5 - X[i,]))  # the distance to the closest edge of hypercube
}
hist(dist_to_edge)
ans = ecdf(dist_to_edge)
ans(0.0273)
