# 1000 samples each of size 30, from U(0, θ) distribution
roll.number <- 2307
theta <- 0.1 * (roll.number %% 100)

n <- 30
m <- 1000

# theta . hat below will be a m X 2 matrix ;
# two columns for the three estimators:

theta.hat <- t(sapply(1:m,function(e) {
    x <- runif(n,0,theta)
    c(max(x),2*mean(x),(2+1/n)*mean(x))
}))

theta.hat



