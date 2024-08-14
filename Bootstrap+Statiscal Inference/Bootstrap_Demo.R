n <- 20
x <- runif( n )
y <- 2 * x + rnorm( n )
plot( x, y )
plot( x, y, bty = 'n' )
fit <- lm( y ~ x )
abline( coef( fit ), lty = 2 )
abline( c( 0, 2 ) )
theta.hat <- cor( x, y )
print(theta.hat)
B <- 100
theta.star <- replicate( B, { i.star <- sample( 1:n, n, replace = TRUE ); cor( x[i.star], y[i.star] ) } )
print(theta.star)
print(var(theta.star))
#qqnorm( theta.star); qqline( theta.star )
hist( theta.star, 'FD', freq = FALSE )
#fitting a curve over the sampling distribution
curve( dnorm( x, mean = mean( theta.star ), sd = sd( theta.star ) ), min( theta.star ), max( theta.star ), 501, add = TRUE )
alpha <- 0.05
zab2 <- qnorm( 1 - alpha / 2 )
zab2
ci.normal <- theta.hat + c(-1,1)*sd(theta.star)*zab2
print(ci.normal)
lines(ci.normal,rep(0.1,2),col='red')