# 1000 samples each of size 30, from U(0, θ) distribution
roll.number <- 2307
theta <- 0.1 * (roll.number %% 100)

n <- 30
m <- 1000

# theta . hat below will be a m X 2 matrix ;
# two columns for the three estimators:
#t returns the transpose
theta.hat <- t(sapply(1:m,function(e) {
    x <- runif(n,0,theta)
    c(max(x),2*mean(x),(2+1/n)*mean(x))
}))

#theta.hat

# estimated MSE for the three estimators
mse.hat <- apply ( theta.hat , 2, function ( x ) { mean ( x - theta ) ^2 } )

# true MSE
mse <- theta^2 * c(
    1 / ( n + 1 ) ^2 + n / ( n + 2 ) - ( n / ( n + 1 ) ) ^2 ,
    1 / ( 3 * n ) ,
    0.25 / n ^2 + ( 2 + 1 / n ) ^2 / ( 12 * n )
)

# difference between true MSE and estimated MSE
mse.table <- cbind ( mse , mse.hat , abs ( mse - mse.hat ) , abs ( ( mse - mse.hat ) / mse
) )
colnames ( mse.table ) <- c( 'mse', 'mse.hat ', 'absdiff ','reldiff' )
rownames ( mse.table ) <- paste ( 'theta', 1: ncol ( theta.hat ) , sep = '')
print ( mse.table )

op <- par ( mfrow = c( 1, ncol ( theta.hat ) ) )
for ( i in 1: ncol (theta.hat ) )
	hist ( ( theta.hat[ ,i] - theta ) / sd( theta.hat [,i] ) ,
	breaks = 'FD', col = 'gray', border = 'white', main = paste ( 'Estimator',i )
	,xlab = expression (( hat( theta ) - theta) / hat (se)(hat( theta ))))
par ( op )