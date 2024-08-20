n <- 5
B <- 10000
m1s <- replicate(B,{x1 <- rexp(n);mean(x1)})
m2s <- replicate(B,{x2 <- rcauchy(n);mean(x2)})
hist(m1s,'FD',freq = FALSE)
curve(dexp( x, rate = mean(m1s)), min( m1s), max(m1s ), 501, add = TRUE )
hist(m2s,'FD',freq = FALSE)
