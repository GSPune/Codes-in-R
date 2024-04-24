#PCA Intro -- DataMatrix,Pair Plots,Correlation
#n is number of individuals
n = 10
p = 3

x = runif(30,10,20)
dm = matrix(x,nrow=n,ncol=p,byrow=TRUE)
par(mfrow=c(p,p))#splits plot panel into p rows and p cols

for (i in 1:p){
    for(j in 1:p){
        plot(dm[,i],dm[,j])
    }
}
# dm[2,3]
#Introduce a linear relationship bwt X3 and X1
dm[,3] = 1.32*dm[,1]+0.56

# WAF to take in two vectors and return Karl Pearson Coefficient
# corr = function(v1,v2){
#     c = cov(v1,v2)
#     return (c/(sd(v1)*sd(v2)))
# }

corr = function(x,y){
    n = length(x)
    covariance = (sum(x*y)/n) - (mean(x)*mean(y))
    sx = sqrt(sum(x^2)/n-(mean(x))^2)
    sy = sqrt(sum(y^2)/n-(mean(y))^2)
    temp = covariance/(sx*sy)
    return(temp)
}


corMat = matrix(nrow=p,ncol=p)
for (i in 1:p){
    for(j in 1:p){
        corMat[i,j] = corr(dm[,i],dm[,j])
    }
}
#accuracy check
corMat
cor(dm)