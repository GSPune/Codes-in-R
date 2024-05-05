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
dev.off()

#calculate the eigenvalues of the sample dispersion
# e=eigen(corMat)
# ev=e$values
# ev

#scree plot

# N = c(1,2,3)
# plot(ev,N)
# plot(1:3,ev,type="1",col="red",xlab="PC Number",ylab="Eigenvalues")
# points(1:3,ev,col="green",pch=19)

# s = sum(ev)
# cs = cumsum(ev)
# contrib = data.frame("PC Nos" = 1:3,"percent cont." = (cs/s)*100)

# evec = e$vectors
# is.matrix(evec)

#checking properties of a matrix of eigen-vectors
# evec[,1]
# o = evec[,1]%*%(t(evec[,1]))
# i = (t(evec[,1]))%*%evec[,1]

# l=c()
# #checking whether eigenvectors have unit length
# for(i in 1:p){
#     l[i] = sqrt(sum(evec[(1:3),i]))
# }
# evec[(1:3),1]
# round(l)

# d = diag(ev)
# evec%*%d%*%(t(evec))

# evec[,1]
# evec[,2]
# #dm is not standardized..
# DM1 = dm%*%evec
# par(mfrow=c(p,p))
# for (i in 1:p){
#     for(j in 1:p){
#         plot(DM1[,i],DM1[,j])
#     }
# }

#centering data matrix
cdm = matrix(nrow=n,ncol=p)
for (i in 1:p)
    cdm[,i] = (dm[,i]-mean(dm[,i])/sd(dm[,i]))

DM2 = cdm%*%evec
par(mfrow=c(p,p))
for (i in 1:p){
    for(j in 1:p){
        plot(DM1[,i],DM1[,j])
    }
}

#inbuilt command
e1 = prcomp(dm,scale=TRUE)
ev1=(e1$sdev)^2
ev
evec1=e1$rotation
ev1
