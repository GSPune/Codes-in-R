source("PCA Intro & Basics.R")

#Evaluate the Eigenvalues and Eigen-vectors of the sample dispersion matrix 'corMat'
e=eigen(corMat)
ev=e$values
evec = e$vectors
is.matrix(evec)

#scree plot
# N = c(1,2,3)
plot(1:p,ev)
plot(1:p,ev,type="l",col="red",xlab="PC Number",ylab="Eigenvalues",main="Scree Plot")
points(1:p,ev,col="green",pch=19)

#Determine Number of PCs based on their contribution in variation
s = sum(ev)
cs = cumsum(ev)
contrib = data.frame("PC Numbers" = 1:3,"Percent Cont." = (cs/s)*100)

#checking properties of a matrix of eigen-vectors
#first eigenvector
e1 = evec[,1]
#l^t * l = 1
o = e1%*%(t(e1)) # outer product
i = (t(e1))%*%e1 # dot product is 1

#Evaluating the norm of eigenvectors and checking whether it's 1
l=c()
for(i in 1:p){
    l[i] = sqrt(sum((evec[(1:3),i])^2))
}
# evec[(1:3),1]
round(l)

#Spectral Decomposition of Sample Dispersion Matrix
d = diag(ev)
#Verify whether Matrix is decomposed properly using d and matrix of eigenvectors 
# 'evec'
S = evec%*%d%*%(t(evec))
S
corMat

#Compute the new score matrix using 'evec'
dmNew = dm%*%evec
par(mfrow=c(p,p))
#check if introduced multi-collinearity is gone between the variables
for (i in 1:p){
    for(j in 1:p){
        plot(dmNew[,i],dmNew[,j])
    }
}
dev.off()
