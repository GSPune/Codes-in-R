# mtcars dataset
data(mtcars)#accessing mtcars data
?mtcars#description of data
head(mtcars, 5) #printing first 5 rows of the data
is.matrix(mtcars)#data is not in the matrix form

n=nrow(mtcars)#no of objects 
# p=ncol(mtcars)#no of variables
p=5
x=c()
for(i in 1:p){
    x = c(x,mtcars[,i])
}

dm=matrix(x,nrow=n, ncol=p,byrow=FALSE)
dm

par(mfrow=c(p,p))#splits plot panel into p rows and p cols

for (i in 1:p){
    for(j in 1:p){
        plot(dm[,i],dm[,j])
    }
}
pairs(dm)


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

cor(dm)
dev.off()

###############################################
# Eigenvalues and eigenvectors
e = eigen(corMat)
ev = e$values
evec = e$vectors

plot(1:p,ev,type="l",col="red",xlab="PC number",ylab = "Eigenvalues",main="Scree Plot")
points(1:p,ev,col="green",pch=19)

#################################################
# checking the norm of 3rd eigen vector
(t(evec[,3])%*%(evec[,3]))
#orthogonality
o=evec%*%(t(evec))
round(o)

#contributions of PCS
s = sum(ev)
cs = cumsum(ev)
contrib = data.frame("PC Numbers" = 1:p,"Percent Cont." = (cs/s)*100)

#Graph of contributions
con = (cs/s)*100
plot(1:p, con, type="l", lwd=5)
points(1:p, con, pch=19, col="red")

#Spectral decomposition verification
d=diag(ev)
evec%*%d%*%t(evec)
corMat
#Determining true imp variables
evec[,1]
evec[,2]
#var 2 and 5 are most important

#centering data matrix
#it is important to standardize the data matrix for score computations
cdm=matrix(nrow=n, ncol=p)
for(i in 1:p)
{cdm[,i]=(dm[,i]-mean(dm[,i]))/sd(dm[,i])}
cdm

#Score matrix
ndata=cdm%*%evec

#checking correlations using plots
par(mfrow=c(p,p))
for(i in 1:p)
{
    for(j in 1:p)
    {
        plot(ndata[,i], ndata[,j])
    }
}
pairs(ndata)

cmat1=matrix(nrow=p, ncol=p)
for(i in 1:p)
{
    for(j in 1:p)
    {
        cmat1[i,j]=corr(ndata[,i],ndata[,j])
    }
}
cmat1

#################
#plotting first two pcs
######
plot(ndata[,1], ndata[,2])
###############
#using direct command
e1=prcomp(dm, scale=TRUE)
ev1=(e1$sdev)^2
ev
evec1=e1$rotation