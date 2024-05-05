
#Car data
data(mtcars)#accessing mtcars data
?mtcars#description of data
head(mtcars, 5) #printing first 5 rows of the data
is.matrix(mtcars)#data is not in the matrix form
n=nrow(mtcars)#no of objects 
p=ncol(mtcars)#no of variables
#preparing data matrix
x=c()
for(i in 1:p)
{
    x=c(x,mtcars[,i])
}
x
dm=matrix(x,nrow=n, ncol=p,byrow=FALSE)
#normalizing data
sdm=matrix(nrow=n, ncol=p)
for(i in 1:p)
{
    sdm[,i]=(dm[,i]-mean(dm[,i]))/(sd(dm[,i]))
}

mean(sdm[,7])#nearly 0
var(sdm[,1])
###################
#investigating correlation structure
pairs(sdm)
round(cor(sdm),2)
################
#k means clustering for k=3
#################33
?kmeans
k=3
# kmeans(sdm,centers=32, nstart=32)
km=kmeans(sdm,centers=k, nstart=32)#nstart gives attempts with different random start points
km$cluster
km$size
#calculating total ss
tss=c()
#k must lie between 1 and n
for(i in 1:(n-2))
{
    temp=kmeans(sdm, centers=i+1, nstart=32)
    tss[i]=temp$tot.withinss
}
tss
length(tss)
plot(1:(n-2), tss,type="l", lwd=5, col="red", xlab="no of cluster", ylab="TSS", main="Elbow plot for TSS")
points(1:(n-2), tss,pch=19)
##############3
#adding horizontal lines
for(i in 1:(n-2))
{abline(h=tss[i], type="dashed")}

#######################
#hierrachical agglomerative
############################3
# Ward Hierarchical Clustering
dis=dist(sdm, method = "euclidean") # distance matrix
fit=hclust(dis, method="ward.D")
plot(fit) # display dendogram
fit$merge
groups=cutree(fit, k=4) # cut tree into 4 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=4, border="red")
rect.hclust(fit, k=2, border="green")

#HW How to perform k mediods clustering?if possible without any possible packages like hclust