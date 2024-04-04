library(scatterplot3d)
n = 30
x1 <- runif(n)
x2 <- runif(n)
y = x1 + x2^3
plot(x1,x2)

train.x = data.frame(x1,x2)

k=4
mean.nearest <- function(test.points,others,y,pos){
    d <- NULL
    for (i in 1:n){
        if (i != pos){
            d[i] = sqrt(sum((test.points-others[i,])^2))
        }
    }
    Sorted <- order(d)
    return (mean(y[Sorted[1:k]]))
}

# special = 0.5 + (0.5)^3
source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
sd <- scatterplot3d(x1,x2,y,xlab="x-axis",ylab="y-axis",zlab="z-axis",main="k-NN Regression",
                    angle = 40.5,box = FALSE,grid=FALSE)

knn.regression <- function(train.x,train.y,k){
    y.est = NULL
    for (j in 1:n){
        y.est[j] = mean.nearest(train.x[j,],train.x,train.y,j)
        addgrids3d(x1,x2,y, grid = c("xy", "xz", "yz"))
        sd$points3d(train.x[j,1],train.x[j,2],y[j],col="purple",pch=19,type="h")
        # sd$points3d(train.x[j,1],train.x[j,2],0,col="orange",pch=19)
        # sd$points3d(x1[Sorted[1:k]],x2[Sorted[1:k]],y[Sorted[1:k]],col="green",pch=19)
        sd$points3d(train.x[j,1],train.x[j,2],y.est[j],col="red",pch=19)
        
        # print(y.est[j])
    }
    return(y.est)
}

m = knn.regression(train.x,y,k)


