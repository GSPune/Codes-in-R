#Matrix Operations
x=1:12
x
mat=matrix(data = NA,nrow=3,ncol=4,byrow=TRUE)
example= matrix(c(1,2,3,4,8,4,6,7,2,7,10,15),3,4,byrow=TRUE)
det(example)
nrow(mat)
ncol(mat)
dim(mat)
y=1:16
mat1=matrix(y,nrow=4,ncol=4,byrow=TRUE)
mat1
# mat%*%mat1
example%*%mat1

mat[1,] = mat[1,]*3
mat[3,] = mat[3,] - mat[2,]*2 #Row operations
#Interchanging rows
r1 = mat[1,]
r2 = mat[2,]
mat[1,] = r2
mat[2,] = r1
####################################
#DATAFRAMES
#####
roll=1:10
marks=c(12,14,17,12,11)
d=data.frame(roll,marks)
row.names(d) <- c()
print(d, row.names = FALSE)
write.table(d,"D://M.Sc. (SC)//R Programs//marks.xls",sep="\t",row.names = FALSE)
d1=data.frame("x"=roll,"y"=marks) #changing colnames
x1=d1$x
y1=d1$y
xbar=mean(x1)
varx1=mean(x1^2)-xbar^2
ybar=mean(y1)
vary1=mean(y1^2)-ybar^2
covxy=mean(x1*y1)-xbar*ybar
#####
#subset
#transform
#####
d[3,2]
d2=subset(d1,x>3)
d3=subset(d1,x>2 & x<5)
d4=transform(d1,ad=x+y) #"x+y" = x+y
#resident datasets in R
d = data(Orange)
Orange
data(CO2)
CO2
length(CO2)
CO2['Plant']
#####
#functions in R
####
x=1:5
y=6:10
dxy=sqrt(sum((x-y)^2))
euclid=function(x,y)
{
    d = sqrt(sum((x-y)^2))
    return(d)
}
euclid(x,y)
########
x=c()
for (i in 1:10)
    #print(i)
    x[i]=i

n = 1:100
plot(n,1/(n^2),type="l",col="red",lwd=5)