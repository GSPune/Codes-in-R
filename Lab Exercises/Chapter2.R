set.seed(1303)
x<-rnorm(50)
y<-x+rnorm(50,mean=50,sd=.1)
cor(x, y)
plot(x,y,xlab="x-axis",ylab = "y-axis",main="a plot of y vs x")

p <- seq(-pi, pi, length = 50)


Auto <- read.table("Lab Exercises//Auto.data",header = TRUE,sep = "",na.strings = "?",
stringsAsFactors = T)
View(Auto)
Auto <- na.omit(Auto)
attach(Auto)
plot(cylinders , mpg)
#The as.factor() function converts quantitative variables into qualitative variables.
cylinders <- as.factor(cylinders)
plot(cylinders, mpg)
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")

hist(mpg,col=2,breaks="Sturges")
hist(mpg,col=2,breaks=5)
mpg

plot(horsepower,mpg)
identify(horsepower,mpg,name)

pairs(Auto)
#scatterplots for just a subset of the variables.
pairs(
    ~ mpg + displacement + horsepower + weight + acceleration ,
    data = Auto
)