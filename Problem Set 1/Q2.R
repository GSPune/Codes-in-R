# Q.(2) Write a dedicated and separate functions with suitable names for each of the measures
# of central tendency mentioned in Q. (1). These functions must take lower bounds,
# upper bounds and frequencies as their input arguments and must return the value of
# a measure of central tendency. Remember, your code must be flexible enough
# to work with all possible number of classes.

l = seq(0,600,100)
u = seq(100,700,100)


fq = c(9,15,18,21,18,14,5)

mean=function(LC,UC,f)
{
    # data = data.frame(LC,UC,f)
    x = (LC+UC)/2
    xf = x * f
    arth_mean = sum(xf)/sum(f)
    return(arth_mean)
}
print(paste("Mean :",mean(l,u,fq)))