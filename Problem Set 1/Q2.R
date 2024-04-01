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
#---------------------------------------------------------------------------------------------------
#Function for calculation of Mode
mode = function(LC,UC,f)
{
    data = data.frame(LC,UC,f)
    max = 1
    
    for (i in 1:length(f)){
        if(f[i] > f[max]){
            max = i; #max gives us the modal class row 
        }
    }
    
    fm = data[max,3]
    f1 = data[max-1,3] #pre-modal frequency
    f2 = data[max+1,3] #post-modal frequency
    h = (UC - LC)[1]
    l = data[max,1]
    Mode = l + (h * ((fm - f1)/(2*fm-f1-f2)))
    return(Mode)
}
print(paste("Mode :",mode(l,u,fq)))
#---------------------------------------------------------------------------------------------------
#Function for calculation of Quartiles
#k <- readline("Enter the Quartile number to be calculated:")

Quartiles <- function(LC,UC,f,k){
    
    data = data.frame(LC,UC,f)
    N = sum(f)
    LCF<-integer(length(f))
    LCF[1] = f[1]
    for (i in 2:length(f)){
        #print(LCF[i])
        LCF[i] = LCF[i-1]+f[i]
    }
    data3 = cbind(data,LCF)
    
    for (i in 2:length(f)){
        if(LCF[i] > (k*N/4)){
            u = i;break; #t gives us the Qk class row 
        }
    }
    
    l = data3[u,1] #lower class boundary of quartile class
    cf = data3[u-1,4] #Lcf of class previous to quartile class
    F0 = data3[u,3] # freq. of quartile class
    h = (UC - LC)[1] #class width of quartile class
    Qk = l + ((((k*N)/4)-cf)*(h/F0))
    return(Qk)
    #print(paste("Quartile :",Qk)
}

print(paste("1st Quartile: ",Quartiles(l,u,fq,1)))
print(paste("2nd Quartile: ",Quartiles(l,u,fq,2)))
print(paste("3rd Quartile: ",Quartiles(l,u,fq,3)))
