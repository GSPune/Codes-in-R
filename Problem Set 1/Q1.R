#Write a from scratch R code to calculate the following measures of central tendency
#for the given frequency distribution.

LC = seq(0,600,100)
UC = seq(100,700,100)

#test data
# LC = seq(0.5,80.5,20)
# UC = seq(20.5,100.5,20)

f = c(9,15,18,21,18,14,5)
# f = c(1,9,32,16,7)
data = data.frame(LC,UC,f)
#-----------------------------------------------------------------------------------------------------
#(a) Arithmetic mean: The results must include a table with classmarks and product
#of classmarks and frequencies.

x = (LC+UC)/2
xf = x * f
data2 = data.frame(LC,UC,f,x,xf)
data2
arth_mean = sum(xf)/sum(f)
print(paste("Mean :",arth_mean))
#-----------------------------------------------------------------------------------------------------
#(b) Median: The results must include a table containing LCF.
N = sum(f)
LCF<-integer(length(f))
LCF[1] = f[1]
for (i in 2:length(f)){
    #print(LCF[i])
    LCF[i] = LCF[i-1]+f[i]
}

for (i in 2:length(f)){
    if(LCF[i] > (N/2)){
        p = i;break; #p gives us the median class row 
    }
}

data3 = cbind(data,LCF)
l = data3[p,1] #lower class boundary of median class
cf = data3[p-1,4] #Lcf of class previous to median class
F0 = data3[p,3] # freq. of median class
h = (UC - LC)[1] #class width of median class
Median  = l + (((N/2)-cf)*(h/F0))
print(paste("Quartile 2 / Median :",Median)) #also the second quartile
#----------------------------------------------------------------------------------------------------
# (c) Mode (Assuming there is the unique mode).
#data4 = data
max = 1

for (i in 1:length(f)){
    if(f[i] > f[max]){
        max = i; #max gives us the modal class row 
    }
}

fm = data[max,3]
f1 = data[max-1,3]
f2 = data[max+1,3]

l = data[max,1]
Mode = l + (h * ((fm - f1)/(2*fm-f1-f2)))
print(paste("Mode :",Mode))
#----------------------------------------------------------------------------------------------------
# (d) Quartiles Q1,Q3 and Q2 = Median
for (i in 2:length(f)){
    if(LCF[i] > (N/4)){
        t = i;break; #t gives us the Q1 class row 
    }
}

l = data3[t,1] #lower class boundary of quartile class
cf = data3[t-1,4] #Lcf of class previous to quartile class
F0 = data3[t,3] # freq. of quartile class
h = (UC - LC)[1] #class width of quartile class
Q1 = l + (((N/4)-cf)*(h/F0))
print(paste("Quartile 1 :",Q1))


for (i in 2:length(f)){
    if(LCF[i] > (3*N/4)){
        u = i;break; #t gives us the Q3 class row 
    }
}

l = data3[u,1] #lower class boundary of quartile class
cf = data3[u-1,4] #Lcf of class previous to quartile class
F0 = data3[u,3] # freq. of quartile class
h = (UC - LC)[1] #class width of quartile class
Q3 = l + ((((3*N)/4)-cf)*(h/F0))
print(paste("Quartile 3 :",Q3))
