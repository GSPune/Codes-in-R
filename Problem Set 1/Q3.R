# Q.(3) Improvise the codes written in Q.(1) and Q.(2) to create a unified function for the
# measures of central tendency which will calculate different measures as per the requirement. For example, it could a function as follows.
# ct=function{lb, ub, fr, t} where lb, ub, fr are respectively the vectors of lower bounds, 
# upper bounds and the frequencies. Further, t can be 1, 2 or 3 where 1,2,3 respectively stand for mean,
# median and mode.

ct <- function(lb,ub,fr,t){
    if (t = 1){
            # data = data.frame(LC,UC,f)
        x = (lb+ub)/2
        xf = x * fr
        arth_mean = sum(xf)/sum(fr)
        return(arth_mean)
        # print(paste("Mean :",mean(l,u,fq)))
    }
    else if (t = 2){
        data = data.frame(lb,ub,fr)
        N = sum(fr)
        LCF<-integer(length(fr))
        LCF[1] = fr[1]
        for (i in 2:length(fr)){
            #print(LCF[i])
            LCF[i] = LCF[i-1]+fr[i]
        }
        data3 = cbind(data,LCF)
        for (i in 2:length(fr)){
            if(LCF[i] > (N/2)){
                u = i;break; #t gives us the median class row 
            }
        }
        
        l = data3[u,1] #lower class boundary of median class
        cf = data3[u-1,4] #Lcf of class previous to median class
        F0 = data3[u,3] # freq. of median class
        h = (ub - lb)[1] #class width of median class
        Median  = l + (((N/2)-cf)*(h/F0))
        return(Median)
    }
}