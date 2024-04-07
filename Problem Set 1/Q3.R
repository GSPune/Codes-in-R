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
        
    }
}