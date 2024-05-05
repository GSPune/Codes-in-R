x=seq(-3,5,0.01)
mu=1;s=2
nrm=function(x){
    alpha = 1/(sqrt(2*pi)*s)
    beta = -2*s*s;
    f = alpha * exp((x-mu)^2/beta)
    return(f)
}
nrm(1)

dnorm(1,mean=1,sd=2)
y=nrm(x)
plot(x,y,type="l",lwd=3,col="brown")
abline(v=1,lwd=3,col="green")

s=c("mu=1","sd=2")
legend(locator(1),legend=s,horiz=FALSE)

##############################################
nrm2=function(x){
    m = 0
    s = 1
    alpha = 1/(sqrt(2*pi)*s)
    beta = -2*s*s;
    f = alpha * exp((x-mu)^2/beta)
    return(f)
}

y1 = nrm(x)
y2 = nrm2(x)

#plot(x,y1,type="l",lwd=3,col="brown")
#abline(v=1,lwd=3,col="green")

legend(locator(1),legend=s,horiz=FALSE)
lines(x,y2,lwd=3,col="violet")
s1=c("mu=0","sd=1")
legend(locator(1),legend=s1,horiz = FALSE)
###########Histogram####################################
Lc=seq(100,700,100)
Uc=seq(200,800,100)

f=c(2,10,15,20,13,8,3)
d=data.frame(Lc,Uc,f)
breaks=c(100,Uc)

#ugdata = c()
m = (Lc+Uc)/2
obs=rep(m,f)
hist(obs,breaks=breaks,col="white",main="My Histogram",xlab="Observations",ylab="Frequencies")

#if and else
lb=seq(100,300,100)
ub=seq(200,400,100)
obs = c(127,254,289,256,233,118,119,325,399,336)
f2 = c(0,0,0)
for (t in obs){
    print(t)
    if(100 <= t && t < 200){
        #Condition
        f2[1] = f2[1] + 1
    }
    else if(200 <= t && t < 300){
        f2[2] = f2[2] + 1
    }
    else
    {
        f2[3] = f2[3] + 1
    }
}
f2
d=data.frame(lb,ub,f2)