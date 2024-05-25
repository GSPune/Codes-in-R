#Sterling's Number using Recursion

#!/usr/bin/env Rscript

S <- function(n,k){
#print(n)
#print(k)
 if (k == 1 || k == 0){
   return (k);
}

if (k == n){
   return (1);
}
   
 return (S(n-1,k-1) + k * S(n-1,k));
}

row <- readLines("stdin",n=1)
cat("Entered value is ::",row,"\n")
#print(S(4,2))
for (i in 1:row){
 for (j in 0:i){
    cat(S(i,j))
    cat(" ")
}
cat("\n")
}