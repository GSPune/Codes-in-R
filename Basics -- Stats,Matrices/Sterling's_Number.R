#Sterling's Number using Recursion
#!/usr/bin/env Rscript

S <- function(n,k){
      #print(n)
      #Base Cases!
      if (k == 1 || k == 0){
         return (k);
      }

      if (k == n){
         return (1);
      }
         
      return (S(n-1,k-1) + k * S(n-1,k));
}

#print(S(4,2))
for (i in 1:5){
   for (j in 0:i){
      #paste(c(S(i,j)))
      #print(S(i,j))
      cat(S(i,j))
   }
   cat("\n")
}