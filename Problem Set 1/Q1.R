#Write a from scratch R code to calculate the following measures of central tendency
#for the given frequency distribution.

LC = seq(0,600,100)
UC = seq(100,700,100)

f = c(9,15,18,21,18,14,5)
data = data.frame(LC,UC,f)

#(a) Arithmetic mean: The results must include a table with classmarks and product
#of classmarks and frequencies.

x = (LC+UC)/2
xf = x * f
data = data.frame(LC,UC,f,x,xf)

arth_mean = sum(xf)/sum(f)

#(b) Median: The results must include a table containing LCF.