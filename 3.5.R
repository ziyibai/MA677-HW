#3.5 MA677 Assignment 

#install.packages("moments")
library(moments)
options(digits=3)
options(scipen = 200)

#original distribution
x <- rexp(10000,rate=1)
x_mean <- mean(x)
x_var <- var(x)

m <- 10000
par(new=T)
par(mfrow=c(2,3))

list_result <- list()

for (n in c(5,10,50,100,500,1000)) {
  s = c()
  for (j in 1:m){
    sample=sample(x,n)
    sample_mean=mean(sample)
    s[j]=sample_mean
  }
  
  s_mean=mean(s)
  s_var=var(s)
  hist(s,breaks = 50,main = paste('n=',n,sep = ''))
}

hist(x)

#So, as n increases, the distribution is likely to be normal distribution.





