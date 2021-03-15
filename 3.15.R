library(Hmisc)

## 1. Fiji earthquakes
#Data on the magnitudes of earthquakes near Fiji are on Blackboard under the 
#Class-21 tab. Estimate the cdf F(x). Compute and plot a 95% confidence envelope
#for F. Find an approximate 95 percent confidence interval for F (4.9) − F (4.3).

setwd("/Users/baiziyi/Desktop/677-hw")
fiji <- read.table("fijiquakes.dat",header=T)

x <- fiji$mag
par(mfrow = c(2,1))
hist(a, main = "Fiji earthquakes")

cdf_fiji <- ecdf(x)
plot(cdf_fiji)

alpha = 0.05
n = length(x)
epsn <- sqrt(log(2/alpha)/(2*n))
r <- max(x)-min(x)
grid = seq(from=min(a)-0.01*r,
           to=max(a)+0.01*r,
           l=1000)
cdf_low <- pmax(cdf_fiji(grid)-epsn,0)
cdf_up <- pmin(cdf_fiji(grid)+epsn,1)
lines(grid, cdf_low, col="red")
lines(grid,cdf_up,col="green")

## approximate 95% CI
x1 = 4.9
x2= 4.3
print(th.hat <- cdf_fiji(x1)-cdf_fiji(x2))
s1 = sum((x<=4.9)&(x>4.3))
binconf(s1,length(x),method="wilson",alpha)


## 2. Old Faithful
#Data on eruption times and waiting times between eruptions of the old faithful 
#geyser (located in Yellowstone National Park) are posted on Blackboard under 
#the Class-21 tab. Estimate the mean waiting time and compute a standard error 
#for the estimate. Also, calculate a 90 percent confidence interval for the mean 
#waiting time. Finally, estimate the median waiting time. We will use this 
#calculation of the median in our discussions next week.

old <- read.csv("geysers.csv")

# mean & standard error
m <- mean(old$waiting)
se <- sqrt(var(old$waiting)/length(old$waiting))

# 90% CI
L <- m-1.645*se
U <- m+1.645*se

# estimate the median waiting time
F_old <- ecdf(old$waiting)
quantile(F_old,0.5)


## 3. KS problem
#Use the Kolmogorov-Smirnov test to test the hypothesis that the 25 values in 
#the table below form a random sample from the uniform distribution on the 
#interval [0, 1].

set.seed(1)
x <- c(0.42, 0.06, 0.88, 0.40, 0.90, 
       0.38, 0.78, 0.71, 0.57, 0.66, 
       0.48, 0.35,0.16, 0.22, 0.08,
       0.11, 0.29, 0.79, 0.75, 0.82,
       0.30, 0.23, 0.01, 0.41, 0.09)
ks.test(x,punif)

# Since p-value=0.3501, so we cannot reject the null hypothesis. The dataset given
# is not drawn from the random distribution.


# Using the table above, test the hypothesis that the 25 values are a random 
# sample from a continuous distribution with pdf:

p = function(x){
  if(x<=0.5&x>=0){
    1.5
  }
  else if (0.5<x & x<1){
    0.5
  }
  else{
    0
  }
}
ks.test(x,p)

# p-value is small enough so that we can reject the null hypothesis, the given 
# dataset is drawn from the pdf.

# 4.
# See pdf 3.15-hw-Q4.



