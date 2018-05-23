library(RColorBrewer);library(viridis)
s <- 0.8
r <- 0.15
c <- 2
sigma <- 1.5
j <- 1
plot(x=c(0,60),y=c(0,1))
for(ni in seq(0.015,0.02,0.0001)){
  n <- c(ni)
  for(i in 1:60){
    n[i+1] <- n[i]*s+(c*n[i]^2)/(r+2*n[i]+(n[i]^2/sigma))
  }
  lines(n,type="l",col=viridis(length(seq(0.015,0.02,0.0001)))[j],ylim=c(0,5))
  j <- j+1
}


seq(1e-3,0.2,0.01)



n <- 0.01; rate <- 0.1
for(i in 1:40){
  rate[i] <- n[i]/(0.1+n[i])
  n[i+1] <- n[i]+n[i]*rate[i]
}
par(mfrow=c(2,1))
plot(n,type="l");plot(rate,type="l")
par(mfrow=c(1,1))

veitgrowth <- function(n0,t,s,c,r,sigma){
  n <- c(n0)
  for(i in 1:t){
    n[i+1] <- n[i]*s+(c*n[i]^2)/(r+2*n[i]+(n[i]^2/sigma))
  }
  return(n)
}

nls(formula = abundance_index~veitgrowth(n0,site_yr,s,c,r,sigma),sea,start=list(n0=0.01,s=0.8,c=2,r=0.15,sigma=1.5))
