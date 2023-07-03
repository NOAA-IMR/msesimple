library(ggplot2)
library(Hmisc)
library(mvtnorm)

library(msesimple)


## Read in the data
data.years.s1 <- 1991:2013
harvest.s1 <- c(0.1,3,15,52,76,139,95,93,84,93,86,103,104,
             92,46,67,59,30,54,59,47,33,44)
index.s1 <- c(NA,NA,NA,NA,NA,NA,NA,NA,935,NA,1057,NA,678,NA,
           420,NA,554,NA,458,NA,474,NA,280)

data.years.s2 <- 1991:2013
harvest.s2 <- harvest.s1*0.1
index.s2 <- index.s1*0.1




##Plot the data
plot(data.years.s1,index.s1, pch=19,xlab="Year",ylab="Million tonnes (B/C)",
     ylim=c(0,1200))
lines(data.years.s1,harvest.s1,lty=2,lwd=2)
points(data.years.s2, index.s2, pch=19, col = 'red')
lines(data.years.s2,harvest.s2,lty=2,lwd=2, col='red')


##Assess substock 1
ini.parms.s1 <- c(log(1200), 0.1, 0.3)

redfish.s1 <- assess(harvest.s1,index.s1,calc.vcov=TRUE,ini.parms.s1)

biomass.s1.mle <- redfish.s1$biomass
print(biomass.s1.mle)

pars.s1.mle <- redfish.s1$pars

##Assess substock 2
ini.parms.s2 <- c(log(120), 0.1, 0.3)

redfish.s2 <- assess(harvest.s2,index.s2,calc.vcov=TRUE,ini.parms.s2)

biomass.s2.mle <- redfish.s2$biomass
print(biomass.s2.mle)

pars.s2.mle <- redfish.s2$pars




#define the number of iterations for the MSE
niter <- 5

#set up a storage matrix for our alternative parameter sets
pars.iter <- matrix(NA,nrow = niter, ncol=3)
colnames(pars.iter) <- c("logK","r","sigma")

# generate the sets of parameter values
for (i in 1:niter) {
  pars.iter.s1[i,] <- mvtnorm::rmvnorm(1, mean = redfish.s1$pars,
                                    sigma = redfish.s1$vcov)

  pars.iter.s2[i,] <- mvtnorm::rmvnorm(1, mean = redfish.s2$pars,
                                    sigma = redfish.s2$vcov)
}

# Now generate replicate model outputs
biomass.iter.s1 <- data.frame()
biomass.iter.s2 <- data.frame()

for (i in 1:niter) {
  #here we calculate the biomass trajectory for each of the above sampled parameter vectors
  biomass.iter.s1 <- rbind(biomass.iter.s1,
                        data.frame(year = seq(min(data.years.s1),
                                              max(data.years.s1)+1),
                                   biomass = dynamics(pars.iter.s1[i,],
                                   harvest = harvest.s1),
                                   iter = i))

  biomass.iter.s2 <- rbind(biomass.iter.s2,
                        data.frame(year = seq(min(data.years.s2),
                                              max(data.years.s2)+1),
                                   biomass = dynamics(pars.iter.s2[i,],
                                   harvestst = harvest.s2),
                                   iter = i))
}

print(pars.mle.s1)
print(pars.mle.s2)

Fig1 <- ggplot(data=biomass.iter.s1,aes(x=year,y=biomass))
Fig1 + stat_summary(fun.data = "median_hilow",
                    geom = "smooth",
                    fun.min = function(x)0,
                    col="black")  +
  geom_line(aes(y=harvest,x=year), data = data.frame(harvest = harvest,
                                                     year = data.years),lty=2) +
  geom_point(aes(y=index, x=year), data = data.frame(index=index,
                                                     year = data.years)) +
  #geom_line(aes(y=biomass,x=year),data = subset(biomass.iter,iter==1), lty=1,lwd=0.5,col="gray") +
  ylab("Estimated B and C (million tonnes)") + theme_bw()

proj.years <- 2014:2034


##define the harvest control rule
control.pars <- list()

control.pars$H1 <- 0.05
control.pars$H2 <- 0
control.pars$Bmax <- max((index.s1+index.s2), na.rm =TRUE)
control.pars$B2 <- 0.2*control.pars$Bmax
control.pars$B1 <- 0.5*control.pars$Bmax

plot(c(0,control.pars$B2,control.pars$B1,control.pars$Bmax),
     c(control.pars$H2,control.pars$H2,control.pars$H1,control.pars$H1),
     type='l',axes=F,xlab="biomass",ylab="exploitation rate",
     ylim=c(0,1.2*control.pars$H1))
axis(1,at=c(control.pars$B2,control.pars$B1),labels=c("B2","B1"))
axis(2,at=c(control.pars$H2,control.pars$H1),labels=c("H2","H1"))
box()

project.hcr <- evaluate(pars.iter, biomass.iter, control.pars,
                        data.years, proj.years, niter)

projectionplot(project.hcr)
#project.hcr

