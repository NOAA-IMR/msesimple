library(ggplot2)

data.years <- 1991:2013
harvest <- c(0.1,3,15,52,76,139,95,93,84,93,86,103,104,
             92,46,67,59,30,54,59,47,33,44)
index <- c(NA,NA,NA,NA,NA,NA,NA,NA,935,NA,1057,NA,678,NA,
           420,NA,554,NA,458,NA,474,NA,280)

plot(data.years,index, pch=19,xlab="Year",ylab="Million tonnes (B/C)",
     ylim=c(0,1200))
lines(data.years,harvest,lty=2,lwd=2)


schaefer <- function(B,C,K,r) {
  #function schaefer takes the current biomass, a catch,
  #and the model parameters to compute next year's biomass
  res <- B + B * r * (1 - B/K) - C
  return(max(0.001,res))  # we add a constraint to prevent negative biomass
}


dynamics <- function(pars,C,yrs) {
  # dynamics takes the model parameters, the time series of catch,
  # & the yrs to do the projection over

  # first extract the parameters from the pars vector (we estimate K in log-space)
  K <- exp(pars[1])
  r <- pars[2]

  # find the total number of years
  nyr <- length(C) + 1

  # if the vector of years was not supplied we create
  # a default to stop the program crashing
  if (missing(yrs)) yrs <- 1:nyr

  #set up the biomass vector
  B <- numeric(nyr)

  #intialize biomass at carrying capacity
  B[1] <- K
  # project the model forward using the schaefer model
  for (y in 2:nyr) {
    B[y] <- schaefer(B[y-1],C[y-1],K,r)
  }

  #return the time series of biomass
  return(B[yrs])

  #end function dynamics
}

# function to calculate the negative log-likelihood
nll <- function(pars,C,U) {  #this function takes the parameters, the catches, and the index data
  sigma <- pars[3]  # additional parameter, the standard deviation of the observation error
  B <- dynamics(pars,C)  #run the biomass dynamics for this set of parameters
  Uhat <- B   #calculate the predicted biomass index - here we assume an unbiased absolute biomass estimate
  output <- -sum(dnorm(log(U),log(Uhat),sigma,log=TRUE),na.rm=TRUE)   #calculate the negative log-likelihood
  return(output)
  #end function nll
}


assess <- function(catch,index,calc.vcov=FALSE,pars.init) {
  # assess takes catch and index data, initial values for the parameters,
  # and a flag saying whether to compute uncertainty estimates for the model parameters

  #fit model
  # optim runs the function nll() repeatedly with differnt values for the parameters,
  # to find the values that give the best fit to the index data
  res <- optim(pars.init,nll,C=catch,U=index,hessian=TRUE)

  # store the output from the model fit
  output <- list()
  output$pars <- res$par
  output$biomass <- dynamics(res$par,catch)
  output$convergence <- res$convergence
  output$nll <- res$value
  if (calc.vcov)
    output$vcov <- solve(res$hessian)

  return(output)
  #end function assess
}


ini.parms <- c(log(1200), 0.1, 0.3)

redfish <- assess(harvest,index,calc.vcov=TRUE,ini.parms)

biomass.mle <- redfish$biomass
print(biomass.mle)

pars.mle <- redfish$pars

#define the number of iterations for the MSE
niter <- 500
#set up a storage matrix for our alternative parameter sets
pars.iter <- matrix(NA,nrow = niter, ncol=3)
colnames(pars.iter) <- c("logK","r","sigma")

# generate the sets of parameter values
for (i in 1:niter) {
  pars.iter[i,] <- mvtnorm::rmvnorm(1, mean = redfish$pars,
                                    sigma = redfish$vcov)
}

# Now generate replicate model outputs
biomass.iter <- data.frame()
for (i in 1:niter) {
  #here we calculate the biomass trajectory for each of the above sampled parameter vectors
  biomass.iter <- rbind(biomass.iter,
                        data.frame(year = seq(min(data.years),
                                              max(data.years)+1),
                                   biomass = dynamics(pars.iter[i,], harvest),
                                   iter = i))
}
print(pars.mle)

Fig1 <- ggplot(data=biomass.iter,aes(x=year,y=biomass))
Fig1 + stat_summary(fun.data = "median_hilow",
                    geom = "smooth",
                    fun.min = function(x)0,
                    col="black")  +
  geom_line(aes(y=harvest,x=year), data = data.frame(harvest = harvest,
                                                     year = data.years),lty=2) +
  geom_point(aes(y=index, x=year), data = data.frame(index=index,
                                                     year = data.years)) +
  geom_line(aes(y=biomass,x=year),data = subset(biomass.iter,iter==1), lty=1,lwd=0.5,col="gray") +
  ylab("Estimated B and C (million tonnes)") + theme_bw()

proj.years <- 2014:2034

observe <- function(biomass, sigma) {
  biomass * exp(rnorm(1, -0.5*sigma^2, sigma))
}






control.pars <- list()
control.pars$Htarg <- 0.1
control <- function(estimated.biomass, control.pars) {
  control.pars$Htarg
}

implement <- function(TAC,...) {
  TAC
}

control <- function(estimated.biomass, control.pars) {
  H1 <- control.pars$H1
  H2 <- control.pars$H2
  Bmax <- control.pars$Bmax
  B2 <- control.pars$B2
  B1 <- control.pars$B1

  harv <- ifelse(estimated.biomass >= B1, H1,
                 ifelse(estimated.biomass < B2, H2,
                        (H1-H2)/(B1-B2)*(estimated.biomass - B2) + H2))

  return(harv)

  #end function control
}


control.pars <- list()
control.pars$H1 <- 0.05
control.pars$H2 <- 0
control.pars$Bmax <- max(index, na.rm =TRUE)
control.pars$B2 <- 0.2*control.pars$Bmax
control.pars$B1 <- 0.5*control.pars$Bmax

plot(c(0,control.pars$B2,control.pars$B1,control.pars$Bmax),
     c(control.pars$H2,control.pars$H2,control.pars$H1,control.pars$H1),
     type='l',axes=F,xlab="biomass",ylab="exploitation rate",
     ylim=c(0,1.2*control.pars$H1))
axis(1,at=c(control.pars$B2,control.pars$B1),labels=c("B2","B1"))
axis(2,at=c(control.pars$H2,control.pars$H1),labels=c("H2","H1"))
box()

evaluate <- function(pars.iter, biomass.iter,
                     control.pars, data.years, proj.years,
                    iterations, ...) {
# function arguments:
# pars.iter & biomass.iter, the parameters & historical biomass trajectories of the operating model
# control.pars, the specifications of the harvest control rule

# set up some indexing values
iyr <- length(data.years)+1
pyr <- length(proj.years)
yrs <- c(data.years, proj.years, max(proj.years)+1)

# set up a data frame to store the results
res <- data.frame()

# loop over the iterations of the MSE, each iteration conducts a 20 year projection with annual generation of biomass
# observations and appliations of the control rule.
for(i in 1:iterations) {
#i = 1

#extract the parameters for this iteration
K.i <- exp(pars.iter[i,1])
r.i <- pars.iter[i,2]
sig.i <- pars.iter[i,3]

#set up vectors for time series of interest.
biomass.i <- c(subset(biomass.iter, iter==i)$biomass, numeric(pyr))
index.i <- c(index,numeric(pyr))
catch.i <- c(harvest, numeric(pyr))
TAC.i <- c(rep(NA,iyr-1),numeric(pyr))

# loop over the projection period.
for (y in iyr:(iyr+pyr-1)) {
y <- iyr

#generate the data for the most recent year
index.i[y] <- observe(biomass.i[y] , sig.i)
#calculate the TAC based on the harvest control rule
# note that the control rule ONLY sees the index data, not the operating model biomass.
TAC.i[y]  <- control(index.i[y], control.pars) * index.i[y]
#find the realized catch after implementation error
catch.i[y] <- implement(TAC.i[y])

# update the true biomass of the operating model based on the output of the HCR
biomass.i[y+1] <- schaefer(biomass.i[y],catch.i[y],K.i,r.i)


# loop over the remaining years of the projection period.
for (y in (iyr+1):(iyr+pyr-1)) {
  #generate the data for the most recent year
  index.i[y] <- observe(biomass.i[y] , sig.i)
  #calculate the TAC based on the harvest control rule
  # note that the control rule ONLY sees the index data, not the operating model biomass.
  TAC.i [y]  <- control(index.i[y], control.pars) * index.i[y]
  #find the realized catch after implementation error
  catch.i[y] <- implement(TAC.i[y])

  # update the true biomass of the operating model based on the output of the HCR
  biomass.i[y+1] <- schaefer(biomass.i[y],catch.i[y],K.i,r.i)

  #end projection year loop for iteration i
}

#store the results for this iteration
res <- rbind(res, data.frame(year = yrs[-length(yrs)],
                             value = index.i, type = "index", iter = i),
             data.frame(year = yrs[-length(yrs)],
                        value = catch.i, type = "catch", iter=i),
             data.frame(year = yrs, value = biomass.i,
                        type= "biomass", iter=i))
#end loop over iterations
}
return(res)
#end function evaluate()
}








project.hcr <- evaluate(pars.iter, biomass.iter, control.pars,
                        data.years, proj.years, niter)

projection.plot(project.hcr)