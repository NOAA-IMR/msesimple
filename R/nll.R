#' nll takes the parameters, catches, and index data and returns the nll of the fit
#' to the dynamics function
#'
#' @param pars parameter values for the dynamics model
#' @param C catch data
#' @param U index data
#'
#' @export
#'


nll <- function(pars,C,U) {
  sigma <- pars[3]  # additional parameter, the standard deviation of the observation error

  B <- dynamics(pars,C)  #run the biomass dynamics for this set of parameters

  Uhat <- B #calculate the predicted biomass index - here we assume an unbiased absolute biomass estimate

  output <- -sum(dnorm(log(U),log(Uhat),sigma,log=TRUE),na.rm=TRUE)   #calculate the negative log-likelihood

  return(output)
  #end function nll
}
