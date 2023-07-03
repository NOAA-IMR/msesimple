#' assess takes catch and index data, initial values for the parameters,
#' and a flag saying whether to compute uncertainty estimates for the model parameters,
#' fits the model to the data, and returns fitted parameter estimates, biomass estimates,
#' the nll, and any convergence diagnostics

#'
#' @param catch vector of catch data
#' @param index vector of index data
#' @param calc.vcoc flag computing uncertainty estimates
#' @param pars.init initial values for the parameters
#'
#' @export
#'


assess <- function(catch,index,calc.vcov=FALSE,pars.init) {

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

