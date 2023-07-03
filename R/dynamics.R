#' dynamics takes the model parameters, a time series of catches,
# & the yrs to do the projection over and returns a time series of biomass
#'
#' @param pars model parameters
#' @param C time series of catch
#' @param yrs years to compute biomass over
#' @export
#'


dynamics <- function(pars,C,yrs) {
  #

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
