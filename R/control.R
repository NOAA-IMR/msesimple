#' control takes the estimated biomass and control rule parameters and returns
#' harvest takes the current biomass, a catch, and the model parameters to compute next year's biomass
#'
#' @param estimated.biomass estimated biomass from assess function
#' @param control.pars control rule parameters
#' @export
#'

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
