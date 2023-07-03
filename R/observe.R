#' observe takes the true biomass from the operating model and adds observation error,
#' defined by sigma
#'
#' @param biomass biomass from the operating model in the function dynamics
#' @param sigma observation error
#' @export
#'

observe <- function(biomass, sigma) {
  biomass * exp(rnorm(1, -0.5*sigma^2, sigma))
}
