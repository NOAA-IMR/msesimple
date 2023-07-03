#' schaefer takes the current biomass, a catch, and the model parameters to compute next year's biomass
#'
#' @param B current year biomass
#' @param C current year catch
#' @param K carrying capacity
#' @param r population growth rate
#' @export
#'
schaefer <- function(B,C,K,r) {

  res <- B + B * r * (1 - B/K) - C

  res <- max(0.001,res)
  return(res)  # we add a constraint to prevent negative biomass

}
