#' schaefer takes the current biomass, a catch, and the model parameters to compute next year's biomass
#'
#' @param B define
#' @param C define
#' @param K define
#' @param r define
#' @export
#'
schaefer <- function(B,C,K,r) {
  #function schaefer takes the current biomass, a catch,
  #and the model parameters to compute next year's biomass
  res <- B + B * r * (1 - B/K) - C
  return(max(0.001,res))  # we add a constraint to prevent negative biomass
}
