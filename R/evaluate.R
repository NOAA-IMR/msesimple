#' evaluate takes the parameters & historical biomass trajectories of the operating model,
#' the specifications of the harvest control rule,
# pars.iter & biomass.iter,
# control.pars rulecurrent biomass, a catch, and the model parameters to compute next year's biomass
#'
#' @param pars.iter operating model parameters from operating model
#' @param biomass.iter historical biomass trajectories from operating model
#' @param control.pars parameters defining the harvest control rule
#' @param data.years years that have data informing the operating model
#' @param proj.years years to do projections over
#' @param iterations number of iterations
#'
#' @export
#'

evaluate <- function(pars.iter, biomass.iter,
                     control.pars, data.years, proj.years,
                     iterations, ...) {

  # set up some indexing values
  iyr <- length(data.years)+1
  pyr <- length(proj.years)
  yrs <- c(data.years, proj.years, max(proj.years)+1)

  # set up a data frame to store the results
  res <- data.frame()

  # loop over the iterations of the MSE, each iteration conducts a 20 year projection with annual generation of biomass
  # observations and applications of the control rule.
  for(i in 1:iterations) {
    i = 1

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
      #y <- iyr

      #generate the data for the most recent year
      index.i[y] <- observe(biomass.i[y] , sig.i)
      #calculate the TAC based on the harvest control rule

      # note that the control rule ONLY sees the index data, not the operating model biomass.
      TAC.i[y]  <- control(index.i[y], control.pars) * index.i[y]

      #find the realized catch after implementation error
      catch.i[y] <- implement(TAC.i[y])

      # update the true biomass of the operating model based on the output of the HCR
      biomass.i[y+1] <- schaefer(biomass.i[y],catch.i[y],K.i,r.i)

    } #end loop over projection years

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
