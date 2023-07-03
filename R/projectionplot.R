#' projectionplot takes the projection results and plots them
#'
#' @param projection.results biomass projections into the future
#'
#' @export
#'



projectionplot <- function(project.results) {
  Fig2 <- ggplot(data = subset(project.results, type != "index"),
                 aes(x = year, y = value))
  Fig2 + geom_line(aes(y=value,x=year),data = subset(project.results, type != "index" & iter==1 & year %in% proj.years), lty=1,lwd=1,col=gray(0.7)) +
    geom_line(aes(y=value,x=year),data = subset(project.results, type != "index" & iter==2 & year %in% proj.years), lty=1,lwd=1,col=gray(0.7)) +
    geom_line(aes(y=value,x=year),data = subset(project.results, type != "index" & iter==3 & year %in% proj.years), lty=1,lwd=1,col=gray(0.7)) +
    stat_summary(fun.data = "median_hilow", geom = "smooth", col="black",
                 lty = 2) +
    stat_summary(fun = "median", fun.min = function(x)0, geom="line",
                 data = subset(project.results, type != "index" &
                                 year %in% data.years), lwd=1) +facet_wrap(~type, scale = "free_y") + ylab("Million tonnes") + theme_bw()

}
