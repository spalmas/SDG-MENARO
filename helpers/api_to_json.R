#' Function to transform the SDG API data call into dataframe
#'
#' This function aggregates...
#'
#' @param x XXX
#' @return A data frame with aggregated data.
#' @export 
#' @examples
#' library(dplyr)
#' library(tidyr)
#' 
#' data <- data.frame(
#'   sdgregion = rep(c("SSA", "Latam"), each = 5),
#'   unicefregion = rep(c(NA, "LACRO"), each = 5),
#'   year = rep(c(2010, 2020), each=5),
#'   value = c(1:10),
#'   weight = c(5:14)
#' )
#' 
#' aggregate_data(data, value="value", by=c("sdgregion", "unicefregion"), global= TRUE, method="sum")
#' data <- data_ind

api.to.json<- function (x) {
  x1<-content(x,"text")
  x2<-fromJSON(x1,flatten=TRUE)
  #x3<-as.data.frame(do.call("rbind", x2),stringsAsFactors=FALSE)
  return((x2))
}