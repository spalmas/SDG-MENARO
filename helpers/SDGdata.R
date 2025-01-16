#' Function to get data from a list of series
#'
#' @param seriescodes Codes of series used in the SDG Global Database
#' @return 
#' @examples
#' 
#' SDGdata(c("SE_DEV_ONTRK", "SE_PRE_PARTN"))
#' 
SDGdata<- function(seriescodes){
  ctr=0
  onecountrysdgdata1 <- data.frame()
  z <- data.frame()
  for (seriescode in unique(seriescodes)) {
    ctr <- ctr + 1
    oneseriessdgdata1 <- SDG_series_getp(seriescode) 
    z <- rbind.fill(z, oneseriessdgdata1)
    print(paste0(ctr,". Completed series ",seriescode))
  }
  return(z)
}