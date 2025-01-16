#' Function to call series data across all countries
#'
#' This function aggregates...
#'
#' @param seriescode Codes of series used in the SDG Global Database
#' @return 
#' @examples
#' 
#' SDGdata("SE_DEV_ONTRK")
#' 
SDGdata<- function(seriescode){
  ctr=0
  onecountrysdgdata1<- data.frame()
  z<- data.frame()
  for (x in unique(seriescode)) {
    ctr=ctr+1
    SDG_series_getp(x) ->oneseriessdgdata1
    # oneseriessdgdata1 <-api.to.json(oneseriessdgdata1)
    # oneseriessdgdata1<- as.data.frame(oneseriessdgdata1[[7]])
    z<- rbind.fill(z,oneseriessdgdata1)
    print(paste(ctr,".Completed downloading series ",x))
  }
  return(z)
}