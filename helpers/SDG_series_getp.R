#' Function to get SDG series data from SDG Global Dataset
#'
#' This function aggregates...
#'
#' @param y XXX
#' @return A data frame with aggregated data.
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

SDG_series_getp <- function(y, max_retries = 3) {
  base_url <- "https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?"
  page_size <- 50000  # Set a fixed page size
  page_number <- 1    # Start from the first page
  all_data <- data.frame() 
  # Initialize as NULL to store the flat data frame
  # Construct the URL with series codes
  seriescodes <- base_url
  for (x in y) {
    seriescodes <- paste(seriescodes, "seriesCode=", x, "&", sep = "")
  }
  
  # Pagination loop to fetch all data
  repeat {
    address <- paste(seriescodes,
                     "timePeriod=2000&timePeriod=2001&timePeriod=2002&timePeriod=2003&timePeriod=2004&timePeriod=2005&timePeriod=2006&timePeriod=2007&timePeriod=2008&timePeriod=2009&timePeriod=2010&timePeriod=2011&timePeriod=2012&timePeriod=2013&timePeriod=2014&timePeriod=2015&timePeriod=2016&timePeriod=2017&timePeriod=2018&timePeriod=2019&timePeriod=2020&timePeriod=2021&timePeriod=2022&timePeriod=2023&timePeriod=2024&pageSize=", page_size, "&page=", page_number, sep="")
    print(paste("Started downloading series ",y))
    result <- GET(address)
    
    result->temp_data
    temp_data <-api.to.json(temp_data)
    temp_data<- as.data.frame(temp_data[[7]])
    print(paste("downloading series ",y, "page", page_number,"data size", nrow(temp_data)))
    
    if (nrow(temp_data)==0) {
      break
    }
    
    all_data<- rbind.fill(all_data,temp_data)
    page_number <- page_number + 1
  }
  return(all_data)
}