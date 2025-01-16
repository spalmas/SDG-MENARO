#' Function to get SDG series data from SDG Global Dataset
#'
#' This function aggregates...
#'
#' @param y XXX
#' @return A data frame with aggregated data.
#' @examples
#' 
#' SDG_series_getp("SE_DEV_ONTRK")

SDG_series_getp <- function(y, max_retries = 3) {
  #y <- "SE_DEV_ONTRK"
  # Construct the URL with series codes
  base_url <- paste0("https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=", y, "&pageSize=50000&")
  page_number <- 1    # Start from the first page
  
  all_data <- data.frame() # empty df store data
  
  # Pagination loop to fetch all data
  print(paste("Started downloading series ",y))
  
  repeat {
    address <- paste0(base_url, "page=", page_number)
    result <- GET(address)
    
    result -> temp_data
    temp_data <-api.to.json(temp_data)
    temp_data<- as.data.frame(temp_data[[7]])
    
    #If there are no rows in the data, break the loop
    if (nrow(temp_data)==0) { break }
    
    #status report
    print(paste0("Downloading page: ", page_number,", rows: ", nrow(temp_data)))
    
    #bind page data to full data
    all_data<- rbind.fill(all_data,temp_data)  
    
    #next page
    page_number <- page_number + 1
  }
  return(all_data)
}