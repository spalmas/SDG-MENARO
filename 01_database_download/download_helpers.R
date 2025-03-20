# Project: Trend analysis for SDG children indicators for UNICEF MENARO
# Script description: functions used for the download scripts
# Author: Sebastian Palmas

api.to.json<- function (x) {
  x1<-content(x,"text")
  x2<-fromJSON(x1,flatten=TRUE)
  #x3<-as.data.frame(do.call("rbind", x2),stringsAsFactors=FALSE)
  return((x2))
}

calculate_percentile <- function(future_rate, historical_rates) {
  if (length(historical_rates) == 0 || is.na(future_rate)) {  
    return(NA_real_)  # Return NA if no valid historical data or future rate is NA
  }
  
  max_historical_rate <- max(historical_rates, na.rm = TRUE)
  
  if (future_rate > max_historical_rate) {
    # Calculate how much higher the future rate is relative to the maximum historical rate
    percentile_above_max <- 100 + ((future_rate - max_historical_rate) / max_historical_rate) * 100
    return(percentile_above_max)
  } else {
    greater_count <- sum(historical_rates <= future_rate, na.rm = TRUE)
    total_count <- sum(!is.na(historical_rates))
    percentile <- (greater_count / total_count) * 100
    
    return(percentile)
  }
}


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

unlist_columns <- function(df) {
  for (col_name in names(df)) {
    if (is.list(df[[col_name]])) {
      df[[col_name]] <- unlist(df[[col_name]])
    }
  }
  return(df)
}