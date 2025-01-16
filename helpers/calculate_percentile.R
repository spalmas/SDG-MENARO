#' Function to calculate percentile rank
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
