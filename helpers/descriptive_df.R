#' Describe indicator data
#'
#' Get descriptive statistics for each indicator
#'
#' @param df data frame with indicator data
#' @return data frame with descriptive statistics with one row per indicator
#' @examples
#' 
#' describe_indicator(df)

describe_indicator <- function(df) {
  
  descriptive_df <- df |> 
    group_by(MENARO.indicator.code) |> 
    reframe(Countries = length(unique(iso3)),
              First.Year = min(time.period),
              Last.Year = max(time.period),
              n.points = length(obs.value),
              n.points.2015 = length(obs.value[time.period>=2015]))
  
  return(descriptive_df)
}