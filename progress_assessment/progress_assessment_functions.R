# Project: Trend analysis for SDG children indicators for UNICEF MENARO
# Script description: functions used for the progress assessment
# Author: Sebastian Palmas



# Current status index ----
# I_cv: current year's value
# I_0: base year's value
# TV: target value
# direction: direction of indicator (increasing or decreasing)
# parity: if indicator is a parity indicator
CS <- function(I_cv, I0, TV, direction = 'increasing', parity=FALSE){
  
  # If current value already reached or exceeded the target value, CS is 10
  if(direction == 'decreasing'){
    if(I_cv <= TV){
      return(10)
    }
  } else {
    if (I_cv >= TV){
      return(10)
    }
  }
  
  # if current value has not reached target, compute exact CS value
  D <- ifelse(direction == 'decreasing', -10, 10)

  if (!parity){
    return(D * (I_cv - I0) / abs(TV - I0))
  } else {
    return(10 - 10 * abs(TV - I_cv) / abs(TV - I0))
  }
}

# Weighted geometric mean ----
wgm <- function(data, year.to.predict){
  #data <- data.i.c #to test
  #year.to.predict <- 2015  #to test
  data <- data |> filter(time.period < year.to.predict)
  year.min = min(data$time.period)
  year.max = max(data$time.period)
  data <- data |> mutate(wi = (year.to.predict - year.min)/(year.to.predict - time.period),
                         obs.value.log.im1 = lag(obs.value.log),
                         ratio.ti.im1 = (obs.value.log/obs.value.log.im1)^wi)
  
  In <- data$obs.value.log[nrow(data)]
  W <- sum(data$wi)
  a <- year.to.predict - year.max 
  
  return(In * (prod(data$ratio.ti.im1[2:nrow(data)]))^(a/W))
  
}


# Evidence strength ----