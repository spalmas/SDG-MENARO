# Project: Trend analysis for SDG children indicators for UNICEF MENARO
# Script description: functions used for benchmarking
# Author: Sebastian Palmas


# Assign score ----
assign_score <- function(){
  
}

# Current status index ----
# I_cv: current year's value
# I_0: base year's value
# TV: target value
# direction: direction of indicator (increasing or decreasing)
# parity: if indicator is a parity indicator
CS <- function(I_cv, I0, TV, direction = 'increasing', parity=FALSE){
  
  D <- 10
  if(direction == 'decreasing'){
    D <- -10
  }
  
  if (!parity){
    return(D * (I_cv - I0) / abs(TV - I0))
  } else {
    return(10 - 10 * abs(TV - I_cv) / abs(TV - I0))
  }
}


# Anticipated progress index ----

# Extrapolation ----

# Evidence strength ----