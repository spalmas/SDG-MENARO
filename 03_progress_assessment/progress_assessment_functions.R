# Project: Trend analysis for SDG children indicators for UNICEF MENARO
# Script description: functions used for the progress assessment
# Author: Sebastian Palmas

# Current status index ----
# I_cv: current year's value
# I_0: base year's value
# TV: target value
# direction: direction of indicator (increasing or decreasing)
CS <- function(I_cv, I0, TV, direction){
  
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
  return(D * (I_cv - I0) / abs(TV - I0))
}

# Geometric mean ----
#based on geometric growth method
gm <- function(data, year.to.predict){
  #data <- data.i.c #to test
  #year.to.predict <- 2030  #to test
  data <- data |> arrange(time.period)  #ordering by year to properly assign value in previous measurement
  T <- max(data$time.period) - min(data$time.period)
  a <- year.to.predict - max(data$time.period)
  return(data$obs.value[nrow(data)] * (data$obs.value[nrow(data)]/data$obs.value[1]) ^ (a/T))
}

# Weighted geometric mean ----
wgm <- function(data, year.to.predict){
  #data <- data.i.c #to test
  #year.to.predict <- 2015  #to test
  data <- data |> arrange(time.period)  #ordering by year to properly assign value in previous measurement
  
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


# log regression ----
reg <- function(data, years.to.predict, scale){
  #data <- data.i.c #to test
  #years.to.predict <- c(2015, 2025,2030)  #to test
  #scale <- 100
  #reg(data.i.c, c(2015, 2025,2030), 100)
  t.mean <- mean(data$time.period)
  data <- data |> 
    mutate(Y=data$obs.value/scale,
           L = log(Y/(1-Y)),
           t.prime = time.period - t.mean)
  
  lm1 <- lm(L~t.prime, data = data)
  
  years.to.predict.prime <- data.frame(t.prime = c(years.to.predict - t.mean))
  
  EXP <- exp(predict.lm(lm1, newdata = years.to.predict.prime))
  
  return(scale * EXP / ( 1 +EXP))
  
}
# Weighted log regression ----
# data <- data.i.c
# scale <- 100
# wreg(data.i.c, 2030, 100)
wreg <- function(data, year.to.predict, scale){
  year.min <- min(data$time.period)
  t.mean <- mean(data$time.period)
  
  #pos <- 1 # to test
  data.pred <- data |> mutate(wi = (year.to.predict - year.min)/(year.to.predict - time.period),
                              Y = data$obs.value / scale,
                              L = log(Y / (1 - Y)),
                              t.prime = time.period - t.mean)
  lm1 <- lm(L ~ t.prime, data = data.pred)
  W <- sum(data.pred$wi)
  w <- sum(data.pred$wi * data.pred$t.prime * data.pred$L)
  x <- sum(data.pred$wi * data.pred$t.prime) * sum(data.pred$wi* data.pred$L) / W
  y <- sum(data.pred$wi * data.pred$t.prime^2)
  z <- sum(data.pred$wi * data.pred$t.prime)^2 / W
  
  r1 <- (w - x) / (y - z)
  
  EXP <- exp(lm1$coefficients[[1]] + r1 * (year.to.predict - t.mean))
  
  return(scale * EXP / ( 1 + EXP))
}


# CAGR ----
# data <- data.i.c
# year.to.predict <- 2025
# scale <- 100
# cagr(data, 2030, scale)
cagr <- function(data, year.to.predict, scale){
  data <- data |> arrange(time.period) |> 
    mutate(Y = obs.value/scale, #scaling 
           obs.value.log = log(Y / (1 - Y)), #logit
           time.difference = time.period - lag(time.period),
           cagr = (obs.value.log - lag(obs.value.log)) / time.difference)

  #mean cagr across periods
  mcagr <- mean(data$cagr, na.rm = TRUE)
  
  if (is.nan(mcagr) | is.infinite(mcagr)){
    #there are cases where
    # - the value has been constant (SP_ACS_BSRVH2O, KWT) and there is no slope and no mcagr
    # - The are values of 1, which return an Infinite logit value and no cagr can be calculated
    #in these cases, we return the latest value of the observation
    return(tail(data$obs.value, n = 1))
  }
  
  #prediction of logit value
  EXP <- exp(data$obs.value.log[nrow(data)] + mcagr * (year.to.predict - data$time.period[nrow(data)]))
  
  #return inverse logit
  return(scale * EXP / (1 + EXP))
}


# Evidence strength ----