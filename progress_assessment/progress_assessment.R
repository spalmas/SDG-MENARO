# Project: Trend analysis for SDG children indicators for UNICEF MENARO
# Script description: Main progress assessment script
# Author: Sebastian Palmas

# PROFILE ----
source("profile.R")

# FILES ----
load("output/indicator_data_WORLD.Rdata")
source("progress_assessment/progress_assessment_functions.R")

# PREPARE TABLE WITH MENARO AND LOG VALUES ----
indicator_data_MENARO <- indicator_data_WORLD |> filter(iso3 %in% MENARO_metadata$iso3)

# PREDICTION YEARS ----
base.year <- 2015
current.year <- 2025
target.year <- 2030

# PROGRESS ASSESSMENT ----
# The calculations are done with a loop for each indicator and each country

## Empty table to store results ----
progress_results <- tibble(iso3 = character(0),
                          MENARO.indicator.code = character(0),
                          points = integer(0),
                          CS = numeric(0),
                          CS.score = character(0),
                          AP = numeric(0),
                          AP.score = character(0))

## Assign score loop ----
for (i in 1:nrow(CR_SDG_indicators)){
  #i <- 44  #to test
  TV.i <- CR_SDG_indicators$target[i]   #final target
  direction.i <- CR_SDG_indicators$direction[i]   #direction of indicator
  denominator <- CR_SDG_indicators$denominator[i]   #denominator to use to transform to 0-1
  
  #indicator values
  data.i <- indicator_data_MENARO |> filter(MENARO.indicator.code == CR_SDG_indicators$MENARO.indicator.code[i]) #data for indicator
  
  #count how many countries have more than 2 points for the indicator
  ctry.2plus.points <- sum(table(data.i$iso3)>1)

  #print some results to console
  print(paste0(CR_SDG_indicators$SDG_INDICATOR[i], ": ", CR_SDG_indicators$`CHILD-RELATED.INDICATOR`[i]))
  print(paste0("How many countries have more 2 or more data points: ", ctry.2plus.points))
  
  ### Assign indicator|country score loop ----  
  for (c in MENARO_metadata$iso3){
    #c <- 'QAT' #to test  
    data.i.c <- data.i |> filter(iso3 == c)   #data for indicator|country
    
    #How many points are in this indicator|country?
    points.i.c <- nrow(data.i.c)
    
    if(points.i.c > 1){ 
      #### 2+ data points score ----
      ##### Base year value----
      base.year.pos <- which(data.i.c$time.period == base.year)
      if(identical(base.year.pos, integer(0))){
        #Estimate base year value when not found
        obs.value.base.year <- cagr(data = data.i.c, year.to.predict = base.year, scale = denominator)
      } else {
        #Use base year value if found
        obs.value.base.year <- data.i.c$obs.value[base.year.pos]
      }
      
      ##### Current year value----
      current.year.pos <- which(data.i.c$time.period == current.year)
      if(identical(current.year.pos, integer(0))){
        #Estimate base year value when not found
        obs.value.current.year <- cagr(data = data.i.c, year.to.predict = current.year, scale = denominator)
      } else {
        #Use base year value if found
        obs.value.current.year <- data.i.c$obs.value[current.year.pos]
      }
      
      ##### Target year value----
      obs.value.target.year <- cagr(data = data.i.c, year.to.predict = target.year, scale = denominator)
      
      ##### Current Status ----
      CS.i.c <- CS(I_cv = obs.value.current.year, 
                   I0 = obs.value.base.year, 
                   TV = TV.i, 
                   direction = direction.i)
      CS.score.i.c = ifelse(CS.i.c > 9, "On-track",
                           ifelse(CS.i.c <= 0, 'Regression', 'Slow progress'))
      
      ##### Anticipated Progress ----
      AP.i.c <- CS(I_cv = obs.value.target.year, 
                   I0 = obs.value.base.year, 
                   TV = TV.i, 
                   direction = direction.i)
      AP.score.i.c = ifelse(AP.i.c > 9, "Will meet target",
                            ifelse(AP.i.c <= 0, 'No progress expected', 'Need to accelerate'))
      
    } else if(points.i.c== 1){
      #### 1 data point score ----
        reached.target <- ifelse(direction.i == 'increasing',
                                 data.i.c$obs.value >= TV.i,
                                 data.i.c$obs.value <= TV.i)
        
        CS.i.c <- ifelse(reached.target, 10, NA)
        CS.score.i.c <- ifelse(reached.target, "On-track", 'Insufficient data')
        AP.i.c. <- ifelse(reached.target, 10, NA)
        AP.score.i.c <- ifelse(reached.target, 'Will meet target', 'Insufficient data')
      
    } else if (points.i.c == 0){
      #### No data points score ----
      CS.i.c <- NA
      CS.score.i.c <- "No data"
      AP.i.c <- NA
      AP.score.i.c <- "No data"
    }
  
    #storing indicator|country results in final table
    progress_results <- bind_rows(progress_results,
                                 tibble(iso3 = c,
                                        MENARO.indicator.code = CR_SDG_indicators$MENARO.indicator.code[i],
                                        points = points.i.c,
                                        CS = CS.i.c,
                                        CS.score = CS.score.i.c,
                                        AP = AP.i.c,
                                        AP.score = AP.score.i.c)) 
  }
  
  print ("-----------------------------------------------")
}

# EXPORTING RESULTS ----
save(progress_results, file = "output/progress.results.Rdata")
