# Project: Trend analysis for SDG children indicators for UNICEF MENARO
# Script description: Main benchmarking script
# Author: Sebastian Palmas

# PROFILE ----
source("profile.R")

# FILES
load("output/indicator_data_WORLD.Rdata")
source("benchmarking/benchmarking_functions.R")

indicator_data_MENARO <- indicator_data_WORLD |> filter(iso3 %in% MENARO_metadata$iso3)

# BENCHMARKING ----
# The benchmarking is done with a loop for each indicator and each country

## Empty table to store results ----
country_results <- tibble(iso3 = character(0),
                          MENARO.indicator.code = character(0),
                          points = integer(0),
                          score = character(0))

## Assign score loop ----
for (i in 1:nrow(CR_SDG_indicators)){
  #i <- 1  #to test
  
  #indicator values
  indicator_data_MENARO.i <- indicator_data_MENARO |> 
    filter(MENARO.indicator.code == CR_SDG_indicators$MENARO.indicator.code[i]) #data for indicator
  TV <- CR_SDG_indicators$target[i]   #final target
  direction.i <- CR_SDG_indicators$direction[i]   #direction of indicator
  
  print(paste0(CR_SDG_indicators$SDG_INDICATOR[i], ": ", CR_SDG_indicators$`CHILD-RELATED.INDICATOR`[i]))
  
  #count how many countries have more than 2 points for the indicator
  ctry.2plus.points <- sum(table(indicator_data_MENARO.i$iso3)>1)
  print(paste0("How many countries have more 2 or more data points: ", ctry.2plus.points))
  
  ### Assign indicator|country score loop ----  
  for (country in MENARO_metadata$iso3){
    #country <- 'JOR' #to test  
    indicator_data_MENARO.i.country <- indicator_data_MENARO.i |> filter(iso3 == country)  #data for indicator|country
    
    #How many points are in this indicator|country
    points_c <- nrow(indicator_data_MENARO.i.country)
    
    
    if(points_c > 1){ 
      #### 2+ data points score ----
      
      ##### Time-weighted regression----
      ##### 2015 value----
      #I0 <- 
      ##### Current status----
      ##### Anticipated Progress and score----
      
      #AP = 5 #to test
      score_c = ifelse(AP>9, "Will meet the target", ifelse(AP<=0, 'No progress expected', 'Need to accelerate'))
      
    } else if(points_c== 1){
      #### 1 data point score ----
        reached_target <- ifelse(direction.i == 'increasing',
                                 indicator_data_MENARO.i.country$obs.value >= TV,
                                 indicator_data_MENARO.i.country$obs.value <= TV)
        score_c <- ifelse(reached_target, 'Will be achieved', 'Insufficient data')
      
    } else if (points_c == 0){
      #### No data points score ----
      score_c <- "No data"
    }
  
    #storing indicator|country results in final table
    country_results <- bind_rows(country_results,
                                 tibble(iso3 = country,
                                        MENARO.indicator.code = CR_SDG_indicators$MENARO.indicator.code[i],
                                        points = points_c,
                                        score = score_c)) 
  }
  
  print ("-----------------------------------------------")
}

# EXPORTING RESULTS ----