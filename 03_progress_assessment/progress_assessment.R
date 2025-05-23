# Project: Trend analysis for SDG children indicators for UNICEF MENARO
# Script description: Main progress assessment script
# Author: Sebastian Palmas

# PROFILE ----
source("profile.R")

# FILES ----
source("03_progress_assessment/progress_assessment_functions.R")
load("04_output/indicator_data_WORLD.Rdata")

# PREPARE TABLE WITH MENARO AND LOG VALUES ----
indicator_data_MENARO <- indicator_data_WORLD |> filter(iso3 %in% MENARO_metadata$iso3)

# PREDICTION YEARS ----
base.year <- 2015
current.year <- 2025 
target.year <- 2030

# PROGRESS ASSESSMENT ----
# The calculations are done with a loop for each indicator and each country

## Empty table to store results ----
progress.results <- tibble(iso3 = character(0),
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
        obs.value.base.year <- cagr(data.i.c, base.year, denominator)
      } else {
        #Use base year value if found
        obs.value.base.year <- data.i.c$obs.value[base.year.pos]
      }
      
      ##### Current year value----
      current.year.pos <- which(data.i.c$time.period == current.year)
      if(identical(current.year.pos, integer(0))){
        #Estimate base year value when not found
        obs.value.current.year <- cagr(data.i.c, current.year, denominator)
      } else {
        #Use current year value if found
        obs.value.current.year <- data.i.c$obs.value[current.year.pos]
      }
      
      ##### Target year value----
      obs.value.target.year <- cagr(data.i.c, target.year, denominator)
      
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
    progress.results <- bind_rows(progress.results,
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
save(progress.results, file = "04_output/progress_results.Rdata")

# CHARTS AND TABLES----
load(file = "04_output/progress_results.Rdata")

# Define colors for AP scores
ap_colors <- c("Will meet target" = "#238823", 
              "Need to accelerate" = "#FFBF00", 
              "No progress expected" = "#D2222D", 
              "Insufficient data" = "lightgrey", 
              "No data" = "darkgrey")

## Tab: Latest value per country per indicator ----
latest_values <- indicator_data_MENARO  |> 
  group_by(iso3, MENARO.indicator.code)  |> 
  summarize(time.period = max(time.period), .groups = 'drop')  |> 
  left_join(indicator_data_MENARO, by = c("iso3", "MENARO.indicator.code", "time.period"))  |> 
  left_join(CR_SDG_indicators |> select(MENARO.indicator.code, `CHILD-RELATED.INDICATOR`, UNICEF.thematic.group), by = "MENARO.indicator.code") |>
  left_join(MENARO_metadata |> select(iso3, LocPrintName, region), by = "iso3") |>
  select(UNICEF.thematic.group, `CHILD-RELATED.INDICATOR`, region, LocPrintName, obs.value, time.period)  |> 
  arrange(UNICEF.thematic.group, `CHILD-RELATED.INDICATOR`, region, LocPrintName) |> 
  mutate(obs.value = ifelse(obs.value>1, round(obs.value), obs.value)) 

write.csv(latest_values, "04_output/table1_latest_values.csv", row.names = FALSE)

## Fig: Country scores per indicator ----
chart.data <- progress.results |>
  left_join(CR_SDG_indicators |> select(MENARO.indicator.code, INDICATOR_SHORT_NAME, UNICEF.thematic.group), by = "MENARO.indicator.code") |>
  left_join(MENARO_metadata |> select(iso3, LocPrintName, region), by = "iso3")
  
g <- ggplot(chart.data, aes(y = INDICATOR_SHORT_NAME, x = LocPrintName)) +
  geom_tile(aes(fill = AP.score)) +
  scale_fill_manual(values = ap_colors) +
  theme_minimal() +
  facet_grid(rows = vars(UNICEF.thematic.group), scales = "free", space = "free") +
  labs(x = "", y = "", fill = "AP Score") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill = guide_legend(reverse=T)) + 
  scale_y_discrete(limits = rev)

  ggsave("04_output/country_score_by_indicator.jpeg", width = 10, height = 12)

## Fig: Country score by theme ----
chart.data <- progress.results  |> 
  left_join(CR_SDG_indicators |> select(MENARO.indicator.code, UNICEF.thematic.group), by = "MENARO.indicator.code") |>
  left_join(MENARO_metadata |> select(iso3, LocPrintName, region), by = "iso3") |>
  group_by(UNICEF.thematic.group, region, LocPrintName, AP.score)  |> 
  summarize(value = n(), .groups = 'drop')  |>
  mutate(AP.score = fct_relevel(AP.score, "No data", "Insufficient data", "No progress expected", "Need to accelerate","Will meet target"))

g  <- ggplot(chart.data, aes(y= LocPrintName, x = value, fill = AP.score)) +
  facet_wrap(~UNICEF.thematic.group, ncol = 5) +
  geom_bar(position = 'fill', stat = 'identity') +
  scale_fill_manual(values = ap_colors) +
  theme_minimal() +
  labs(x = "", y = "", fill = "AP Score") +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank()) +
  guides(fill = guide_legend(reverse=T)) + 
  scale_y_discrete(limits = rev)

ggsave("04_output/country_score_by_theme.jpeg", width = 12, height = 8)

## Fig: Subregion scores by indicator ----
chart.data <- progress.results  |> 
  left_join(CR_SDG_indicators |> select(MENARO.indicator.code, INDICATOR_SHORT_NAME, UNICEF.thematic.group), by = "MENARO.indicator.code") |>
  left_join(MENARO_metadata |> select(iso3, region), by = "iso3") |>
  group_by(UNICEF.thematic.group, INDICATOR_SHORT_NAME, region, AP.score)  |> 
  summarize(value = n(), .groups = 'drop')  |>
  mutate(AP.score = fct_relevel(AP.score, "No data", "Insufficient data", "No progress expected", "Need to accelerate","Will meet target"))

g  <- ggplot(chart.data, aes(y= INDICATOR_SHORT_NAME, x = value, fill = AP.score)) +
  facet_grid(cols = vars(region), rows = vars(UNICEF.thematic.group), scales = "free", space = "free") +
  geom_bar(position = 'fill', stat = 'identity') +
  scale_fill_manual(values = ap_colors) +
  theme_minimal() +
  labs(x = "", y = "", fill = "AP Score") +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank()) +
  guides(fill = guide_legend(reverse=T)) + 
  scale_y_discrete(limits = rev)

ggsave(plot = g, filename = "04_output/subregion_score_by_indicator.jpeg", width = 12, height = 10)

## Fig: Subregion scores by theme ----
chart.data <- progress.results  |> 
  left_join(CR_SDG_indicators |> select(MENARO.indicator.code, UNICEF.thematic.group), by = "MENARO.indicator.code") |>
  left_join(MENARO_metadata |> select(iso3, region), by = "iso3") |>
  group_by(UNICEF.thematic.group, region, AP.score)  |> 
  summarize(value = n(), .groups = 'drop')  |>
  mutate(AP.score = fct_relevel(AP.score, "No data", "Insufficient data", "No progress expected", "Need to accelerate","Will meet target"))

g  <- ggplot(chart.data, aes(y= UNICEF.thematic.group, x = value, fill = AP.score)) +
  facet_grid(cols = vars(region), scales = "free", space = "free") +
  geom_bar(position = 'fill', stat = 'identity') +
  scale_fill_manual(values = ap_colors) +
  theme_minimal() +
  labs(x = "", y = "", fill = "AP Score") +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank()) +
  guides(fill = guide_legend(reverse=T)) + 
  scale_y_discrete(limits = rev)

ggsave(plot = g, filename = "04_output/subregion_score_by_theme.jpeg", width = 12, height = 4)

## Fig: Region scores by indicator ----
chart.data <- progress.results  |> 
  left_join(CR_SDG_indicators |> select(MENARO.indicator.code, INDICATOR_SHORT_NAME, UNICEF.thematic.group), by = "MENARO.indicator.code") |>
  group_by(UNICEF.thematic.group, INDICATOR_SHORT_NAME, AP.score)  |> 
  summarize(value = n(), .groups = 'drop')  |>
  mutate(AP.score = fct_relevel(AP.score, "No data", "Insufficient data", "No progress expected", "Need to accelerate","Will meet target"))

g  <- ggplot(chart.data, aes(y= INDICATOR_SHORT_NAME, x = value, fill = AP.score)) +
  facet_grid(rows = vars(UNICEF.thematic.group), scales = "free", space = "free") +
  geom_bar(position = 'fill', stat = 'identity') +
  scale_fill_manual(values = ap_colors) +
  theme_minimal() +
  labs(x = "", y = "", fill = "AP Score") +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank()) +
  guides(fill = guide_legend(reverse=T)) + 
  scale_y_discrete(limits = rev)

ggsave(plot = g, filename = "04_output/region_score_by_indicator.jpeg", width = 8, height = 10)

## Fig: Region scores by theme ----
chart.data <- progress.results  |> 
  left_join(CR_SDG_indicators |> select(MENARO.indicator.code, INDICATOR_SHORT_NAME, UNICEF.thematic.group), by = "MENARO.indicator.code") |>
  group_by(UNICEF.thematic.group, AP.score)  |> 
  summarize(value = n(), .groups = 'drop')  |>
  mutate(AP.score = fct_relevel(AP.score, "No data", "Insufficient data", "No progress expected", "Need to accelerate","Will meet target"))

g  <- ggplot(chart.data, aes(y= UNICEF.thematic.group, x = value, fill = AP.score)) +
  geom_bar(position = 'fill', stat = 'identity') +
  scale_fill_manual(values = ap_colors) +
  theme_minimal() +
  labs(x = "", y = "", fill = "AP Score") +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank()) +
  guides(fill = guide_legend(reverse=T)) + 
  scale_y_discrete(limits = rev)

ggsave(plot = g, filename = "04_output/region_score_by_theme.jpeg", width = 8, height = 4)
