# Get the Data

# Read in the data manually
features<-readr::read_csv('https://raw.githubusercontent.com/BjnNowak/UltraTrailRunning/main/features.csv')
ranking<-readr::read_csv('https://raw.githubusercontent.com/BjnNowak/UltraTrailRunning/main/ranking.csv')

# The 'Age' variable provided by ITRA gives runners' age in 2021 for each race,
# regardless of the year of the races. The few lines below allow to create a 
# new variable 'AgeAtRace', which gives runners' ages at the time of the race

library(tidyverse)

ranking_full<-ranking%>%
  # Merging features to get date of race
  left_join(features)%>%
  # Get year from date of race
  mutate(Year=lubridate::year(Date))%>%
  # Compute AgeAtRace
  mutate(AgeAtRace=Age-(2021-Year))%>%
  # Set to NA if age is not available
  mutate(AgeAtRace=case_when(
    AgeAtRace==0~NA_real_,
    TRUE~AgeAtRace)
  )
