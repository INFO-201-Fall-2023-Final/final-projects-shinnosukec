library(tidyverse)
library("stringr")

df_education <- read.csv("state_year_education.csv")
df_tobacco_use <- read.csv("Behavioral_Risk_Factor_Data__Tobacco_Use__2011_to_present_.csv")

df_tobacco_use_mod <- df_tobacco_use %>% 
  select(YEAR, LocationDesc, TopicDesc, MeasureDesc, Data_Value) %>% 
  filter(TopicDesc == "Cigarette Use (Adults)" & MeasureDesc == "Current Smoking") %>% 
  filter(YEAR == 2015 & LocationDesc == "Michigan")