library(tidyverse)
library("stringr")

# IMPORTING DATA
df_education <- read.csv("state_year_education.csv")
df_tobacco_use <- read.csv("Behavioral_Risk_Factor_Data__Tobacco_Use__2011_to_present_.csv")

# DATA CLEANING AND JOINING

# selecting columns in tobacco use dataset and cleaning it 
df_tobacco_use_mod <- df_tobacco_use %>% 
  select(YEAR, LocationDesc, TopicDesc, MeasureDesc, Response, Data_Value) %>% 
  filter(TopicDesc == "Cigarette Use (Adults)" | TopicDesc == "E-Cigarette Use (Adults)") %>% 
  filter(MeasureDesc == "Current Smoking" | MeasureDesc == "Current Use" | Response == "Current") %>% 
  group_by(YEAR, LocationDesc, TopicDesc) %>% 
  summarise(smoking_pop_perc = mean(Data_Value, na.rm = TRUE)) %>% 
  ungroup()

# merging
df_cig <- df_tobacco_use_mod %>% 
  filter(TopicDesc == "Cigarette Use (Adults)") %>% 
  select(YEAR, LocationDesc, TopicDesc, smoking_pop_perc)

df_ecig <- df_tobacco_use_mod %>% 
  filter(TopicDesc == "E-Cigarette Use (Adults)") %>% 
  select(YEAR, LocationDesc, TopicDesc, smoking_pop_perc)

df_tobacco_use_final <- merge(df_cig, df_ecig, by = c("YEAR", "LocationDesc"), all.x = TRUE)

df_education$State <- str_replace_all(df_education$State, "\\.", " ")
df_education$Percent.of.bachelor.s.degree.or.higher <- as.numeric(str_remove(df_education$Percent.of.bachelor.s.degree.or.higher, "%")) 

df <- merge(df_education, df_tobacco_use_final, by.x = c("Year", "State"), by.y = c("YEAR", "LocationDesc"))


# DATA AUGMENTATION

df$TopicDesc.x <- NULL
df$TopicDesc.y <- NULL

df <- setNames(df, c("Year", "State", "Percent of bachelor's degree or higher", "Percentage of cig user", "Percentage of E-cig user"))

# new categorical 
df <- df %>%
  mutate("E-cig existence" = !is.na(`Percentage of E-cig user`))

# new numeric 
df <- df %>%
  mutate("E-cig ratio" = `Percentage of E-cig user`/(`Percentage of E-cig user` + `Percentage of cig user`))

# new summarization data frame 
df_all_time <- df %>% 
  group_by(State) %>% 
  summarise("All time average percent of bachelor's degree or higher" = mean(`Percent of bachelor's degree or higher`, na.rm = TRUE), "All time average percent of smokers" = mean(`Percentage of E-cig user`, na.rm = TRUE))

df_all_time_ecig <- df %>% 
  filter(Year == 2016 | Year == 2017) %>% 
  group_by(State) %>% 
  summarise(edulevel = mean(`Percent of bachelor's degree or higher`, na.rm = TRUE), ecig = mean(`Percentage of E-cig user`, na.rm = TRUE))

df_by_year <- df %>% 
  group_by(Year) %>% 
  summarise(`Percent of bachelor's degree or higher` = mean(`Percent of bachelor's degree or higher`, na.rm = TRUE), `Percentage of cig user` = mean(`Percentage of cig user`, na.rm = TRUE)) 

df_by_year <- df_by_year %>% 
  mutate(State = "All states average", .after = Year)

df_temp <- df %>% 
  select(Year, State, `Percent of bachelor's degree or higher`, `Percentage of cig user`)
  
df_view3 <- rbind(df_by_year, df_temp)

# CALCULATIONS

cor_avg <- round(cor(df_all_time$`All time average percent of smokers`, df_all_time$`All time average percent of bachelor's degree or higher`), digits = 2)

cor_avg_ecig <- round(cor(df_all_time_ecig$ecig, df_all_time_ecig$edulevel), digits = 2)

# FUNCTIONS

get_alltime_stats <- function(state) {
  selected <- filter(df_all_time, State == state)
  return(HTML(paste("bachelor's degree or higher:"), round(selected$`All time average percent of bachelor's degree or higher`, digits = 1), "<br>", "smokers:", round(selected$`All time average percent of smokers`, digits = 1)))
}

get_alltime_stats_ecig <- function(state) {
  selected <- filter(df_all_time_ecig, State == state)
  return(HTML(paste("bachelor's degree or higher:"), round(selected$edulevel, digits = 1), "<br>", "E-cig smokers:", round(selected$ecig, digits = 1)))
}

