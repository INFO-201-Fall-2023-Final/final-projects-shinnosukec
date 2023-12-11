library(tidyverse)
library("stringr")

state_year_education <- data.frame()

#2011

df2011 <- read.csv("ACSST1Y2011.S1501-2023-10-30T061355.csv")

df2011_per_bach <- df2011 %>% 
  filter(Label..Grouping. == "Percent bachelor's degree or higher") %>% 
  select(matches("Total")) %>% 
  rename_with(~str_remove(., "..Total..Estimate"))

df2011_per_bach2 <- data.frame(t(df2011_per_bach))

df2011_per_bach2 <- data.frame(cbind(names(df2011_per_bach), t(df2011_per_bach)))
rownames(df2011_per_bach2) <- NULL

df2011_per_bach2 <- mutate(df2011_per_bach2, Year = 2011)


#2012

df2012 <- read.csv("ACSST1Y2012.S1501-2023-10-30T083650.csv")

df2012_per_bach <- df2012 %>% 
  filter(Label..Grouping. == "Percent bachelor's degree or higher") %>% 
  select(matches("Total")) %>% 
  rename_with(~str_remove(., "..Total..Estimate"))

df2012_per_bach2 <- data.frame(t(df2012_per_bach))

df2012_per_bach2 <- data.frame(cbind(names(df2012_per_bach), t(df2012_per_bach)))
rownames(df2012_per_bach2) <- NULL

df2012_per_bach2 <- mutate(df2012_per_bach2, Year = 2012)


#2013

df2013 <- read.csv("ACSST1Y2013.S1501-2023-10-30T084107.csv")

df2013_per_bach <- df2013 %>% 
  filter(Label..Grouping. == "Percent bachelor's degree or higher") %>% 
  select(matches("Total")) %>% 
  rename_with(~str_remove(., "..Total..Estimate"))

df2013_per_bach2 <- data.frame(t(df2013_per_bach))

df2013_per_bach2 <- data.frame(cbind(names(df2013_per_bach), t(df2013_per_bach)))
rownames(df2013_per_bach2) <- NULL

df2013_per_bach2 <- mutate(df2013_per_bach2, Year = 2013)


#2014

df2014 <- read.csv("ACSST1Y2014.S1501-2023-10-30T084354.csv")

df2014_per_bach <- df2014 %>% 
  filter(Label..Grouping. == "Percent bachelor's degree or higher") %>% 
  select(matches("Total")) %>% 
  rename_with(~str_remove(., "..Total..Estimate"))

df2014_per_bach2 <- data.frame(t(df2014_per_bach))

df2014_per_bach2 <- data.frame(cbind(names(df2014_per_bach), t(df2014_per_bach)))
rownames(df2014_per_bach2) <- NULL

df2014_per_bach2 <- mutate(df2014_per_bach2, Year = 2014)


#2015

df2015 <- read.csv("ACSST1Y2015.S1501-2023-10-31T020223.csv")

df2015_per_bach <- df2015 %>% 
  filter(Label..Grouping. == "Percent bachelor's degree or higher") %>% 
  select(matches("..Percent..Estimate")) %>% 
  rename_with(~str_remove(., "..Percent..Estimate"))

df2015_per_bach2 <- data.frame(t(df2015_per_bach))

df2015_per_bach2 <- data.frame(cbind(names(df2015_per_bach), t(df2015_per_bach)))
rownames(df2015_per_bach2) <- NULL

df2015_per_bach2 <- mutate(df2015_per_bach2, Year = 2015)


#2016

df2016 <- read.csv("ACSST1Y2016.S1501-2023-10-31T020503.csv")

df2016_per_bach <- df2016 %>% 
  filter(Label..Grouping. == "Percent bachelor's degree or higher") %>% 
  select(matches("..Percent..Estimate")) %>% 
  rename_with(~str_remove(., "..Percent..Estimate"))

df2016_per_bach2 <- data.frame(t(df2016_per_bach))

df2016_per_bach2 <- data.frame(cbind(names(df2016_per_bach), t(df2016_per_bach)))
rownames(df2016_per_bach2) <- NULL

df2016_per_bach2 <- mutate(df2016_per_bach2, Year = 2016)


#2017

df2017 <- read.csv("ACSST1Y2017.S1501-2023-10-31T020644.csv")

df2017_per_bach <- df2017 %>% 
  filter(Label..Grouping. == "Percent bachelor's degree or higher") %>% 
  select(matches("..Percent..Estimate")) %>% 
  rename_with(~str_remove(., "..Percent..Estimate"))

df2017_per_bach2 <- data.frame(t(df2017_per_bach))

df2017_per_bach2 <- data.frame(cbind(names(df2017_per_bach), t(df2017_per_bach)))
rownames(df2017_per_bach2) <- NULL

df2017_per_bach2 <- mutate(df2017_per_bach2, Year = 2017)


#2018

df2018 <- read.csv("ACSST5Y2018.S1501-2023-10-31T021254.csv")

df2018_per_bach <- df2018 %>% 
  slice(16) %>% 
  select(matches("..Percent..Estimate")) %>% 
  rename_with(~str_remove(., "..Percent..Estimate"))

df2018_per_bach2 <- data.frame(t(df2018_per_bach))

df2018_per_bach2 <- data.frame(cbind(names(df2018_per_bach), t(df2018_per_bach)))
rownames(df2018_per_bach2) <- NULL

df2018_per_bach2 <- mutate(df2018_per_bach2, Year = 2018)


#2019

df2019 <- read.csv("ACSST5Y2019.S1501-2023-10-31T022236.csv")

df2019_per_bach <- df2019 %>% 
  slice(16) %>% 
  select(matches("..Percent..Estimate")) %>% 
  rename_with(~str_remove(., "..Percent..Estimate"))

df2019_per_bach2 <- data.frame(t(df2019_per_bach))

df2019_per_bach2 <- data.frame(cbind(names(df2019_per_bach), t(df2019_per_bach)))
rownames(df2019_per_bach2) <- NULL

df2019_per_bach2 <- mutate(df2019_per_bach2, Year = 2019)


#####################

# Binding each year's dataframe into one and naming columns.

state_year_education <- rbind(df2011_per_bach2, df2012_per_bach2, df2013_per_bach2, df2014_per_bach2, df2015_per_bach2, df2016_per_bach2, df2017_per_bach2, df2018_per_bach2, df2019_per_bach2)
names(state_year_education) <- c("State", "Percent of bachelor's degree or higher", "Year")

# Exporting the combined dataframe as .csv

# write.csv(state_year_education, "/Users/shinnosukechuman/Documents/INFO201/final-projects-shinnosukec/state_year_education.csv", row.names=FALSE)

