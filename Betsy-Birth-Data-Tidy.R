library(tidyverse)
library(dplyr)
library(lubridate)
library(skimr)
library(scales)


Birth_Data <- read_csv("Betsy Dalton_Birth Comm Data for R Project.csv")


# Use mutate to create a new variable named "days_postpartum"
Birth_Data <- Birth_Data |> 
  mutate(Birth_Date = mdy(Birth_Date),   # Convert Birth_Date to Date object
         Recorded_Date = mdy(Recorded_Date),       # Convert end_date to Date object
         Days_postpartum = as.numeric(Recorded_Date - Birth_Date))


# Next I need to reverse code some items and then create new "totals" variables from scale items


Birth_Data <- read_csv("Betsy Dalton_Birth Comm Data for R Project.csv") 

reverse_cols = c("Understand1R_lingering_questions", "Understand2R_incomplete_knowledge" , "Uncert1_Understand_details" , 
                 "SDM5R_not_given_choice")

#reverse code Q2 and Q5 columns
Birth_Data[ , reverse_cols] = 6 - Birth_Data[ , reverse_cols]



# Use mutate to create a new variable named "days_postpartum"

Birth_Data <- Birth_Data |> 
  mutate(Birth_Date = mdy(Birth_Date),   # Convert Birth_Date to Date object
         Recorded_Date = mdy(Recorded_Date),       # Convert end_date to Date object
         Days_postpartum = as.numeric(Recorded_Date - Birth_Date))

# Calculate scale totals (taking mean of grouped items for totals on Uncertainty, 
# Understanding, and Shared Decision-Making

Birth_Data <- Birth_Data|> 
  rowwise() |> 
  mutate(UnderstandMean = mean(c_across(c("Understand1R_lingering_questions", "Understand2R_incomplete_knowledge" , 
                                          "Understand3_why_decisions_made" , "Understand4_Can_accurately_describe" , 
                                          "Understand5_Discharge" , "Understand6_Discharge_baby")), na.rm=TRUE))

Birth_Data <- Birth_Data|> 
  rowwise() |> 
  mutate(UncertMean = mean(c_across(c("Uncert1_Understand_details" , "Uncert2R_does_not_make_sense" , 
                                      "Uncert3R_Puzzling")), na.rm=TRUE))

Birth_Data <- Birth_Data|> 
  rowwise() |> 
  mutate(SDMmean = mean(c_across(c("SDM1_Opinions_respected", "SDM2_Self_respected" , 
                                   "SDM3_Equal_Say_how", "SDM4_Equal_say_when" , "SDM5R_not_given_choice" , 
                                   "SDM6_pleased_how_decisions_made")), na.rm=TRUE))

# Data already pretty tidy, so I will practice advanced summarizing to determine the
# percentage of women at each level of familiarity with attending provider who are 
# interested in a postpartum discussion session

Familiarity_Interest_PPDisc <- Birth_Data |> 
  group_by(Familiarity_with_Provider, Would_do_PP_Discussion) |> 
  summarise(number_of_women = n()) |> 
  mutate(pct = number_of_women / sum(number_of_women, na.rm = TRUE)) |> 
  view() |> 
  ungroup()

