#Housekeeping#---------------------
#Clearing envionment
rm(list=ls())
#Setting working directory
setwd('C:/Users/samra/Documents/My Documents/Uni/Imperial/Winter Project/R')

#Opening Required Libraries#---------------------
library(readxl)
library(ggplot2)
library(lme4)
library(dplyr)
library(stringr)
library(tidyr)
library(MCMCglmm)
library(parallel)
library(coda)

#Importing Data#---------------
#Importing excel document as a dataframe
chicken <- read_xlsx('C:/Users/samra/Documents/My Documents/Uni/Imperial/Winter Project/Chicken_Data_3.xlsx', 
                     sheet= "Datasheet", col_names=TRUE)

#Data Wrangling#-----------
#Setting "NA"s from excel document to be NAs
chicken[chicken=="NA"] <- NA

#Creating blank columns to define whether an estimate had been inferred
chicken$Egg_Production_Av_Inferred <- NA
chicken$Longevity_Av_Inferred <- NA
chicken$Productive_Lifespan_Av_Inferred <- NA
chicken$Productive_Lifespan_Av_Inferred[370:394] <- "Y" #These were inferred from laying periods in scientific papers
chicken$Productive_Lifespan_Av_Inferred[399:411] <- "Y" #These were inferred from laying periods in scientific papers

#Making Source a factor rather than numeric
chicken$Source <- as.factor(chicken$Source)

#Setting necessary columns to numeric values
chicken$Egg_Production_Av <- as.numeric(chicken$Egg_Production_Av)
chicken$Egg_Production_Min <- as.numeric(chicken$Egg_Production_Min)
chicken$Egg_Production_Max <- as.numeric(chicken$Egg_Production_Max)
chicken$Longevity_Av <- as.numeric(chicken$Longevity_Av)
chicken$Longevity_Min <- as.numeric(chicken$Longevity_Min)
chicken$Longevity_Max <- as.numeric(chicken$Longevity_Max)
chicken$Productive_Lifespan_Av <- as.numeric(chicken$Productive_Lifespan_Av)
chicken$Productive_Lifespan_Min <- as.numeric(chicken$Productive_Lifespan_Min)
chicken$Productive_Lifespan_Max <- as.numeric(chicken$Productive_Lifespan_Max)

#Conversions#---------
#Converting Productive lifespans in Days to Years
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Productive_Lifespan_Units[i])&
           chicken$Productive_Lifespan_Units[i]=="Days",
         chicken$Productive_Lifespan_Av[i] <- chicken$Productive_Lifespan_Av[i]/365,
         chicken$Productive_Lifespan_Av[i] <- chicken$Productive_Lifespan_Av[i])
  #The first ifelse in these loops is for conversion of average values
  #For example, in this case, if a productive lifespan isn't an NA and has the units "Days", then it is divided by 365 to give the productive lifespan in years
  #Otherwise the original value is retained
  ifelse(!is.na(chicken$Productive_Lifespan_Units[i])&
           chicken$Productive_Lifespan_Units[i]=="Days",
         chicken$Productive_Lifespan_Min[i] <- chicken$Productive_Lifespan_Min[i]/365,
         chicken$Productive_Lifespan_Min[i] <- chicken$Productive_Lifespan_Min[i])
  #Does the same as the previous loop, but for minimum values
  ifelse(!is.na(chicken$Productive_Lifespan_Units[i])&
           chicken$Productive_Lifespan_Units[i]=="Days",
         chicken$Productive_Lifespan_Max[i] <- chicken$Productive_Lifespan_Max[i]/365,
         chicken$Productive_Lifespan_Max[i] <- chicken$Productive_Lifespan_Max[i])
  #Does the same as the previous loop, but for maximimum values
  ifelse(!is.na(chicken$Productive_Lifespan_Units[i])&
           chicken$Productive_Lifespan_Units[i]=="Days",
         chicken$Productive_Lifespan_Units[i] <- "Years",
         chicken$Productive_Lifespan_Units[i] <- chicken$Productive_Lifespan_Units[i])
  #The final ifelse statement in these loops converts the units for the updated values to "Years"
}

#Converting Productive lifespans in Weeks to Years
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Productive_Lifespan_Units[i])&
           chicken$Productive_Lifespan_Units[i]=="Weeks",
         chicken$Productive_Lifespan_Av[i] <- chicken$Productive_Lifespan_Av[i]/52,
         chicken$Productive_Lifespan_Av[i] <- chicken$Productive_Lifespan_Av[i])
  ifelse(!is.na(chicken$Productive_Lifespan_Units[i])&
           chicken$Productive_Lifespan_Units[i]=="Weeks",
         chicken$Productive_Lifespan_Min[i] <- chicken$Productive_Lifespan_Min[i]/52,
         chicken$Productive_Lifespan_Min[i] <- chicken$Productive_Lifespan_Min[i])
  ifelse(!is.na(chicken$Productive_Lifespan_Units[i])&
           chicken$Productive_Lifespan_Units[i]=="Weeks",
         chicken$Productive_Lifespan_Max[i] <- chicken$Productive_Lifespan_Max[i]/52,
         chicken$Productive_Lifespan_Max[i] <- chicken$Productive_Lifespan_Max[i])
  ifelse(!is.na(chicken$Productive_Lifespan_Units[i])&
           chicken$Productive_Lifespan_Units[i]=="Weeks",
         chicken$Productive_Lifespan_Units[i] <- "Years",
         chicken$Productive_Lifespan_Units[i] <- chicken$Productive_Lifespan_Units[i])
}

#Converting Productive lifespans in Months to Years
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Productive_Lifespan_Units[i])&
           chicken$Productive_Lifespan_Units[i]=="Months",
         chicken$Productive_Lifespan_Av[i] <- chicken$Productive_Lifespan_Av[i]/12,
         chicken$Productive_Lifespan_Av[i] <- chicken$Productive_Lifespan_Av[i])
  ifelse(!is.na(chicken$Productive_Lifespan_Units[i])&
           chicken$Productive_Lifespan_Units[i]=="Months",
         chicken$Productive_Lifespan_Min[i] <- chicken$Productive_Lifespan_Min[i]/12,
         chicken$Productive_Lifespan_Min[i] <- chicken$Productive_Lifespan_Min[i])
  ifelse(!is.na(chicken$Productive_Lifespan_Units[i])&
           chicken$Productive_Lifespan_Units[i]=="Months",
         chicken$Productive_Lifespan_Max[i] <- chicken$Productive_Lifespan_Max[i]/12,
         chicken$Productive_Lifespan_Max[i] <- chicken$Productive_Lifespan_Max[i])
  ifelse(!is.na(chicken$Productive_Lifespan_Units[i])&
           chicken$Productive_Lifespan_Units[i]=="Months",
         chicken$Productive_Lifespan_Units[i] <- "Years",
         chicken$Productive_Lifespan_Units[i] <- chicken$Productive_Lifespan_Units[i])
}

#Converting Egg Production in /Week to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/Week",
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i]*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/Week",
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i]*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/Week",
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i]*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/Week",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /100 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/100 Weeks",
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i]*0.52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/100 Weeks",
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i]*0.52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/100 Weeks",
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i]*0.52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/100 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /102 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/102 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/102)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/102 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/102)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/102 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/102)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/102 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /103 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/103 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/103)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/103 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/103)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/103 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/103)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/103 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /60 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/60 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/60)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/60 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/60)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/60 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/60)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/60 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /63 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/63 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/63)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/63 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/63)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/63 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/63)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/63 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /65 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/65 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/65)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/65 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/65)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/65 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/65)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/65 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /67 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/67 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/67)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/67 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/67)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/67 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/67)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/67 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /68 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/68 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/68)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/68 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/68)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/68 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/68)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/68 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /70 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/70 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/70)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/70 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/70)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/70 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/70)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/70 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /72 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/72 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/72)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/72 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/72)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/72 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/72)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/72 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /74 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/74 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/74)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/74 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/74)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/74 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/74)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/74 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /77 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/77 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/77)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/77 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/77)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/77 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/77)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/77 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /78 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/78 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/78)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/78 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/78)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/78 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/78)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/78 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /79 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/79 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/79)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/79 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/79)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/79 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/79)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/79 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /80 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/80 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/80)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/80 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/80)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/80 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/80)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/80 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /81 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/81 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/81)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/81 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/81)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/81 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/81)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/81 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /83 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/83 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/83)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/83 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/83)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/83 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/83)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/83 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}


#Converting Egg Production in /85 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/85 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/85)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/85 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/85)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/85 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/85)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/85 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /86 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/86 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/86)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/86 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/86)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/86 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/86)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/86 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /87 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/87 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/87)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/87 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/87)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/87 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/87)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/87 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /88 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/88 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/88)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/88 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/88)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/88 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/88)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/88 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /89 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/89 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/89)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/89 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/89)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/89 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/89)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/89 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /90 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/90 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/90)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/90 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/90)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/90 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/90)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/90 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /91 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/91 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/91)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/91 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/91)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/91 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/91)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/91 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /92 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/92 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/92)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/92 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/92)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/92 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/92)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/92 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /94 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/94 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/94)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/94 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/94)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/94 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/94)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/94 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /95 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/95 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/95)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/95 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/95)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/95 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/95)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/95 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /96 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/96 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/96)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/96 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/96)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/96 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/96)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/96 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in /98 Weeks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/98 Weeks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/98)*52,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/98 Weeks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/98)*52,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/98 Weeks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/98)*52,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="/98 Weeks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in % Hen-Day Egg Production to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="% Hen-Day Egg Production",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/100)*365,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="% Hen-Day Egg Production",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/100)*365,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="% Hen-Day Egg Production",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/100)*365,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="% Hen-Day Egg Production",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Egg Production in Eggs per Hen-Day in 52 Wks to /Year
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="Eggs per Hen-Day in 52 Wks",
         chicken$Egg_Production_Av[i] <- (chicken$Egg_Production_Av[i]/364)*365,
         chicken$Egg_Production_Av[i] <- chicken$Egg_Production_Av[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="Eggs per Hen-Day in 52 Wks",
         chicken$Egg_Production_Min[i] <- (chicken$Egg_Production_Min[i]/364)*365,
         chicken$Egg_Production_Min[i] <- chicken$Egg_Production_Min[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="Eggs per Hen-Day in 52 Wks",
         chicken$Egg_Production_Max[i] <- (chicken$Egg_Production_Max[i]/364)*365,
         chicken$Egg_Production_Max[i] <- chicken$Egg_Production_Max[i])
  ifelse(!is.na(chicken$Egg_Production_Units[i])&
           chicken$Egg_Production_Units[i]=="Eggs per Hen-Day in 52 Wks",
         chicken$Egg_Production_Units[i] <- "/Year",
         chicken$Egg_Production_Units[i] <- chicken$Egg_Production_Units[i])
}

#Converting Longevity in Months to Years
for(i in 1:nrow(chicken)){
  ifelse(!is.na(chicken$Longevity_Units[i])&
           chicken$Longevity_Units[i]=="Months",
         chicken$Longevity_Av[i] <- chicken$Longevity_Av[i]/12,
         chicken$Longevity_Av[i] <- chicken$Longevity_Av[i])
  ifelse(!is.na(chicken$Longevity_Units[i])&
           chicken$Longevity_Units[i]=="Months",
         chicken$Longevity_Min[i] <- chicken$Longevity_Min[i]/12,
         chicken$Longevity_Min[i] <- chicken$Longevity_Min[i])
  ifelse(!is.na(chicken$Longevity_Units[i])&
           chicken$Longevity_Units[i]=="Months",
         chicken$Longevity_Max[i] <- chicken$Longevity_Max[i]/12,
         chicken$Longevity_Max[i] <- chicken$Longevity_Max[i])
  ifelse(!is.na(chicken$Longevity_Units[i])&
           chicken$Longevity_Units[i]=="Months",
         chicken$Longevity_Units[i] <- "Years",
         chicken$Longevity_Units[i] <- chicken$Longevity_Units[i])
}

#Checking numbers of useful estimates#---------
nrow(chicken[which(!is.na(chicken$Egg_Production_Av) &
                     !is.na(chicken$Longevity_Av)),]) #Checking how many rows have both average egg production and average longevity estimates
#24 estimates of egg production AND longevity

nrow(chicken[which(!is.na(chicken$Egg_Production_Av) &
                     !is.na(chicken$Productive_Lifespan_Av)),]) #Checking how many rows have both average egg production and average productive lifespan estimates
#64 estimates of egg production AND productive lifespan

#Inferring averages form mins and maxes#----------
#If there's min. and max. values but no av., substituting in min. max. mean
#Egg production
for(i in 1:nrow(chicken)){
  ifelse(is.na(chicken$Egg_Production_Av[i])&
           !is.na(chicken$Egg_Production_Min[i])&
           !is.na(chicken$Egg_Production_Max[i]),
         chicken$Egg_Production_Av_Inferred[i]<-"Y",
         chicken$Egg_Production_Av_Inferred[i]<-chicken$Egg_Production_Av_Inferred[i])}
#This first for loop is putting a "Y" in the "Egg_Production_Av_Inferred" column for all rows which have a minimum and maximum egg production value but no average
for(i in 1:nrow(chicken)){
  ifelse(is.na(chicken$Egg_Production_Av[i])&
           !is.na(chicken$Egg_Production_Min[i])&
           !is.na(chicken$Egg_Production_Max[i]),
         chicken$Egg_Production_Av[i]<-(chicken$Egg_Production_Min[i]+
                                          chicken$Egg_Production_Max[i])/2,
         chicken$Egg_Production_Av[i]<-chicken$Egg_Production_Av[i])}
#This for loop is setting the value of the average egg production to the mean of the minimum and maximum for all rows with a min. and max. egg production value but no av.

#Longevity
#Same set up as for egg production
for(i in 1:nrow(chicken)){
  ifelse(is.na(chicken$Longevity_Av[i])&
           !is.na(chicken$Longevity_Min[i])&
           !is.na(chicken$Longevity_Max[i]),
         chicken$Longevity_Av_Inferred[i]<-"Y",
         chicken$Longevity_Av_Inferred[i]<-chicken$Longevity_Av_Inferred[i])}
for(i in 1:nrow(chicken)){
  ifelse(is.na(chicken$Longevity_Av[i])&
           !is.na(chicken$Longevity_Min[i])&
           !is.na(chicken$Longevity_Max[i]),
         chicken$Longevity_Av[i]<-(chicken$Longevity_Min[i]+
                                     chicken$Longevity_Max[i])/2,
         chicken$Longevity_Av[i]<-chicken$Longevity_Av[i])}

#Productive lifespan
#Same set up as for egg production
for(i in 1:nrow(chicken)){
  ifelse(is.na(chicken$Productive_Lifespan_Av[i])&
           !is.na(chicken$Productive_Lifespan_Min[i])&
           !is.na(chicken$Productive_Lifespan_Max[i]),
         chicken$Productive_Lifespan_Av_Inferred[i]<-"Y",
         chicken$Productive_Lifespan_Av_Inferred[i]<-chicken$Productive_Lifespan_Av_Inferred[i])}
for(i in 1:nrow(chicken)){
  ifelse(is.na(chicken$Productive_Lifespan_Av[i])&
           !is.na(chicken$Productive_Lifespan_Min[i])&
           !is.na(chicken$Productive_Lifespan_Max[i]),
         chicken$Productive_Lifespan_Av[i]<-(chicken$Productive_Lifespan_Min[i]+
                                               chicken$Productive_Lifespan_Max[i])/2,
         chicken$Productive_Lifespan_Av[i]<-chicken$Productive_Lifespan_Av[i])}

#Re-evaluating number of useful estimates#---------
nrow(chicken[which(!is.na(chicken$Egg_Production_Av) &
                     !is.na(chicken$Longevity_Av)),]) #Checking how many rows have both average egg production and average longevity estimates
#108 estimates of egg production AND longevity

nrow(chicken[which(!is.na(chicken$Egg_Production_Av) &
                     !is.na(chicken$Longevity_Max)),])

nrow(chicken[which(!is.na(chicken$Egg_Production_Av) &
                     !is.na(chicken$Productive_Lifespan_Av)),]) #Checking how many rows have both average egg production and average productive lifespan estimates
#64 estimates of egg production AND productive lifespan

#Creating some new columns#---------
#Source Reliability Score (out of 3)
chicken <- mutate(chicken, Source_Reliability=as.numeric(Grey_Literature)+
                    as.numeric(Worse_than_Grey)+
                    as.numeric(Not_Primary)) #Adding a column called "Source_Reliability" whose value for a given row is the sum of the entries in the "Grey_Literature", "Worse_than_Grey" and "Not_Primary" columns
chicken$Source_Reliability <- as.factor(chicken$Source_Reliability) #Setting the "Source_Reliability" column to a factor so R doesn't treat it as continuous

#Purpose (Layer, Dual, Other) Column
#Creating a new column which defines the purpose as one of three things based on key words in the "Purpose" column
Purpose <- data.frame(Purpose_2 <- as.character()) #Creating a blank data frame to put the output of the for loop into
for(i in 1:nrow(chicken)){ #For each row in turn
  ifelse((is.na(chicken$Purpose[i])==TRUE),
         a <- NA, #If "Purpose" column is an NA, the entry will be an NA
         (ifelse((length(grep("Eggs", 
                              strsplit(chicken$Purpose[i], " ")[[1]], 
                              ignore.case=TRUE))==1) & #Searching for a key word
                   (length(grep("Meat", 
                                strsplit(chicken$Purpose[i], " ")[[1]], 
                                ignore.case=TRUE))==1),
                 a <- "Dual", #Otherwise, if the "Purpose" column contains "Eggs" and "Meat", the entry will be "Dual"
                 (ifelse((length(grep("Eggs", 
                                      strsplit(chicken$Purpose[i], " ")[[1]], 
                                      ignore.case=TRUE))==1),
                         a <- "Layer", #Otherwise, if the "Purpose" column contains "Eggs", the entry will be "Layer"
                         a <- "Other"))))) #Otherwise, the entry will be "Other"
  Purpose_2 <- a #Adding the temporary file to a thing called "Purpose_2"
  Purpose <- rbind(Purpose, Purpose_2) #Binding the temporary file to the blank Purpose data frame
}
chicken <- cbind(chicken, Purpose) #Adding the output from the for loop to the chicken dataframe as a column

chicken <- rename(chicken, Purpose_2 = X.Dual.) #Renaming the column "Purpose_2"

#Setting remaining inference values to N
chicken$Egg_Production_Av_Inferred[which(is.na(chicken$Egg_Production_Av_Inferred))]<-"N"
chicken$Longevity_Av_Inferred[which(is.na(chicken$Longevity_Av_Inferred))]<-"N"
chicken$Productive_Lifespan_Av_Inferred[which(is.na(chicken$Productive_Lifespan_Av_Inferred))]<-"N"

#Saving dataset#-----------
save(chicken, file = "chicken.Rda")
#Saving the chicken dataframe as an R file

#Loading in saved dataset (Can run models without having to re-run data cleaning)#-------
load(file = "chicken.Rda")

#Setting priors#--------
#Setting weak prior
prior.1 <- list(R = list(V = diag(2),
                         nu = 0.002),
                G = list(G1 = list(V = diag(2),
                                   nu = 0.002),
                         (G2 = list(V = diag(2),
                                    nu = 0.002))))

prior.1a <- list(R = list(V = diag(2),
                          nu = 0.002),
                 G = list(G1 = list(V = diag(2),
                                    nu = 0.002)))

#Creating required function#------
#Function code from Malsburg (2021)
plot.acfs <- function(x) {
  n <- dim(x)[2]
  par(mfrow=c(ceiling(n/2),2), mar=c(3,2,3,0))
  for (i in 1:n) {
    acf(x[,i], lag.max=100, main=colnames(x)[i])
    grid()
  }
}


#Model 1: Average Longevity#-----
#Subsetting chicken database to just include those with complete data entries
chicken_longevity <- subset(chicken, 
                            !is.na(chicken$Egg_Production_Av) & 
                              !is.na(chicken$Longevity_Av) &
                              !is.na(chicken$Purpose_2))
#How many entries?
nrow(chicken_longevity)
#96

#Scaling data
chicken_longevity$z.Egg_Production_Av <- scale(chicken_longevity$Egg_Production_Av)
chicken_longevity$z.Longevity_Av <- scale(chicken_longevity$Longevity_Av)

#Checking which variables differ in the subset so should be included
boxplot(chicken_longevity$Egg_Production_Av~
          chicken_longevity$Source)
boxplot(chicken_longevity$Egg_Production_Av~
          chicken_longevity$Breed)
boxplot(chicken_longevity$Egg_Production_Av~
          chicken_longevity$Egg_Production_Av_Inferred)
boxplot(chicken_longevity$Egg_Production_Av~
          chicken_longevity$Longevity_Av_Inferred)
boxplot(chicken_longevity$Egg_Production_Av~
          chicken_longevity$Source_Reliability)#Ignore
boxplot(chicken_longevity$Egg_Production_Av~
          chicken_longevity$Purpose_2)

#Bivariate Model
mAvLongevity <- MCMCglmm(cbind(z.Egg_Production_Av, z.Longevity_Av)~trait-1+
                           trait:Purpose_2 +
                           trait:Source +
                           trait:Egg_Production_Av_Inferred +
                           trait:Longevity_Av_Inferred,
                         random = ~us(trait):Breed,
                         rcov = ~us(trait):units,
                         family = c("gaussian", "gaussian"),
                         data = chicken_longevity,
                         nitt = 20006000,
                         thin= 900,
                         burnin= 6000,
                         verbose = TRUE,
                         prior = prior.1a)
plot(mAvLongevity) #Plotting the caterpillar and density plots
#To check how well the model's run
autocorr(mAvLongevity$Sol) #Autocorrelation values for means
autocorr(mAvLongevity$VCV) #Autocorrelation values for variances
summary(mAvLongevity) #Model summary

plot.acfs(mAvLongevity2$Sol) #Checking autocorrelation of means
plot.acfs(mAvLongevity2$VCV) #Checking autocorrelatio  of variances

#Checking merging between runs
#Running the model 4 times and saving the outut as "m1"
set.seed(1)
m1 <- mclapply(1:4, function(i){
  MCMCglmm(cbind(z.Egg_Production_Av, z.Longevity_Av)~trait-1+
             trait:Purpose_2 +
             trait:Source +
             trait:Egg_Production_Av_Inferred +
             trait:Longevity_Av_Inferred,
           random = ~us(trait):Breed,
           rcov = ~us(trait):units,
           family = c("gaussian", "gaussian"),
           data = chicken_longevity,
           nitt = 20006000,
           thin= 900,
           burnin= 6000,
           verbose = TRUE,
           prior = prior.1a)
}, mc.cores = 1)

#Saving the means component of the output as "m1a"
m1a <- lapply(m1, function(m) m$Sol)
m1a <- do.call(mcmc.list, m1a)

#Plotting the Gelman plots of the mean estimates to visualise autocorrelation across the run
par(mfrow=c(5,2), mar=c(2, 2, 1, 2)) #Changing the plotting parameters
gelman.plot(m1a, auto.layout = F) #Plotting the Gelman plots

#Plotting the diagnostic plots of the mean estimates to visualise chain merging
par(mfrow=c(5,2), mar=c(2, 1, 1, 1)) #Changing the plotting parameters
plot(m1a, ask=F, auto.layout=F) #Plotting the diagnostic plots

gelman.diag(m1a) #Retrieving the Gelman diagnostic statistics for mean estimates
summary(m1a) #Retrieving the model summary for the mean estimates

#Saving the variance component of the output as "m1b"
m1b <- lapply(m1, function(m) m$VCV)
m1b <- do.call(mcmc.list, m1b)

#Plotting the Gelman plots for the variance estimates (same as above)
par(mfrow=c(5, 2), mar=c(2, 2, 1, 2))
gelman.plot(m1b, auto.layout=F)

#Plotting the diagnostic plots of the variance estimates (same as above)
par(mfrow=c(5,2), mar=c(2,1,1,1))
plot(m1b, ask=F, auto.layout=F)

summary(m1b) #Retreiving the model summary for the variance estimates

#Model 2: Maximum Longevity (same procedure as Model 1)#=---------
#Subsetting chicken database to just include those with complete data entries
chicken_max_longevity <- subset(chicken, 
                                !is.na(chicken$Egg_Production_Av) & 
                                  !is.na(chicken$Longevity_Max) &
                                  !is.na(chicken$Purpose_2))
#How many entries?
nrow(chicken_max_longevity)
#48

#Scaling data
chicken_max_longevity$z.Egg_Production_Av <- scale(chicken_max_longevity$Egg_Production_Av)
chicken_max_longevity$z.Longevity_Max <- scale(chicken_max_longevity$Longevity_Max)

#Checking which variables differ in the subset so should be included
boxplot(chicken_max_longevity$Egg_Production_Av~
          chicken_max_longevity$Source)
boxplot(chicken_max_longevity$Egg_Production_Av~
          chicken_max_longevity$Breed)
boxplot(chicken_max_longevity$Egg_Production_Av~
          chicken_max_longevity$Egg_Production_Av_Inferred)
boxplot(chicken_max_longevity$Egg_Production_Av~
          chicken_max_longevity$Longevity_Av_Inferred)#Ignore
boxplot(chicken_max_longevity$Egg_Production_Av~
          chicken_max_longevity$Source_Reliability)#Ignore
boxplot(chicken_max_longevity$Egg_Production_Av~
          chicken_max_longevity$Purpose_2)


#Bivariate Model
mMaxLongevity <- MCMCglmm(cbind(z.Egg_Production_Av, z.Longevity_Max)~trait-1+
                   trait:Purpose_2+
                   trait:Source+
                   trait:Egg_Production_Av_Inferred,
                 random=~us(trait):Breed,
                 rcov = ~us(trait):units,
                 family = c("gaussian", "gaussian"),
                 data = chicken_max_longevity,
                 nitt = 20006000,
                 thin= 900,
                 burnin= 6000,
                 verbose = TRUE,
                 prior = prior.1a)

plot(mMaxLongevity)
autocorr(mMaxLongevity$Sol)
autocorr(mMaxLongevity$VCV)
summary(mMaxLongevity)

plot.acfs(mMaxLongevity$Sol)
plot.acfs(mMaxLongevity$VCV)

#Checking merging between runs
set.seed(1)
m2 <- mclapply(1:4, function(i){
  MCMCglmm(cbind(z.Egg_Production_Av, z.Longevity_Max)~trait-1+
             trait:Purpose_2+
             trait:Source+
             trait:Egg_Production_Av_Inferred,
           random=~us(trait):Breed,
           rcov = ~us(trait):units,
           family = c("gaussian", "gaussian"),
           data = chicken_max_longevity,
           nitt = 20006000,
           thin= 900,
           burnin= 6000,
           verbose = TRUE,
           prior = prior.1a)
}, mc.cores = 1)

m2a <- lapply(m2, function(m) m$Sol)
m2a <- do.call(mcmc.list, m2a)

library(coda)
par(mfrow=c(5,2), mar=c(2, 2, 1, 2))
gelman.plot(m2a, auto.layout = F)

par(mfrow=c(5,2), mar=c(2, 1, 1, 1))
plot(m2a, ask=F, auto.layout=F)

gelman.diag(m2a)
summary(m2a)

m2b <- lapply(m2, function(m) m$VCV)
m2b <- do.call(mcmc.list, m2b)

par(mfrow=c(5, 2), mar=c(2, 2, 1, 2))
gelman.plot(m2b, auto.layout=F)

par(mfrow=c(5,2), mar=c(2,1,1,1))
plot(m2b, ask=F, auto.layout=F)

summary(m2b)

#Model 3: Average Productive Lifespan (same procedure as Model 1)#---------
#Subsetting chicken database to just include those with complete data entries
chicken_prod_life <- subset(chicken, 
                            !is.na(chicken$Egg_Production_Av) & 
                              !is.na(chicken$Productive_Lifespan_Av)&
                              !is.na(chicken$Source_Reliability)&
                              !is.na(chicken$Purpose_2))

#How many entries?
nrow(chicken_prod_life)
#62

#Scaling data
chicken_prod_life$z.Egg_Production_Av <- scale(chicken_prod_life$Egg_Production_Av)
chicken_prod_life$z.Productive_Lifespan_Av <- scale(chicken_prod_life$Productive_Lifespan_Av)

#Checking which variables differ in the subset so should be included
boxplot(chicken_prod_life$Egg_Production_Av~
          chicken_prod_life$Source)
boxplot(chicken_prod_life$Egg_Production_Av~
          chicken_prod_life$Breed)
boxplot(chicken_prod_life$Egg_Production_Av~
          chicken_prod_life$Egg_Production_Av_Inferred) #Ignore
boxplot(chicken_prod_life$Egg_Production_Av~
          chicken_prod_life$Productive_Lifespan_Av_Inferred)
boxplot(chicken_prod_life$Egg_Production_Av~
          chicken_prod_life$Source_Reliability)
boxplot(chicken_prod_life$Egg_Production_Av~
          chicken_prod_life$Purpose_2)

#Bivariate Model
mAvProductive <- MCMCglmm(cbind(z.Egg_Production_Av, z.Productive_Lifespan_Av)~trait-1+
                            trait:Purpose_2+
                            trait:Source_Reliability+
                            trait:Productive_Lifespan_Av_Inferred,
                          rcov = ~us(trait):units,
                          random=~us(trait):Breed+
                            idh(trait):Source,
                          family = c("gaussian", "gaussian"),
                          data = chicken_prod_life,
                          nitt = 10000000,
                          thin= 400,
                          verbose = TRUE,
                          prior = prior.1)
plot(mAvProductive)
autocorr(mAvProductive$Sol)
autocorr(mAvProductive$VCV)
summary(mAvProductive)

plot.acfs(mAvProductive$Sol)
plot.acfs(mAvProductive$VCV)

#Checking merging between runs
set.seed(1)
m3 <- mclapply(1:4, function(i){
  MCMCglmm(cbind(z.Egg_Production_Av, z.Productive_Lifespan_Av)~trait-1+
             trait:Purpose_2+
             trait:Source_Reliability+
             trait:Productive_Lifespan_Av_Inferred,
           rcov = ~us(trait):units,
           random=~us(trait):Breed+
             idh(trait):Source,
           family = c("gaussian", "gaussian"),
           data = chicken_prod_life,
           nitt = 10000000,
           thin= 400,
           verbose = TRUE,
           prior = prior.1)
}, mc.cores = 1)

m3a <- lapply(m3, function(m) m$Sol)
m3a <- do.call(mcmc.list, m3a)

library(coda)
par(mfrow=c(5,2), mar=c(2, 2, 1, 2))
gelman.plot(m3a, auto.layout = F)

par(mfrow=c(5,2), mar=c(2, 1, 1, 1))
plot(m3a, ask=F, auto.layout=F)

gelman.diag(m3a)
summary(m3a)

m3b <- lapply(m3, function(m) m$VCV)
m3b <- do.call(mcmc.list, m3b)

par(mfrow=c(5, 2), mar=c(2, 2, 1, 2))
gelman.plot(m3b, auto.layout=F)

par(mfrow=c(5,2), mar=c(2,1,1,1))
plot(m3b, ask=F, auto.layout=F)

summary(m3b)