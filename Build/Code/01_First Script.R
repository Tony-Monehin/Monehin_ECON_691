#Script for IN-class exercis in Economics 691

#Tony Monehin
#November 11, 2026

rm(list=ls())
library(tidyverse)


#Read in Data
  e.data <- read.csv(file = "./Build/Input/ge2024cty.csv", header == TRUE,
                     as.is = TRUE)
  # Read in Data
  e.data <- read.csv(
    file = "./Build/Input/ge2024cty.csv",
    header = TRUE,
    as.is = TRUE
  )
  
  view(e.data)
  head(e.data)
  Tail(e.data)
  e.data[657,4]
  e.data[,4]
  e.data$BallotGroup[657]
  head(e.data$OfficeName)  
  e.data[1:4.]  
  summary(e.data)
  
  e.data <- read.csv(
    file = "./Build/Input/ge2024cty.csv",
    header = TRUE,
    as.is = TRUE,
    fileEncoding = "Windows-1252"
  )  
  summary(e.data)
  