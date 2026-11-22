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
  
 
  #pipped syntax %>%
  
  #Wrangle Data
    e.data.2 <- e.data %>%
      select(OfficeName, County, Votes, PartyName, BallotGroup) %>%
      group_by(County, OfficeName) %>% #to calculate the number of votes in County and OfficeName
        mutate(Total.Votes = sum(Votes)) %>% # = to create a new vector and not <-
      ungroup() %>% #Once you group, always end by ungroup and indent to get back to the main
  
    mutate(Share = Votes / Total.Votes,
           County = toupper(County)) %>%
    filter(BallotGroup == "A",
      !is.na(Share))
      
    library(tidycensus)
    census_api_key("3abc2766913825ba5eb66c4a9b9ad5efe5b349cd", overwrite = FALSE, install = TRUE)
    readRenviron("~/.Renviron")
    library(tidycensus)
    
    population <- get_acs(
      geography = "state",
      variables = "B01003_001",
      state = "IL",
      year = 2023
    )
    
    View(population)
    
    readRenviron("~/.Renviron")
    library(tidycensus)
    
    acs <- load_variables(year = 2024, dataset = "acs5")
    acs <- load_variables(
      2024,
      "acs5",
      cache = TRUE
    )    
    
    
    library(tidycensus)
    
    census_api_key(
      "3abc2766913825ba5eb66c4a9b9ad5efe5b349cd",
      overwrite = TRUE,
      install = TRUE
    )
    
    readRenviron("~/.Renviron")
    
    nzchar(Sys.getenv("CENSUS_API_KEY"))
    acs <- load_variables(
      year = 2024,
      dataset = "acs5"
    )
    
    View(acs)   
    
    library(tidycensus)
    
    census_api_key(
      "3abc2766913825ba5eb66c4a9b9ad5efe5b349cd",
      install = TRUE,
      overwrite = TRUE
    )
    
    #Define list of variable
    temp <- paste("B01001", str_pad(7:25, width = 3, side = "left", pad = "0"), sep = "--")
    temp2 <- paste("B01001", str_pad(31:49, width = 3, side = "left", pad = "0"), sep = "--")
    vars <- c(temp, temp2)
    
    #call the US census API
    cen.dat <- get_acs(georgraphy = "county", variables = vars, state = "17", year = 2024, geometry = TRUE)
    
    
    library(tidycensus)
    library(stringr)
    library(dplyr)
    
    # Male age groups
    temp <- paste("B01001",
                  str_pad(7:25, width = 3, side = "left", pad = "0"),
                  sep = "_")
    
    # Female age groups
    temp2 <- paste("B01001",
                   str_pad(31:49, width = 3, side = "left", pad = "0"),
                   sep = "_")
    
    # Combine variables
    vars <- c(temp, temp2)
    
    # Download Illinois county data
    cen.dat <- get_acs(
      geography = "county",
      variables = vars,
      state = "17",
      year = 2024,
      geometry = TRUE
    )
    
    View(cen.dat)
    
    library(tidycensus)
    
    nzchar(Sys.getenv("CENSUS_API_KEY"))
    
    
    census_api_key(
      "83617e27851aec5c82bc990a1ab744000da05d5d",
      install = TRUE,
      overwrite = TRUE
    )
    
    readRenviron("~/.Renviron")
    
    library(tidycensus)
    library(stringr)
    library(dplyr)
    
    cen.dat <- get_acs(
      geography = "county",
      variables = "B01001_001",
      state = "17",
      year = 2024,
      geometry = FALSE
    )
    
    View(cen.dat)
    
    
    library(tidycensus)
    
    census_api_key(
      "83617e27851aec5c82bc990a1ab744000da05d5d",
      install = TRUE,
      overwrite = TRUE
    )
    
    library(tidycensus)
    
    readRenviron("~/.Renviron")
    
    cen.test <- get_acs(
      geography = "state",
      variables = "B01001_001",
      state = "IL",
      year = 2024
    )
    
    View(cen.test)
    
    cen.dat <- get_acs(
      geography = "county",
      variables = vars,
      state = "17",
      year = 2024,
      geometry = TRUE
    )
    
    View(cen.dat)
    
    head(cen.dat)
    
    
    #Process the  census Data
    cen.dat.2 <- cen.dat %>%
      summarise(pop = sum(estimate), .by = c(NAME, geometry))  %>%
      mutate(temp = str_split_i(NAME, ",", 1),
             temp = gsub(" County", "", temp),
             County = toupper(temp),
             County = case_when(County == "DE WITT" ~ "DEWITT",
                                County == "JO DAVIESS" ~ "JODAVIESS",
                                TRUE ~ County))
    
    #dataframe for men in each county
    fast.male <- cen.dat %>%
      st_drop_geometry() %>%
      filter(variable %in% temp) %>%
      summarise(male = sum(estimate), .by = c(NAME))
    
    
    head(fast.male)
    
    
    #merge the data 
     core <- cen.dat.2 %>%
       left_join(., e.data.2, by = "County") %>%
       mutate(vote.rate = Total.Votes / pop)
    # Merge the data this is perfect
     core <- cen.dat.2 %>%
       left_join(e.data.2, by = c("county" = "County")) %>%
       mutate(vote.rate = Total.Votes / pop)
     
     View(core)
     head(core)
     
     #Visualization Quickly
     core.2 <- core %>%
       select(County, vote.rate) %>%
       distinct()
     
     ggplot(core.2) +
       geom_sf(aes(fill = vote.rate)) +
       theme_minimal()
     