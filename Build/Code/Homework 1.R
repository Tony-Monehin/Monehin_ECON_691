


# ECON 691 - Homework 1

# Clear Environment
rm(list = ls())

# Load Packages
library(tidyverse)

# Read in Data
e2020 <- read.csv(
  file = "./Build/Input/ge2020cty.csv",
  header = TRUE,
  as.is = TRUE
)

e2024 <- read.csv(
  file = "./Build/Input/ge2024cty.csv",
  header = TRUE,
  as.is = TRUE
)

# Examine Data
head(e2020)
head(e2024)
names(e2020)
names(e2024)

# PART 2

# Wrangle 2020 Data
e2020.2 <- e2020 %>%
  select(OfficeName, County, Votes, PartyName, BallotGroup) %>%
  group_by(County, OfficeName) %>%
  mutate(Total.Votes = sum(Votes)) %>%
  ungroup() %>%
  mutate(Share = Votes / Total.Votes,
         County = toupper(County)) %>%
  filter(BallotGroup == "A",
         OfficeName == "PRESIDENT AND VICE PRESIDENT",
         PartyName %in% c("DEMOCRATIC", "REPUBLICAN"),
         !is.na(Share)) %>%
  select(County, PartyName, Votes, Total.Votes, Share)

# Wrangle 2024 Data
e2024.2 <- e2024 %>%
  select(OfficeName, County, Votes, PartyName, BallotGroup) %>%
  group_by(County, OfficeName) %>%
  mutate(Total.Votes = sum(Votes)) %>%
  ungroup() %>%
  mutate(Share = Votes / Total.Votes,
         County = toupper(County)) %>%
  filter(BallotGroup == "A",
         OfficeName == "PRESIDENT AND VICE PRESIDENT",
         PartyName %in% c("DEMOCRATIC", "REPUBLICAN"),
         !is.na(Share)) %>%
  select(County, PartyName, Votes, Total.Votes, Share)

# Pivot 2020 Data
e2020.3 <- e2020.2 %>%
  pivot_wider(
    id_cols = c("County", "Total.Votes"),
    names_from = "PartyName",
    values_from = "Share",
    names_prefix = "Y20"
  )

# Pivot 2024 Data
e2024.3 <- e2024.2 %>%
  pivot_wider(
    id_cols = c("County", "Total.Votes"),
    names_from = "PartyName",
    values_from = "Share",
    names_prefix = "Y24"
  )

# Join 2020 and 2024 Data
e.data <- inner_join(
  e2020.3,
  e2024.3,
  by = "County",
  suffix = c(".2020", ".2024")
)

# Examine Final Data
head(e.data)
dim(e.data)
View(e.data)

# PART 3

# Save Joined Data
save(
  e.data,
  file = "./Build/Output/20-24 Election.RData"
)
