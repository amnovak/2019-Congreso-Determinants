# This script creates PAACS "client penetration" layers with categories for bivariate choropleth

library(tidyverse)
library(data.table)
library(sf)

# Load PAACS data aggregated to Block Groups
paacs <- st_read("data/interim/client_mapping/paacsFY1719_bgs.shp")

# map all PAACS students
plot(paacs["TOTAL_P"])

# map PAACS students not receiving a Congreso service
plot(paacs["PAACS_O"])

# Add field for percentage of PAACS students NOT receiving a Congreso service
paacs <- paacs %>% mutate(pct_only_PAACS = PAACS_O/TOTAL_P)

plot(paacs["pct_only_PAACS"])

# select just columns we want
paacs <- paacs %>% select("FIPS", "TOTAL_P", "PAACS_O", "pct_only_PAACS")

# PAACS is grades K-8 
# ratio of PAACS students/total pop under 15

# Load ACS age data. Downloaded from SocialExplorer
age <-fread('data/raw/ACS/age/R12220720_SL150.csv', header=TRUE, sep=",", colClasses='numeric') %>% 
  select("Geo_FIPS", "SE_A01001_001", "SE_A01001_002","SE_A01001_003","SE_A01001_004")

# rename columns
colnames(age) <- c("FIPS", "TotalPop", "Under5", "5to9", "10to14")

# calculate total of under 18 age groups and select just the total column
age <- age %>% mutate(Pop_under15 = Under5 + `5to9` + `10to14`) %>% 
  select("FIPS", "TotalPop", "Pop_under15")

# convert GEOID to character for join
age$FIPS <- as.character(age$FIPS)

# Join ACS age data to block group aggregated PAACS data
# Calculate percent of population under 15 that are PAACS students
paacs_under15 <- left_join(paacs, age, by = "FIPS") %>% mutate(paacs_cp = (TOTAL_P/Pop_under15)*100)

plot(paacs_under15["paacs_cp"])

# export
st_write(paacs_under15, "data/final/cp_paacs.shp")

