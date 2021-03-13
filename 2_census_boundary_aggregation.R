# This script aggregates Congreso client and PAACS data to Block Group and Tract Boundaries
# Anna Novak, June 2019


library("tidyverse")
library("sf")

# Load client points and transform to epsg 2272 (Pennsylvania State Plane)
clientsFY1719 <- st_read("data/interim/client_mapping/clientsFY1719.shp") %>% st_transform(2272)

# Load 2010 Philadelphia Block Groups. Set CRS and add column for "FIPS"
bgs <- st_read("data/raw/census_boundaries/Census_Block_Groups_2010.geojson") %>% 
  mutate(FIPS = GEOID10) %>% st_transform(2272)

# Load 2010 Philadelphia Census Tracts. 
tracts <- st_read("data/raw/census_boundaries/Census_Tracts_2010.geojson") %>% 
  mutate(FIPS = GEOID10) %>% st_transform(2272)

# Congreso aggregation---------------------------------------------

# Subset Congreso clients data for aggregation to the block group level
client.points <- clientsFY1719 %>% filter(PAACS == 0)

# Add field to client data for total count summary
client.points$total <- 1

# Left join client points to block groups
bgs.clients <- st_join(bgs, client.points)

# # Tests:
# nrow(bgs.clients %>% filter(EWS == 1))  # 3,617 non-PAACS EWS clients in Philly, fY1719
# sum(is.na(bgs.clients$EWS))  # 361 NA values = 361 Block groups w/ no EWS clients

# group by block group FIPS and find totals of each category per block group
bgs.clients.summary <- bgs.clients %>% group_by(FIPS) %>% 
  summarize(TOTAL_CLIENTS = sum(total), 
            under18 = sum(u18FY18),
            EWS = sum(EWS),
            FHS = sum(FHS),
            HPW = sum(HPW))

# export block group level data to shapefile
# st_write(bgs.clients.summary, "data/interim/client_mapping/clientsFY1719_bgs.shp")


# left join client points to census tracts
tracts.clients <- st_join(tracts, client.points)

# group by tract FIPS and find totals of each category by tract
tracts.clients.summary <- tracts.clients %>% group_by(FIPS) %>% 
  summarize(TOTAL_CLIENTS = sum(total), 
            under18 = sum(u18FY18),
            EWS = sum(EWS),
            FHS = sum(FHS),
            HPW = sum(HPW))

# export tract level data to shapefile
#st_write(tracts.clients.summary, "data/interim/client_mapping/clientsFY1719_tracts.shp")


# PAACS aggregation ----------------------------------------------

# Subset for PAACS students
paacs.points <- clientsFY1719 %>% filter(PAACS == 1)

# add field for total count summary
paacs.points$total <- 1

# left join paacs points to block groups
bgs.paacs <- st_join(bgs, paacs.points)

# group by block group FIPS and find totals of each category per block group
bgs.paacs.summary <- bgs.paacs %>% group_by(FIPS) %>%
  summarize(TOTAL_PAACS = sum(total),  # count total PAACS students
            under18 = sum(u18FY18),  # count total PAACS students under age 18 (should be 100%)
            PAACS_O = sum(PAACS_O),  # count PAACS only
            PAACS_C = sum(PAACS_C)) # count PAACS + Congreso client

# Filter to block groups with at least 1 PAACS student
bgs.paacs.summary <- bgs.paacs.summary %>% filter(TOTAL_PAACS > 0)

# export to shapefile
# st_write(bgs.paacs.summary, "data/interim/client_mapping/paacsFY1719_bgs.shp")



