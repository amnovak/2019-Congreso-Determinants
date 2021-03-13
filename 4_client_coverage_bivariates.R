# This script creates client coverage layers with categories for bivariate choropleth
# Some ACS data is pulled from the files that are output by 3_needs_assment.R, and some is
# obtained from the Census API with the tidycensus package.
# Produces four datasets: cp, cp under 18, cp/housing, cp/health insurance


# Load libraries
library("tidyverse")
library("stringr")
library("sf")
library("data.table")
library('tidycensus')

census_api_key("YOUR KEY HERE")  # register a census api key at: https://api.census.gov/data/key_signup.html

# Load libraries of available Subject Table and Detailed Table variables from the ACS 2017 5-Year estimates
v17_subject <- load_variables(2017, "acs5/subject", cache = TRUE)  # subject tables
v17 <- load_variables(2017, "acs5", cache = TRUE)  # detailed tables

# View the libraries and search for the variables you want:
View(v17_subject)
View(v17)

# Load Block Group aggregated client data
# Transform to EPSG 2272 and add GEOID column
clients.bg <- st_read("data/interim/client_mapping/clientsFY1719_bgs.shp") %>% 
  mutate(GEOID = FIPS) %>% st_transform(2272)

# Load Total Pop and Total Hispanic Pop from ACS Detailed Table, block group level
# Reformat & add column for pct_hispanic
pct_hisp <- get_acs(geography = "block group",
                     variables = c(total_pop = "B03002_001", total_hisp = "B03002_012"),
                     state = "PA",
                     county = "Philadelphia") %>%
  dplyr::select("GEOID", "variable", "estimate") %>%
  tidyr::spread(variable, estimate) %>%
  mutate(pct_hisp = (total_hisp/total_pop)*100)


# Filter to block groups with nonzero population
# pct_hisp <- pct_hisp %>% filter(total_pop > 0)

# Join pct_hisp data to block group aggregated client data with boundaries
cp <- left_join(clients.bg, pct_hisp, by = "GEOID")

# Filter to block groups with nonzero population 
cp <- cp %>% filter(total_pop > 0)

# Replace NA TOTAL_C data with 0 to avoid creating infinite values when dividing num clients/total pop
cp <- cp %>% mutate(TOTAL_C = ifelse(is.na(TOTAL_C), 0, TOTAL_C))

# Add column for client penetration
cp <- cp %>% mutate(cp = (TOTAL_C/total_pop) * 100)

# check that this makes sense:
plot(cp["cp"])  

# select columns we need
cp <- cp %>% select("GEOID", "TOTAL_C", "under18", "total_pop", "pct_hisp", "cp", "geometry")


#### Create classes for bivariate color scheme

# Assign classes for Client Penetration (var_1)
cp <- cp %>% mutate(var1_class = ifelse(cp > 15, "C",
                                        ifelse(cp > 5, "B", "A"))) 

# Assign classes for pct_hispanic (var_2)
cp <- cp %>% mutate(var2_class = ifelse(pct_hisp > 66, 3,
                                 ifelse(pct_hisp > 33, 2, 1)))


# check:
plot(cp["var1_class"])
plot(cp["var2_class"])


# concatenate var1_class and var2_class into a bivariate code:
cp <- cp %>% mutate(bi_class = paste(var1_class, var2_class, sep = ''))

# check:
plot(cp["bi_class"])


# write to interim file
# st_write(cp, "data/final/FY1719_cp.shp")


# Client Penetration, under 18 -----------------------------------------------------------------

# ratio of clients under 18/total pop under 18 (var1) BY pct hispanic or latino (var2)

# Load ACS age data downloaded from SocialExplorer
age <-fread('data/raw/ACS/age/R12220720_SL150.csv', header=TRUE, sep=",", colClasses='numeric') %>% 
  select("Geo_FIPS", "SE_A01001_001", "SE_A01001_002","SE_A01001_003","SE_A01001_004", "SE_A01001_005")

# rename columns
colnames(age) <- c("GEOID", "TotalPop", "Under5", "5to9", "10to14", "15to17")

# calculate total of under 18 age groups and select just the total column
age <- age %>% mutate(Pop_under18 = Under5 + `5to9` + `10to14` + `15to17`) %>% 
  select("GEOID", "TotalPop", "Pop_under18")

# convert GEOID to character for join
age$GEOID <- as.character(age$GEOID)


# Join ACS age data to block group aggregated client data with boundaries
cp18 <- left_join(clients.bg, age, by = "GEOID")

# load the cp file we just created, btu select just the var2_class (pct_hisp)
cp <- st_read("data/final/FY1719_cp.shp") %>% select("GEOID", "var2_class")

# drop geometry to allow for tabular left join
cp$geometry <- NULL


# left join the cp data with var2 classes to cp18 data
cp18 <- left_join(cp18, cp, by = "GEOID")


# filter out block groups with no population under 18 or no Congreso clients
cp18 <- cp18 %>% filter(Pop_under18 > 0) %>% filter(!is.na(TOTAL_C))


# add column for under 18 client penetration
cp18 <- cp18 %>%  mutate(cp_u18 = (under18/Pop_under18)*100)

# round 
cp18$cp_u18 <- round(cp18$cp_u18, digits = 2)


# decide on break values for Under18 Client Penetration (var_1)
cp18 <- cp18 %>% mutate(var1_class = ifelse(cp_u18 > 15, "C",
                                        ifelse(cp_u18 > 5, "B", "A"))) 


# concatenate var1_class and var2_class into a bivariate code:
cp18 <- cp18 %>% mutate(bi_class = paste(var1_class, var2_class, sep = ''))

# check results
plot(cp18["cp_u18"], breaks = "jenks")
plot(cp18["var1_class"])
plot(cp18["bi_class"])

# export shapefile
# st_write(cp18, "data/final/FY1719_cp_under18.shp")


# cp & housing -----------------------------------------------------------------

# Client Penetration for HOUSING

# load the cp file we created & keep cp, pct_hisp, and var1_class (client penetration)
cp <- st_read("data/final/FY1719_cp.shp") %>%
  select("GEOID", "cp", "pct_hisp", "var1_class")

# load ACS individual indicators dataset, select relevant fields, add field for GEOID
housing <- st_read("data/interim/needs_assessment/housing_burden_bgs.shp") %>% 
  select("FIPS", "Ttl50pc") %>% mutate(GEOID = FIPS)

# drop geometry for tabular join
housing$geometry <- NULL

# join housing burden data to client penetration data
cp_ho <- left_join(cp, housing, by = "GEOID")


# add column for var2_class: percentage of people spending > 50% of income on housing
# breaks are 20 & 40 pct
cp_ho <- cp_ho %>% mutate(var2_class = ifelse(Ttl50pc > .40, "3",
                                            ifelse(Ttl50pc > .20, "2", "1"))) 

# concatenate classes to create bivariate class
cp_ho <- cp_ho %>% mutate(bi_class = paste(var1_class, var2_class, sep = ''))

# filter to block groups with a valid housing cost burden % value
cp_ho <- cp_ho %>% filter(!is.na(Ttl50pc)) 

plot(cp_ho['bi_class'])

# # export to shapefile
# st_write(cp_ho, "data/final/FY1719_cp_housing.shp")


# cp & health ------------------------------------------------------------------

# Note: this is the only client penetration layer at the Census TRACT level

# load client data aggregated to Tracts, add GEOID column and set CRS to 2272
tracts.clients <- st_read("data/interim/client_mapping/clientsFY1719_tracts.shp") %>% 
  mutate(GEOID = FIPS) %>% st_transform(2272)

# Load Total Pop and Total Hispanic Pop from ACS detailed table, Tract level
# Reformat, add column for pct_hispanic
pct_hisp <- get_acs(geography = "tract",
                    variables = c(total_pop = "B03002_001", total_hisp = "B03002_012"),
                    state = "PA",
                    county = "Philadelphia") %>%
  dplyr::select("GEOID", "variable", "estimate") %>%
  tidyr::spread(variable, estimate) %>%
  mutate(pct_hisp = (total_hisp/total_pop)*100)


# Filter to tracts with nonzero population
pct_hisp <- pct_hisp %>% filter(total_pop > 0)

# replace NA values in tracts.clients data with 0
tracts.clients <- tracts.clients %>% mutate(TOTAL_C = ifelse(is.na(TOTAL_C), 0, TOTAL_C))

# Left join pct_hisp data to tract-aggregated client data with boundaries
cp_he <- left_join(tracts.clients, pct_hisp, by = "GEOID")

# calculate ratio of clients to total population
cp_he <- cp_he %>% mutate(cp_he = (TOTAL_C/total_pop) * 100)


# load health insurance coverage data created in o5_needs_assessment.R
tracts.hi <- st_read("data/interim/needs_assessment/health_insurance_tracts.shp")

# drop geometry of health insurance data for tabular join
tracts.hi$geometry <- NULL

# join health insurance tracts (376) to client data tracts (384)
# filter to only tracts with health insurance data
cp_he <- left_join(cp_he, tracts.hi, by = "FIPS") %>% filter(!is.na(Pct_Unn))

# filter out one outlier
cp_he <- cp_he %>% filter(Pct_Unn < .4)

# check that these make sense
plot(cp_he["Pct_Unn"])
plot(cp_he["pct_hisp"])
plot(cp_he["cp_he"])


### Create bivariate classes

# Assign classes for pct Congreso clients (var_1) breaks are 5% and 15%
cp_he <- cp_he %>% mutate(var1_class = ifelse(cp_he > 15, "C",
                                              ifelse(cp_he > 5, "B", "A"))) 

# Assign classes for pct Uninsured (var_2). Breaks are 10% and 20% (Philadelphia avg.)
cp_he <- cp_he %>% mutate(var2_class = ifelse(Pct_Unn > .20, 3,
                                        ifelse(Pct_Unn > .10, 2, 1)))


# Concatenate var1_class and var2_class into bivariate code
cp_he <- cp_he %>% mutate(bi_class = paste(var1_class, var2_class, sep = ''))

# check:
plot(cp_he["var1_class"])
plot(cp_he["var2_class"])
plot(cp_he["bi_class"])


# # export to shapefile
# st_write(cp_he, "data/final/FY1719_cp_health.shp")





