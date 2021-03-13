# Script cleans and joins data from ACS 2017 5-year estimates and 500Cities to block group and tract boundaries
# Anna Novak, June 2019

# Load libraries
library("tidyverse")
library("stringr")
library("sf")
library("data.table")


# Load data ---------------------------------------------------------------

# Load 2010 Philadelphia Block Groups & rename GEOID10 to "FIPS"
bgs <- st_read("data/raw/census_boundaries/Census_Block_Groups_2010.geojson") %>% 
  mutate(FIPS = GEOID10) %>% transform(2272)


# Load 2010 Philadelphia Census Tracts & rename GEOID10
tracts <- st_read("data/raw/census_boundaries/Census_Tracts_2010.geojson") %>% 
  mutate(FIPS = GEOID10) %>% transform(2272)

tracts <- tracts.data 


# ACS data downloaded from SocialExplorer

hl_data<-fread('data/raw/ACS/latino_by_race/R12220737_SL150.csv', 
               header=TRUE, sep=",", colClasses='numeric')

age_data<-fread('data/raw/ACS/age/R12220720_SL150.csv',
                header=TRUE, sep=",", colClasses='numeric')

unemployment_data<-fread('data/raw/ACS/unemployment_rates/R12217232_SL150.csv', 
                         header=TRUE, sep=",", colClasses='numeric')

dropout_data<-fread('data/raw/ACS/hs_dropouts/R12217249_SL150.csv', 
                    header=TRUE, sep=",", colClasses='numeric')

tenure_data <-fread('data/raw/ACS/tenure/R12220663_SL150.csv',
                    header=TRUE, sep=",", colClasses='numeric')

renter_expenses<-fread('data/raw/ACS/renter_expenses/R12220676_SL150.csv',
                       header=TRUE, sep=",", colClasses='numeric')

home_expenses<-fread('data/raw/ACS/homeowner_expenses/R12220673_SL150.csv',
                     header=TRUE, sep=",", colClasses='numeric')

health_insurance<-fread('data/raw/ACS/health_insurance/R12220706_SL140.csv',
                        header=TRUE, sep=",", colClasses='numeric')

hh_w_minor <- fread('data/raw/ACS/hh_minors/R12221014_SL150.csv',
                    header=TRUE, sep=",", colClasses='numeric')


# 500 Cities

asthma_data <- fread('data/raw/CDC_500Cities/asthma.csv', header=TRUE, sep=",", colClasses='numeric')

diabetes_data <- fread('data/raw/CDC_500Cities/diabetes.csv', header=TRUE, sep=",", colClasses='numeric')

bp_data <- fread('data/raw/CDC_500Cities/bloodpressure.csv', header=TRUE, sep=",", colClasses='numeric')


# Individual indicators --------------------------------------------------------------

# Unemployment rate
ue <- unemployment_data 

ue <- ue %>% select("Geo_FIPS","SE_A17005_001","SE_A17005_002", "SE_A17005_003")
colnames(ue) <- c("FIPS", "TotalPop", "Employed", "Unemployed")
ue$FIPS <- as.character(ue$FIPS)
ue <- ue %>% mutate(PctUnemployed = Unemployed/TotalPop) 
ue$PctUnemployed = round(ue$PctUnemployed, digits = 2)


# join to block groups and remove bgs with 0 total pop
bgs.ue <- left_join(bgs, ue, by = "FIPS") %>% filter(!is.na(PctUnemployed))

# st_write(bgs.ue, "data/interim/needs_assessment/unemployment_rates_bgs.shp")


# % of ages 16-19 not in school
hsdo<- dropout_data
hsdo<- hsdo %>% select("Geo_FIPS","SE_A12003_001", "SE_A12003_002", "SE_A12003_003")
colnames(hsdo) <- c("FIPS", "Total1619", "DO", "Enrolled")
hsdo$FIPS <- as.character(hsdo$FIPS)
hsdo <- hsdo %>% mutate(DO_rate = DO/Total1619)
hsdo$DO_rate = round(hsdo$DO_rate, digits = 2)

# Join to philly bgs and filter out 0 total pop
bgs.do <- left_join(bgs, hsdo, by = "FIPS") %>% filter(Total1619 > 0)

#st_write(bgs.do, "data/interim/needs_assessment/dropout_rates_bgs.shp")



# % of renter&owner households spending >50% of income on housing costs

re <- renter_expenses
he <- home_expenses

re <- re %>% select("Geo_FIPS","SE_B18002_001", "SE_B18002_002", "SE_B18002_003")
colnames(re) <- c("FIPS", "TotalRenters", "Rent30pct", "Rent50pct")
re$FIPS <- as.character(re$FIPS)

he <- he %>% select("Geo_FIPS","SE_B10040_001", "SE_B10040_002", "SE_B10040_003")
colnames(he) <- c("FIPS", "TotalHomeowners", "Home30pct", "Home50pct")
he$FIPS <- as.character(he$FIPS)

re <- left_join(re, he, by = "FIPS")
re <- re %>% mutate(TotalHU_b = TotalRenters + TotalHomeowners,
                    Sum50pct = Rent50pct + Home50pct,
                    Total50pct = Sum50pct/TotalHU_b)  # ratio of total households spending >50% of income on rent or housign costs

# round ratio
re$Total50pct = round(re$Total50pct, digits = 2)

# join to bgs and filter to bgs with nonzero total homeowners or renters
bgs.hb <- left_join(bgs, re, by ="FIPS") %>% filter(TotalHU_b > 0)

# export shapefile
# st_write(bgs.hb, "data/interim/needs_assessment/housing_burden_bgs.shp")

 

# % of housing units owner occupied
tenure <- tenure_data

tenure <- tenure %>% select("Geo_FIPS", "SE_A10060_001", "SE_A10060_002", "SE_A10060_003")
colnames(tenure) <- c("FIPS", "TotalHU", "Owner", "Renter")
tenure$FIPS <- as.character(tenure$FIPS)
tenure <- tenure %>% mutate(PctOwnerOccupied = Owner/TotalHU) 
tenure$PctOwnerOccupied = round(tenure$PctOwnerOccupied, digits = 2)

# join to philly bgs and filter to nonzero total housing units
bgs.oo <- left_join(bgs, tenure, by = "FIPS") %>% filter(TotalHU > 0)

#st_write(bgs.oo, "data/interim/needs_assessment/pct_owner_occupied_bgs.shp")



## Health Insurance Coverage (TRACTS)

hi<- health_insurance
hi <- hi %>% select("Geo_FIPS", "SE_A20001_001","SE_A20001_002","SE_A20001_003","SE_A20001_004","SE_A20001_005")
colnames(hi) <- c("FIPS", "TotalPop", "Uninsured", "Insured", "PublicHC", "PrivateHI")
hi$FIPS <- as.character(hi$FIPS)
hi <- hi %>% mutate(Pct_Insured = Insured/TotalPop)
hi <- hi %>% mutate(Pct_Uninsured = Uninsured/TotalPop)
hi$Pct_Uninsured = round(hi$Pct_Uninsured, digits = 2)

# join to tract boudaries and filter to nonzero total population
tracts.hi <- left_join(tracts, hi, by = "FIPS") %>% filter(TotalPop >0)

#export shapefile
#st_write(tracts.hi, "data/interim/needs_assessment/health_insurance_tracts.shp")




# Combining scores ----------------------------------------------------------------

# EWS need - unemployment & dropout rates

#drop geometry of unemployment rates for tabular join
bgs.ue$geometry <- NULL
bgs.ue <- bgs.ue %>% select("FIPS","PctUnemployed")

# Join ue rates to bgs with do rates
# rescale both measures from 0-1, combine scores, and calculate percentile rank
bgs.ews <- left_join(bgs.do, bgs.ue, by = "FIPS") %>% mutate(ue_score = (PctUnemployed - min(PctUnemployed)) / 
                                                               (max(PctUnemployed) - min(PctUnemployed)),
                                                             do_score = (DO_rate - min(DO_rate)) /
                                                               (max(DO_rate) - min(DO_rate))) %>% 
                                                      mutate(EWSneed = ue_score + do_score) %>%
                                                      mutate(EWSpctl= percent_rank(EWSneed))

#export to shapefile
#st_write(bgs.ews, "data/final/EWS_need_scores.shp")



# FHS need - housing burden & families with minors

# prepare % households with a minor data
hwm <- hh_w_minor
hwm <- hwm %>% select("Geo_FIPS", "SE_A10009_001","SE_A10009_002")
colnames(hwm) <- c("FIPS", "TotalHouseholds", "HWMinor")
hwm$FIPS <- as.character(hwm$FIPS)

# calculate % households with a minor and filter to nonzero total hh 
hwm <- hwm %>% mutate(PctHWM = HWMinor/TotalHouseholds) %>% filter(TotalHouseholds > 0)

# join to housing burden by block groups data
# rescale measures from 0-1 and combine scores
bgs.fhs <- left_join(bgs.hb, hwm, by = "FIPS") %>% mutate(hb_score = (Total50pct - min(Total50pct))/
                                                            (max(Total50pct) - min(Total50pct)),
                                                          hwm_score = (PctHWM - min(PctHWM))/
                                                            (max(PctHWM) - min(PctHWM))) %>%
                                                    mutate(FHSneed = hb_score + hwm_score) %>%
                                                    mutate(FHSpctl = percent_rank(FHSneed))

# export to shapefile
#st_write(bgs.fhs, "data/final/FHS_need_scores.shp")



# HPW need - asthma rates, high BP rates & pct uninsured

# prepare % asthma data 
asthma <- asthma_data
asthma <- asthma %>% select("TractFIPS", "Data_Value")
colnames(asthma) <- c("FIPS", "PctAsthma")
asthma$FIPS <- as.character(asthma$FIPS)


# prepare % high BP data
bp <- bp_data
bp <- bp %>% select("TractFIPS", "Data_Value")
colnames(bp) <- c("FIPS", "HBP")
bp$FIPS <- as.character(bp$FIPS)


# tabular left join asthma and high BP data
cdc.data <- left_join(asthma, bp, by = "FIPS")


# tabular left join health outcome data to bgs w/ health insurance
# rescale % uninsured, %asthma, %high bp from 0-1 and combine scores
tracts.hpw <- left_join(tracts.hi, cdc.data) %>% mutate(ui_score = (Pct_Uninsured - min(Pct_Uninsured))/
                                                    (max(Pct_Uninsured) - min(Pct_Uninsured)),
                                                  asthma_score = (PctAsthma -  min(PctAsthma))/
                                                    (max(PctAsthma) - min(PctAsthma)),
                                                  bp_score = (HBP - min(HBP))/
                                                    (max(HBP) - min(HBP))) %>%
                                                  mutate(HPWneed = ui_score + asthma_score + bp_score) %>%
                                                  mutate(HPWpctl = percent_rank(HPWneed))

# export to shapefile
#st_write(tracts.hpw, "data/final/HPW_need_scores.shp")


