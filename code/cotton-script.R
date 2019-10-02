########################################################################################
# Summary: Tidying and analyzing cotton production trends in NC
# Date: September 30, 2019
# Maddie Keefer
########################################################################################

# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)

# 2. Read & inspect the dataset ----
# Read in your data ----
cotton <- read_csv("data/cotton-usda-nass.csv")
# Check the packaging ----
str(cotton)
colnames(cotton)
# Look at the top and bottom of your data ----
head(cotton)
tail(cotton)
# Check your “n”s ----
dim(cotton)
nrow(cotton)
ncol(cotton)
# Validate with at least one external data source ----
summary(cotton)

# 3.1. Create a NC data subset ----
nc_cotton <- cotton %>%
  dplyr::select(year, state, ag_district, county, data_item, value) %>%
  filter(state == "NORTH CAROLINA") 

# 3.2. Divide the data_item column ----
nc_cotton_sep <- nc_cotton %>% 
  separate(data_item, into = c("cotton_type", "measurement"), sep = " - ")

# 3.3. Convert the value column to numeric type ----
nc_cotton_filt <- nc_cotton_sep %>% 
  filter(value != "(D)")
str(nc_cotton_filt)
nc_cotton_filt$value <- as.numeric(nc_cotton_filt$value)
str(nc_cotton_filt)
nc_cotton_filt

# 4. Visualizing trends ----
library(ggplot2)
ggplot(data = nc_cotton_filt) + 
  geom_point(mapping = aes(x = year, y = value),
             size = 1) + 
  labs(x = "Year", y = "", title = "Cotton production in NC", caption = "Source: USDA NASS") +
  theme_minimal() + 
  theme(axis.text.x
        = element_text(angle = 90)) +
  facet_grid(rows = vars(measurement), cols = vars(ag_district), 
             scales = "free", labeller = label_value) 
  
#We can see that yields have improved over time thanks to advances in agricultural technology

# 5. Summarize data from 2018 ----
#What were the top 3 cotton producing counties in NC in terms of total lbs of cotton for 2018? 
nc_cotton_lbs <- nc_cotton_filt %>%
  filter(year == "2018") %>% 
  spread(measurement, value) %>% 
  mutate(total_lbs = (`ACRES HARVESTED` * `YIELD, MEASURED IN LB / ACRE`))

top_3_harvest <- nc_cotton_lbs %>%
  select(county, total_lbs)
  top_n(top_3_harvest, 3, total_lbs)
#top 3 cotton producing counties in NC
# 1 HALIFAX      40860800
# 2 MARTIN       25090800
# 3 NORTHAMPTON  36304400