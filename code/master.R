library(tidyverse)
library(epiextractr)
library(realtalk)
library(MetricsWeighted)

# Load org data 
org_data <- load_org(1979:2024, year, orgwgt, wage, selfemp, selfinc, age)

# Base Year CPI 
cpi_base <- c_cpi_u_extended_annual %>%
  filter(year == 2024) %>%
  pull(c_cpi_u_extended) 

# Create annual real wages and output to csv
real_avg_annual_wages <- org_data %>% 
  filter(age>16, selfemp==0, selfinc==0, !is.na(wage)) %>% # standard restrictions
  left_join(c_cpi_u_extended_annual, by = "year") %>% # join cpi data 
  mutate(realwage = wage * (cpi_base/c_cpi_u_extended)) %>% # inflation adjust all wages
  group_by(year) %>% 
  summarise(
    real_avg_wages = weighted_mean(realwage, orgwgt, na.rm = TRUE)
    )
  
  write_csv(real_avg_annual_wages, "./output/real_avg_wages.csv") # write to csv 


  

 