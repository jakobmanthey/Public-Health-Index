# PHI 
# Data preprocessing
# Carolin Kilian
# 10-11-2025

library(tidyverse)
library(openxlsx)

## Load data

rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Research projects/Public Health Index/Github/Public-Health-Index/")
DATE <- 20260225

data <- read.xlsx("data/phi_raw_data.xlsx", na.strings = ".") 

# define rounding function

round_xlsx <- function(x, n) {
  sign(x) * trunc(abs(x) * 10^n + 0.5) / 10^n
}

## Preprocessing

# rescale subscales to range between 0 to 100

pdat <- data %>% 
  group_by(country, subscale) %>% 
  mutate(maxvalue_scale = ifelse(subscale == "Physical inactivity", mean(maxvalue), sum(maxvalue))) %>% 
  group_by(country, subscale, item) %>% 
  mutate(svalue = value * (100 / maxvalue_scale),
         subscale = ifelse(subscale == "Unhealthy diet", "Unhealthy dietary patterns", subscale))
  

# total scores by sub-scale

pdat_scale <- pdat %>% 
  group_by(country, subscale) %>% 
  summarise(svalue = round_xlsx(sum(svalue, na.rm = T) / 4, 1)) %>% 
  mutate(svalue = ifelse(subscale == "Physical inactivity", round_xlsx(svalue, 0), svalue),
         svalue = ifelse(subscale == "Physical inactivity", round_xlsx(svalue / 4, 1), svalue)) %>% ungroup() %>% 
  mutate(subscale = factor(subscale, levels = c("Public Health Index", "Physical inactivity", "Unhealthy dietary patterns", "Alcohol", "Tobacco")),
         xcor = as.numeric(fct_rev(subscale))-0.5) 

pdat_phi <- pdat_scale %>% group_by(country) %>%
  summarise(svalue = round_xlsx(sum(svalue), 1),
            subscale = "Public Health Index", 
            xcor = 4.5) 

pdat_scale_phi <- pdat_scale %>% bind_rows(pdat_phi) %>% 
  mutate(phi = ifelse(subscale == "Public Health Index", 1, 0))

pdat_scale_phi %>% pivot_wider(
  id_cols = country,      
  names_from = subscale,  
  values_from = svalue)

## Save as RDS

saveRDS(pdat, "data/phi_preprocessed.rds")
saveRDS(pdat_scale_phi, "data/final_subscale_phi.rds")



