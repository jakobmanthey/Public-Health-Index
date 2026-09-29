# PHI 
# Validation Unhealthy Dietary Patterns
# Carolin Kilian
# 15-09-2026

library(tidyverse)

# Load data
rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Research projects/Public Health Index/Github/Public-Health-Index/")
DATE <- 20260915

# Subscale Unhealthy Dietary Patterns
phi <- readRDS("data/final_subscale_phi.rds") %>% filter(subscale == "Unhealthy dietary patterns") %>%
  mutate(rank = rank(svalue, ties.method = "min"))
frei <- readxl::read_excel('data/Val_FREI_LimaEtAl2021.xlsx')

# Spearman rank correlations
cor.test(phi$rank, frei$FREI_rank, method = 'spearman', exact = FALSE)


