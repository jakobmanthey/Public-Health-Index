# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# PROJECT TITLE:  PHI
# CODE AUTHOR:    JAKOB
# DATE STARTED:   251223

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 0) ESSENTIALS
# ______________________________________________________________________________________________________________________

# clean workspace
rm(list=ls())

packages <- c("data.table", "ggplot2", "ggthemes", "ggrepel", "extrafont", "tidyverse")
#              "Hmisc", "tidyr", "nnet",
#              "tableone", "knitr", "kableExtra",
#              "survey")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Load packages
invisible(lapply(packages, library, character.only = TRUE))

# current date:
DATE <- format(Sys.Date(), "%y%m%d")

# themes and options
#theme_set( theme_gdocs() )
options(scipen = 999)

theme_set(theme_minimal() + 
            theme(axis.text = element_text(size = 12, color = "black", family = "Times New Roman"),
                  strip.text = element_text(size = 12, color = "black", family = "Times New Roman"),
                  legend.text = element_text(size = 12, color = "black", family = "Times New Roman"),
                  legend.title = element_text(size = 12, color = "black", family = "Times New Roman"),
                  #axis.title = element_blank()
                  )
          )

# define rounding function
round_xlsx <- function(x, n) {
  sign(x) * trunc(abs(x) * 10^n + 0.5) / 10^n
}


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 1) LOAD DATA
# ______________________________________________________________________________________________________________________

##  PHI data
# -------------------------------------------------------

path <- file.path("data","phi_data_combined.xlsx")
input1 <- data.table(openxlsx::read.xlsx(path, na.strings = ".") )
nrow(input1) # 468

##  SDG 3.4.1 - probability of NCD deaths between 30-70 years 
# -------------------------------------------------------
#https://www.who.int/data/gho/data/indicators/indicator-details/GHO/probability-of-dying-between-exact-ages-30-and-70-from-any-of-cardiovascular-disease-cancer-diabetes-or-chronic-respiratory-(-)

path <- file.path("data","data_sdg341.csv")
input2 <- data.table(read.csv(path))
nrow(input2) # 12210





# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) PREPARE DATA
# ______________________________________________________________________________________________________________________

##  PHI data
# -------------------------------------------------------

# code from Caro:

## Preprocessing

# rescale subscales to range between 0 to 100

pdat <- input1 %>% 
  group_by(country, subscale) %>% 
  mutate(maxvalue_scale = ifelse(subscale == "Physical activity", mean(maxvalue), sum(maxvalue))) %>% 
  group_by(country, subscale, item) %>% 
  mutate(svalue = value * (100 / maxvalue_scale))

# total scores by domain

pdat_total <- pdat %>%
  group_by(country, domain) %>% 
  summarise(svalue = round_xlsx(sum(svalue, na.rm = T) / 4, 1)) %>% 
  mutate(subscale = "Public Health Index")

# z-standardizing scores by domain

pdat_domain <- pdat %>% 
  bind_rows(pdat_total) %>% 
  group_by(country, subscale, domain) %>% 
  summarise(svalue = sum(svalue)) %>%
  group_by(subscale, domain) %>%
  mutate(zvalue = (svalue - mean(svalue, na.rm = T)) / sd(svalue, na.rm = T)) %>% 
  ungroup()  %>%
  mutate(domain = factor(domain, levels = c("other", "public policy", "marketing", "living environment", "pricing")),
         subscale = factor(subscale, levels = c("Public Health Index", "Physical activity", "Nutrition", "Alcohol", "Tobacco")))

# total scores by sub-scale

pdat_scale <- pdat %>% 
  group_by(country, subscale) %>% 
  summarise(svalue = round_xlsx(sum(svalue, na.rm = T) / 4, 1)) %>% 
  mutate(svalue = ifelse(subscale == "Physical activity", round_xlsx(svalue, 0), svalue),
         svalue = ifelse(subscale == "Physical activity", round_xlsx(svalue / 4, 1), svalue)) %>% ungroup() %>% 
  mutate(subscale = factor(subscale, levels = c("Public Health Index", "Physical activity", "Nutrition", "Alcohol", "Tobacco")),
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


##  SDG data
# -------------------------------------------------------

valdat <- copy(input2)
unique(input1$country)[!unique(input1$country) %in% valdat$Location]
valdat[Location %like% "United|Nether",.(unique(Location))]
valdat[, country := fcase(default = Location,
                          Location == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom",
                          Location == "Netherlands (Kingdom of the)", "Netherlands")]

valdat <- valdat[Period == 2021 & Dim1 == "Both sexes",.(country,sdg341 = FactValueNumeric)]

##  Combine
# -------------------------------------------------------

data <- merge(as.data.table(pdat_scale_phi)[,.(country,subscale,score = svalue)], 
              valdat,
              by = c("country"), all.x = T)

data$subscale <- factor(data$subscale, levels = c("Tobacco","Alcohol","Nutrition","Physical activity","Public Health Index"))



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) FIGURES
# ______________________________________________________________________________________________________________________

pdat <- copy(data) #[,.(country,subscale,phi_sub_sum_stand,exp_label,exp_value_stand)])

ggplot(pdat[subscale %like% "Index"], aes(x = score, y = sdg341)) + 
  geom_point() + 
  geom_smooth() +
  scale_x_continuous("PHI score") + 
  scale_y_continuous("SDG 3.4.1 - probability of NCD death ages 30-70")

ggplot(pdat, aes(x = score, y = sdg341)) + 
  ggtitle("PHI (x) and SDG 3.4.1 indicator (y)") + 
  facet_wrap(. ~ subscale, scales = "free") + 
  geom_point() + 
  geom_smooth() +
  scale_x_continuous("PHI (subscale) score") + 
  scale_y_continuous("SDG 3.4.1 - probability of NCD death ages 30-70")

ggsave(paste0("figures/fig_phi vs SDG341_",DATE,".png"), width = 12, height = 8)

