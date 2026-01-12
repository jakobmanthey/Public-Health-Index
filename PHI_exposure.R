# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# PROJECT TITLE:  PHI
# CODE AUTHOR:    JAKOB
# DATE STARTED:   251124

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
                  #axis.title = element_blank()´
                  ))

# define rounding function
round_xlsx <- function(x, n) {
  sign(x) * trunc(abs(x) * 10^n + 0.5) / 10^n
}



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 1) LOAD DATA
# ______________________________________________________________________________________________________________________

##  exposure data
# -------------------------------------------------------

path <- file.path("data", "exposure_v2.xlsx")
input1 <- data.table(openxlsx::read.xlsx(path, 
                                         sheet = "summary",
                                         rows = c(3:22),
                                         cols = c(2:4,7,8,11,12)
))

##  PHI data
# -------------------------------------------------------

path <- file.path("data","phi_data_combined.xlsx")
input2 <- data.table(openxlsx::read.xlsx(path, na.strings = ".") )
nrow(input2) # 468


##  NCD target data
# -------------------------------------------------------

path <- file.path("data", "exposure_v2_incl. NCDTargets.xlsx")
input3a <- data.table(openxlsx::read.xlsx(path, 
                                          sheet = 2,
                                          rows = c(1:19),
                                          cols = c(1:4)
))
input3b <- data.table(openxlsx::read.xlsx(path, 
                                          sheet = 3,
                                          rows = c(1:19),
                                          cols = c(1:4)
))
input3c <- data.table(openxlsx::read.xlsx(path, 
                                          sheet = 4,
                                          rows = c(1:19),
                                          cols = c(1:4)
))
input3d <- data.table(openxlsx::read.xlsx(path, 
                                          sheet = 5,
                                          rows = c(1:19),
                                          cols = c(1:4)
))




# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) PREPARE DATA
# ______________________________________________________________________________________________________________________

##  exposure data
# -------------------------------------------------------

expdat <- copy(input1)
expdat[, country := dplyr::recode(country_en, 
                                  "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
                                  "Netherlands (Kingdom of the)" = "Netherlands")]

expdat$ISO <- expdat$country_de <- expdat$country_en <- NULL

expdat <- melt(expdat, id.vars = c("country"),
               variable.name = "exp_label",
               value.name = "exp_value")

expdat[, subscale := dplyr::recode(exp_label,
                                   "alcohol_per_drinker" = "Alcohol",
                                   "diet_ahei" = "Unhealthy diet",
                                   "insuff_pa_prev" = "Physical inactivity",
                                   "smoking_prev" = "Tobacco")]

expdat$subscale <- factor(expdat$subscale, levels = c("Tobacco","Alcohol","Unhealthy diet","Physical inactivity"))

# adjust DIET -> higher values = negative
expdat[subscale == "Unhealthy diet", exp_value := -exp_value]

##  PHI data
# -------------------------------------------------------

# code from Caro:

## Preprocessing

# rescale subscales to range between 0 to 100

pdat <- input2 %>% 
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
         subscale = factor(subscale, levels = c("Public Health Index", "Physical inactivity", "Unhealthy diet", "Alcohol", "Tobacco")))

# total scores by sub-scale

pdat_scale <- pdat %>% 
  group_by(country, subscale) %>% 
  summarise(svalue = round_xlsx(sum(svalue, na.rm = T) / 4, 1)) %>% 
  mutate(svalue = ifelse(subscale == "Physical activity", round_xlsx(svalue, 0), svalue),
         svalue = ifelse(subscale == "Physical activity", round_xlsx(svalue / 4, 1), svalue)) %>% ungroup() %>% 
  mutate(subscale = factor(subscale, levels = c("Public Health Index", "Physical inactivity", "Unhealthy diet", "Alcohol", "Tobacco")),
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


##  Combine
# -------------------------------------------------------

data <- merge(expdat, 
              as.data.table(pdat_scale)[,.(country,subscale,phi_score = svalue)], 
                by = c("country","subscale"), all = T)

data$subscale <- factor(data$subscale, levels = c("Tobacco","Alcohol","Unhealthy diet","Physical inactivity"))

##  Standardized vars by subscale
# -------------------------------------------------------

##  PHI
data[, phi_score_stand := (phi_score-mean(phi_score))/sd(phi_score), by = .(subscale)]

##  Exposure
data[, exp_value_stand := (exp_value-mean(exp_value))/sd(exp_value), by = .(subscale)]


##  NCD TARGET
# -------------------------------------------------------

data2 <- rbind(input3a[,.(country,subscale = "Tobacco",x2 = prev_2022,x1 = prev_2010,change = `%change`)],
               input3b[,.(country,subscale = "Unhealthy diet",x2 = prev_2022,x1 = prev_2010,change = `%change`)],
               input3c[,.(country,subscale = "Alcohol",x2 = apcCD_2020,x1 = apcCD_2010,change = `apcCD_%change`)],
               input3d[,.(country,subscale = "Physical inactivity",x2 = prev_2022,x1 = prev_2010,change = `%change`)])
data2[, country := dplyr::recode(country, 
                                  "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
                                  "Netherlands (Kingdom of the)" = "Netherlands")]

#data2 <- melt(data2, id.vars = c("country","subscale","change"),variable.name = "year")
#data2[, year := as.numeric(substr(year,6,9))]

data2 <- merge(data2, 
              as.data.table(pdat_scale)[,.(country,subscale,phi_score = svalue)], 
              by = c("country","subscale"), all = T)

data2[, achieved := ifelse(subscale == "Tobacco" & change <= -.3, T,
                           ifelse(subscale == "Alcohol" & change <= -.1, T,
                                  ifelse(subscale == "Unhealthy diet" & change <= 0, T,
                                         ifelse(subscale == "Physical inactivity" & change <= -0.1, T, F))))]

data2$subscale <- factor(data2$subscale, levels = c("Tobacco","Alcohol","Unhealthy diet","Physical inactivity"))


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) FIGURES
# ______________________________________________________________________________________________________________________

##  EXPOSURE vs PHI
# -------------------------------------------------------

pdat <- copy(data[,.(country,subscale,phi_score_stand,exp_label,exp_value_stand)])

ggplot(pdat, aes(x = exp_value_stand, y = phi_score_stand)) + 
  ggtitle("Variation of exposure (x) and PHI (y) using z-standardized values",
             "higher values = greater exposure (negative implication for public health) or stronger response") + 
  facet_wrap(. ~ subscale) + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_point() + 
  scale_x_continuous("Exposure") + 
  scale_y_continuous("PHI score")
  
ggplot(pdat, aes(x = exp_value_stand, y = phi_score_stand)) + 
  ggtitle(
    "Variation of exposure (x) and PHI (y) using z-standardised values",
    "higher values = greater exposure (negative implication for public health) or stronger response\nbottom-right panels contain countries with high exposure and low response"
  ) +
  facet_wrap(. ~ subscale) +
  labs(x = "z-standardised exposure", y = "z-standardised PHI subscale score") +
  annotate(
    "rect",
    xmin = 1, xmax = Inf,
    ymin = -Inf, ymax = 0,
    fill = "red", alpha = 0.1
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point() +
  geom_point(
    data = subset(pdat, exp_value_stand > 0 & phi_score_stand < 0),
    color = "red"
  ) +
  ggrepel::geom_text_repel(
    data = ~ subset(.x, exp_value_stand > 0 & phi_score_stand < 0),
    aes(label = country),
    hjust = -0.1, vjust = 1.1, size = 3
  )

ggsave(paste0("figures/fig_exp vs phi_panels_",DATE,".png"), width = 8, height = 8)


##  NCD TARGET vs PHI
# -------------------------------------------------------

setorder(data2, country, subscale)

pdat <- data2[, .(x1,x2,
                  y= phi_score[1L],
                  yend = phi_score[1L]
), by = .(country,subscale,achieved)]

pdat[, label := as.factor(fcase(
  subscale == "Tobacco","Tobacco: at least 30% reduction",
  subscale == "Alcohol","Alcohol: at least 10% reduction",
  subscale == "Unhealthy diet","Unhealthy diet: no rise",
  subscale == "Physical inactivity","Physical inactivity: at least 10% reduction",
  default = NA
  ))]
pdat$label <- factor(pdat$label, levels = c(
  "Tobacco: at least 30% reduction",
  "Alcohol: at least 10% reduction",
  "Unhealthy diet: no rise",
  "Physical inactivity: at least 10% reduction"))

ggplot(pdat, aes(x = x1, xend = x2, y = y, yend = yend, color = achieved)) +
  ggtitle(
    "NCD Target Achievement between 2010 and 2022 (x) and PHI (y)",
    "Green: target achieved | Red: target not achieved"
  ) +
  facet_wrap(. ~ label) +
  geom_segment(arrow = arrow(length = unit(0.15, "cm")), show.legend = F) +
  labs(x = "value", y = "phi_score") + 
  scale_color_manual("NCD target achieved", values = c("red","darkgreen")) + 
  labs(x = "NCD target indicator (prevalence or APC)", y = "PHI score") 
  
  
ggsave(paste0("figures/fig_NCD target vs phi_panels_",DATE,".png"), width = 8, height = 8)
