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
                  plot.margin = margin(l = 3, unit = "cm"),
                  #panel.border = element_rect(color = "black", fill = NA)
                  plot.background = element_rect(color = "black", fill = NA, size = 0.5)
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

# tobacco
path <- file.path("data", "WHO downloads", "tobacco.csv")
input3 <- data.table(read.csv(path))

# alcohol
path <- file.path("data", "WHO downloads", "alcohol.csv")
input4 <- data.table(read.csv(path))

# unhealthy diet (obesity)
path <- file.path("data", "WHO downloads", "unhealthy diet_obesity.csv")
input5 <- data.table(read.csv(path))

# physical inactivity
path <- file.path("data", "WHO downloads", "physicalactivity.csv")
input6 <- data.table(read.csv(path))



##  ISO codes
# -------------------------------------------------------

iso.all <- c("BEL","DNK","DEU","EST","FIN","FRA","IRL","LVA","LTU","LUX","NLD","NOR","AUT","POL","SWE","CHE","CZE","GBR")



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 2) PREPARE DATA
# ______________________________________________________________________________________________________________________

##  prepare exposure data
# -------------------------------------------------------

##  tobacco
tob.dat <- copy(input3[Dim1 == "Both sexes",.(Indicator,country = Location,ISO = SpatialDimValueCode,year = Period,
                          est = FactValueNumeric, lci = FactValueNumericLow, uci = FactValueNumericHigh)])
tob.dat[, table(Indicator)]
tob.dat <- tob.dat[Indicator == "Estimate of current tobacco use prevalence (%) (age-standardized)"]
tob.dat$Indicator <- NULL
tob.dat <- tob.dat[ISO %in% iso.all]
tob.dat[, table(year)]
tob.dat <- tob.dat[year %in% c(2010,2025)][order(ISO,year)]
tob.dat$subscale <- "Tobacco"


##  alcohol
alc.dat <- copy(input4[,.(Indicator,country = Location,ISO = SpatialDimValueCode,year = Period,
                                              est = FactValueNumeric, lci = FactValueNumericLow, uci = FactValueNumericHigh)])
alc.dat[, table(Indicator)]
alc.dat$Indicator <- NULL
alc.dat <- alc.dat[ISO %in% iso.all]
alc.dat[, table(year)]
alc.dat <- alc.dat[year %in% c(2010,2022)][order(ISO,year)]
alc.dat$subscale <- "Alcohol"


# unhealthy diet (obesity)
und.dat <- copy(input5[Dim1 == "Both sexes",.(Indicator,country = Location,ISO = SpatialDimValueCode,year = Period,
                          est = FactValueNumeric, lci = FactValueNumericLow, uci = FactValueNumericHigh)])
und.dat[, table(Indicator)]
und.dat$Indicator <- NULL
und.dat <- und.dat[ISO %in% iso.all]
und.dat[, table(year)]
und.dat <- und.dat[year %in% c(2010,2022)][order(ISO,year)]
und.dat$subscale <- "Unhealthy diet"


##  physical inactivity
ipa.dat <- copy(input6[Dim1 == "Both sexes",.(Indicator,country = Location,ISO = SpatialDimValueCode,year = Period,
                                              est = FactValueNumeric, lci = FactValueNumericLow, uci = FactValueNumericHigh)])
ipa.dat[, table(Indicator)]
ipa.dat$Indicator <- NULL
ipa.dat <- ipa.dat[ISO %in% iso.all]
ipa.dat[, table(year)]
ipa.dat <- ipa.dat[year %in% c(2010,2022)][order(ISO,year)]
ipa.dat$subscale <- "Physical inactivity"



##  comine exposure data
# -------------------------------------------------------

expdat <- rbind(tob.dat,alc.dat,und.dat,ipa.dat)
expdat[, period := ifelse(year == 2010,"y1","y2")]
expdat[, country := dplyr::recode(country, 
                                  "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
                                  "Netherlands (Kingdom of the)" = "Netherlands")]
expdat$ISO <- NULL

#expdat <- melt(expdat, id.vars = c("country"),
#               variable.name = "exp_label",
#               value.name = "exp_value")


#expdat[, subscale := dplyr::recode(exp_label,
#                                   "alcohol_per_drinker" = "Alcohol",
#                                   "diet_ahei" = "Unhealthy diet",
#                                   "insuff_pa_prev" = "Physical inactivity",
#                                   "smoking_prev" = "Tobacco")]

expdat$subscale <- factor(expdat$subscale, levels = c("Tobacco","Alcohol","Unhealthy diet","Physical inactivity"))

# transform to wide

expdat_wide <- dcast(expdat[,.(country,subscale,period,exp = est)], country + subscale ~ period)
expdat_wide[, change.prop := y2/y1-1]

# achieved goal?
expdat_wide[, achieved := ifelse(subscale == "Tobacco" & change.prop <= -.3, T,
                                 ifelse(subscale == "Alcohol" & change.prop <= -.1, T,
                                        ifelse(subscale == "Unhealthy diet" & change.prop <= 0, T,
                                               ifelse(subscale == "Physical inactivity" & change.prop <= -0.1, T, F))))]



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

data <- merge(expdat_wide, 
              as.data.table(pdat_scale)[,.(country,subscale,phi_score = svalue)], 
                by = c("country","subscale"), all = T)

out <- copy(data[,.(subscale,country,phi_score,y1,y2,change.prop = paste0(round(change.prop,4)*100,"%"),achieved)])[order(subscale,country)]
names(out) <- c("subscale","country","subscale_PHI_score","NCD_year_1","NCD_year_2","NCD_change_%","NCD_achieved")

#openxlsx::write.xlsx(out, paste0("tables/SuppTableY_",Sys.Date(),".xlsx"))

# add z-standardised PHI

data[, phi_score_zstand := (phi_score - mean(phi_score))/sd(phi_score), by = subscale]




# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) FIGURES
# ______________________________________________________________________________________________________________________

##  NCD TARGET vs PHI - bar plots
# -------------------------------------------------------

pdat <- copy(data)
pdat[, sum(achieved), by = country][order(V1)] # how many have achieved the aims?
redgreen <- c("#E4572E", "#4CAF50")

# TOBACCO
# ...................

pdat1 <- pdat[subscale == "Tobacco"][order(y2, decreasing = T)]
pdat1$country <- factor(pdat1$country, levels = unique(pdat1$country))
pdat1[, ':=' (y1 = y1/100,
             y2 = y2/100,
             phi_score = phi_score/100) ]


plot1 <- ggplot(pdat1, aes(x = country, y = y2)) + 
  geom_col(aes(fill = achieved), show.legend = F) + 
  geom_segment(aes(y = y1, yend = y2), 
               arrow = arrow(length = unit(0.15, "cm")), show.legend = F) +
  geom_point(aes(y = phi_score), shape = 23, size = 3, show.legend = F, fill = "#5A7D9A") +
  scale_fill_manual(values = redgreen) + 
  scale_color_manual(values = redgreen) + 
  scale_x_discrete("") +
  scale_y_continuous(name = "% tobacco smoking prevalence in 2025 (bars) and change since 2010 (arrows)", labels = scales::percent,
                     sec.axis = sec_axis(~.*100, name="PHI score: tobacco subscale (blue squares)")) +
  coord_flip()

#ggsave(paste0("figures/Fig4_A_NCD target vs phi_tobacco_",DATE,".png"), width = 8, height = 6)

# good practice
pdat1[order(y2, decreasing = F)][1:5]
pdat1[order(change.prop, decreasing = F)][1:9]

# bad practice
pdat1[order(y2, decreasing = T)][1:5]
pdat1[order(change.prop, decreasing = T)][1:5]

pdat1[, mean(phi_score), by = achieved]


# ALCOHOL
# ...................

pdat2 <- pdat[subscale == "Alcohol"][order(y2, decreasing = T)]
pdat2$country <- factor(pdat2$country, levels = unique(pdat2$country))
pdat2[, ':=' (phi_score_adj = phi_score/2) ]

plot2 <- ggplot(pdat2, aes(x = country, y = y2)) + 
  geom_col(aes(fill = achieved), show.legend = F) + 
  geom_segment(aes(y = y1, yend = y2), 
               arrow = arrow(length = unit(0.15, "cm")), show.legend = F) +
  geom_point(aes(y = phi_score_adj), shape = 23, size = 3, show.legend = F, fill = "#5A7D9A") +
  scale_fill_manual(values = redgreen) + 
  scale_color_manual(values = redgreen) + 
  scale_x_discrete("") +
  scale_y_continuous(name = "Alcohol per capita consumption in 2022 (bars) and change since 2010 (arrows)",
                     sec.axis = sec_axis(~.*2, name="PHI score: alcohol subscale (blue squares)")) +
  coord_flip()

#ggsave(paste0("figures/Fig4_b_NCD target vs phi_alcohol_",DATE,".png"), width = 8, height = 6)

pdat2[, table(achieved)]

# good practice
pdat2[order(y2, decreasing = F)][1:5]
pdat2[order(change.prop, decreasing = F)][1:5]
pdat2[,.(country,y1,y2,change = y1-y2)][order(change, decreasing = T)][1:5]

# bad practice
pdat2[order(y2, decreasing = T)][1:5]
pdat2[order(change.prop, decreasing = T)][1:5]

pdat2[, mean(phi_score), by = achieved]


# UNHEALTHY DIET
# ...................

pdat3 <- pdat[subscale == "Unhealthy diet"][order(y2, decreasing = T)]
pdat3$country <- factor(pdat3$country, levels = unique(pdat3$country))
pdat3[, table(phi_score)]
pdat3[, ':=' (y1 = y1/100,
              y2 = y2/100,
              phi_score = phi_score/50) ]

plot3 <- ggplot(pdat3, aes(x = country, y = y2)) + 
  geom_col(aes(fill = achieved), show.legend = F) + 
  geom_segment(aes(y = y1, yend = y2), 
               arrow = arrow(length = unit(0.15, "cm")), show.legend = F) +
  geom_point(aes(y = phi_score), shape = 23, size = 3, show.legend = F, fill = "#5A7D9A") +
  scale_fill_manual(values = redgreen) + 
  scale_color_manual(values = redgreen) + 
  scale_x_discrete("") +
  scale_y_continuous(name = "% obesity in 2022 (bars) and change since 2010 (arrows)", labels = scales::percent,
                     sec.axis = sec_axis(~.*50, name="PHI score: subscale unhealthy diet (blue squares)")) +
  coord_flip()

#ggsave(paste0("figures/Fig4_C_NCD target vs phi_unhealthy diet_",DATE,".png"), width = 8, height = 6)

pdat3[, table(achieved)]

# good practice
pdat3[order(y2, decreasing = F)][1:5]
pdat3[order(change.prop, decreasing = F)][1:5]
pdat3[,.(country,y1,y2,change = y1-y2)][order(change, decreasing = T)][1:5]

# bad practice
pdat3[order(y2, decreasing = T)][1:5]
pdat3[order(change.prop, decreasing = T)][1:5]
pdat3[order(y1-y2, decreasing = F),][1:5]
pdat3[order(y1-y2, decreasing = F),.(country,y1-y2)][1:5]


pdat3[, mean(phi_score), by = achieved]

# PHYSICAL INACTIVITY
# ...................

pdat4 <- pdat[subscale == "Physical inactivity"][order(y2, decreasing = T)]
pdat4$country <- factor(pdat4$country, levels = unique(pdat4$country))
pdat4[, table(phi_score)]
pdat4[, ':=' (y1 = y1/100,
              y2 = y2/100,
              phi_score = phi_score/100) ]

plot4 <- ggplot(pdat4, aes(x = country, y = y2)) + 
  geom_col(aes(fill = achieved), show.legend = F) + 
  geom_segment(aes(y = y1, yend = y2), 
               arrow = arrow(length = unit(0.15, "cm")), show.legend = F) +
  geom_point(aes(y = phi_score), shape = 23, size = 3, show.legend = F, fill = "#5A7D9A") +
  scale_fill_manual(values = redgreen) + 
  scale_color_manual(values = redgreen) + 
  scale_x_discrete("") +
  scale_y_continuous(name = "% physical inactivity in 2022 (bars) and change since 2010 (arrows)", labels = scales::percent,
                     sec.axis = sec_axis(~.*100, name="PHI score: subscale physical inactivity (blue squares)")) +
  coord_flip()

pdat4[, table(achieved)]

# good practice
pdat4[order(y2, decreasing = F)][1:5]
pdat4[order(change.prop, decreasing = F)][1:5]
pdat4[,.(country,y1,y2,change = y1-y2)][order(change, decreasing = T)][1:5]

# bad practice
pdat4[order(y2, decreasing = T)][1:5]
pdat4[order(change.prop, decreasing = T)][1:5]

pdat4[, mean(phi_score), by = achieved]
pdat4[, mean(y2), by = achieved]

# COMBINE
# ...................

cowplot::plot_grid(plot1,plot2,plot3,plot4,
                   nrow = 2,
                   labels = c("A", "B","C","D"),
                   #labels = c("A: tobacco", "B: alcohol", "C: unhealthy diet", "D: physical activity"),  # Add this
                   label_size = 16,                  # Optional: bigger labels
                   label_fontface = "bold",          # Optional: bold labels
                   rel_widths = c(1, 1, 1, 1),  # Give left plots more width
                   align = "h")
ggsave(paste0("figures/Fig4_NCD target vs phi_bars and arrows_",DATE,".png"), width = 15, height = 10)
ggsave(paste0("figures/Fig4_NCD target vs phi_bars and arrows_",DATE,".svg"), width = 15, height = 10)


