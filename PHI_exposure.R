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

# tobacco
path <- file.path("data", "WHO downloads", "tobacco.csv")
input3 <- data.table(read.csv(path))

# alcohol
path <- file.path("data", "WHO downloads", "alcohol.csv")
input4 <- data.table(read.csv(path))

# overweight and obesity
path <- file.path("data", "WHO downloads", "overweight_obesity.csv")
input5 <- data.table(read.csv(path))

# overweight and obesity
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


##  overweight/obesity
ove.dat <- copy(input5[Dim1 == "Both sexes",.(Indicator,country = Location,ISO = SpatialDimValueCode,year = Period,
                          est = FactValueNumeric, lci = FactValueNumericLow, uci = FactValueNumericHigh)])
ove.dat[, table(Indicator)]
ove.dat$Indicator <- NULL
ove.dat <- ove.dat[ISO %in% iso.all]
ove.dat[, table(year)]
ove.dat <- ove.dat[year %in% c(2010,2022)][order(ISO,year)]
ove.dat$subscale <- "Unhealthy diet"


##  insufficient physical activity
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

expdat <- rbind(tob.dat,alc.dat,ove.dat,ipa.dat)
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

openxlsx::write.xlsx(out, paste0("tables/SuppTableY_",Sys.Date(),".xlsx"))


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) FIGURES
# ______________________________________________________________________________________________________________________

##  NCD TARGET vs PHI - without adjustments and labels
# -------------------------------------------------------

pdat <- copy(data)
setorder(pdat, country, subscale)

pdat <- pdat[, .(change.prop,
                 x1 = y1,
                 x2 = y2,
                 y= phi_score[1L],
                 yend = phi_score[1L]
), by = .(country,subscale,achieved)]
pdat[, phi_avg := mean(y), by = subscale]
pdat[, ncd_avg := mean(x2), by = subscale]

pdat[, label := as.factor(fcase(
  subscale == "Tobacco","Tobacco: at least 30% reduction",
  subscale == "Alcohol","Alcohol: at least 10% reduction",
  subscale == "Unhealthy diet","Unhealthy diet: halt the rise in obesity",
  subscale == "Physical inactivity","Physical inactivity: at least 10% reduction",
  default = NA
))]
pdat$label <- factor(pdat$label, levels = c(
  "Tobacco: at least 30% reduction",
  "Alcohol: at least 10% reduction",
  "Unhealthy diet: halt the rise in obesity",
  "Physical inactivity: at least 10% reduction"))

mean_lines <- pdat[, .(hori_line = mean(y)), by = label]

ggplot(pdat, aes(x = x1, xend = x2, y = y, yend = yend, color = achieved)) +
  facet_wrap(. ~ label, scales = "free_x") +
  #geom_hline(aes(yintercept = hori_line)) +
  #geom_rect(data = mean_lines,
  #          aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = hori_line),
  #          fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_point(aes(x = x1, y = y), show.legend = F) +
  geom_segment(arrow = arrow(length = unit(0.15, "cm")), show.legend = F) +
  scale_color_manual("NCD target achieved", values = c("red","darkgreen")) + 
  labs(x = "NCD target indicator (prevalence or alcohol per capita consumption)", y = "PHI subscale score") 

ggsave(paste0("figures/fig_NCD target vs phi_",DATE,".png"), width = 8, height = 8)


##  NCD TARGET vs PHI - with adjustments and labels
# -------------------------------------------------------

pdat2 <- copy(pdat)

n_diff <- 0.1
pdat2$phi_score_adj <- pdat2$y

# tobacco adjustment:
pdat2[subscale == "Tobacco"][order(phi_score_adj)]
dp <- unique(pdat2[subscale == "Tobacco"][duplicated(phi_score_adj)]$phi_score_adj)
for(d in dp){
  ncountry <- pdat2[subscale == "Tobacco" & phi_score_adj == d,.N]
  d_new <- seq(d-(n_diff*ncountry/2),d+(n_diff*ncountry/2),n_diff)
  d_new <- d_new[1:ncountry]
  #if(ncountry %%2 == 1) ncountry <- ncountry+1
  pdat2[subscale == "Tobacco" & phi_score_adj == d]$phi_score_adj <- d_new
  rm(d, d_new, ncountry)
}
anyDuplicated(pdat2[subscale == "Tobacco"]$phi_score_adj) # none
rm(dp)

# alcohol adjustment:
pdat2[subscale == "Alcohol"][order(phi_score_adj)]
dp <- unique(pdat2[subscale == "Alcohol"][duplicated(phi_score_adj)]$phi_score_adj)
for(d in dp){
  ncountry <- pdat2[subscale == "Alcohol" & phi_score_adj == d,.N]
  d_new <- seq(d-(n_diff*ncountry/2),d+(n_diff*ncountry/2),n_diff)
  d_new <- d_new[1:ncountry]
  pdat2[subscale == "Alcohol" & phi_score_adj == d]$phi_score_adj <- d_new
  rm(d, d_new, ncountry)
}
anyDuplicated(pdat2[subscale == "Alcohol"]$phi_score_adj) # none
rm(dp)

# Unhealthy diet adjustment:
pdat2[subscale == "Unhealthy diet"][order(phi_score_adj)]
dp <- unique(pdat2[subscale == "Unhealthy diet"][duplicated(phi_score_adj)]$phi_score_adj)
for(d in dp){
  ncountry <- pdat2[subscale == "Unhealthy diet" & phi_score_adj == d,.N]
  d_new <- seq(d,d+(n_diff*ncountry),n_diff)
  d_new <- d_new[1:ncountry]
  pdat2[subscale == "Unhealthy diet" & phi_score_adj == d]$phi_score_adj <- d_new
  rm(d, d_new, ncountry)
}
anyDuplicated(pdat2[subscale == "Unhealthy diet"]$phi_score_adj) # none
rm(dp)

# Physical inactivity adjustment:
pdat2[subscale == "Physical inactivity"][order(phi_score_adj)]
dp <- unique(pdat2[subscale == "Physical inactivity"][duplicated(phi_score_adj)]$phi_score_adj)
for(d in dp){
  ncountry <- pdat2[subscale == "Physical inactivity" & phi_score_adj == d,.N]
  d_new <- seq(d,d+(n_diff*ncountry),n_diff)
  d_new <- d_new[1:ncountry]
  pdat2[subscale == "Physical inactivity" & phi_score_adj == d]$phi_score_adj <- d_new
  rm(d, d_new, ncountry)
}
anyDuplicated(pdat2[subscale == "Physical inactivity"]$phi_score_adj) # none
rm(dp)

##  which countries for highlight?
# tobacco:
pdat2[subscale == "Tobacco" & achieved == F][order(x2,decreasing = T),.(country,achieved,x2,change.prop,ncd_avg,y,phi_avg)] # not achieved
pdat2[subscale == "Tobacco" & achieved == F & y < phi_avg][order(y,decreasing = F),.(country,achieved,x2,change.prop,ncd_avg,y,phi_avg)] # below avg PHI
pdat2[subscale == "Tobacco"][order(x2,decreasing = T),.(country,achieved,x2,change.prop,ncd_avg,y,phi_avg)][1:10] # highest use prev

countries_tob <- pdat2[subscale == "Tobacco" & achieved == F & y < phi_avg][order(y,decreasing = F)]$country

# alcohol:
pdat2[subscale == "Alcohol" & achieved == F][order(x2,decreasing = T),.(country,achieved,x2,change.prop,ncd_avg,y,phi_avg)] # not achieved
pdat2[subscale == "Alcohol" & achieved == F & y < phi_avg][order(y,decreasing = F),.(country,achieved,x2,change.prop,ncd_avg,y,phi_avg)] # below avg PHI
pdat2[subscale == "Alcohol"][order(x2,decreasing = T),.(country,achieved,x2,change.prop,ncd_avg,y,phi_avg)][1:10] # highest use prev

countries_alc <- pdat2[subscale == "Alcohol" & achieved == F & y < phi_avg][order(y,decreasing = F)]$country

# unhealthy diet:
pdat2[subscale == "Unhealthy diet" & achieved == F][order(x2,decreasing = T),.(country,achieved,x2,change.prop,ncd_avg,y,phi_avg)] # not achieved
pdat2[subscale == "Unhealthy diet" & achieved == F & y < phi_avg][order(y,decreasing = F),.(country,achieved,x2,change.prop,ncd_avg,y,phi_avg)] # below avg PHI
pdat2[subscale == "Unhealthy diet"][order(x2,decreasing = T),.(country,achieved,x2,change.prop,ncd_avg,y,phi_avg)][1:10] # highest use prev

countries_ove <- pdat2[subscale == "Unhealthy diet" & achieved == F & y < phi_avg][order(y,decreasing = F)]$country

# physical inactivity:
pdat2[subscale == "Physical inactivity" & achieved == F][order(x2,decreasing = T),.(country,achieved,x2,change.prop,ncd_avg,y,phi_avg)] # not achieved
pdat2[subscale == "Physical inactivity" & achieved == F & y < phi_avg][order(y,decreasing = F),.(country,achieved,x2,change.prop,ncd_avg,y,phi_avg)] # below avg PHI
pdat2[subscale == "Physical inactivity"][order(x2,decreasing = T),.(country,achieved,x2,change.prop,ncd_avg,y,phi_avg)][1:10] # highest use prev

countries_ove <- pdat2[subscale == "Physical inactivity" & achieved == F & y < phi_avg][order(y,decreasing = F)]$country

##  plot with country labels

ggplot(pdat2, aes(x = x1, xend = x2, y = phi_score_adj, yend = phi_score_adj, color = achieved)) +
  facet_wrap(. ~ label, scales = "free_x") +
  geom_point(aes(x = x1, y = phi_score_adj), show.legend = F) +
  geom_segment(arrow = arrow(length = unit(0.15, "cm")), show.legend = F) +
  ggrepel::geom_label_repel(
    data = ~ subset(.x, achieved == F & y < phi_avg),
    aes(label = country),
    #hjust = -0.1, vjust = -1.1, 
    alpha = 0.3, size = 3, show.legend = F) +
  scale_color_manual("NCD target achieved", values = c("red","darkgreen")) + 
  labs(x = "NCD target indicator (prevalence or alcohol per capita consumption)", y = "PHI subscale score") 

ggsave(paste0("figures/fig_NCD target vs phi_adjusted_",DATE,".png"), width = 8, height = 8)
ggsave(paste0("figures/fig_NCD target vs phi_adjusted_",DATE,".svg"), width = 8, height = 8)




















# OLD FIGURES
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
