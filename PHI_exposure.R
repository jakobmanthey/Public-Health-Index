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
options(scipen = 999)

theme_set(theme_minimal() + 
            theme(axis.text = element_text(size = 12, color = "black", family = "Times New Roman"),
                  strip.text = element_text(size = 12, color = "black", family = "Times New Roman"),
                  legend.text = element_text(size = 12, color = "black", family = "Times New Roman"),
                  legend.title = element_text(size = 12, color = "black", family = "Times New Roman"),
                  plot.margin = margin(l = 3, unit = "cm"),
                  plot.background = element_rect(color = "black", fill = NA, size = 0.5)
                  ))


# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 1) LOAD DATA
# ______________________________________________________________________________________________________________________

##  PHI data
# -------------------------------------------------------

path <- file.path("data","final_subscale_phi.rds")
input2 <- data.table(readRDS(path))
nrow(input2) # 90
input2 <- input2[subscale != "Unhealthy dietary patterns"]


##  NCD target data
# -------------------------------------------------------

# tobacco
path <- file.path("data", "WHO downloads", "tobacco.csv")
input3 <- data.table(read.csv(path))

# alcohol
path <- file.path("data", "WHO downloads", "alcohol.csv")
input4 <- data.table(read.csv(path))

# unhealthy diet (obesity)
#path <- file.path("data", "WHO downloads", "unhealthy diet_obesity.csv")
#input5 <- data.table(read.csv(path))

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
#und.dat <- copy(input5[Dim1 == "Both sexes",.(Indicator,country = Location,ISO = SpatialDimValueCode,year = Period,
#                          est = FactValueNumeric, lci = FactValueNumericLow, uci = FactValueNumericHigh)])
#und.dat[, table(Indicator)]
#und.dat$Indicator <- NULL
#und.dat <- und.dat[ISO %in% iso.all]
#und.dat[, table(year)]
#und.dat <- und.dat[year %in% c(2010,2022)][order(ISO,year)]
#und.dat$subscale <- "Unhealthy dietary patterns"


##  physical inactivity
ipa.dat <- copy(input6[Dim1 == "Both sexes",.(Indicator,country = Location,ISO = SpatialDimValueCode,year = Period,
                                              est = FactValueNumeric, lci = FactValueNumericLow, uci = FactValueNumericHigh)])
ipa.dat[, table(Indicator)]
ipa.dat$Indicator <- NULL
ipa.dat <- ipa.dat[ISO %in% iso.all]
ipa.dat[, table(year)]
ipa.dat <- ipa.dat[year %in% c(2010,2022)][order(ISO,year)]
ipa.dat$subscale <- "Physical inactivity"



##  combine exposure data
# -------------------------------------------------------

expdat <- rbind(tob.dat,alc.dat,ipa.dat)
expdat[, period := ifelse(year == 2010,"y1","y2")]
expdat[, country := dplyr::recode(country, 
                                  "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
                                  "Netherlands (Kingdom of the)" = "Netherlands")]
expdat$ISO <- NULL

expdat$subscale <- factor(expdat$subscale, levels = c("Tobacco","Alcohol","Physical inactivity"))

# transform to wide

expdat_wide <- dcast(expdat[,.(country,subscale,period,exp = est)], country + subscale ~ period)
expdat_wide[, change.prop := y2/y1-1]

# achieved goal?
expdat_wide[, achieved := ifelse(subscale == "Tobacco" & change.prop <= -.3, T,
                                 ifelse(subscale == "Alcohol" & change.prop <= -.1, T,
                                        #ifelse(subscale == "Unhealthy dietary patterns" & change.prop <= 0, T,
                                               ifelse(subscale == "Physical inactivity" & change.prop <= -0.1, T, F)))]



##  add PHI data
# -------------------------------------------------------

add <- copy(input2[subscale != "Public Health Index",.(country,subscale, phi_score = svalue)])#
add[, phi_score_avg := mean(phi_score), by = subscale]

data <- merge(expdat_wide, add,
              by = c("country","subscale"), all = T)

out <- copy(data[,.(subscale,country,phi_score,phi_score_avg,y1,y2,change.prop = paste0(round(change.prop,4)*100,"%"),achieved)])[order(subscale,country)]
names(out) <- c("subscale","country","subscale_PHI_score","subscale_PHI_score_average","NCD_year_1","NCD_year_2","NCD_change_%","NCD_achieved")

openxlsx::write.xlsx(out,"tables/Supplementary Material 2_260610.xlsx")

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) FIGURES
# ______________________________________________________________________________________________________________________

##  NCD TARGET vs PHI - bar plots
# -------------------------------------------------------

pdat <- copy(data)
pdat[, sum(achieved), by = country][order(V1)] # how many have achieved the aims?
fillcol  <- "#FFF3C4"                 
redgreen <- c("#F46D43", "#66BD63")   
#redgreen <- c("#ca0020", "#008837")
colors4  <- rev(c("#a1dab4","#41b6c4","#2c7fb8","#253494"))

# TOBACCO
# ...................

pdat1 <- pdat[subscale == "Tobacco"][order(y2, decreasing = T)]
pdat1$country <- factor(pdat1$country, levels = unique(pdat1$country))
coef <- max(pdat1$y2/100) / max(pdat1$phi_score)
coef1 <- 0.0133333
pdat1[, ':=' (y1 = y1/100,
             y2 = y2/100,
             phi_score_plot = phi_score*coef1) ]

plot1 <- ggplot(pdat1, aes(x = country, y = y2)) + 
  geom_col(fill = colors4[1], show.legend = F) + 
  geom_segment(aes(y = y1, yend = y2, color = achieved), linewidth = 1.5,
               arrow = arrow(length = unit(0.15, "cm")), show.legend = F) +
  geom_point(aes(y = phi_score_plot), shape = 23, size = 3, show.legend = F, fill = fillcol) +
  scale_color_manual(values = redgreen) + 
  scale_x_discrete("") +
  scale_y_continuous(name = "Prevalence of tobacco smoking in 2025 (bars) and change since 2010 (arrows)", 
                     breaks = c(0,0.1,0.2,0.3,0.4), labels = scales::percent, limits = c(0, 0.4),
                     sec.axis = sec_axis(~./coef1, breaks = c(0,10,20,30), name="PHI score: subscale tobacco (yellow squares)")) +
  coord_flip()

pdat1[, table(achieved)]

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
coef <- max(pdat2$y2) / max(pdat2$phi_score)
coef2 <- 0.666666
pdat2[, ':=' (phi_score_plot = phi_score*coef2) ]

plot2 <- ggplot(pdat2, aes(x = country, y = y2)) + 
  geom_col(fill = colors4[2], show.legend = F) + 
  geom_segment(aes(y = y1, yend = y2, color = achieved), linewidth = 1.5,
               arrow = arrow(length = unit(0.15, "cm")), show.legend = F) +
  geom_point(aes(y = phi_score_plot), shape = 23, size = 3, show.legend = F, fill = fillcol) +
  scale_color_manual(values = redgreen) + 
  scale_x_discrete("") +
  scale_y_continuous(name = "Alcohol per capita consumption in 2022 (bars) and change since 2010 (arrows)",
                     breaks = c(0,10,20), limits = c(0, 20),
                     sec.axis = sec_axis(~./coef2, breaks = c(0,10,20,30), name="PHI score: subscale alcohol (yellow squares)")) +
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


# PHYSICAL INACTIVITY
# ...................

pdat4 <- pdat[subscale == "Physical inactivity"][order(y2, decreasing = T)]
pdat4$country <- factor(pdat4$country, levels = unique(pdat4$country))
coef <- max(pdat4$y2/100) / max(pdat4$phi_score)
coef4 <- 0.013333333
pdat4[, table(phi_score)]
pdat4[, ':=' (y1 = y1/100,
              y2 = y2/100,
              phi_score_plot = phi_score*coef4) ]

plot4 <- ggplot(pdat4, aes(x = country, y = y2)) + 
  geom_col(fill = colors4[3], show.legend = F) + 
  geom_segment(aes(y = y1, yend = y2, color = achieved), linewidth = 1.5,
               arrow = arrow(length = unit(0.15, "cm")), show.legend = F) +
  geom_point(aes(y = phi_score_plot), shape = 23, size = 3, show.legend = F, fill = fillcol) +
  scale_color_manual(values = redgreen) + 
  scale_x_discrete("") +
  scale_y_continuous(name = "Prevalence of physical inactivity in 2022 (bars) and change since 2010 (arrows)",
                     breaks = c(0,0.1,0.2,0.3,0.4), labels = scales::percent, limits = c(0, 0.4),
                     sec.axis = sec_axis(~./coef4, breaks = c(0,10,20,30), name="PHI score: subscale physical inactivity (yellow squares)")) +
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

cowplot::plot_grid(plot1,plot2,plot4,
                   nrow = 3,
                   labels = c("A","B","C"),
                   label_size = 16,
                   label_fontface = "bold",
                   rel_widths = c(1, 1, 1, 1),
                   align = "h")

ggsave(paste0("figures/Fig4_NCD target vs phi_bars and arrows_",DATE,".svg"), width = 8, height = 10)


# Summary
# ...................

pdat[, sum(achieved), by = country][order(V1)]
pdat[, sum(achieved), by = country][V1 == 3]
pdat[, sum(achieved), by = country][V1 == 2]
pdat[, sum(achieved), by = country][V1 == 1]
pdat[, sum(achieved), by = country][V1 == 0]


