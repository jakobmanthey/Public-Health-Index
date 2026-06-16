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

#path <- file.path("data","final_subscale_phi.rds")
#input2 <- data.table(readRDS(path))
#nrow(input2) # 90
#input2 <- input2[subscale != "Unhealthy dietary patterns"]


##  NCD target data
# -------------------------------------------------------

# tobacco
path <- file.path("data", "WHO downloads", "tobacco.csv")
input1 <- data.table(read.csv(path))

# alcohol
path <- file.path("data", "WHO downloads", "alcohol.csv")
input2 <- data.table(read.csv(path))

# physical inactivity
path <- file.path("data", "WHO downloads", "physicalactivity.csv")
input3 <- data.table(read.csv(path))



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
tob.dat <- copy(input1[Dim1 == "Both sexes",.(Indicator,country = Location,ISO = SpatialDimValueCode,year = Period,
                          est = FactValueNumeric, lci = FactValueNumericLow, uci = FactValueNumericHigh)])
tob.dat[, table(Indicator)]
tob.dat <- tob.dat[Indicator == "Estimate of current tobacco use prevalence (%) (age-standardized)"]
tob.dat$Indicator <- NULL
tob.dat <- tob.dat[ISO %in% iso.all]
tob.dat[, table(year)]
tob.dat <- tob.dat[year %in% c(2010,2025)][order(ISO,year)]
tob.dat$subscale <- "Tobacco"


##  alcohol
alc.dat <- copy(input2[,.(Indicator,country = Location,ISO = SpatialDimValueCode,year = Period,
                                              est = FactValueNumeric, lci = FactValueNumericLow, uci = FactValueNumericHigh)])
alc.dat[, table(Indicator)]
alc.dat$Indicator <- NULL
alc.dat <- alc.dat[ISO %in% iso.all]
alc.dat[, table(year)]
alc.dat <- alc.dat[year %in% c(2010,2022)][order(ISO,year)]
alc.dat$subscale <- "Alcohol"


##  physical inactivity
ipa.dat <- copy(input3[Dim1 == "Both sexes",.(Indicator,country = Location,ISO = SpatialDimValueCode,year = Period,
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
                                        ifelse(subscale == "Physical inactivity" & change.prop <= -0.1, T, F)))]



##  export as Supp Mat 2
# -------------------------------------------------------

out <- copy(expdat_wide[,.(subscale,country,y1,y2,change.prop = paste0(round(change.prop,4)*100,"%"),achieved)])[order(subscale,country)]
names(out) <- c("subscale","country","NCD_year_1","NCD_year_2","NCD_change_%","NCD_achieved")

openxlsx::write.xlsx(out,"tables/Supplementary Material 2_260616.xlsx")

# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) FIGURES
# ______________________________________________________________________________________________________________________

##  NCD TARGET vs PHI - bar plots
# -------------------------------------------------------

pdat <- copy(expdat_wide)
pdat[, sum(achieved), by = country][order(V1)] # how many have achieved the aims?
redgreen <- c("#F46D43", "#66BD63")   
colors3  <- rev(c("#a1dab4","#41b6c4","#2c7fb8","#253494"))[1:3]

# TOBACCO
# ...................

pdat1 <- pdat[subscale == "Tobacco"][order(y2, decreasing = T)]
pdat1$country <- factor(pdat1$country, levels = unique(pdat1$country))
pdat1[, ':=' (y1 = y1/100,
             y2 = y2/100) ]

plot1 <- ggplot(pdat1, aes(x = country, y = y2)) + 
  geom_col(fill = colors3[1], show.legend = F) + 
  geom_segment(aes(y = y1, yend = y2, color = achieved), linewidth = 1.5,
               arrow = arrow(length = unit(0.15, "cm")), show.legend = F) +
  scale_color_manual(values = redgreen) + 
  scale_x_discrete("") +
  scale_y_continuous(name = "Prevalence of tobacco smoking in 2025 (bars) and change since 2010 (arrows)", 
                     breaks = c(0,0.1,0.2,0.3,0.4), labels = scales::percent, limits = c(0, 0.4)) +
  coord_flip()

pdat1[, table(achieved)]

# good practice
pdat1[order(y2, decreasing = F)][1:5]
pdat1[order(change.prop, decreasing = F)][1:9]

# bad practice
pdat1[order(y2, decreasing = T)][1:5]
pdat1[order(change.prop, decreasing = T)][1:5]


# ALCOHOL
# ...................

pdat2 <- pdat[subscale == "Alcohol"][order(y2, decreasing = T)]
pdat2$country <- factor(pdat2$country, levels = unique(pdat2$country))

plot2 <- ggplot(pdat2, aes(x = country, y = y2)) + 
  geom_col(fill = colors3[2], show.legend = F) + 
  geom_segment(aes(y = y1, yend = y2, color = achieved), linewidth = 1.5,
               arrow = arrow(length = unit(0.15, "cm")), show.legend = F) +
  scale_color_manual(values = redgreen) + 
  scale_x_discrete("") +
  scale_y_continuous(name = "Alcohol per capita consumption in 2022 (bars) and change since 2010 (arrows)",
                     breaks = c(0,10,20), limits = c(0, 20)) +
  coord_flip()

pdat2[, table(achieved)]

# good practice
pdat2[order(y2, decreasing = F)][1:5]
pdat2[order(change.prop, decreasing = F)][1:5]
pdat2[,.(country,y1,y2,change = y1-y2)][order(change, decreasing = T)][1:5]

# bad practice
pdat2[order(y2, decreasing = T)][1:5]
pdat2[order(change.prop, decreasing = T)][1:5]


# PHYSICAL INACTIVITY
# ...................

pdat4 <- pdat[subscale == "Physical inactivity"][order(y2, decreasing = T)]
pdat4$country <- factor(pdat4$country, levels = unique(pdat4$country))
pdat4[, ':=' (y1 = y1/100,
              y2 = y2/100) ]

plot4 <- ggplot(pdat4, aes(x = country, y = y2)) + 
  geom_col(fill = colors3[3], show.legend = F) + 
  geom_segment(aes(y = y1, yend = y2, color = achieved), linewidth = 1.5,
               arrow = arrow(length = unit(0.15, "cm")), show.legend = F) +
  scale_color_manual(values = redgreen) + 
  scale_x_discrete("") +
  scale_y_continuous(name = "Prevalence of physical inactivity in 2022 (bars) and change since 2010 (arrows)",
                     breaks = c(0,0.1,0.2,0.3,0.4), labels = scales::percent, limits = c(0, 0.4)) +
  coord_flip()

pdat4[, table(achieved)]

# good practice
pdat4[order(y2, decreasing = F)][1:5]
pdat4[order(change.prop, decreasing = F)][1:5]
pdat4[,.(country,y1,y2,change = y1-y2)][order(change, decreasing = T)][1:5]

# bad practice
pdat4[order(y2, decreasing = T)][1:5]
pdat4[order(change.prop, decreasing = T)][1:5]

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
ggsave(paste0("figures/Fig4_NCD target vs phi_bars and arrows_",DATE,".tiff"), width = 8, height = 10, dpi = 300)



# Summary
# ...................

pdat[, sum(achieved), by = country][order(V1)]
pdat[, sum(achieved), by = country][V1 == 3]
pdat[, sum(achieved), by = country][V1 == 2]
pdat[, sum(achieved), by = country][V1 == 1]
pdat[, sum(achieved), by = country][V1 == 0]


