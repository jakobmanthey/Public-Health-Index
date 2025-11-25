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

packages <- c("data.table", "ggplot2", "ggthemes", "ggrepel")
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
                  axis.title = element_blank()))



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 1) LOAD DATA
# ______________________________________________________________________________________________________________________

##  exposure data
# -------------------------------------------------------

path <- file.path("exposure", "exposure_v1.xlsx")
input1 <- data.table(openxlsx::read.xlsx(path, 
                                        sheet = "summary",
                                        rows = c(3:22),
                                        cols = c(2:4,7,8,9,11)
                                        ))

nrow(input1) # 18


##  PHI data
# -------------------------------------------------------

path <- file.path("phi_data_combined.xlsx")
input2 <- data.table(read.xlsx(path, na.strings = ".") )
nrow(input2) # 450


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
                                   "diet_fruit" = "Nutrition",
                                   "insuff_pa_prev" = "Physical activity",
                                   "smoking_prev" = "Tobacco")]

# adjust DIET
expdat[subscale == "Nutrition", exp_value := -exp_value]

##  PHI data
# -------------------------------------------------------

phidat <- copy(input2)
phidat <- phidat[, .(phi_sub_sum = sum(value, na.rm = T)), by = .(country,subscale)]


##  Combine
# -------------------------------------------------------

data <- merge(expdat, 
                phidat, 
                by = c("country","subscale"), all = T)


##  Standardized vars by subscale
# -------------------------------------------------------

##  PHI
data[, phi_sub_sum_stand := (phi_sub_sum-mean(phi_sub_sum))/sd(phi_sub_sum), by = .(subscale)]

##  Exposure
data[, exp_value_stand := (exp_value-mean(exp_value))/sd(exp_value), by = .(subscale)]



# ==================================================================================================================================================================
# ==================================================================================================================================================================
# ==================================================================================================================================================================

# 3) FIGURES
# ______________________________________________________________________________________________________________________

pdat <- copy(data[,.(country,subscale,phi_sub_sum_stand,exp_label,exp_value_stand)])
pdat[, ]

ggplot(pdat, aes(x = exp_value_stand, y = phi_sub_sum_stand)) + 
  ggtitle("Variation of exposure (x) and PHI (y) using z-standardized values",
             "higher values = greater exposure (negative implication for public health) or stronger response") + 
  facet_wrap(. ~ subscale) + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  geom_point() + 
  scale_x_continuous("Exposure") + 
  scale_y_continuous("PHI score")
  

ggplot(pdat, aes(x = exp_value_stand, y = phi_sub_sum_stand)) + 
  ggtitle(
    "Variation of exposure (x) and PHI (y) using z-standardized values",
    "higher values = greater exposure (negative implication for public health) or stronger response\nbottom-right panels contain countries with high exposure and low response"
  ) +
  facet_wrap(. ~ subscale) +
  annotate(
    "rect",
    xmin = 0, xmax = Inf,
    ymin = -Inf, ymax = 0,
    fill = "red", alpha = 0.1
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point() +
  geom_point(
    data = subset(pdat, exp_value_stand > 0 & phi_sub_sum_stand < 0),
    color = "red"
  ) +
  ggrepel::geom_text_repel(
    data = ~ subset(.x, exp_value_stand > 0 & phi_sub_sum_stand < 0),
    aes(label = country),
    hjust = -0.1, vjust = 1.1, size = 3
  )

ggsave(paste0("figures/fig_exp vs phi_panels_",DATE,".png"), width = 8, height = 8)

