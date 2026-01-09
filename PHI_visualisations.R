# PHI 
# Data visualisation
# Carolin Kilian
# 10-11-2025

library(tidyverse)
library(openxlsx)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(rnaturalearth)
library(rnaturalearthdata)

# Load data
rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Research projects/Public Health Index/Github/Public-Health-Index/")
DATE <- 20260109

data <- read.xlsx("data/phi_data_combined.xlsx", na.strings = ".") 

# set ggplot standard theme

theme_set(theme_minimal() + 
            theme(axis.text = element_text(size = 12, color = "black", family = "Times New Roman"),
                  strip.text = element_text(size = 12, color = "black", family = "Times New Roman"),
                  legend.text = element_text(size = 12, color = "black", family = "Times New Roman"),
                  legend.title = element_text(size = 12, color = "black", family = "Times New Roman"),
                  axis.title = element_blank()))

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
  mutate(domain = factor(domain, levels = c("other", "health communication", "marketing", "living environment", "pricing")),
         subscale = factor(subscale, levels = c("Public Health Index", "Physical inactivity", "Unhealthy diet", "Alcohol", "Tobacco")))

# total scores by sub-scale

pdat_scale <- pdat %>% 
  group_by(country, subscale) %>% 
  summarise(svalue = round_xlsx(sum(svalue, na.rm = T) / 4, 1)) %>% 
  mutate(svalue = ifelse(subscale == "Physical inactivity", round_xlsx(svalue, 0), svalue),
         svalue = ifelse(subscale == "Physical inactivity", round_xlsx(svalue / 4, 1), svalue)) %>% ungroup() %>% 
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

# Fig 1) PHI map

pdat_map <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  merge(., pdat_phi, by.x = c("sovereignt"), by.y = "country", all = T) %>% 
  filter(continent == "Europe")

ggplot(data = pdat_map) + 
  geom_sf(aes(fill = as.numeric(svalue), color = "black")) +
  scale_color_manual(values = c("black")) + 
  scale_fill_gradient2(low = "#8E3B27", mid = "#FFEB24", high = "#52B382", midpoint = 52, 
                       limits = c(30,75), na.value = "white") +
  guides(color = F, fill = guide_legend(title = "Public Health Index")) + 
  theme_void() + theme(legend.position = c(0, 0.22)) +
  coord_sf(xlim = c(-25,40), ylim = c(30,75), expand = FALSE) 

ggsave("figures/Fig1_PHIgesamt.png", width = 8, height = 6, dpi = 300, bg = "white")

# Fig 2) PHI by subscale 

bpanel <- 
  ggplot(pdat_scale) + 
    geom_bar(aes(y = reorder(country, svalue), x = svalue, fill = subscale), stat = "identity", position = "stack") + 
    geom_vline(xintercept = 100, color = "grey50") +
    scale_fill_manual(values = c("#CECDE7", "#AABFCD", "#497389", "#2F394D"), name = "") + 
    scale_x_continuous(limits = c(0,100), sec.axis = dup_axis(breaks = 1, labels = c("\n"))) + 
    scale_y_discrete(labels = NULL) +
    guides(fill = guide_legend(reverse = T)) +
    theme(panel.grid.major.y = element_blank())

apanel <- 
  ggplot(pdat_scale_phi) + 
    geom_text(aes(y = reorder(country, svalue), x = xcor, label = paste(sprintf("%.1f", svalue)), fontface = factor(phi)), family = "Times New Roman") +
    scale_x_continuous(limits=c(0,4.5), breaks = 0.5:4.5, labels = c("Tobacco", "Alcohol", "Unhealthy diet", "Physical\ninactivity", "PHI"), position = "top",
                       sec.axis = dup_axis(breaks = 1, labels = c(""))) +
    scale_discrete_manual(values = c("plain", "bold"), aesthetics = "fontface", guide = "none") + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(face = "bold")) 

fig1 <- grid.arrange(apanel, bpanel, widths = c(1,2))

ggsave("figures/Fig2_PHIgesamt.png", fig1, width = 14, height = 8, dpi = 300, bg = "white")

# Fig 3) svalues by policy domain

ggplot(pdat_domain[pdat_domain$subscale != "Public Health Index",]) + 
  geom_bar(aes(y = subscale, x = zvalue, fill = domain), stat = "identity", position = "stack") + 
  geom_vline(aes(xintercept = 0)) + 
  facet_wrap(~country, ncol = 3) +
  scale_fill_manual(values = c("#E4C287", "#DB8580", "#8F74C6", "#0A2F51", "#52B382"), name = "Policy domain") +
  guides(fill = guide_legend(reverse = T)) 

ggsave("figures/Fig3_PolicyContribution.png", width = 16, height = 8, dpi = 300, bg = "white")
