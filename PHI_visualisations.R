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
                  legend.text = element_text(size = 10, color = "black", family = "Times New Roman"),
                  legend.title = element_text(size = 10, color = "black", family = "Times New Roman"),
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

# reformate for plotting

pdat_scale_mean <- pdat_scale_phi %>% group_by(subscale) %>% 
  summarise(country = "Average",
            xcor = mean(xcor),
            svalue = mean(svalue),
            phi = 1)

order <- pdat_scale_phi %>% filter(subscale == "Public Health Index") %>% arrange(svalue) %>% pull(country)
order <- c("Average", order)

pdat_scale_phi_mean <- pdat_scale_phi %>% bind_rows(pdat_scale_mean) %>%
  mutate(country = factor(country, levels = order))

add_rows <- pdat_scale_mean %>% filter(subscale != "Public Health Index") %>% select(-phi)

pdat_scale_b <- pdat_scale %>% bind_rows(add_rows) %>% 
  mutate(country = factor(country, levels = order),
         svalue = ifelse(country == "Average", NA, svalue),
         subscale = factor(subscale, levels = c("Physical inactivity", "Unhealthy diet", "Alcohol", "Tobacco")))

# Fig 1) PHI map

pdat_map <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  merge(., pdat_phi, by.x = c("sovereignt"), by.y = "country", all = T) %>% 
  filter(continent == "Europe")

ggplot(data = pdat_map) + 
  geom_sf(aes(fill = as.numeric(svalue), color = "black")) +
  scale_color_manual(values = c("black")) + 
  scale_fill_gradient2(low = "#ca0020", mid = "#ffffbf", high = "#008837", midpoint = 52, 
                       limits = c(30,75), na.value = "white") +
  guides(color = F, fill = guide_legend(title = "Public Health Index")) + 
  theme_void() + theme(legend.position = c(0, 0.22)) +
  coord_sf(xlim = c(-25,40), ylim = c(30,75), expand = FALSE) 

ggsave("figures/Fig1_PHIgesamt.png", width = 8, height = 6, dpi = 300, bg = "white")

# Fig 2) PHI by subscale 

bpanel <- 
  ggplot(pdat_scale_b) + 
    geom_bar(aes(y = country, x = svalue, fill = subscale), stat = "identity", position = "stack") + 
    geom_vline(xintercept = 100, color = "grey50") +
    scale_fill_manual(values = c('#a1dab4','#41b6c4','#2c7fb8','#253494'), name = "") + 
    scale_x_continuous(limits = c(0,100), sec.axis = dup_axis(breaks = 1, labels = c("\n"))) + 
    scale_y_discrete(labels = NULL) +
    guides(fill = guide_legend(reverse = T)) +
    theme(panel.grid.major.y = element_blank())

apanel <- 
  ggplot(pdat_scale_phi_mean) + 
    geom_text(aes(y = country, x = xcor, label = paste(sprintf("%.1f", svalue)), fontface = factor(phi)), family = "Times New Roman") +
    scale_x_continuous(limits=c(0,4.5), breaks = 0.5:4.5, labels = c("Tobacco", "Alcohol", "Unhealthy\ndiet", "Physical\ninactivity", "PHI"), position = "top",
                       sec.axis = dup_axis(breaks = 1, labels = c(""))) +
    scale_y_discrete(labels = function(x) {
      ifelse(x == "Average", expression(bold("Average")), x)
      }) +
    scale_discrete_manual(values = c("plain", "bold"), aesthetics = "fontface", guide = "none") + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(face = "bold")) 

fig1 <- grid.arrange(apanel, bpanel, widths = c(1,1))

ggsave("figures/Fig2_PHIgesamt_V2.png", fig1, width = 12, height = 5, dpi = 300, bg = "white")

# Fig 3) % by policy domain

# % in scores by domain

pdat_domain <- pdat %>% 
  group_by(country, subscale, domain) %>% 
  summarise(value = sum(value, na.rm = T),
            maxvalue = sum(maxvalue)) %>%
  group_by(country, domain) %>%
  mutate(prop = ifelse(domain == "pricing" | domain == "marketing" | domain == "other", 
                       (value/maxvalue) / 3, (value/maxvalue) / 4)) %>%
  # zvalue = (svalue - mean(svalue, na.rm = T)) / sd(svalue, na.rm = T)) %>% 
  ungroup()  %>%
  mutate(domain = factor(domain, levels = c("pricing", "living environment", "marketing", "health communication", "other")),
         subscale = factor(subscale, levels = c("Public Health Index", "Physical inactivity", "Unhealthy diet", "Alcohol", "Tobacco")),
         country = factor(country, levels = order))

fig2a <- ggplot(pdat_domain) + 
  geom_bar(aes(y = country, x = prop, fill = subscale), stat = "identity", position = "stack") + 
  facet_wrap(~ domain, ncol = 5) +
  #geom_vline(xintercept = 1, color = "grey50") +
  scale_fill_manual(values = c('#a1dab4','#41b6c4','#2c7fb8','#253494'), name = "") + 
  scale_x_continuous(labels = scales::percent) + 
  guides(fill = guide_legend(reverse = T)) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom")

not_used <- ggplot(pdat_domain) + 
  geom_bar(aes(y = fct_rev(domain), x = prop, fill = subscale), stat = "identity", position = "stack") + 
  facet_wrap(~ fct_rev(country), ncol = 3) +
  #geom_vline(xintercept = 1, color = "grey50") +
  scale_fill_manual(values = c('#a1dab4','#41b6c4','#2c7fb8','#253494'), name = "") + 
  scale_x_continuous(labels = scales::percent) + 
  guides(fill = guide_legend(reverse = T)) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "right")
legend <- cowplot::get_legend(not_used)

# wide format including country ranking

pdat_domain_wide <- pdat_domain %>% 
  pivot_wider(id_cols = c("country", "subscale"), names_from = "domain", values_from = "prop")

PRICE <- ggplot(pdat_domain_wide[!is.na(pdat_domain_wide$pricing),]) + 
  geom_bar(aes(y = reorder(country, pricing), x = pricing, fill = subscale), stat = "identity", position = "stack") + 
  scale_fill_manual(values = c('#41b6c4','#2c7fb8','#253494'), name = "") + 
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) + 
  ggtitle("Pricing") +
  guides(fill = guide_legend(reverse = T)) +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 12, color = "black", family = "Times New Roman", face = "bold"),
        legend.position = "none")

MARKT <- ggplot(pdat_domain_wide[!is.na(pdat_domain_wide$marketing),]) + 
  geom_bar(aes(y = reorder(country, marketing), x = marketing, fill = subscale), stat = "identity", position = "stack") + 
  scale_fill_manual(values = c('#41b6c4','#2c7fb8','#253494'), name = "") + 
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) + 
  ggtitle("Marketing") +
  guides(fill = guide_legend(reverse = T)) +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 12, color = "black", family = "Times New Roman", face = "bold"),
        legend.position = "none")

ENVIR <- ggplot(pdat_domain_wide) + 
  geom_bar(aes(y = reorder(country, `living environment`), x = `living environment`, fill = subscale), stat = "identity", position = "stack") + 
  scale_fill_manual(values = c('#a1dab4','#41b6c4','#2c7fb8','#253494'), name = "") + 
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) + 
  ggtitle("Living environment") +
  guides(fill = guide_legend(reverse = T)) +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 12, color = "black", family = "Times New Roman", face = "bold"),
        legend.position = "none")

HCOM <- ggplot(pdat_domain_wide) + 
  geom_bar(aes(y = reorder(country, `health communication`), x = `health communication`, fill = subscale), stat = "identity", position = "stack") + 
  scale_fill_manual(values = c('#a1dab4','#41b6c4','#2c7fb8','#253494'), name = "") + 
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) + 
  ggtitle("Health communication") +
  guides(fill = guide_legend(reverse = T)) +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 12, color = "black", family = "Times New Roman", face = "bold"),
        legend.position = "none")

OTH <- ggplot(pdat_domain_wide[!is.na(pdat_domain_wide$other),]) + 
  geom_bar(aes(y = reorder(country, other), x = other, fill = subscale), stat = "identity", position = "stack") + 
  scale_fill_manual(values = c('#a1dab4','#2c7fb8','#253494'), name = "") + 
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) + 
  ggtitle("Other") +
  guides(fill = guide_legend(reverse = T)) +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 12, color = "black", family = "Times New Roman", face = "bold"),
        legend.position = "none")

R1 <- grid.arrange(PRICE, ENVIR, MARKT, widths = c(1,1,1))
R2 <- grid.arrange(HCOM, OTH, legend, widths = c(1,1,1))

fig2b <- grid.arrange(R1, R2, nrow = 2, heights = c(1,1))

#ggplot(pdat_domain[pdat_domain$subscale != "Public Health Index",]) + 
#  geom_bar(aes(y = subscale, x = zvalue, fill = domain), stat = "identity", position = "stack") + 
#  geom_vline(aes(xintercept = 0)) + 
#  scale_fill_manual(values = c("#E4C287", "#DB8580", "#8F74C6", "#0A2F51", "#52B382"), name = "Policy domain") +
#  guides(fill = guide_legend(reverse = T)) 

ggsave("figures/Fig3_PolicyContribution.png", fig2a, width = 16, height = 6, dpi = 300, bg = "white")
ggsave("figures/Fig3_PolicyContribution_V2.png", fig2b, width = 16, height = 6, dpi = 300, bg = "white")
