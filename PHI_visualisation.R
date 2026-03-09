# PHI 
# Data visualisation
# Carolin Kilian
# 10-11-2025

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(rnaturalearth)
library(rnaturalearthdata)

# Load data
rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Research projects/Public Health Index/Github/Public-Health-Index/")
DATE <- 20260225

data <- readRDS("data/phi_preprocessed.rds") 
phi <- readRDS("data/final_subscale_phi.rds") 

# set ggplot standard theme

theme_set(theme_minimal() + 
            theme(axis.text = element_text(size = 12, color = "black", family = "Times New Roman"),
                  strip.text = element_text(size = 12, color = "black", family = "Times New Roman"),
                  legend.text = element_text(size = 10, color = "black", family = "Times New Roman"),
                  legend.title = element_text(size = 10, color = "black", family = "Times New Roman"),
                  axis.title = element_blank()))

# Figure 1: Overall PHI map

# select only overall PHI score
phi_score <- phi %>% filter(subscale == "Public Health Index")

# map data onto geocodes
pdat_map <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  merge(., phi_score, by.x = c("sovereignt"), by.y = "country", all = T) %>% 
  filter(continent == "Europe")

# plot
ggplot(data = pdat_map) + 
  geom_sf(aes(fill = as.numeric(svalue), color = "black")) +
  scale_color_manual(values = c("black")) + 
  scale_fill_gradientn(colors = c("#a50026", "#f46d43", "#ffffbf", "#66bd63", "#006837"), 
                       limits = c(0,100), breaks = c(0,25,50,75,100), na.value = "white") +
  guides(color = F, fill = guide_legend(title = "Overall score\nPublic Health Index")) + 
  theme_void() + theme(legend.position = c(0, 0.22)) +
  coord_sf(xlim = c(-25,40), ylim = c(30,75), expand = FALSE) 

# export
ggsave("figures/Fig1_PHI_map.tiff", width = 8, height = 6, dpi = 300, bg = "white")


# Figure 2: Overall PHI Scoreing and by Subscale

# get average across scale
pdat_scale_mean <- phi %>% group_by(subscale) %>% 
  summarise(country = "Average",
            xcor = mean(xcor),
            svalue = mean(svalue),
            phi = 1)

# get vector to sort data
order <- phi %>% filter(subscale == "Public Health Index") %>% arrange(svalue) %>% pull(country)
order <- c("Average", order)

# add subscale averages to phi data
phi_mean <- phi %>% bind_rows(pdat_scale_mean) %>%
  mutate(country = factor(country, levels = order))

# plot panel A (table plot)
apanel <- 
  ggplot(phi_mean) + 
  geom_text(aes(y = country, x = xcor, label = paste(sprintf("%.1f", svalue)), fontface = factor(phi)), family = "Times New Roman") +
  scale_x_continuous(limits=c(0,4.5), breaks = 0.5:4.5, labels = c("Tobacco", "Alcohol", "Unhealthy\ndietary\npatterns", "Physical\ninactivity", "PHI"), position = "top",
                     sec.axis = dup_axis(breaks = 1, labels = c(""))) +
  scale_y_discrete(labels = function(x) {
    ifelse(x == "Average", expression(bold("Average")), x)
  }) +
  scale_discrete_manual(values = c("plain", "bold"), aesthetics = "fontface", guide = "none") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(face = "bold")) 

# get subset of data for bar plot
add_rows <- pdat_scale_mean %>% filter(subscale != "Public Health Index") %>% select(-phi)

# rearrange data for bar plot
pdat_scale_b <- phi %>% filter(subscale != "Public Health Index") %>% bind_rows(add_rows) %>% 
  mutate(country = factor(country, levels = order),
         svalue = ifelse(country == "Average", NA, svalue),
         subscale = factor(subscale, levels = c("Physical inactivity", "Unhealthy dietary patterns", "Alcohol", "Tobacco")))

# plot panel B (bar plot)
bpanel <- 
  ggplot(pdat_scale_b) + 
  geom_bar(aes(y = country, x = svalue, fill = subscale), stat = "identity", position = "stack") + 
  geom_vline(xintercept = 100, color = "grey50") +
  scale_fill_manual(values = c('#a1dab4','#41b6c4','#2c7fb8','#253494'), name = "") + 
  scale_x_continuous(limits = c(0,100), sec.axis = dup_axis(breaks = 1, labels = c("\n"))) + 
  scale_y_discrete(labels = NULL) +
  guides(fill = guide_legend(reverse = T)) +
  theme(panel.grid.major.y = element_blank())

# combine panel A + B
fig_comb <- grid.arrange(apanel, bpanel, widths = c(1,1))

# export
ggsave("figures/Fig2_PHI_CountryRanking.tiff", fig_comb, width = 12, height = 5, dpi = 300, bg = "white")


# Figure 3: % by policy domain

# get % in scores by domain
pdat_domain <- data %>% 
  group_by(country, subscale, domain) %>% 
  summarise(value = sum(value, na.rm = T),
            maxvalue = sum(maxvalue)) %>%
  group_by(country, domain) %>%
  mutate(prop = ifelse(domain == "pricing" | domain == "marketing" | domain == "other", 
                       (value/maxvalue) / 3, (value/maxvalue) / 4)) %>%
  ungroup()  %>%
  mutate(domain = factor(domain, levels = c("pricing", "living environment", "marketing", "health communication", "other")),
         subscale = factor(subscale, levels = c("Public Health Index", "Physical inactivity", "Unhealthy dietary patterns", "Alcohol", "Tobacco")),
         country = factor(country, levels = order))

# wide format for country ranking
pdat_domain_wide <- pdat_domain %>% 
  pivot_wider(id_cols = c("country", "subscale"), names_from = "domain", values_from = "prop")

# plot price domain
PRICE <- ggplot(pdat_domain_wide[!is.na(pdat_domain_wide$pricing),]) + 
  geom_bar(aes(y = reorder(country, pricing), x = pricing, fill = subscale), stat = "identity", position = "stack") + 
  scale_fill_manual(values = c('#41b6c4','#2c7fb8','#253494'), name = "") + 
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) + 
  ggtitle("Pricing") +
  guides(fill = guide_legend(reverse = T)) +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 12, color = "black", family = "Times New Roman", face = "bold"),
        legend.position = "none")

# plot marketing domain
MARKT <- ggplot(pdat_domain_wide[!is.na(pdat_domain_wide$marketing),]) + 
  geom_bar(aes(y = reorder(country, marketing), x = marketing, fill = subscale), stat = "identity", position = "stack") + 
  scale_fill_manual(values = c('#41b6c4','#2c7fb8','#253494'), name = "") + 
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) + 
  ggtitle("Marketing") +
  guides(fill = guide_legend(reverse = T)) +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 12, color = "black", family = "Times New Roman", face = "bold"),
        legend.position = "none")

# plot living environment domain
ENVIR <- ggplot(pdat_domain_wide) + 
  geom_bar(aes(y = reorder(country, `living environment`), x = `living environment`, fill = subscale), stat = "identity", position = "stack") + 
  scale_fill_manual(values = c('#a1dab4','#41b6c4','#2c7fb8','#253494'), name = "") + 
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) + 
  ggtitle("Living environment") +
  guides(fill = guide_legend(reverse = T)) +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 12, color = "black", family = "Times New Roman", face = "bold"),
        legend.position = "none")

# plot health communication domain
HCOM <- ggplot(pdat_domain_wide) + 
  geom_bar(aes(y = reorder(country, `health communication`), x = `health communication`, fill = subscale), stat = "identity", position = "stack") + 
  scale_fill_manual(values = c('#a1dab4','#41b6c4','#2c7fb8','#253494'), name = "") + 
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) + 
  ggtitle("Health communication") +
  guides(fill = guide_legend(reverse = T)) +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 12, color = "black", family = "Times New Roman", face = "bold"),
        legend.position = "none")

# plot other domain
OTH <- ggplot(pdat_domain_wide[!is.na(pdat_domain_wide$other),]) + 
  geom_bar(aes(y = reorder(country, other), x = other, fill = subscale), stat = "identity", position = "stack") + 
  scale_fill_manual(values = c('#a1dab4','#2c7fb8','#253494'), name = "") + 
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) + 
  ggtitle("Other") +
  guides(fill = guide_legend(reverse = T)) +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 12, color = "black", family = "Times New Roman", face = "bold"),
        legend.position = "none")

# get joint legend
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

# combine
R1 <- grid.arrange(PRICE, ENVIR, MARKT, widths = c(1,1,1))
R2 <- grid.arrange(HCOM, OTH, legend, widths = c(1,1,1))

fig_comb <- grid.arrange(R1, R2, nrow = 2, heights = c(1,1))

# export
ggsave("figures/Fig3_PHI_PolicyContribution.tiff", fig_comb, width = 12, height = 8, dpi = 300, bg = "white")

