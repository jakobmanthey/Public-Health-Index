# PHI 
# Data visualisation
# Carolin Kilian
# 10-11-2025

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(paletteer)
library(openxlsx)

# Load data
rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Research projects/Public Health Index/Github/Public-Health-Index/")
DATE <- 20260914

data <- readRDS("data/phi_preprocessed.rds") 
phi <- readRDS("data/final_subscale_phi.rds") 

# functions
round_xlsx <- function(x, n) {
  sign(x) * trunc(abs(x) * 10^n + 0.5) / 10^n
}

# set ggplot standard theme

theme_set(theme_minimal() + 
            theme(axis.text = element_text(size = 12, color = "black", family = "Times New Roman"),
                  strip.text = element_text(size = 12, color = "black", family = "Times New Roman"),
                  legend.text = element_text(size = 10, color = "black", family = "Times New Roman"),
                  legend.title = element_text(size = 10, color = "black", family = "Times New Roman"),
                  axis.title = element_blank()))

# SA1: exclude PA in overall PHI score
phi_noPA <- data %>% filter(subscale != "Physical inactivity") %>%
  group_by(country, subscale) %>% 
  summarise(svalue = round_xlsx(sum(svalue, na.rm = T) / 3, 0)) %>% ungroup() %>% 
  pivot_wider(id_cols = country, names_from = subscale, values_from = svalue) %>%
  mutate(`Public Health Index` = Alcohol + Tobacco + `Unhealthy dietary patterns`)
phi_noPA %>% arrange(-`Public Health Index`)

# SA2: use weights based on contribution to NCD burden  
phi_NCDweight <- data %>% 
  group_by(country, subscale) %>% 
  summarise(svalue = round_xlsx(sum(svalue, na.rm = T), 0)) %>% 
  mutate(svalue = ifelse(subscale == "Physical inactivity", round_xlsx(svalue / 4, 0), svalue)) %>% 
  pivot_wider(id_cols = country, names_from = subscale, values_from = svalue) %>%
  mutate(`Public Health Index` = round_xlsx((0.039 / (0.102 + 0.091 + 0.039 + 0.01) * Alcohol) + 
                                 (0.102 / (0.102 + 0.091 + 0.039 + 0.01) * Tobacco) + 
                                 (0.091 / (0.102 + 0.091 + 0.039 + 0.01) * `Unhealthy dietary patterns`) + 
                                 (0.01 / (0.102 + 0.091 + 0.039 + 0.01) * `Physical inactivity`), 0))
phi_NCDweight %>% arrange(-`Public Health Index`)

# Join different solutions 
phi_alt <- phi %>% filter(subscale == "Public Health Index") %>% select(country, svalue) %>% rename("EqualWeight" = "svalue") %>% 
  left_join(phi_noPA[,c("country", "Public Health Index")]) %>% rename("NoPA" = "Public Health Index") %>% 
  left_join(phi_NCDweight[,c("country", "Public Health Index")]) %>% rename("NCDweight" = "Public Health Index") 

# 1) Bump diagram  to illustrate scoring

temp <- phi_alt %>%  
  pivot_longer(cols = c("EqualWeight",  "NoPA", "NCDweight"), 
               names_to = "measure", values_to = "score") %>%
  mutate(x = case_when(
    measure == "EqualWeight" ~ 1,
    measure == "NCDweight" ~ 2,
    measure == "NoPA" ~ 3,
    TRUE ~ NA)) 
order <- temp %>% filter(x == max(x)) %>% 
  arrange(-score) %>% pull(country)   
colors <- as.vector(paletteer_c("grDevices::Zissou 1", 18))
names(colors) <- order 

ggplot(temp, aes(x = x, y = score, group = country, color = country)) +
  geom_line(linewidth = 0.5) +  
  geom_point(size = 3, shape = 18) +      
  geom_text_repel(data = temp %>% filter(measure != "InsuffPol") %>% filter(x == max(x)),  
                  aes(x = x, y = score, label = country),
                  direction = "y", nudge_x = 0.5,
                  segment.color = NA, box.padding = 0.5, size = 3, show.legend = FALSE) +
  scale_x_continuous(breaks = c(1,2,3), 
                     labels = c("Equal weight\n(original)", "NCD impact\nweight", "Physical acitivty\nexcluded")) + 
  scale_color_manual(values = colors) + 
  labs(x = "", y = "Public Health Index") + guides(color = "none") + 
  theme_classic() + theme(axis.ticks.x = element_blank())

# export
ggsave("figures/FigSA1_PHI.tiff", width = 6, height = 8, dpi = 300, bg = "white")


# 2) country ranking in arrow diagram

temp <- phi_alt %>%
  mutate(EqualWeight = rank(-EqualWeight, ties.method = "min"),
         NCDweight = rank(-NCDweight, ties.method = "min"),
         NoPA = rank(-NoPA, ties.method = "min")) %>% 
  pivot_longer(cols = c("EqualWeight", "NCDweight", "NoPA"),
               names_to = "measure", values_to = "rank") %>%
  mutate(measure_num = case_when(
    measure == "EqualWeight" ~ 1,
    measure == "NCDweight" ~ 2, 
    measure == "NoPA" ~ 3, 
    TRUE ~ NA)) 

temp_segments <- temp %>% 
  arrange(country, measure_num) %>%
  group_by(country) %>%
  mutate(x = measure_num, y = rank, xend = lead(measure_num), yend = lead(rank)) %>%
  drop_na()

# Step 2: Plot with arrows, points, and right-side labels
ggplot() +
  # Arrows between consecutive years
  geom_segment(data = temp_segments,
    aes(x = x, y = y, xend = xend, yend = yend, color = country),
    linewidth = 0.8) +
  geom_point(
    data = temp, aes(x = measure_num, y = rank, color = country),
    size = 3, shape = 18) +
  geom_text_repel(data = temp %>% filter(measure_num == min(measure_num)),  
                  aes(x = measure_num, y = rank, label = country, colour = country),
                  direction = "y", nudge_x = 2.5,
                  segment.color = NA, box.padding = 0.5, size = 3, show.legend = FALSE) +
  scale_x_continuous(breaks = c(1,2,3), 
                     labels = c("Equal weight\n(original)", "NCD impact\nweight", "Physical acitivty\nexcluded")) +
  scale_y_reverse(breaks = 1:18) + 
  scale_color_manual(values = colors) +  
  labs(x = "", y = "Country ranking") + guides(color = "none") + 
  theme_classic() + theme(axis.ticks = element_blank())

# export
ggsave("figures/FigSA2_PHIranking.tiff", width = 5, height = 7, dpi = 300, bg = "white")

# save appendix table
write.xlsx(phi_alt, "tables/Supp_Tables_PHI-SensAnalyse.xlsx")

# get Spearman rank correlations
cor.test(phi_alt$EqualWeight, phi_alt$NCDweight, method = 'spearman', exact = FALSE)
cor.test(phi_alt$EqualWeight, phi_alt$NoPA, method = 'spearman', exact = FALSE)
