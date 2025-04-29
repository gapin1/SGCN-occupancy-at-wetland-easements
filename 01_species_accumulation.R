# Species accumulation curve analysis from article titled"Stable occupancy of
# conservation-priority birds amid community shifts across 16 years on Iowa 
# wetland easements" by Gapinski et al. 2025 published in Ornitholocial Applications

# Code author: Lindsey A. W. Gapinski


# Load libraries ----
library(tidyverse)
library(cowplot)
library(vegan)
theme_set(theme_cowplot(font_size = 10))
library(gridExtra)

# Read in data ----
birds_combined <- read.csv("bird_data.csv")

# Format data ----
# All species ----
all07 <- birds_combined %>%
  filter(year == "2007"|
           year == "2008"|
           year == "2009") %>%
  group_by(CommonName, PropName) %>%
  summarize(sum_birds = sum(sum_birds, na.rm = TRUE)) %>%
  pivot_wider(names_from = CommonName, values_from = sum_birds) %>%
  select(-PropName)

all22 <- birds_combined %>%
  filter(year == "2022"|
           year == "2023") %>%
  group_by(CommonName, PropName) %>%
  summarize(sum_birds = sum(sum_birds, na.rm = TRUE)) %>%
  pivot_wider(names_from = CommonName, values_from = sum_birds) %>%
  select(-PropName)

## shrubland ----
shrub07 <- birds_combined %>%
  filter(group == "shrub") %>%
  filter(year == "2007"|
           year == "2008"|
           year == "2009") %>%
  group_by(CommonName, PropName) %>%
  summarize(sum_birds = sum(sum_birds, na.rm = TRUE)) %>%
  pivot_wider(names_from = CommonName, values_from = sum_birds) %>%
  select(-PropName)

shrub22 <- birds_combined %>%
  filter(group == "shrub") %>%
  filter(year == "2022"|
           year == "2023") %>%
  group_by(CommonName, PropName) %>%
  summarize(sum_birds = sum(sum_birds, na.rm = TRUE)) %>%
  pivot_wider(names_from = CommonName, values_from = sum_birds) %>%
  select(-PropName)

## grassland ----
grass07 <- birds_combined %>%
  filter(group == "grassland") %>%
  filter(year == "2007"|
           year == "2008"|
           year == "2009") %>%
  group_by(CommonName, PropName) %>%
  summarize(sum_birds = sum(sum_birds, na.rm = TRUE)) %>%
  pivot_wider(names_from = CommonName, values_from = sum_birds) %>%
  select(-PropName)

grass22 <- birds_combined %>%
  filter(group == "grassland") %>%
  filter(year == "2022"|
           year == "2023") %>%
  group_by(CommonName, PropName) %>%
  summarize(sum_birds = sum(sum_birds, na.rm = TRUE)) %>%
  pivot_wider(names_from = CommonName, values_from = sum_birds) %>%
  select(-PropName)

## forest ----
forest07 <- birds_combined %>%
  filter(group == "forest") %>%
  filter(year == "2007"|
           year == "2008"|
           year == "2009") %>%
  group_by(CommonName, PropName) %>%
  summarize(sum_birds = sum(sum_birds, na.rm = TRUE)) %>%
  pivot_wider(names_from = CommonName, values_from = sum_birds) %>%
  select(-PropName)

forest22 <- birds_combined %>%
  filter(group == "forest") %>%
  filter(year == "2022"|
           year == "2023") %>%
  group_by(CommonName, PropName) %>%
  summarize(sum_birds = sum(sum_birds, na.rm = TRUE)) %>%
  pivot_wider(names_from = CommonName, values_from = sum_birds) %>%
  select(-PropName)

## wetland ----
wet07 <- birds_combined %>%
  filter(group == "wetland") %>%
  filter(year == "2007"|
           year == "2008"|
           year == "2009") %>%
  group_by(CommonName, PropName) %>%
  summarize(sum_birds = sum(sum_birds, na.rm = TRUE)) %>%
  pivot_wider(names_from = CommonName, values_from = sum_birds) %>%
  select(-PropName)

wet22 <- birds_combined %>%
  filter(group == "wetland") %>%
  filter(year == "2022"|
           year == "2023") %>%
  group_by(CommonName, PropName) %>%
  summarize(sum_birds = sum(sum_birds, na.rm = TRUE)) %>%
  pivot_wider(names_from = CommonName, values_from = sum_birds) %>%
  select(-PropName)

## open woodland ----
openwood07 <- birds_combined %>%
  filter(group == "open woodland") %>%
  filter(year == "2007"|
           year == "2008"|
           year == "2009") %>%
  group_by(CommonName, PropName) %>%
  summarize(sum_birds = sum(sum_birds, na.rm = TRUE)) %>%
  pivot_wider(names_from = CommonName, values_from = sum_birds) %>%
  select(-PropName)

openwood22 <- birds_combined %>%
  filter(group == "open woodland") %>%
  filter(year == "2022"|
           year == "2023") %>%
  group_by(CommonName, PropName) %>%
  summarize(sum_birds = sum(sum_birds, na.rm = TRUE)) %>%
  pivot_wider(names_from = CommonName, values_from = sum_birds) %>%
  select(-PropName)


# Species accumulation curves ----
## All species ----
### 2007-2009 ----
all_old <- specaccum(all07)

### 2022-2023 ----
all_new <- specaccum(all22)

to_plot <- data.frame(rich = all_old$richness,
                      upper = all_old$richness + 2*all_old$sd,
                      lower = all_old$richness - 2*all_old$sd,
                      Sites = all_old$sites,
                      period = "2007-2009")

to_plot2 <- data.frame(rich = all_new$richness,
                       upper = all_new$richness + 2*all_new$sd,
                       lower = all_new$richness - 2*all_new$sd,
                       Sites = all_new$sites,
                       period = "2022-2023")
plot_all <- bind_rows(to_plot, to_plot2)

a <- ggplot(plot_all, aes(x = Sites, y = rich, fill = period, col = period)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = 0) +
  ylim(0,120) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  labs(x = "Sites surveyed", y = "Species") +
  theme(legend.position = "none") +
  ggtitle("All species")

# Create plot to extract legend from later
a_legend <- ggplot(plot_all, aes(x = Sites, y = rich, fill = period, col = period)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = 0) +
  ylim(0,120) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  labs(x = "Sites surveyed", y = "Species", col = "Time period    ", fill = "Time period    ") +
  theme(legend.position = "bottom")

## By guild ----
### Shrub ----
#### 2007-2009 ----
spa_shrub_old <- specaccum(shrub07)

#### 2022-2023 ----
spa_shrub_new <- specaccum(shrub22)

shrub1 <- data.frame(rich = spa_shrub_old$richness,
                     upper = spa_shrub_old$richness + 2*spa_shrub_old$sd,
                     lower = spa_shrub_old$richness - 2*spa_shrub_old$sd,
                     Sites = spa_shrub_old$sites,
                     period = "2007-2009")
shrub2 <- data.frame(rich = spa_shrub_new$richness,
                     upper = spa_shrub_new$richness + 2*spa_shrub_new$sd,
                     lower = spa_shrub_new$richness - 2*spa_shrub_new$sd,
                     Sites = spa_shrub_new$sites,
                     period = "2022-2023")

shrub_plot <- bind_rows(shrub1, shrub2)
b <- ggplot(shrub_plot, aes(x = Sites, y = rich, fill = period, col = period)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = 0) +
  # ylim(0,10) +
  scale_y_continuous(breaks = c(0,5,10), limits = c(0,10)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  labs(x = "Sites surveyed", y = "Species") +
  theme(legend.position = "none") +
  ggtitle("Shrubland")


### Grassland ----
#### 2007-2009 ----
spa_grassland_old <- specaccum(grass07)

#### 2022-2023 ----
spa_grassland_new <- specaccum(grass22)

grassland1 <- data.frame(rich = spa_grassland_old$richness,
                         upper = spa_grassland_old$richness + 2*spa_grassland_old$sd,
                         lower = spa_grassland_old$richness - 2*spa_grassland_old$sd,
                         Sites = spa_grassland_old$sites,
                         period = "2007-2009")
grassland2 <- data.frame(rich = spa_grassland_new$richness,
                         upper = spa_grassland_new$richness + 2*spa_grassland_new$sd,
                         lower = spa_grassland_new$richness - 2*spa_grassland_new$sd,
                         Sites = spa_grassland_new$sites,
                         period = "2022-2023")

grassland_plot <- bind_rows(grassland1, grassland2)
c <- ggplot(grassland_plot, aes(x = Sites, y = rich, fill = period, col = period)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = 0) +
  ylim(0,25) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  labs(x = "Sites surveyed", y = "Species") +
  theme(legend.position = "none") +
  ggtitle("Grassland")


### Forest ----
#### 2007-2009 ----
spa_forest_old <- specaccum(forest07)

#### 2022-2023 ----
spa_forest_new <- specaccum(forest22)

forest1 <- data.frame(rich = spa_forest_old$richness,
                      upper = spa_forest_old$richness + 2*spa_forest_old$sd,
                      lower = spa_forest_old$richness - 2*spa_forest_old$sd,
                      Sites = spa_forest_old$sites,
                      period = "2007-2009")
forest2 <- data.frame(rich = spa_forest_new$richness,
                      upper = spa_forest_new$richness + 2*spa_forest_new$sd,
                      lower = spa_forest_new$richness - 2*spa_forest_new$sd,
                      Sites = spa_forest_new$sites,
                      period = "2022-2023")

forest_plot <- bind_rows(forest1, forest2)
# fix negative values... set them to 0
forest_plot[1,3] <- 0
forest_plot[2,3] <- 0
forest_plot[3,3] <- 0
forest_plot[56,3] <- 0

d <- ggplot(forest_plot, aes(x = Sites, y = rich, fill = period, col = period)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = 0) +
  ylim(0,30) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  labs(x = "Sites surveyed", y = "Species") +
  theme(legend.position = "none") +
  ggtitle("Forest")


### Wetland ----
#### 2007-2009 ----
spa_wetland_old <- specaccum(wet07)

#### 2022-2023 ----
spa_wetland_new <- specaccum(wet22)

wetland1 <- data.frame(rich = spa_wetland_old$richness,
                       upper = spa_wetland_old$richness + 2*spa_wetland_old$sd,
                       lower = spa_wetland_old$richness - 2*spa_wetland_old$sd,
                       Sites = spa_wetland_old$sites,
                       period = "2007-2009")
wetland2 <- data.frame(rich = spa_wetland_new$richness,
                       upper = spa_wetland_new$richness + 2*spa_wetland_new$sd,
                       lower = spa_wetland_new$richness - 2*spa_wetland_new$sd,
                       Sites = spa_wetland_new$sites,
                       period = "2022-2023")

wetland_plot <- bind_rows(wetland1, wetland2)

e <- ggplot(wetland_plot, aes(x = Sites, y = rich, fill = period, col = period)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = 0) +
  ylim(0,40) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  labs(x = "Sites surveyed", y = "Species") +
  theme(legend.position = "none") +
  ggtitle("Wetland")

### Open woodland ----
#### 2007-2009 ----
spa_openwood_old <- specaccum(openwood07)

#### 2022-2023 ----
spa_openwood_new <- specaccum(openwood22)

openwood1 <- data.frame(rich = spa_openwood_old$richness,
                        upper = spa_openwood_old$richness + 2*spa_openwood_old$sd,
                        lower = spa_openwood_old$richness - 2*spa_openwood_old$sd,
                        Sites = spa_openwood_old$sites,
                        period = "2007-2009")
openwood2 <- data.frame(rich = spa_openwood_new$richness,
                        upper = spa_openwood_new$richness + 2*spa_openwood_new$sd,
                        lower = spa_openwood_new$richness - 2*spa_openwood_new$sd,
                        Sites = spa_openwood_new$sites,
                        period = "2022-2023")

openwood_plot <- bind_rows(openwood1, openwood2)

f <- ggplot(openwood_plot, aes(x = Sites, y = rich, fill = period, col = period)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5, linetype = 0) +
  ylim(0,25) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  labs(x = "Sites surveyed", y = "Species") +
  ggtitle("Open woodland") +
  theme(legend.position = "none")

# Plot together ----
get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

legend <- get_only_legend(a_legend)
plots <- grid.arrange(a,b,c,d,e,f, nrow = 3)
grid.arrange(plots,legend, nrow = 2, heights = c(10,0.5))
