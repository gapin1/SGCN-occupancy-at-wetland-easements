# Visualize results from occupancy analyses from article titled "Stable occupancy
# of conservation-priority birds amid community shifts across 16 years on Iowa 
# wetland easements" by Gapinski et al. 2025 published in Ornitholocial Applications

# Code author: Lindsey A. W. Gapinski

# load packages ----
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot(font_size = 12))
library(patchwork)
library(gridExtra)
library(ubms)

# load data ----
covs_combined <- read.csv("data/covs_combined.csv")

# load top models ----
# here we read in models we have already run; to run models you have run, change
# the folder path from "top_models" to "model_output"
baor0 <- readRDS("top_models/Baltimore oriole_fit0.rds")
brth0 <- readRDS("top_models/Brown thrasher_fit0.rds")
grsp0 <- readRDS("top_models/Grasshopper sparrow_fit0.rds")
upsa0 <- readRDS("top_models/Upland sandpiper_fit0.rds")
ambi0 <- readRDS("top_models/American bittern_fit0.rds")
beki0 <- readRDS("top_models/Belted kingfisher_fit0.rds")
bwte0 <- readRDS("top_models/Blue-winged teal_fit0.rds")
dick4 <- readRDS("top_models/Dickcissel_fit4.rds")
eawp0 <- readRDS("top_models/Eastern wood-pewee_fit0.rds")
fisp4 <- readRDS("top_models/Field sparrow_fit4.rds")
hesp0 <- readRDS("top_models/Henslow's sparrow_fit0.rds")
nofl0 <- readRDS("top_models/Northern flicker_fit0.rds")
weme0 <- readRDS("top_models/Western meadowlark_fit0.rds")
blte0 <- readRDS("top_models/Black tern_fit0.rds")
bobo4 <- readRDS("top_models/Bobolink_fit4.rds")
eaki0 <- readRDS("top_models/Eastern kingbird_fit0.rds")
eame0 <- readRDS("top_models/Eastern meadowlark_fit0.rds")
rhwo0 <- readRDS("top_models/Red-headed woodpecker_fit0.rds")
sewr0 <- readRDS("top_models/Sedge wren_fit0.rds")
ybcu0 <- readRDS("top_models/Yellow-billed cuckoo_fit0.rds")


# Prepare to graph ----
# extract posteriors and quantiles
## baor ----
zpost_baor <- posterior_predict(baor0, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_baor[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_baor[,77:180], na.rm=TRUE)

baor_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "Baltimore Oriole"))

## brth ----
zpost_brth <- posterior_predict(brth0, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_brth[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_brth[,77:180], na.rm=TRUE)

brth_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "Brown Thrasher"))

## grsp ----
zpost_grsp <- posterior_predict(grsp0, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_grsp[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_grsp[,77:180], na.rm=TRUE)

grsp_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "Grasshopper Sparrow"))

## upsa ----
zpost_upsa <- posterior_predict(upsa0, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_upsa[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_upsa[,77:180], na.rm=TRUE)

upsa_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "Upland Sandpiper"))

## beki ----
zpost_beki <- posterior_predict(beki0, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_beki[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_beki[,77:180], na.rm=TRUE)

beki_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "Belted Kingfisher"))


## bwte ----
zpost_bwte <- posterior_predict(bwte0, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_bwte[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_bwte[,77:180], na.rm=TRUE)

bwte_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "Blue-winged Teal"))

## dick ----
zpost_dick <- posterior_predict(dick4, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_dick[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_dick[,77:180], na.rm=TRUE)

dick_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "Dickcissel"))

## eawp ----
zpost_eawp <- posterior_predict(eawp0, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_eawp[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_eawp[,77:180], na.rm=TRUE)

eawp_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "Eastern Wood-Pewee"))

## fisp ----
zpost_fisp <- posterior_predict(fisp4, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_fisp[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_fisp[,77:180], na.rm=TRUE)

fisp_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "Field Sparrow"))

## hesp ----
zpost_hesp <- posterior_predict(hesp0, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_hesp[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_hesp[,77:180], na.rm=TRUE)

hesp_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "Henslow's Sparrow"))

## nofl ----
zpost_nofl <- posterior_predict(nofl0, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_nofl[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_nofl[,77:180], na.rm=TRUE)

nofl_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "Northern Flicker"))

## weme ----
zpost_weme <- posterior_predict(weme0, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_weme[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_weme[,77:180], na.rm=TRUE)

weme_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "Western Meadowlark"))

## blte ----
zpost_blte <- posterior_predict(blte0, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_blte[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_blte[,77:180], na.rm=TRUE)

blte_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "Black Tern"))

## bobo ----
zpost_bobo <- posterior_predict(bobo4, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_bobo[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_bobo[,77:180], na.rm=TRUE)

bobo_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "Bobolink"))

## eaki ----
zpost_eaki <- posterior_predict(eaki0, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_eaki[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_eaki[,77:180], na.rm=TRUE)

eaki_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "Eastern Kingbird"))

## eame ----
zpost_eame <- posterior_predict(eame0, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_eame[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_eame[,77:180], na.rm=TRUE)

eame_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "Eastern Meadowlark"))

## rhwo ----
zpost_rhwo <- posterior_predict(rhwo0, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_rhwo[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_rhwo[,77:180], na.rm=TRUE)

rhwo_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "Red-headed Woodpecker"))

## sewr ----
zpost_sewr <- posterior_predict(sewr0, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_sewr[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_sewr[,77:180], na.rm=TRUE)

sewr_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "Sedge Wren"))

## ybcu ----
zpost_ybcu <- posterior_predict(ybcu0, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_ybcu[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_ybcu[,77:180], na.rm=TRUE)

ybcu_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "Yellow-billed Cuckoo"))

## ambi ----
zpost_ambi <- posterior_predict(ambi0, "z", draws=6000)
"2007-2009" <- rowMeans(zpost_ambi[,1:76], na.rm=TRUE)
"2022-2023" <- rowMeans(zpost_ambi[,77:180], na.rm=TRUE)

ambi_plot <- rbind(data.frame(occ_diff=mean(`2022-2023`-`2007-2009`),
                               low50=quantile(`2022-2023`-`2007-2009`, 0.25),
                               upper50=quantile(`2022-2023`-`2007-2009`, 0.75),
                               lower=quantile(`2022-2023`-`2007-2009`, 0.025),
                               upper=quantile(`2022-2023`-`2007-2009`, 0.975),
                               species = "American Bittern"))

## Combine species ----
all_species <- rbind(baor_plot, brth_plot, grsp_plot, upsa_plot, beki_plot,
                     bwte_plot, dick_plot, eawp_plot, fisp_plot, hesp_plot,
                     nofl_plot, weme_plot, blte_plot, bobo_plot, eaki_plot, eame_plot,
                     rhwo_plot, sewr_plot, ybcu_plot, ambi_plot)


# Graph ----
## Change in occupancy ----
all_species$Guild <- c("Forest", "Shrubland", "Grassland", "Grassland", "Wetland",
                        "Wetland", "Grassland", "Forest", "Shrubland", "Grassland",
                        "Open woodland", "Grassland", "Wetland", "Grassland", "Grassland",
                        "Grassland", "Open woodland", "Grassland", "Open woodland", "Wetland")

all_species <- all_species %>%
  arrange(Guild, occ_diff)

all_species$order <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)

fig4 <- all_species %>% 
  mutate(species = fct_reorder(species, -order)) %>%
  ggplot(aes(x = species, col = `Guild`)) +
  geom_linerange(aes(ymin = low50, ymax = upper50), lwd = 1.2) +
  geom_linerange(aes(ymin = lower, ymax = upper), lwd = 1.2, alpha = 0.5) +
  geom_point(aes(y = occ_diff), size = 2.7) +
  ylim(-0.65,0.65) +
  coord_flip() +
  labs(x = NULL, y = 'Change in Occupancy Probability') +
  theme(axis.text = element_text(size = 11), plot.margin = unit(c(0,0,0,0), "pt")) +
  scale_color_manual(values = c("#117733", "#E69F00", "#44AA99", "#882255", "#332288")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(breaks = round(seq(-0.6, 0.6, by = 0.2), 1))
fig4

## Woody veg ----
covs_combined %>%
  ggplot(aes(x = period)) +
  geom_boxplot(aes(y = percent_woody), width = 0.5, fill = "gray", outlier.size = 2.5) +
  ylab("Percent woody vegetation") +
  xlab("Period") +
  theme(legend.position = "none", axis.text = element_text(size = 12))

## Effects ----
# Forest plot of percent woody betas and easement age betas
baor <- summary(baor0, "state")
baor_wood <- data.frame(baor[3,], species = "Baltimore Oriole")

brth <- summary(brth0, "state")
brth_wood <- data.frame(brth[3,], species = "Brown Thrasher")

grsp <- summary(grsp0, "state")
grsp_wood <- data.frame(grsp[3,], species = "Grasshopper Sparrow")

upsa <- summary(upsa0, "state")
upsa_wood <- data.frame(upsa[3,], species = "Upland Sandpiper")

beki <- summary(beki0, "state")
beki_wood <- data.frame(beki[3,], species = "Belted Kingfisher")

bwte <- summary(bwte0, "state")
bwte_wood <- data.frame(bwte[3,], species = "Blue-winged Teal")

dick <- summary(dick4, "state")
dick_wood <- data.frame(dick[6,], species = "Dickcissel")

eawp <- summary(eawp0, "state")
eawp_wood <- data.frame(eawp[3,], species = "Eastern Wood-Pewee")

fisp <- summary(fisp4, "state")
fisp_wood <- data.frame(fisp[6,], species = "Field Sparrow")

hesp <- summary(hesp0, "state")
hesp_wood <- data.frame(hesp[3,], species = "Henslow's Sparrow")

nofl <- summary(nofl0, "state")
nofl_wood <- data.frame(nofl[3,], species = "Northern Flicker")

weme <- summary(weme0, "state")
weme_wood <- data.frame(weme[3,], species = "Western Meadowlark")

blte <- summary(blte0, "state")
blte_wood <- data.frame(blte[3,], species = "Black Tern")

bobo <- summary(bobo4, "state")
bobo_wood <- data.frame(bobo[6,], species = "Bobolink")

eaki <- summary(eaki0, "state")
eaki_wood <- data.frame(eaki[3,], species = "Eastern Kingbird")

eame <- summary(eame0, "state")
eame_wood <- data.frame(eame[3,], species = "Eastern Meadowlark")

rhwo <- summary(rhwo0, "state")
rhwo_wood <- data.frame(rhwo[3,], species = "Red-headed Woodpecker")

sewr <- summary(sewr0, "state")
sewr_wood <- data.frame(sewr[3,], species = "Sedge Wren")

ybcu <- summary(ybcu0, "state")
ybcu_wood <- data.frame(ybcu[3,], species = "Yellow-billed Cuckoo")

ambi <- summary(ambi0, "state")
ambi_wood <- data.frame(ambi[3,], species = "American Bittern")

bird_wood <- bind_rows(baor_wood, brth_wood, grsp_wood, upsa_wood, beki_wood, 
                       bwte_wood, dick_wood, eawp_wood, fisp_wood,
                       hesp_wood, nofl_wood, weme_wood, blte_wood, bobo_wood,
                       eaki_wood, eame_wood, rhwo_wood, sewr_wood, ybcu_wood,
                       ambi_wood)

wood_plot <- bird_wood %>% 
  ggplot(aes(x = reorder(species, mean, decreasing = TRUE))) +
  geom_linerange(aes(ymin = X2.5., ymax = X97.5.), lwd = 1.2, alpha = 0.4) +
  geom_linerange(aes(ymin = X25., ymax = X75.), lwd = 1.2) +
  geom_point(aes(y = mean), size = 2) +
  coord_flip() +
  labs(x = NULL, y = 'Effect of woody vegetation') +
  theme(axis.text = element_text(size = 10), plot.margin = unit(c(0,0,0,0), "pt")) +
  geom_hline(yintercept = 0, linetype = "dashed")
wood_plot


# Forest plot of easement age
baor <- summary(baor0, "state")
baor_age <- data.frame(baor[4,], species = "Baltimore Oriole")

brth <- summary(brth0, "state")
brth_age <- data.frame(brth[4,], species = "Brown Thrasher")

grsp <- summary(grsp0, "state")
grsp_age <- data.frame(grsp[4,], species = "Grasshopper Sparrow")

upsa <- summary(upsa0, "state")
upsa_age <- data.frame(upsa[4,], species = "Upland Sandpiper")

beki <- summary(beki0, "state")
beki_age <- data.frame(beki[3,], species = "Belted Kingfisher")

bwte <- summary(bwte0, "state")
bwte_age <- data.frame(bwte[4,], species = "Blue-winged Teal")

dick <- summary(dick4, "state")
dick_age <- data.frame(dick[7,], species = "Dickcissel")

eawp <- summary(eawp0, "state")
eawp_age <- data.frame(eawp[4,], species = "Eastern Wood-Pewee")

fisp <- summary(fisp4, "state")
fisp_age <- data.frame(fisp[7,], species = "Field Sparrow")

hesp <- summary(hesp0, "state")
hesp_age <- data.frame(hesp[4,], species = "Henslow's Sparrow")

nofl <- summary(nofl0, "state")
nofl_age <- data.frame(nofl[4,], species = "Northern Flicker")

weme <- summary(weme0, "state")
weme_age <- data.frame(weme[4,], species = "Western Meadowlark")

blte <- summary(blte0, "state")
blte_age <- data.frame(blte[4,], species = "Black Tern")

bobo <- summary(bobo4, "state")
bobo_age <- data.frame(bobo[7,], species = "Bobolink")

eaki <- summary(eaki0, "state")
eaki_age <- data.frame(eaki[4,], species = "Eastern Kingbird")

eame <- summary(eame0, "state")
eame_age <- data.frame(eame[4,], species = "Eastern Meadowlark")

rhwo <- summary(rhwo0, "state")
rhwo_age <- data.frame(rhwo[4,], species = "Red-headed Woodpecker")

sewr <- summary(sewr0, "state")
sewr_age <- data.frame(sewr[4,], species = "Sedge Wren")

ybcu <- summary(ybcu0, "state")
ybcu_age <- data.frame(ybcu[4,], species = "Yellow-billed Cuckoo")

ambi <- summary(ambi0, "state")
ambi_age <- data.frame(ambi[4,], species = "American Bittern")

bird_age <- bind_rows(baor_age, brth_age, grsp_age, upsa_age, beki_age, 
                      bwte_age, dick_age, eawp_age, fisp_age,
                      hesp_age, nofl_age, weme_age, blte_age, bobo_age,
                      eaki_age, eame_age, rhwo_age, sewr_age, ybcu_age,
                      ambi_age)

bird_age$order <- c(20,9,6,1,16,10,8,14,17,11,15,4,2,5,12,7,19,3,18,13)

age_plot <- bird_age%>%
  ggplot(aes(x = reorder(species, order, decreasing = TRUE))) +
  geom_linerange(aes(ymin = X2.5., ymax = X97.5.), lwd = 1.2, alpha = 0.4) +
  geom_linerange(aes(ymin = X25., ymax = X75.), lwd = 1.2) +
  geom_point(aes(y = mean), size = 2) +
  coord_flip() +
  labs(x = NULL, y = 'Effect of easement age') +
  theme(axis.text = element_text(size = 10), plot.margin = unit(c(0,0,0,0), "pt"),
        axis.text.y = element_blank()) +
  geom_hline(yintercept = 0, linetype = "dashed")
age_plot

fig5 <- wood_plot + age_plot
fig5


## Interaction of age:period ----
baor <- summary(baor0, "state")
baor_int <- data.frame(baor[5,], species = "Baltimore Oriole")

brth <- summary(brth0, "state")
brth_int <- data.frame(brth[5,], species = "Brown Thrasher")

grsp <- summary(grsp0, "state")
grsp_int <- data.frame(grsp[5,], species = "Grasshopper Sparrow")

upsa <- summary(upsa0, "state")
upsa_int <- data.frame(upsa[5,], species = "Upland Sandpiper")

beki <- summary(beki0, "state")
beki_int <- data.frame(beki[3,], species = "Belted Kingfisher")

bwte <- summary(bwte0, "state")
bwte_int <- data.frame(bwte[5,], species = "Blue-winged Teal")

dick <- summary(dick4, "state")
dick_int <- data.frame(dick[8,], species = "Dickcissel")

eawp <- summary(eawp0, "state")
eawp_int <- data.frame(eawp[5,], species = "Eastern Wood-Pewee")

fisp <- summary(fisp4, "state")
fisp_int <- data.frame(fisp[8,], species = "Field Sparrow")

hesp <- summary(hesp0, "state")
hesp_int <- data.frame(hesp[5,], species = "Henslow's Sparrow")

nofl <- summary(nofl0, "state")
nofl_int <- data.frame(nofl[5,], species = "Northern Flicker")

weme <- summary(weme0, "state")
weme_int <- data.frame(weme[5,], species = "Western Meadowlark")

blte <- summary(blte0, "state")
blte_int <- data.frame(blte[5,], species = "Black Tern")

bobo <- summary(bobo4, "state")
bobo_int <- data.frame(bobo[8,], species = "Bobolink")

eaki <- summary(eaki0, "state")
eaki_int <- data.frame(eaki[5,], species = "Eastern Kingbird")

eame <- summary(eame0, "state")
eame_int <- data.frame(eame[5,], species = "Eastern Meadowlark")

rhwo <- summary(rhwo0, "state")
rhwo_int <- data.frame(rhwo[5,], species = "Red-headed Woodpecker")

sewr <- summary(sewr0, "state")
sewr_int <- data.frame(sewr[5,], species = "Sedge Wren")

ybcu <- summary(ybcu0, "state")
ybcu_int <- data.frame(ybcu[5,], species = "Yellow-billed Cuckoo")

ambi <- summary(ambi0, "state")
ambi_int <- data.frame(ambi[5,], species = "American Bittern")


bird_int <- bind_rows(baor_int, brth_int, grsp_int, upsa_int, beki_int, 
                      bwte_int, dick_int, eawp_int, fisp_int,
                      hesp_int, nofl_int, weme_int, blte_int, bobo_int,
                      eaki_int, eame_int, rhwo_int, sewr_int, ybcu_int,
                      ambi_int)

int_plot <- bird_int %>%
  ggplot(aes(x = reorder(species, mean, decreasing = TRUE))) +
  geom_linerange(aes(ymin = X2.5., ymax = X97.5.), lwd = 1.2, alpha = 0.4) +
  geom_linerange(aes(ymin = X25., ymax = X75.), lwd = 1.2) +
  geom_point(aes(y = mean), size = 2.5) +
  coord_flip() +
  labs(x = NULL, y = 'Effect of age:period') +
  theme(axis.text = element_text(size = 12)) +
  geom_hline(yintercept = 0, linetype = "dashed")
int_plot


## Woody veg species effects ----
nd <- data.frame(period=factor("07-09", levels=c("07-09","22-23")),
                 percent_woody=seq(min(baor0@data@siteCovs$percent_woody),max(baor0@data@siteCovs$percent_woody),length=100),
                 age_2007 = mean(baor0@data@siteCovs$age_2007), length = 100)

### baor ----
baor_wood <- predict(baor0, submodel="state", newdata=nd, re.form = NA)
baor_wood <- cbind(nd, baor_wood)

baor <- baor_wood %>% 
  ggplot(aes(x = percent_woody)) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.3) +
  geom_line(aes(y = Predicted), linewidth = 1) +
  ylim(0,1) +
  labs(x = str_wrap("Percent woody vegetation", width = 30), y = 'Occupancy probability') +
  theme(axis.text = element_text(size = 10), axis.title.y = element_text(vjust = 1.3), plot.margin = unit(c(0,0,0,3), "pt")) +
  ggtitle("a)")
baor

### sewr ----
sewr_wood <- predict(sewr0, submodel="state", newdata=nd, re.form = NA)
sewr_wood <- cbind(nd, sewr_wood)

sewr <- sewr_wood %>% 
  ggplot(aes(x = percent_woody)) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.3) +
  geom_line(aes(y = Predicted), linewidth = 1) +
  ylim(0,1) +
  labs(x = str_wrap("Percent woody vegetation", width = 30), y = 'Occupancy probability') +
  theme(axis.text = element_text(size = 10), axis.title.y = element_text(vjust = 1.3), plot.margin = unit(c(0,0,0,3), "pt")) +
  ggtitle("b)")
sewr

### eame ----
nd <- data.frame(period=factor("07-09", levels=c("07-09","22-23")),
                 percent_woody=seq(min(eame0@data@siteCovs$percent_woody),max(eame0@data@siteCovs$percent_woody),length=100),
                 age_2007 = mean(eame0@data@siteCovs$age_2007), length = 100)

eame_wood <- predict(eame0, submodel="state", newdata=nd, re.form = NA)
eame_wood <- cbind(nd, eame_wood)

eame <- eame_wood %>% 
  ggplot(aes(x = percent_woody)) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.3) +
  geom_line(aes(y = Predicted), linewidth = 1) +
  ylim(0,1) +
  labs(x = str_wrap("Percent woody vegetation", width = 30), y = 'Occupancy probability') +
  theme(axis.text = element_text(size = 10), axis.title.y = element_text(vjust = 1.3), plot.margin = unit(c(0,0,0,3), "pt")) +
  ggtitle("c)")
eame

fig6b <- grid.arrange(baor, sewr, eame, nrow = 3)


## Detection probability ----
nd <- data.frame(period=factor(c("07-09"), levels=c("07-09","22-23")),
                 date = mean(ambi0@data@obsCovs$date, na.rm = TRUE),
                 n_points = mean(ambi0@data@obsCovs$n_points, na.rm = TRUE))

baor_det <- predict(baor0, submodel="det", newdata=nd, re.form = NA)
brth_det <- predict(brth0, submodel="det", newdata=nd, re.form = NA)
grsp_det <- predict(grsp0, submodel="det", newdata=nd, re.form = NA)
upsa_det <- predict(upsa0, submodel="det", newdata=nd, re.form = NA)
beki_det <- predict(beki0, submodel="det", newdata=nd, re.form = NA)
bwte_det <- predict(bwte0, submodel="det", newdata=nd, re.form = NA)
dick_det <- predict(dick4, submodel="det", newdata=nd, re.form = NA)
eawp_det <- predict(eawp0, submodel="det", newdata=nd, re.form = NA)
fisp_det <- predict(fisp4, submodel="det", newdata=nd, re.form = NA)
hesp_det <- predict(hesp0, submodel="det", newdata=nd, re.form = NA)
nofl_det <- predict(nofl0, submodel="det", newdata=nd, re.form = NA)
weme_det <- predict(weme0, submodel="det", newdata=nd, re.form = NA)
blte_det <- predict(blte0, submodel="det", newdata=nd, re.form = NA)
bobo_det <- predict(bobo4, submodel="det", newdata=nd, re.form = NA)
eaki_det <- predict(eaki0, submodel="det", newdata=nd, re.form = NA)
eame_det <- predict(eame0, submodel="det", newdata=nd, re.form = NA)
rhwo_det <- predict(rhwo0, submodel="det", newdata=nd, re.form = NA)
sewr_det <- predict(sewr0, submodel="det", newdata=nd, re.form = NA)
ybcu_det <- predict(ybcu0, submodel="det", newdata=nd, re.form = NA)
ambi_det <- predict(ambi0, submodel="det", newdata=nd, re.form = NA)

all_det <- bind_rows(baor_det, brth_det, grsp_det, upsa_det, beki_det, bwte_det,
                     dick_det, eawp_det, fisp_det, hesp_det, nofl_det, weme_det,
                     blte_det, bobo_det, eaki_det, eame_det, rhwo_det, sewr_det,
                     ybcu_det, ambi_det)

all_det$species <- c("Baltimore Oriole", "Brown Thrasher", "Grasshopper Sparrow",
                     "Upland Sandpiper", "Belted Kingfisher", "Blue-winged Teal",
                     "Dickcissel", "Eastern Wood-Pewee", "Field Sparrow", "Henslow's Sparrow",
                     "Northern Flicker", "Western Meadowlark", "Black Tern", "Bobolink",
                     "Eastern Kingbird", "Eastern Meadowlark", "Red-headed Woodpecker",
                     "Sedge Wren", "Yellow-billed Cuckoo", "American Bittern")

detection <- all_det %>% 
  ggplot(aes(x = reorder(species, Predicted, decreasing = TRUE))) +
  geom_linerange(aes(ymin = `2.5%`, ymax = `97.5%`), lwd = 1) +
  geom_point(aes(y = Predicted), size = 2) +
  coord_flip() +
  ylim(0,1) +
  labs(x = NULL, y = 'Detection probability') +
  theme(axis.text = element_text(size = 10), plot.margin = unit(c(0,4,0,0), "pt"))
detection


# Beta estimates ----
# for supplementary table
## no clustering ----
coefs <- data.frame()
mods <- list(bwte0, ybcu0, upsa0, blte0, ambi0, beki0, rhwo0, nofl0, eaki0, eawp0,
             sewr0, brth0, grsp0, hesp0, eame0, weme0, baor0)

sp_order <- c(rep("bwte",6), rep("ybcu",6), rep("upsa",6), rep("blte",6), rep("ambi",6), 
              rep("beki",6), rep("rhwo",6), rep("nofl",6), rep("eaki",6),
              rep("eawp",6), rep("sewr",6), rep("brth",6), rep("grsp",6), rep("hesp",6),
              rep("eame",6), rep("weme",6),
              rep("baor",6))

for(i in mods){
  coef <- summary(i, submodel = "state")
  coefs <- rbind(coefs, coef)
}

coefs$sp <- sp_order

## with clustering ----
coefs2 <- data.frame()
mods2 <- list(fisp4, bobo4, dick4)

sp_order2 <- c(rep("fisp",9), rep("bobo",9), rep("dick",9))

for(i in mods2){
  coef <- summary(i, submodel = "state")
  coefs2 <- rbind(coefs2, coef)
}

coefs2$sp <- sp_order2


# Alpha estimates ----
Alphacoefs <- data.frame()
mods <- list(bwte0, ybcu0, upsa0, blte0, ambi0, beki0, rhwo0, nofl0, eaki0, eawp0,
             sewr0, brth0, grsp0, fisp4, hesp0, bobo4, eame0, weme0, baor0, dick4)

sp_order <- c(rep("bwte",4), rep("ybcu",4), rep("upsa",4), rep("blte",4), rep("ambi",4), 
              rep("beki",4), rep("rhwo",4), rep("nofl",4), rep("eaki",4),
              rep("eawp",4), rep("sewr",4), rep("brth",4), rep("grsp",4), rep("fisp",4),
              rep("hesp",4), rep("bobo",4),
              rep("eame",4), rep("weme",4),
              rep("baor",4), rep("dick", 4))

for(i in mods){
  coef <- summary(i, submodel = "det")
  Alphacoefs <- rbind(Alphacoefs, coef)
}

Alphacoefs$sp <- sp_order
