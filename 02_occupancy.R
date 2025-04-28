# Single-species occupancy analyses for Iowa Species of Greatest Conservation Need from
# article titled "Stable occupancy of conservation-priority birds amid community
# shifts across 16 years on Iowa wetland easements" by Gapinski et al. 2025 published
# in Ornithological Applications

# Code author: Lindsey A. W. Gapinski

# load packages ----
library(tidyverse)
library(unmarked)
library(ubms)

# load data ----
sgcn <- read.csv("bird_data.csv")
covs_combined <- read.csv("covs_combined.csv")


# occupancy analysis ----
# this will take ~5-20min per model to run (20 species x 3 models each)

# create empty data structures to hold output
modsel_df <- data.frame()

# vector to loop over
species = unique(sgcn$CommonName)

for(sp in species) {
  # Subset ----
  dat <- sgcn %>%
    filter(CommonName == sp) %>%
    pivot_wider(names_from = visit, values_from = sum_birds) %>%
    mutate(uniqueID = str_c(PropName, year, sep = "__")) %>%
    merge(covs_combined, by = "uniqueID") %>%
    arrange(year)
  
  ## Model ----
  ### 4 Clusters ----
  umf_stack <-
    unmarkedFrameOccu(
      y = dat[, 5:10],
      siteCovs = dat[, c(2, 4, 23:27), drop = F],
      obsCovs = list(
        date = dat[, 11:16],
        obs = dat[, 17:22],
        n_points = dat[,28:33]
      )
    )
  
  fit_4 <-
    stan_occu(
      ~ scale(date) + (1 | obs) + scale(n_points) ~ (1 | year) + period + Clusters4 + scale(percent_woody) +
        scale(age_2007) + scale(age_2007)*period,
      data = umf_stack,
      chains = 3,
      iter = 100000,
      warmup = 70000,
      thin = 15,
      cores = 3
    )
  
  ppc <- gof(fit_4, draws = 100)
  pval_4 <- ppc@post_pred_p
  
  ### 5 Clusters ----
  fit_5 <-
    stan_occu(
      ~ scale(date) + (1 | obs) + scale(n_points) ~ (1 | year) + period + Clusters5 + scale(percent_woody) +
        scale(age_2007) + scale(age_2007)*period,
      data = umf_stack,
      chains = 3,
      iter = 100000,
      warmup = 70000,
      thin = 15,
      cores = 3
    )
  
  ppc <- gof(fit_5, draws = 100)
  pval_5 <- ppc@post_pred_p
  
  
  ### No Clusters ----
  fit_0 <- stan_occu(
    ~ scale(date) + (1 | obs) + scale(n_points) ~ (1 | year) + period + scale(percent_woody) +
      scale(age_2007) + scale(age_2007)*period,
    data = umf_stack,
    chains = 3,
    iter = 100000,
    warmup = 70000,
    thin = 15,
    cores = 3
  )
  
  ppc <- gof(fit_0, draws = 100)
  pval_0 <- ppc@post_pred_p
  
  
  # Model fit ----
  mods <- modSel(fitList(fit_4, fit_5, fit_0))
  mods$model <- row.names(mods)
  mods$species <- sp
  
  # add bayesian p values
  mods <- arrange(mods, model)
  mods$pval <- c(pval_0, pval_4, pval_5)
  
  # save model selection outputs
  modsel_df <- rbind(modsel_df, mods)
  
  # save model files
  
  # create file names
  filename_0 <- paste0("model_output/", sp, "_fit0.rds")
  filename_4 <- paste0("model_output/", sp, "_fit4.rds")
  filename_5 <- paste0("model_output/", sp, "_fit5.rds")
  
  # save model outputs
  saveRDS(fit_0, file = filename_0)
  saveRDS(fit_4, file = filename_4)
  saveRDS(fit_5, file = filename_5)
  
}

# model selection ----
write.csv(modsel_df, "model_output/model_selection.csv", row.names = FALSE)


# Explore output ----
## Model selection ----
modsel_df <- read_csv("model_output/model_selection.csv")

# Check elpddiff/se; if greater than 2, model is significantly worse and should
# go with simplest model with fewest parameters
significance <- modsel_df %>%
  group_by(species, nparam, elpd_diff, se_diff, model) %>%
  summarize(lower = elpd_diff - (2*se_diff),
            upper = elpd_diff + (2*se_diff),
            sig = elpd_diff/se_diff)

## Read in top models ----
# here we read in models we have already run; to run models you have run, change
# the folder path from "top_models" to "model_output"
baor0 <- readRDS("Baltimore oriole_fit0.rds")
brth0 <- readRDS("Brown thrasher_fit0.rds")
grsp0 <- readRDS("Grasshopper sparrow_fit0.rds")
upsa0 <- readRDS("Upland sandpiper_fit0.rds")
ambi0 <- readRDS("American bittern_fit0.rds")
beki0 <- readRDS("Belted kingfisher_fit0.rds")
bwte0 <- readRDS("Blue-winged teal_fit0.rds")
dick4 <- readRDS("Dickcissel_fit4.rds")
eawp0 <- readRDS("Eastern wood-pewee_fit0.rds")
fisp4 <- readRDS("Field sparrow_fit4.rds")
hesp0 <- readRDS("Henslow's sparrow_fit0.rds")
nofl0 <- readRDS("Northern flicker_fit0.rds")
weme0 <- readRDS("Western meadowlark_fit0.rds")
blte0 <- readRDS("Black tern_fit0.rds")
bobo4 <- readRDS("Bobolink_fit4.rds")
eaki0 <- readRDS("Eastern kingbird_fit0.rds")
eame0 <- readRDS("Eastern meadowlark_fit0.rds")
rhwo0 <- readRDS("Red-headed woodpecker_fit0.rds")
sewr0 <- readRDS("Sedge wren_fit0.rds")
ybcu0 <- readRDS("Yellow-billed cuckoo_fit0.rds")

# Some species may need to be rerun if models did not converge

## Check model fit and convergence ----
# can do this for each species (we just show 1 example)
ambi0
loo(ambi0)

traceplot(ambi0, pars = c("beta_state", "beta_det"))

### Posterior predictive check ----
ppc <- gof(ambi0, draws = 100)
ppc