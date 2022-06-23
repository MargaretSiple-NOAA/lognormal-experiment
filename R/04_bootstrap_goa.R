############################################################################
# Bootstrap sampling from catch data- all using GOA as an example
# Author: Megsie Siple with much input from Cole Monnahan and Lewis Barnett
# Everything is calculated for 2021 unless written otherwise
#
# NOTE: If you don't have local versions of the RACEBASE tables, you will need them! 
# 1) Make sure your Oracle id and pw are updated
# 2) Download and run this script: https://github.com/afsc-gap-products/design-based-indices/blob/master/R/00_download_data_from_oracle.R
# You can do step (2) by cloning the whole repo or just putting that script in this folder and running it.
############################################################################

source("R/01_cleanup_data.R")

library(tidyverse)

options(dplyr.summarise.inform = FALSE) # silences verbose dplyr outputs

# Dir to store outputs
dir.create(paste0("outputs/", Sys.Date()))

# Preliminary stuff -------------------------------------------------------
# How many data points per stratum?
samplecounts <- haul %>%
  filter(region == "GOA" & abundance_haul == "Y") %>%
  group_by(cruise, stratum) %>%
  count() %>%
  ungroup()

summary(samplecounts)
# 1-77 abundance hauls per year per stratum (median of 10 hauls per stratum)

# Load functions ----------------------------------------------------------

# These scripts are all from afsc-gap-products/design-based-indices
source("R/02_get_cpue.R")
# source("R/03_get_biomass_stratum.R")
source("R/03_get_biomass_stratum_cpuein.R")
#source("R/04_get_biomass_total.R")

# Set up species, year, region for the bootstrapping ----------------------
species_in <- 10110 # Set species. POP = 30060, ATF = 10110
yr_in <- 2021
region_in <- "GOA"
nboots <- 1000
# no need to specify stratum

# Bootstrap at haul level -------------------------------------------------
# Main question: are stratum-level biomass estimates actually lognormally distributed? Bootstrap at haul scale within a stratum (?) to find out.

# Prep haul data
abund_hauls <- haul %>%
  filter(abundance_haul == "Y" &
    lubridate::year(start_time) == yr_in &
    region == region_in)


# Set up dataframe with bootstrap design ----------------------------------
# Size of bootstrap sample - 50% of total hauls
prop_boot <- 1
# (samplesize <- floor(nrow(abund_hauls) * prop_boot))


# Generate bootstrapped CPUE ----------------------------------------------
bootscpue <- list()

start.time <- Sys.time() # 13 sec for 1000 boots, 1.4 mins for the whole thing+cpue tables

for (i in 1:nboots) {
  boothaul <- abund_hauls %>%
    group_by(stratum) %>%
    dplyr::slice_sample(prop = prop_boot, replace = TRUE) %>%
    ungroup()

  bootscpue[[i]] <- get_cpue(
    racebase_tables = list(
      cruisedat = cruisedat,
      haul = boothaul,
      catch = catch
    ), speciescode = species_in,
    survey_area = region_in
  ) %>%
    mutate(boot_id = i)
}

Sys.time() - start.time

# Save list of CPUE tables
save(bootscpue,
  file = paste0("outputs/", Sys.Date(), "/", "CPUE_", species_in, "_", yr_in, "_", region_in, ".RData")
)


# Turn CPUE list into a list of biomass_stratums --------------------------
bootsbiomassstratum <- lapply(bootscpue,
  FUN = function(x) get_biomass_stratum(cpue_table = x, 
                                        speciescode = species_in, 
                                        survey_area = region_in)
)

save(bootsbiomassstratum, file = paste0("outputs/", Sys.Date(), "/", "stratumbiomass_", species_in, "_", yr_in, "_", region_in, ".RData"))


# Peek at results ---------------------------------------------------------

test <- bind_rows(bootsbiomassstratum) %>% 
  mutate(log_b = log(stratum_biomass))


summary_df <- test %>%
  group_by(year, stratum) %>%
  summarize(
    n = 10000,
    mean = mean(log_b, na.rm = TRUE),
    sd = sd(log_b, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(log_b = pmap(list(n, mean, sd), rnorm)) %>%
  unnest_longer(log_b)


test %>%
  ggplot(aes(x = log_b, y = ..density..)) +
  geom_histogram() +
  geom_density(
    data = summary_df,
    color = "red"
  ) +
  facet_wrap(~stratum, scales = "free_y")


# Bootstraps for all the SAFE species in GOA ------------------------------

safe <- read.csv("data/siple_safe_species.csv") %>%
  filter(!is.na(species_code)) %>%
  select(spp_name_informal, GOA, species_code)
yrs <- seq(2007, 2021, by = 2)

for (s in 1:length(unique(safe$species_code))) {
  for (y in 1:length(yrs)) {
    species_in <- safe$species_code[s] # Set species. POP = 30060, ATF = 10110
    yr_in <- yrs[y]
    region_in <- "GOA"
    nboots <- 1000
    # no need to specify stratum

    # Bootstrap at haul level -------------------------------------------------
    # Main question: are stratum-level biomass estimates actually lognormally distributed? Bootstrap at haul scale within a stratum (?) to find out.

    # Prep haul data
    abund_hauls <- haul %>%
      filter(abundance_haul == "Y" &
        lubridate::year(start_time) == yr_in &
        region == region_in)


    # Set up dataframe with bootstrap design ----------------------------------
    # Size of bootstrap sample - 50% of total hauls
    prop_boot <- 1
    # (samplesize <- floor(nrow(abund_hauls) * prop_boot))


    # Generate bootstrapped CPUE ----------------------------------------------
    bootscpue <- list()

    start.time <- Sys.time() # 13 sec for 1000 boots, 1.4 mins for the whole thing+cpue tables

    for (i in 1:nboots) {
      boothaul <- abund_hauls %>%
        group_by(stratum) %>%
        dplyr::slice_sample(prop = prop_boot, replace = TRUE) %>%
        ungroup()

      bootscpue[[i]] <- get_cpue(
        racebase_tables = list(
          cruisedat = cruisedat,
          haul = boothaul,
          catch = catch
        ), speciescode = species_in,
        survey_area = region_in
      ) %>%
        mutate(boot_id = i)
    } #/end boots

    Sys.time() - start.time

    save(bootscpue,
      file = paste0("outputs/", Sys.Date(), "/", "CPUE_", species_in, "_", yr_in, "_", region_in, ".RData")
    )

    bootsbiomassstratum <- lapply(bootscpue,
      FUN = function(x) {
        get_biomass_stratum(
          cpue_table = x,
          speciescode = species_in,
          survey_area = region_in
        )
      }
    )

    save(bootsbiomassstratum, file = paste0("outputs/", Sys.Date(), "/", "stratumbiomass_", species_in, "_", yr_in, "_", region_in, ".RData"))
  }
  print(s/length(unique(safe$species_code)))
}
