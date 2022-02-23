# Bootstrap sampling from catch data- all using GOA as an example
# POP
source("R/01_cleanup_data.R") #from design-based code
library(tidyverse)

catch %>% 
  filter(haul==199 & cruise==200501)

# How many data points per stratum?
samplecounts <- haul %>% 
  filter(region == "GOA" & abundance_haul=="Y") %>%
  group_by(cruise, stratum) %>%
  count() %>%
  ungroup()
summary(samplecounts)
# 1-77 abundance hauls per year per stratum (median of 10 hauls per stratum)


# Example with POP --------------------------------------------------------
#These scripts are all from design-based-indices (a repo which should be a package)
source("R/02_get_cpue.R")
source("R/03_get_biomass_stratum_cpuein.R")
source("R/04_get_biomass_total_stratumin.R")

#####
# haul level
# subset to abundance hauls in GOA 2021
samplesize <- 50
abund_hauls <- haul %>% 
  filter(abundance_haul=="Y" & 
           lubridate::year(start_time) == 2021 & 
           region == "GOA")

x <- sample(1:nrow(abund_hauls), size = samplesize)
boot_hauls <- abund_hauls[x,]
boot_cpue <- get_cpue(racebase_tables = list(
  cruisedat = cruisedat,
  haul = boot_hauls, #***
  catch = catch
),speciescode = 30060,
survey_area = "GOA")
boot_biomass_stratum <- get_biomass_stratum(cpue_table = boot_cpue, 
                                            speciescode = 30060, 
                                            survey_area = "GOA")

