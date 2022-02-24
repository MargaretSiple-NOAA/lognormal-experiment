# Bootstrap sampling from catch data- all using GOA as an example
# POP
source("R/01_cleanup_data.R") #from design-based code
library(tidyverse)



# Preliminary stuff -------------------------------------------------------
# Look at strata, see which are similar and diff in terms of biomass mean and CV
catch %>% 
  filter(haul==199 & cruise==200501)

# How many data points per stratum?
samplecounts <- haul %>% 
  filter(region == "GOA" & abundance_haul=="Y") %>%
  group_by(cruise, stratum) %>%
  count() %>%
  ungroup()

samplecounts %>% filter(cruise==202101)

summary(samplecounts)
# 1-77 abundance hauls per year per stratum (median of 10 hauls per stratum)

# Summarize biomass mean and CV by stratum and year - POP
source("R/02_get_cpue.R")
source("R/03_get_biomass_stratum.R")
source("R/04_get_biomass_total.R")

b_stratum_pop <- get_biomass_stratum(speciescode = 30060, survey_area = "GOA")

b_stratum_pop %>%
  ggplot(aes(x=year, y = mean_wgt_cpue, colour = factor(stratum))) +
  geom_line()

b_stratum_pop %>%
  filter(year==2021) %>%
  filter(mean_wgt_cpue > 0) %>%
  ggplot(aes(x=factor(stratum),y=mean_wgt_cpue)) +
  geom_col() +
  labs(title = "Mean CPUE by weight, GOA POP in 2021")

# Find strata with similar biomasses
x <- b_stratum_pop %>% filter(year==2021)
xx <- outer(x$stratum_biomass,x$stratum_biomass, FUN = "-")
rownames(xx) <- colnames(xx) <- x$stratum
xx[lower.tri(xx)] <- NA # get upper triangle only
xx <- abs(xx)

# xx %>% 
#   as_tibble() %>% 
#   pivot_longer(cols=colnames(xx), names_to = "stratum", values_to = "biomass_diff") %>%
#   filter(value !=0) %>%
#   top_n(wt = value, n = -10)


# Example with POP --------------------------------------------------------
#These scripts are all from design-based-indices (a repo which should be a package)

source("R/03_get_biomass_stratum_cpuein.R")
source("R/04_get_biomass_total_stratumin.R")


#####

# Bootstrap at haul level -------------------------------------------------
# Main question: are stratum-level biomass estimates actually lognormally distributed? Bootstrap at haul scale within a stratum (?) to find out. 
# subset to abundance hauls in GOA 2021

# Size of bootstrap sample and which stratum to focus on
strata_boot <- c(130) #, 221, 250, 131 eyeballed these to see which strata have similar biomasses
nboots <- 1000  

abund_hauls <- haul %>% 
  filter(abundance_haul=="Y" & 
           lubridate::year(start_time) == 2021 & 
           stratum %in% strata_boot & 
           region == "GOA")

# Size of bootstrap sample - 50% of total hauls?
samplesize <- floor(nrow(abund_hauls) * .5)

boots_biomass<- vector()
set.seed(123)

for(i in 1:nboots){
x <- sample(1:nrow(abund_hauls), size = samplesize)
boot_hauls <- abund_hauls[x,]
boot_cpue <- get_cpue(racebase_tables = list(
  cruisedat = cruisedat,
  haul = boot_hauls, #***
  catch = catch
), speciescode = 30060,
survey_area = "GOA")
boot_biomass_stratum <- get_biomass_stratum(cpue_table = boot_cpue, 
                                            speciescode = 30060, 
                                            survey_area = "GOA")
boots_biomass[i] <- boot_biomass_stratum$stratum_biomass
}

hist(boots_biomass)