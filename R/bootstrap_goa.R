# Bootstrap sampling from catch data- all using GOA as an example
# POP
source("R/01_cleanup_data.R") # from design-based code
library(tidyverse)
options(dplyr.summarise.inform = FALSE)


# Preliminary stuff -------------------------------------------------------
# Look at strata, see which are similar and diff in terms of biomass mean and CV
catch %>%
  filter(haul == 199 & cruise == 200501)

# How many data points per stratum?
samplecounts <- haul %>%
  filter(region == "GOA" & abundance_haul == "Y") %>%
  group_by(cruise, stratum) %>%
  count() %>%
  ungroup()

samplecounts %>% filter(cruise == 202101)

summary(samplecounts)
# 1-77 abundance hauls per year per stratum (median of 10 hauls per stratum)

# Summarize biomass mean and CV by stratum and year - POP
source("R/02_get_cpue.R")
source("R/03_get_biomass_stratum.R")
source("R/04_get_biomass_total.R")

b_stratum_pop <- get_biomass_stratum(speciescode = 30060, survey_area = "GOA")

b_stratum_pop %>%
  ggplot(aes(x = year, y = mean_wgt_cpue, colour = factor(stratum))) +
  geom_line()

b_stratum_pop %>%
  filter(year == 2021) %>%
  filter(mean_wgt_cpue > 0) %>%
  ggplot(aes(x = factor(stratum), y = mean_wgt_cpue)) +
  geom_col() +
  labs(title = "Mean CPUE by weight, GOA POP in 2021")

# Find strata with similar biomasses
x <- b_stratum_pop %>% filter(year == 2021)
xx <- outer(x$stratum_biomass, x$stratum_biomass, FUN = "-")
rownames(xx) <- colnames(xx) <- x$stratum
xx[lower.tri(xx)] <- NA # get upper triangle only
xx <- abs(xx)

# xx %>%
#   as_tibble() %>%
#   pivot_longer(cols=colnames(xx), names_to = "stratum", values_to = "biomass_diff") %>%
#   filter(value !=0) %>%
#   top_n(wt = value, n = -10)


# Example with POP --------------------------------------------------------
# These scripts are all from design-based-indices (a repo which should be a package)

source("R/03_get_biomass_stratum_cpuein.R")
source("R/04_get_biomass_total_stratumin.R")


#####

# Bootstrap at haul level -------------------------------------------------
# Main question: are stratum-level biomass estimates actually lognormally distributed? Bootstrap at haul scale within a stratum (?) to find out.
# subset to abundance hauls in GOA 2021

# Size of bootstrap sample and which stratum to focus on
strata_boot <- c(130) # , 221, 250, 131 eyeballed these to see which strata have similar biomasses
nboots <- 1000

abund_hauls <- haul %>%
  filter(abundance_haul == "Y" &
    lubridate::year(start_time) == 2021 &
    stratum %in% strata_boot &
    region == "GOA")

# Size of bootstrap sample - 50% of total hauls?
samplesize <- floor(nrow(abund_hauls) * .5)

boots_biomass <- vector()
set.seed(123)

for (i in 1:nboots) {
  x <- ifelse(samplesize == 0, 1, sample(1:nrow(abund_hauls), size = samplesize, replace = TRUE))
  boot_hauls <- abund_hauls[x, ]
  boot_cpue <- get_cpue(
    racebase_tables = list(
      cruisedat = cruisedat,
      haul = boot_hauls, #***
      catch = catch
    ), speciescode = 30060,
    survey_area = "GOA"
  )
  boot_biomass_stratum <- get_biomass_stratum(
    cpue_table = boot_cpue,
    speciescode = 30060,
    survey_area = "GOA"
  )
  boots_biomass[i] <- boot_biomass_stratum$stratum_biomass
}

png("img/POP_GOA_2021_1000.png", width = 10, height = 5, units = "in", res = 120)
par(mfrow = c(1, 2))
# Remove zeroes
boots_biomass <- boots_biomass[which(boots_biomass != 0)]
hist(log(boots_biomass), freq = FALSE, main = "POP - GOA - 2021 - stratum 130", xlab = "Log(stratum biomass)", xlim = c(min(log(boots_biomass)), max(log(boots_biomass) * 1.05)))
curve(dnorm(x, mean = mean(log(boots_biomass)), sd = sd(log(boots_biomass))),
  add = TRUE,
  from = min(log(boots_biomass)), to = max(log(boots_biomass)),
  lwd = 2
)

qqnorm(log(boots_biomass), main = "POP - GOA - 2021 - stratum 130")
qqline(log(boots_biomass), col = 2)
dev.off()

bb <- data.frame(sim = 1:length(boots_biomass), stratum = 130, year = 2021, species = "POP", biomass_stratum_boot = boots_biomass)

GOAPOP_2021 <- bb


# Try with arrowtooth -----------------------------------------------------


boots_biomass <- vector()
set.seed(123)

for (i in 1:nboots) {
  x <- ifelse(samplesize == 0, 1, sample(1:nrow(abund_hauls), size = samplesize))
  boot_hauls <- abund_hauls[x, ]
  boot_cpue <- get_cpue(
    racebase_tables = list(
      cruisedat = cruisedat,
      haul = boot_hauls, #***
      catch = catch
    ), speciescode = 10110,
    survey_area = "GOA"
  )
  boot_biomass_stratum <- get_biomass_stratum(
    cpue_table = boot_cpue,
    speciescode = 10110,
    survey_area = "GOA"
  )
  boots_biomass[i] <- boot_biomass_stratum$stratum_biomass
}

png("img/ATF_GOA_2021_1000.png", width = 10, height = 5, units = "in", res = 120)
par(mfrow = c(1, 2))
hist(log(boots_biomass), freq = FALSE, main = "ATF - GOA - 2021 - stratum 130", xlab = "Log(stratum biomass)", xlim = c(min(log(boots_biomass)), max(log(boots_biomass) * 1.05)))
curve(dnorm(x, mean = mean(log(boots_biomass)), sd = sd(log(boots_biomass))),
  add = TRUE,
  from = min(log(boots_biomass)), to = max(log(boots_biomass) * 1.05),
  lwd = 2
)

qqnorm(log(boots_biomass), main = "ATF - GOA - 2021 - stratum 130")
qqline(log(boots_biomass), col = 2)
dev.off()

bb <- data.frame(sim = 1:length(boots_biomass), stratum = 130, year = 2021, species = "ATF", biomass_stratum_boot = boots_biomass)
GOAATF_2021 <- bb

save(file = "outputs/POP_ATF.Rdata", list = c("GOAATF_2021", "GOAPOP_2021"))


# Big simulation with multiple strata -------------------------------------
strata_boot <- unique(haul$stratum) # , 221, 250, 131 eyeballed these to see which strata have similar biomasses
nboots <- 500

POP_out <- vector()
start.time=Sys.time()
for (j in 1:length(strata_boot)) {
  abund_hauls <- haul %>%
    filter(abundance_haul == "Y" &
      lubridate::year(start_time) == 2021 &
      stratum %in% strata_boot[j] &
      region == "GOA")
  if (nrow(abund_hauls) == 0) {
    next
  }

  # Size of bootstrap sample - 50% of total hauls?
  samplesize <- floor(nrow(abund_hauls) * .5)

  boots_biomass <- vector()
  set.seed(123)

  for (i in 1:nboots) {
    x <- ifelse(samplesize == 0, 1, sample(1:nrow(abund_hauls), size = samplesize, replace = TRUE))
    boot_hauls <- abund_hauls[x, ]
    boot_cpue <- get_cpue(
      racebase_tables = list(
        cruisedat = cruisedat,
        haul = boot_hauls, #***
        catch = catch
      ), speciescode = 30060,
      survey_area = "GOA"
    )
    boot_biomass_stratum <- get_biomass_stratum(
      cpue_table = boot_cpue,
      speciescode = 30060,
      survey_area = "GOA"
    )
    boots_biomass[i] <- boot_biomass_stratum$stratum_biomass
  }

  bb <- data.frame(
    sim = 1:length(boots_biomass),
    stratum = strata_boot[j],
    year = 2021, species = "POP",
    biomass_stratum_boot = boots_biomass
  )
  POP_out <- rbind(POP_out, bb)
 # cat("\n",j / length(strata_boot))
}

Sys.time()-start.time
save(list = "POP_out", file = "outputs/POP_allstrata_new.Rdata")


ggplot(POP_out, aes(x = log(biomass_stratum_boot))) +
  geom_histogram(bins = 10) +
  facet_wrap(~stratum, scales = "free")

test <- POP_out %>% filter(stratum == 130)
hist(log(test$biomass_stratum_boot), freq = FALSE)
curve(dnorm(x,
  mean = mean(log(test$biomass_stratum_boot)),
  sd = sd(log(test$biomass_stratum_boot))
), add = TRUE, lwd = 2)



# Big bootstrap - ATF -----------------------------------------------------

strata_boot <- unique(haul$stratum) # , 221, 250, 131 eyeballed these to see which strata have similar biomasses
nboots <- 50

ATF_out <- vector()
Rprof()
#start_time <- Sys.time()
for (j in 1:1) { #length(strata_boot)
  abund_hauls <- haul %>%
    filter(abundance_haul == "Y" &
      lubridate::year(start_time) == 2021 &
      stratum %in% strata_boot[j] &
      region == "GOA")
  if (nrow(abund_hauls) == 0) {
    next
  }

  # Size of bootstrap sample - 50% of total hauls?
  samplesize <- floor(nrow(abund_hauls) * .5)

  boots_biomass <- vector()
  set.seed(123)

  for (i in 1:nboots) {
    x <- ifelse(samplesize == 0, 1, sample(1:nrow(abund_hauls), size = samplesize, replace = TRUE))
    boot_hauls <- abund_hauls[x, ]
    boot_cpue <- get_cpue(
      racebase_tables = list(
        cruisedat = cruisedat,
        haul = boot_hauls, #***
        catch = catch
      ), speciescode = 10110,
      survey_area = "GOA"
    )
    boot_biomass_stratum <- get_biomass_stratum(
      cpue_table = boot_cpue,
      speciescode = 10110,
      survey_area = "GOA"
    )
    boots_biomass[i] <- boot_biomass_stratum$stratum_biomass
  }

  bb <- data.frame(sim = 1:length(boots_biomass), stratum = strata_boot[j], year = 2021, species = "ATF", biomass_stratum_boot = boots_biomass)
  ATF_out <- rbind(ATF_out, bb)
  cat("\n",j / length(strata_boot))
}
#Sys.time() - start.time
Rprof(NULL)
summaryRprof()

save(list = "ATF_out", file = "outputs/ATF_allstrata_new.Rdata")


ggplot(ATF_out, aes(x = log(biomass_stratum_boot))) +
  geom_histogram(bins = 10) +
  facet_wrap(~stratum, scales = "free")

test <- ATF_out %>% filter(stratum == 130)
hist(log(test$biomass_stratum_boot), freq = FALSE)
curve(dnorm(x,
  mean = mean(log(test$biomass_stratum_boot)),
  sd = sd(log(test$biomass_stratum_boot))
), add = TRUE, lwd = 2)







# png("img/ATF_GOA_2021_1000.png",width = 10,height = 5,units='in',res = 120)
# par(mfrow=c(1,2))
# hist(log(boots_biomass),freq=FALSE,main = "ATF - GOA - 2021",xlab="Log(stratum biomass)",xlim=c(min(log(boots_biomass)),max(log(boots_biomass)*1.05)))
# curve(dnorm(x, mean=mean(log(boots_biomass)), sd=sd(log(boots_biomass))), add=TRUE,
#       from=min(log(boots_biomass)), to=max(log(boots_biomass)*1.05),
#       lwd=2)
#
# qqnorm(log(boots_biomass),main = "ATF - GOA - 2021")
# qqline(log(boots_biomass), col = 2)
# dev.off()



# save(file = 'outputs/POP_ATF.Rdata',list = c("GOAATF_2021","GOAPOP_2021"))



# Read in bootstrap data and check ----------------------------------------

load(here::here("outputs","POP_allstrata.Rdata"))
load(here::here("outputs","ATF_allstrata.Rdata"))

POP_out %>%
  ggplot(aes(x = log(biomass_stratum_boot))) +
  geom_histogram(bins = 10) +
  facet_wrap(~stratum, scales = "free")

ATF_out %>%
  ggplot(aes(x = log(biomass_stratum_boot))) +
  geom_histogram(bins = 10) +
  facet_wrap(~stratum, scales = "free")
