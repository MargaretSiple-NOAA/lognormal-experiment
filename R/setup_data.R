
# POP and ATF data --------------------------------------------------------
# Look at distribution of mean and CV of POP across strata
source("R/01_cleanup_data.R") #takes a second
# Save POP and ATF data from this step to save time:
catch <- filter(catch, species_code %in% c(30060, 10110))
save(list = c("catch", 
              "cruisedat", 
              "haul",
              "goa_strata",
              "ai_strata"),
     file = "POP_ATF.Rdata")


