#############################################################################
# Make figures for outputs
# 

#############################################################################
library(tidyverse)

# Load data for your species/year/region ----------------------------------
species_in <- 10110 # Set species. POP = 30060, ATF = 10110
yr_in <- 2021
region_in <- "GOA"
outputsdate <- "2022-06-21"

load(file = paste0("outputs/",outputsdate,"/","stratumbiomass_",species_in,"_",yr_in,"_",region_in,".RData")) # dataframe: bootsbiomassstratum

length(bootsbiomassstratum) # should be equal to nboots

bstratum_df <- dplyr::bind_rows(bootsbiomassstratum) %>%
  mutate(log_b = log(stratum_biomass))

bstratum_df2 <- bstratum_df 
# %>% 
#   filter(stratum_biomass>0)



# Histograms and density curves for species_in ----------------------------

summary_df <- bstratum_df2 %>%
  group_by(year, stratum) %>%
  summarize(
    n = 10000,
    mean = mean(log_b, na.rm = TRUE),
    sd = sd(log_b, na.rm = TRUE)
  ) %>%
  mutate(log_b = pmap(list(n, mean, sd), rnorm)) %>%
  unnest_longer(log_b)

mypal <- MetBrewer::met.brewer(name="Benedictus", n=10)

p1 <- bstratum_df2 %>%
      ggplot(aes(x = log_b, y = ..density..)) +
      geom_histogram(fill=mypal[8],colour = mypal[8]) +
      geom_density(
        data = summary_df,
        color = mypal[10], lwd = 1
      ) +
      facet_wrap(~stratum, scales = "free_y") +
      labs(x = "Log(Biomass)", 
           y = "Density",
           title = paste("Stratum biomass for",species_in,"(",yr_in,")")) +
      theme_classic() +
      theme(strip.background = element_blank())

png(paste0("img/histograms",species_in,"_",yr_in,"_",region_in,".png"),width = 10, height = 8,units = 'in',res = 200)
p1
dev.off()



# qqnorm plots for species_in ---------------------------------------------

p2 <- ggplot(bstratum_df2, aes(sample = log_b)) + 
      stat_qq() + 
      stat_qq_line() + 
      facet_wrap(~stratum, scales="free") +
      labs(title = paste("Quantile-quantile plots for",species_in,"(",yr_in,")")) +
      theme_classic() +
      theme(strip.background = element_blank())
  

png(paste0("img/qqplots",species_in,"_",yr_in,"_",region_in,".png"),width = 10, height = 8,units = 'in',res = 200)
p2
dev.off()