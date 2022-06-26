# Get CPUE data from API instead
library("httr")
library("jsonlite")
# link to the API
api_link <- "https://origin-tst-ods-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey/"

res <- httr::GET(api_link)
# base::rawToChar(res$content) # Test connection
data <- jsonlite::fromJSON(base::rawToChar(res$content))

head(data)


# Try just for GOA species ------------------------------------------------

res <- httr::GET(api_link, query = list(srvy = "EBS", year = 2017))
data <- jsonlite::fromJSON(base::rawToChar(res$content))
x <- data$items
head(x)


res <- httr::GET(api_link, query = list(srvy = "EBS", year = 2018))
data <- jsonlite::fromJSON(base::rawToChar(res$content))
x <- data$items
x <- x[,c("stratum", "station", "vessel_name", "latitude_dd", "longitude_dd", 
          "species_code", "common_name", "scientific_name", "taxon_confidence", 
          "cpue_kgha", "cpue_noha", "weight_kg", "count", 
          "bottom_temperature_c", "surface_temperature_c", "depth_m")]
head(x)
