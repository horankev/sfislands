## code to prepare `uk` dataset goes here

### UK data
library(tidyverse)
library(sf)
library(parlitools)

# prepare the data
# extract and join census and election data from parlitools package
census_11 <- parlitools::census_11 |>
  dplyr::select(-constituency_name,-constituency_type,-pano, -region, -country)
bes_2019 <- parlitools::bes_2019

elect_results <- dplyr::left_join(bes_2019,census_11, by=c("ons_const_id"))

# download constituency boundaries for 2019 using the st_read() command below
# they come from: https://geoportal.statistics.gov.uk/datasets/ons::wpc-dec-2019-ultra-generalised-clipped-boundaries-uk
# which results in file: WPC_Dec_2019_UGCB_UK_2022_-5406762260244410508.geojson

uk_map_download <- sf::st_read(
  "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/WPC_Dec_2019_UGCB_UK_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  quiet = TRUE)

# only need the boundaries and the IDs for merging with parlitools data
uk<- uk_map_download |>
  dplyr::select(pcon19cd,geometry) |>
  sf::st_transform(crs=27700) |>
  sf::st_make_valid() # ensure valid line overlaps etc

# create dataframe for filtering-out speaker constituencies below
speakers <- data.frame(
  year = c(2017,2019),
  constituency_name = c("Buckingham","Chorley")
)

# join constituency polygons to parlitools data
# filter out Scotland and Northern Ireland
# filter out the speakers' constituencies
# calculate the desired explanatory variables
# make levels into factors for use with `mgcv`

df <- elect_results |>
  dplyr::left_join(uk, by=c("ons_const_id"="pcon19cd")) |>
  # filter(!country %in% c("Scotland","Northern Ireland")) |>
  dplyr::filter(!country %in% c("Northern Ireland")) |>
  # filter(!constituency_name %in% speakers$constituency_name) |>
  dplyr::mutate(degree_educated = qual_level_4,
                health_not_good = health_fair + health_bad + health_very_bad,
                white = ethnicity_white,
                con_change = con_19 - con_17, # difference in %
                lab_change = lab_19 - lab_17,
                con_swing = (con_change - lab_change)/2, # Butler swing
                region = factor(ifelse(county == "Merseyside", "Merseyside", region)),
                county = factor(county)) |>
  sf::st_as_sf()

# give the empty speaker seats the mean value, just for demostration purposes here
df$con_swing[df$constituency_name %in% speakers$constituency_name] <- mean(df$con_swing,na.rm = TRUE)

# filter out required independent variables and scale them
# then add back to the features which were not scaled
df_scaled <- df |>
  sf::st_drop_geometry() |>
  dplyr::select(degree_educated,
                health_not_good,
                white) |>
  scale() |>
  as.data.frame() |>
  dplyr::mutate(con_swing = df$con_swing,
                population = df$population,
                region = factor(df$region),
                county = factor(df$county),
                constituency_name = df$constituency_name)

# create a simple features (spatial) version of this with a geometry column
uk <- df_scaled |>
  dplyr::mutate(geometry = df$geometry) |>
  sf::st_as_sf() |>
  sf::st_transform(crs=27700)

usethis::use_data(uk, overwrite = TRUE, compress = "xz", ascii = TRUE)
