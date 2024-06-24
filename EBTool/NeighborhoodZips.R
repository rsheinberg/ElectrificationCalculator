### Neighborhood Zone Matching 
### 5.21.24
### Rachel Sheinberg

setwd("C:/Users/Rachel/OneDrive - UCLA IT Services/Documents/ATC/EBTool")
library(sf)
library(dplyr)
library(ggplot2)

#importing neighborhoods for LA and zip codes for LA County 
neighborhoods <- st_read("LA_Times_Neighborhood_Boundaries-shp")
zip_codes <- st_read("LA_County_ZIP_Codes")

#matching geometries 
neighborhoods <- st_transform(neighborhoods, crs = st_crs(zip_codes))
neighborhoods <- st_make_valid(neighborhoods)
zip_codes <- st_make_valid(zip_codes)


### sample plotting just to test! 
ggplot(data = zip_codes) +
  geom_sf(aes(fill = Shape_Area)) +
  scale_fill_viridis_c() +  # Optional: for a nice color scale
  theme_minimal() +  # Optional: to use a minimal theme
  labs(title = "Map of Population Density",
       fill = "Population Density")


# Perform spatial overlay
overlay <- st_intersection(zip_codes, neighborhoods)

# Rename OBJECTID columns to ensure clarity in joins
colnames(overlay)[which(colnames(overlay) == "OBJECTID")] <- "zip_code_OBJECTID"
colnames(overlay)[which(colnames(overlay) == "OBJECTID.1")] <- "neighborhood_OBJECTID"

# Fix invalid geometries
overlay <- st_make_valid(overlay)

# Calculate the area of intersections
overlay <- overlay %>%
  mutate(area_intersection = st_area(geometry))

# Calculate the area of each neighborhood
neighborhoods <- neighborhoods %>%
  mutate(area_neighborhood = st_area(geometry))

# estimate how much of each neighborhood the zips represent
overlay <- overlay %>%
  st_join(neighborhoods %>% select(OBJECTID, area_neighborhood), join = st_intersects) %>%
  mutate(percent_of_neighborhood = (area_intersection / area_neighborhood) * 100)

## dropping geometry for functionality 
overlay_nogeo <-  st_drop_geometry(overlay)

# Summary result with total percent coverage by zip code
# summary_result <- overlay_nogeo %>%
 # group_by(neighborhood_OBJECTID, zip_code_OBJECTID) %>%
 # summarize(total_percent_coverage = sum(percent_of_neighborhood)) %>%
  #ungroup()

# Summary result with total percent coverage by zip code
summary_result <- overlay_nogeo %>%
  group_by(neighborhood_OBJECTID, zip_code_OBJECTID) %>%
  summarize(total_percent_coverage = sum(percent_of_neighborhood)) %>%
  ungroup() %>%
  left_join(zip_codes, by = c("zip_code_OBJECTID" = "OBJECTID"))

summary_result <- st_drop_geometry(summary_result)

## matching with zipcodes 

# read in the file i made of zipcodes 
zip_zone <- read.csv("zip_zone.csv")
zip_zone$zip <- as.character(zip_zone$zip)
## adding zip as a column because I don't understand the syntax for join
summary_result$zip <- summary_result$ZIPCODE

## adding the zones!! 
summary_result <- summary_result %>%
  left_join(zip_zone, by = "zip")

### summarizing the main zone for the neighborhood
predominant_zone <- summary_result %>%
  group_by(neighborhood_OBJECTID) %>%
  summarize(zone = zone[which.max(total_percent_coverage)]) %>%
  ungroup()

## adding again bc I am dumb with joins
predominant_zone$OBJECTID <- predominant_zone$neighborhood_OBJECTID

## making an sf object so i can make map
predominant_zone <- st_as_sf(predominant_zone)

## joining yeet yeet
predominant_zone <- predominant_zone %>%
  left_join(neighborhoods, by = "OBJECTID")

## making zone a factor for discrete map
predominant_zone$zone <- as.factor(predominant_zone$zone)

## mapping zone assignments
ggplot(data = predominant_zone) +
  geom_sf(aes(fill = zone)) +
  scale_fill_manual(values = c("paleturquoise", "paleturquoise4"))  +  # Use a predefined color palette
  theme_minimal() +
  labs(title = "Climate Zone Assignments",
       fill = "Zone")

## dropping geometry to export
predominant_zone <- predominant_zone[,1:4]

## exporting my list 
write.csv(predominant_zone, "zone_neighborhoods.csv")





