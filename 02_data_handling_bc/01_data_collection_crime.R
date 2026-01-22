# ==============================================================================
# CRIME RATE — Block-Level Incidents 2017–2019

# GOAL: data frame of blocks with key selection characteristics per block/ block group
# this script: crime rate

# GEOID = block id (primary key)

# OUTPUTS:
#   - crime.csv

# TABLE OF CONTENTS
#   0) Packages
#   1) Data Import
#   2) Data Cleaning
#   3) Crime assignment to blocks
#       3.1) Same projected CRS (meters)
#       3.2) Buffer incidents
#       3.3) Intersections & equal weights
#       3.4) Aggregate to blocks
#       3.5) Attach zero-count blocks
#   4) Export
# ==============================================================================


# 0) Packages -------------------------------------------------------------------
# package installation helper (if needed)
# if (!requireNamespace("tidycensus", quietly=TRUE))     install.packages("digest")

library(sf)
library(lwgeom)
library(dplyr)
library(tibble)


# 1) Data Import ----------------------------------------------------------------

# 1.1) Crime incidents (2017–2019) 
# https://opendataphilly.org/datasets/crime-incidents/ 


# extracting zip files helper (if needed)
{
# unzip("crime_2017.zip",
#       exdir = "crime_2017",
#       unzip = "internal")
}

# crime 19
shp_files19 <- list.files(
  "crime_2019",
  pattern = "\\.shp$", full.names = TRUE, recursive = TRUE
)
c19 <- st_read(shp_files19[1], quiet = FALSE)

# crime 18
shp_files18 <- list.files(
  "crime_2018",
  pattern = "\\.shp$", full.names = TRUE, recursive = TRUE
)
c18 <- st_read(shp_files18[1], quiet = FALSE)

# crime 17
shp_files17 <- list.files(
  "crime_2017",
  pattern = "\\.shp$", full.names = TRUE, recursive = TRUE
)
c17 <- st_read(shp_files17[1], quiet = FALSE)

# combine
c1719 <- bind_rows(c17, c18, c19)


# 1.2) Blocks (2010) 
blocks10 <- st_read(
  "Census_Blocks_2010.geojson") %>%
  select(GEOID = GEOID10) %>%
  st_make_valid()


# 2) Data Cleaning -------------------------------------------------------------

# 2.1) Filter to localized crimes
# Filter to localized crimes
keep_codes <- c(
  "Homicide - Criminal", "Rape",
  "Robbery Firearm", "Robbery No Firearm",
  "Aggravated Assault Firearm", "Aggravated Assault No Firearm",
  "Burglary Residential", "Burglary Non-Residential",
  "Thefts", "Theft from Vehicle", "Motor Vehicle Theft",
  "Other Assaults", "Arson", "Vandalism/Criminal Mischief",
  "Offenses Against Family and Children",
  "Public Drunkenness", "Disorderly Conduct", "Vagrancy/Loitering"
)
c1719 <- subset(c1719, text_gener %in% keep_codes)


# 3) Crime assignment to blocks -----------------------------------------------

# 3.1) Same projected CRS (meters) 
blocks10 <- st_transform(blocks10, 26918)     # NAD83 / UTM 18N
c1719    <- st_transform(c1719, 26918)

c1719$crime_id <- seq_len(nrow(c1719))

# 3.2) Small buffer so centerline points touch both sides
buf <- 20                                      
c1719_buf <- st_buffer(c1719, buf)

# 3.3) Intersect + equal weights over all touched blocks 
hits <- st_intersects(c1719_buf, blocks10)      # list: blocks per incident

# fallback (assign those that get intersected above - less than 1% of observations - to nearest block)
zero <- lengths(hits) == 0
cat("0-hit incidents:", sum(zero), "\n")
if (any(zero)) {
  hits[zero] <- as.list(st_nearest_feature(c1719[zero, ], blocks10))
}

assign <- tibble(
  crime_id = rep(c1719$crime_id, lengths(hits)),
  GEOID    = blocks10$GEOID[unlist(hits)]
) %>%
  group_by(crime_id) %>%
  mutate(w = 1 / n()) %>%           # each incident sums to 1 across blocks
  ungroup()

# 3.4) Aggregate to blocks -
crime_block <- assign %>%
  group_by(GEOID) %>%
  summarise(crime = sum(w), .groups = "drop")

# 3.5) Sanity check: totals preserved
nrow(c1719) == sum(crime_block$crime)

# 3.6) Attach zero-count blocks 
crime_block <- blocks10 %>%
  st_drop_geometry() %>%
  left_join(crime_block, by = "GEOID") %>%
  mutate(crime = coalesce(crime, 0))



# 4) Export --------------------------------------------------------------------

write.csv(
  crime_block,
  "crime.csv",
  row.names = FALSE
)
