# Description -------------------------------------------------------------

## Fri May 03 10:23:08 2019 

## Clean up the spatial aspect of data (remove invalid coordinates)
## Check that points fall within the expert polygons
## Write species shapefiles 
## Use "buffer value" in user query to choose a filter for outliers (from expert polygon buffers) 

library(tidyverse)
library(lubridate)
library(sf)
library(viridis)
library(glue)
library(gt)

# Import data -------------------------------------------------------------
load("data output/amph_data_clean.RData")
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
za <- st_read("data input/PR_SA.shp",crs = latlongCRS)

## Create spatial directory for outputs
spat_dir <- c("data output/occurence records spatial")
dir.create(spat_dir)

# Filter low quality occurence records ------------------------------------
amphsf <- amph %>% ## Records with no location errors, not QDS scale, and are within SA shapefile
  filter(!(is.na(decimallatitude) | is.na(decimallongitude) | 
             decimallatitude == 0 | decimallongitude == 0 |
             decimallatitude > -15 | decimallatitude < -35 |
             decimallongitude < 15 | decimallongitude > 35)) %>% 
  filter(qds == 0) %>%
  filter(!coordinateprecision %in% "Nearest Quarter Degree") %>% 
  st_as_sf(coords = c("decimallongitude","decimallatitude"), crs = latlongCRS) %>% 
  st_crop(st_bbox(za)) %>% 
  st_intersection(st_buffer(za,0.01)) # 0.01 arc_degree ~ 1km (Keep points within shapefile)

# Records with location errors --------------------------------------------
amph_locerr <- amph %>% 
  filter(is.na(decimallatitude) | is.na(decimallongitude) | 
           decimallatitude == 0 | decimallongitude == 0 |
           decimallatitude > -21 | decimallatitude < -35 |
           decimallongitude < 15 | decimallongitude > 35) %>% 
  print(n = 10) %>% 
  write_csv(glue("{err_dir}/non_spatial_records.csv"))

# QDS records -------------------------------------------------------------

## Records that only have QDS resolution (although there could be others with different names)
amph_qds <- amph %>% 
  filter(!(is.na(decimallatitude) | is.na(decimallongitude) | 
             decimallatitude == 0 | decimallongitude == 0 |
             decimallatitude > -21 | decimallatitude < -35 |
             decimallongitude < 15 | decimallongitude > 35)) %>% 
  filter(coordinateprecision == "Nearest Quarter Degree" | qds == 1) %>% 
  st_as_sf(coords = c("decimallongitude","decimallatitude"), crs = latlongCRS) %>% 
  st_crop(st_bbox(za)) 

amph_qds %>% 
  st_write(glue("{spat_dir}/Amphibian_QDS_records.shp"), delete_dsn = TRUE)

amph_qds %>% 
  write_csv(glue("{spat_dir}/Amphibian_QDS_records.csv"))

amph_qds %>% 
  group_by(scientificname) %>% 
  tally %>% 
  as_tibble() %>% 
  select(scientificname,n) %>% 
  rename(QDS_record_count = n) %>% 
  write_csv(glue("{spat_dir}/Amphibian_QDS_summary.csv"))

# Write a summary that shows QDS as a proportion of total errors ----------
amph_all_summary <- amph %>% 
  filter(!(is.na(decimallatitude) | is.na(decimallongitude) | 
             decimallatitude == 0 | decimallongitude == 0 |
             decimallatitude > -21 | decimallatitude < -35 |
             decimallongitude < 15 | decimallongitude > 35)) %>% 
  group_by(scientificname) %>% 
  tally %>% 
  as_tibble() %>% 
  select(scientificname,n) %>% 
  rename(All_record_count = n)

amph_qds_summary <- amph_qds %>% 
  group_by(scientificname) %>% 
  tally %>% 
  as_tibble() %>% 
  select(scientificname,n) %>% 
  rename(QDS_record_count = n)

amph_gps_summary <- amphsf %>% 
  group_by(scientificname) %>% 
  tally %>% 
  as_tibble() %>% 
  select(scientificname,n) %>% 
  rename(GPS_record_count = n)

spatial_summary <- amph_all_summary %>% 
  full_join(amph_gps_summary) %>% 
  full_join(amph_qds_summary) %>% 
  mutate(prop_qds = round((QDS_record_count/All_record_count*100) ,2)) %>% 
  left_join(rl %>% select(latin_name,rls), by = c("scientificname" = "latin_name")) %>% 
  select(scientificname, rls, everything())

write_csv(spatial_summary,"data output/occurence records spatial/Amphibian_occ_records_summary.csv")

# Summary table gt --------------------------------------------------------
spatial_gt <- spatial_summary %>% 
  gt() %>% 
  tab_header(
    title = md("**Breakdown of spatial occurence records**"),
    subtitle = md(glue("*Includes data for {length(spp)} Amphibian species*"))) %>% 
  tab_source_note(source_note = md("*Analysis based on database provided by John Measey*")) %>% 
  cols_label(scientificname = md("**Species**"),
             rls = md("**Red-list status**"),
             All_record_count  = md("**Total occurence records**"),
             GPS_record_count  = md("**Valid spatial location (GPS)**"),
             QDS_record_count  = md("**QDS records**"),
             prop_qds = md("**Proportion QDS (%)**")) 

spatial_gt <- spatial_gt %>% # Need to do this in two steps for some reason
  tab_style(style = cells_styles(text_style = "italic"),
            locations = cells_data(columns = vars(scientificname))) %>% 
  tab_style(style = cells_styles(text_align =  "center"),
            locations = cells_data(columns = vars(All_record_count,GPS_record_count,
                                                  QDS_record_count,prop_qds)))
spatial_gt


# Expert polygon check ----------------------------------------------------

## Rename files and remove underscore between genus and species name
polyfiles <- dir("data input/Amphibian Interpreted Distributions/", full.names = T)
new_polyfiles <- str_replace(polyfiles, "_", " ") 
file.rename(from = polyfiles, to = new_polyfiles)

## Import interpreted distributions 
polynames <- dir("data input/Amphibian Interpreted Distributions/", pattern = ".shp")
polylist <- dir("data input/Amphibian Interpreted Distributions/", pattern = ".shp" ,full.names = T)

source("src/functions/check_occ_poly.R")
polyresults <- map(spp,
                   check_occ_poly)

## Either species has no records or there is no expert shapefile
missing_spp <- spp[which(unlist(map(polyresults, is.null)))]

## Figure out which it is (missing data or missing polygon)
tibble(missing_spp,
       polygon_available = missing_spp %in% polynames) %>% 
  full_join(amph %>% 
              filter(scientificname %in% missing_spp) %>% 
              group_by(scientificname) %>% 
              tally %>% 
              rename(missing_spp = scientificname,
                     occ_point_count = n)) %>% 
  write_csv(glue("{err_dir}/missing_poly_occ_points.csv"))
  
polydf <- polyresults %>% 
  purrr::discard(is.null) 

polydf <- do.call(rbind,polydf)
polydf

st_geometry(polydf) <- NULL # Coerce to df

amph <- amph %>% 
  left_join(polydf %>% select(objectid, core:buff3), by = "objectid") 

amphsf <- amphsf %>% 
  left_join(polydf %>% select(objectid, core:buff3), by = "objectid")

## Remove points outside buffers
buff_rm <- amphsf %>% 
  filter(core == FALSE & buff1 == FALSE & buff2 == FALSE & buff3 == FALSE) 

## Write outlying points to file
buff_rm %>% 
  st_write("data output/occurence record errors/points_outside_buffers.shp")

buff_rm %>% 
  write_csv("data output/occurence record errors/points_outside_buffers.csv")

amphsf <- amphsf %>% 
  filter(!objectid %in% buff_rm$objectid)

# Write to shapefile ------------------------------------------------------
st_write(amphsf, glue("{spat_dir}/Amphibian_occ_records_full.shp"), delete_dsn = TRUE)

# Write workspaces --------------------------------------------------------
save(list = c("amph","amph_all","amphsf","rl","spp","err_dir"), file = "data output/amph_data_clean.RData")
