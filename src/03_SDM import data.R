 # Description -------------------------------------------------------------

## Thu Jan 31 12:35:22 2019
## Species distribution modelling of amphibians

library(lubridate)
library(gghighlight)
library(gridExtra)
library(ggpubr)
library(sf)
library(viridis)
library(readxl)
library(raster)
library(tidyverse)
library(glue)

# Import query details ----------------------------------------------------
query <- read_xlsx("data input/SDM_query.xlsx") %>% 
  print

## Create output folder
sdm_pro_dir <- glue("data output/sdm data processing")
if(dir.exists(sdm_pro_dir)) {
  print("Folder exists")
} else {
  dir.create(sdm_pro_dir)
  print("Folder created")
}

# Import species data -----------------------------------------------------
load("data output/amph_data_clean.RData")

# Filter occurence data based on query ------------------------------------

## Species and time frame
occ_data <- amphsf %>% 
  filter(scientificname == query$Value[which(query$Input == "Species")]) %>% 
  filter(year >= query$Value[which(query$Input == "Start year")]) %>% 
  filter(year <= query$Value[which(query$Input == "End year")])

## Date quality
dateval <- query$Value[which(query$Input == "Date accuracy")]

if(dateval == "Complete"){
  
  occ_data <- occ_data %>% 
    filter(!is.na(date))
  
} else if (dateval == "Year and month"){
  
  occ_data <- occ_data %>% 
    filter(year_check == "valid" & month_check == "valid")
  
} else {
  
  occ_data <- occ_data %>% 
    filter(year_check == "valid")
  
}

# Spatial projections  ----------------------------------------------------
geo_proj <- query$Value[which(query$Input == "Geographic projection")]
aeaproj <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

if (geo_proj == "Yes") {
  occ_points <- occ_data %>% 
    st_as_sf(coords = c("decimallongitude","decimallatitude"),crs = latlongCRS) %>% 
    st_transform(aeaproj)
} else {
  occ_points <- occ_data %>% 
    st_as_sf(coords = c("decimallongitude","decimallatitude"),crs = latlongCRS)
  }

# Remove duplicate coordinate values
occ_points <- occ_points %>% 
  distinct(geometry, .keep_all = TRUE)

# Create background points ------------------------------------------------

## Extract buffer value from query
buffval <- as.numeric(query$Value[which(query$Input == "Background buffer (km)")])*1000
buff_latlong <- as.numeric(query$Value[which(query$Input == "Background buffer (km)")])/100 # Divide km by 100 to convert to arc_degree

latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
za <- st_read("data input/PR_SA.shp",crs = latlongCRS)

if (geo_proj == "Yes") {
  # Create buffer around occ points, create single polygon and intersect with SA border
  occ_buffer <- st_intersection(st_union(st_buffer(occ_points, buffval)),za)
} else {
  occ_buffer <- st_intersection(st_union(st_buffer(occ_points, buff_latlong)),za) 
}

## Import 2.5arcmin grid, project it, intersect with buffer and create polyID column
if (geo_proj == "Yes") {
  zagrid <- st_read("data input/southern_africa_2.5arcmin_grid.shp",crs = latlongCRS) %>% 
    st_transform(aeaproj)
} else {
  zagrid <- st_read("data input/southern_africa_2.5arcmin_grid.shp",crs = latlongCRS)
}

zagrid <- zagrid %>% 
  st_intersection(occ_buffer) %>% 
  dplyr::select(variety) %>% 
  rename(polyID = variety)

## Add polyID values representing number of grids
zagrid <- mutate(zagrid, polyID = str_c("P", 1:nrow(zagrid)))

## Read in value of background points from query
n_points <- as.numeric(query$Value[which(query$Input == "Background points")])

## Randomly sample points within buffer (returns sfc object - use st_sf() to convert to sf)
bck_points <- st_sample(occ_buffer, n_points) %>% 
  st_sf() 

## Create a background point unique ID
bck_points <- mutate(bck_points, ID = str_c("B", 1:nrow(bck_points)))

## Assign a polygonID to each background point
bck_grid <- st_join(bck_points,zagrid,join = st_intersects, left = FALSE) 

## Group by polygons and keep first point (i.e. remove duplicate points)
bck_points <- bck_grid %>% #
  group_by(polyID) %>% 
  filter(row_number() == 1)

## Assign a polygonID to each species occurence point
occ_grid <- st_join(occ_points,zagrid,join = st_intersects, left = FALSE)

## Remove background points that fall within polygons that have occurence points
bck_points <- bck_points %>% 
  filter(!polyID %in% occ_grid$polyID)

## Create a bounding box for map inset
occ_bbox <- st_make_grid(occ_buffer, n = 1) # Make a grid with one cell

# Occurence and BP plot checks --------------------------------------------

## Create output folder (if neccessary)
sppselect <- query$Value[which(query$Input == "Species")]

if(dir.exists(glue("data output/sdm data processing/{sppselect}"))) {
  print("Folder exists")
  } else {
    print("Folder created")
    dir.create(glue("data output/sdm data processing/{sppselect}"))
    }

p1 <- ggplot() +
  geom_sf(data = za, fill = alpha("grey",0.5))+
  geom_sf(data = occ_points, size = 1.5)+
  geom_sf(data = occ_bbox, fill = NA, col = "dodgerblue", size = 1.3) +
  theme_bw()+
  theme(legend.text = element_text(size = 14))+
  ggtitle(glue("{query$Value[which(query$Input == 'Species')]} - {nrow(occ_points)} occurence points"))

p2 <- ggplot() +
  geom_sf(data = za, fill = alpha("grey",0.5), size = 1.0)+
  geom_sf(data = zagrid, fill = alpha("grey",0.3))+
  geom_sf(data = bck_points, size = 0.2, col = "red")+
  geom_sf(data = occ_points, size = 0.8, col = "blue")+
  coord_sf(xlim = c(st_bbox(occ_bbox)$xmin,st_bbox(occ_bbox)$xmax),
           ylim = c(st_bbox(occ_bbox)$ymin,st_bbox(occ_bbox)$ymax))+
  theme_bw()+
  theme(panel.border = element_rect(colour = "dodgerblue", fill=NA, size=1.6))+
  ggtitle(query$Value[which(query$Input == "Species")])

p3 <- ggplot() +
  geom_sf(data = za, fill = alpha("grey",0.5), size = 1.0)+
  geom_sf(data = occ_points, size = 1.5)+
  geom_sf(data = occ_buffer, fill = alpha("red",0.1))+
  coord_sf(xlim = c(st_bbox(occ_bbox)$xmin,st_bbox(occ_bbox)$xmax),
           ylim = c(st_bbox(occ_bbox)$ymin,st_bbox(occ_bbox)$ymax))+
  theme_bw()+
  theme(panel.border = element_rect(colour = "dodgerblue", fill=NA, size=1.6))+
  theme(legend.text = element_text(size = 14))+
  ggtitle(glue("{query$Value[which(query$Input == 'Species')]} - {buffval/1000}km buffer"))

p4 <- ggplot() +
  geom_sf(data = za, fill = alpha("grey",0.5), size = 1.0)+
  geom_sf(data = bck_points, size = 0.3)+
  coord_sf(xlim = c(st_bbox(occ_bbox)$xmin,st_bbox(occ_bbox)$xmax),
           ylim = c(st_bbox(occ_bbox)$ymin,st_bbox(occ_bbox)$ymax))+
  theme_bw()+
  theme(panel.border = element_rect(colour = "dodgerblue", fill=NA, size=1.6))+
  ggtitle(glue("{query$Value[which(query$Input == 'Species')]} - {nrow(bck_points)} background points"))

pdf(glue("data output/sdm data processing/{sppselect}/fig1.pdf"), width = 16, height = 9)
grid.arrange(grobs = list(p1,p2,p3,p4), ncol = 2) # Also see ggarrange
dev.off()

p <- arrangeGrob(grobs = list(p1,p2,p3,p4), ncol = 2)
ggsave(glue("data output/sdm data processing/{sppselect}/fig1.png"),
       p,
       width = 12, height = 6)

# Environmental data ------------------------------------------------------

## Extract query layer groups
envlayers <- query %>% 
  filter(str_detect(Input, "layers")) %>% 
  filter(Value == "Yes") %>% 
  select(Input) %>% 
  pull

## Extract environmental layers and create raster stack
extractfiles <- function(x) {
  stack(dir(str_c("C:/Users/DominicH/Documents/GIS data/Environmental data/",x),full.names = TRUE))
}

envstack <- stack(map(envlayers,extractfiles))

## Shorten the names of the Envirem variables
names(envstack)[which(str_detect(names(envstack),"current_2.5arcmin"))] <- str_replace(names(envstack)[which(str_detect(names(envstack),"current_2.5arcmin"))],"current_2.5arcmin_","")
names(envstack)

# Mask envstack raster ----------------------------------------------------

# This function makes sure that only cells that are defined across ALL raster layers are kept in the raster stack (if this isn't done then errors occur in the BIOMOD_projection step).

# I think this is more important when the rasters and other spatial data are projected.

## Function to define the intersect of rasters
intersect_mask <- function(x){
  values_x <- getValues(x)
  inter_x <- values_x %*% rep(1,nlayers(x))
  mask <- setValues(subset(x,1),values = (inter_x>0))
  return(mask)
}

## Keep only all cells that are defined for all layers
envstack <- stack(mask(envstack, intersect_mask(envstack)))

# Project rasters ---------------------------------------------------------

## Project the rasters to AEA and crop (20% buffer added) to occurence buffer
projenv_aea <- function(x){
  projection(x) <- latlongCRS
  projectRaster(x, crs=aeaproj)
}

projenv_latlong <- function(x){
  projection(x) <- latlongCRS
  projectRaster(x, crs=latlongCRS)
}

if (geo_proj == "Yes") {
  envstack <- stack(map(envstack@layers,projenv_aea)) %>% 
    crop(extent(bck_points)*1.10) # Add a 10% buffer
} else {
  envstack <- stack(map(envstack@layers,projenv_latlong)) %>% 
    crop(extent(bck_points)*1.10)
  }

## ****
## It may be good to scale and center environmental rasters before including them in the models
## Give this some more thought
## ****

# Collinearity of environemental predictors -------------------------------
ifelse(ncell(envstack) < 12000, bg_points <- ncell(envstack)/2, bg_points <- 10000)

pdf(glue("data output/sdm data processing/{sppselect}/fig2.pdf"), width = 12, height = 9)
non_collinear_vars <- virtualspecies::removeCollinearity(envstack, 
                                                         multicollinearity.cutoff = 0.7, 
                                                         select.variables = TRUE, 
                                                         sample.points = TRUE, 
                                                         nb.points = bg_points, 
                                                         plot = TRUE)
dev.off()

## Subset to only include non-collinear environmental variables
envstack <- raster::subset(envstack, subset = non_collinear_vars)

# Double check collinearity -----------------------------------------------
cormatrix <- raster::layerStats(envstack, "pearson", na.rm = TRUE)

cormatrix <- as_tibble(cormatrix$`pearson correlation coefficient`) %>% 
  mutate(var = colnames(cormatrix$`pearson correlation coefficient`)) %>% 
  select(var, everything()) %>% 
  write_csv(glue("data output/sdm data processing/{sppselect}/raster_corr_matrix.csv"))

## Read details and check whether we need to remove more variables
usdm::vif(envstack)
usdm::vifcor(envstack, th=0.9) 
usdm::vifstep(envstack, th=10) 

# Raster predictor plots --------------------------------------------------
enlen <- 1:nlayers(envstack)
plists <- split(enlen, ceiling(seq_along(enlen)/4))
plotras <- function(x){plot(envstack[[x]])}

pdf(glue("data output/sdm data processing/{sppselect}/fig3.pdf"), width = 16, height = 9)
map(plists, plotras)
dev.off()

# Write occurence points to shapefile -------------------------------------
occ_points %>% 
  st_write(glue("data output/sdm data processing/{sppselect}/occ_points_{sppselect}.shp"),
           delete_dsn=TRUE)

# Write workspace ---------------------------------------------------------
save(list = c("occ_points","bck_points","envstack"), 
     file = glue("data output/sdm data processing/{sppselect}/sdm_input_data.RData"))

print("SUCCESSFULLY COMPLETED")
