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
library(biomod2)
library(dismo)
library(rasterVis)
library(glue)
library(RColorBrewer)

# TODO Need to synthesise all information from 04_blockCV and decide how many CV runs I want to do. Then adapt the code below to handle the relevant inputs. 

# Notes -------------------------------------------------------------------

# Import query details ----------------------------------------------------
query <- read_xlsx("data input/SDM_query.xlsx") %>% 
  print

# Import species and environmental data -----------------------------------
sppselect <- query$Value[which(query$Input == "Species")]
load(glue("data output/sdm data processing/{sppselect}/sdm_input_data.RData"))

# Blocking fold processing ------------------------------------------------

# Read in fold tables
foldtable <- dir(glue("data output/sdm data processing/{sppselect}/blockCV_results"),
    pattern = "fold_table", full.names = TRUE) %>% 
  map(read_csv) %>% 
  map_df(bind_rows) %>% 
  filter(type != "Buffer") # Remove buffer folds for now
  
# Filter folds which have insufficient testPr values
foldtable <- foldtable %>% 
  filter(testPr > 10) %>% 
  mutate(assignment = ifelse(is.na(assignment),"environ", assignment)) %>% 
  mutate(fold_dir = str_c(assignment,"_",foldID)) %>% 
  print(n = 20)

eb_keep <- foldtable %>% 
  filter(type == "Environmental") %>% 
  select(foldID) %>% 
  mutate(foldID = str_replace(foldID,"fold","")) %>% 
  pull %>% 
  as.numeric

eb_folds <- eb[["biomodTable"]][,eb_keep]

sb_check_keep <- foldtable %>% 
  filter(type == "Spatial" & assignment == "sb_check") %>% 
  select(foldID) %>% 
  mutate(foldID = str_replace(foldID,"fold","")) %>% 
  pull %>% 
  as.numeric

sb_check_folds <- sb_list$sb_check[["biomodTable"]][,sb_check_keep]

sb_ran_keep <- foldtable %>% 
  filter(type == "Spatial" & assignment == "sb_ran") %>% 
  select(foldID) %>% 
  mutate(foldID = str_replace(foldID,"fold","")) %>% 
  pull %>% 
  as.numeric

sb_ran_folds <- sb_list$sb_ran[["biomodTable"]][,sb_ran_keep]

sb_sys_keep <- foldtable %>% 
  filter(type == "Spatial" & assignment == "sb_sys") %>% 
  select(foldID) %>% 
  mutate(foldID = str_replace(foldID,"fold","")) %>% 
  pull %>% 
  as.numeric

sb_sys_folds <- sb_list$sb_sys[["biomodTable"]][,sb_sys_keep]

# Create Maxent output folders --------------------------------------------

## Root directory
maxres_dir <- glue("data output/sdm maxent results/{sppselect}")

if(dir.exists(maxres_dir)) {
  print("Folder exists")
} else {
  dir.create(maxres_dir)
  print("Folder created")
}

## Directories for each blocking run
folder_dir <- map_chr(foldtable %>% 
                        select(fold_dir) %>% 
                        pull,
                      function (x) glue("{maxres_dir}/{x}"))

walk(folder_dir, dir.create) ## Create output directories for each model


# Run Maxent --------------------------------------------------------------

## Information on the Maxent model: 

# "x" = raster stack 
# "p" = occ data (use occtrain to "train" the model)
# "a" = background data (See Elith et al. 2011)
# removeDuplicates = selects only one point in each raster grid cell
# Note on categorical data - use "factors" argument which takes a character (from layer name or index in raster stack) - see help 

# Arguments to specify:
# randomtestpoints = 30% of data used for testing, 70% used for training (split the data twice, once kfold and once inside the model) 
# betamultiplier = regularisation coefficient = 1
# features = linear, quadratic and hinge
# Jackknife (measure importance of each predictor variable by training with each predictor variable first omitted, then used in isolation)

## TODO Create input for betamultiplier and reg parameter
## TODO Move these functions into separate scripts

## Maxent function
maxent_fold <- function(fold_data, fold_ref, fold_name, fold_num, fold_dir) {
  
  fold_sf <- PB_data %>% 
    mutate(fold_ref = fold_ref) %>% 
    bind_cols(as_tibble(fold_data)) %>% 
    rename_at(vars(contains("value")),funs(str_replace(.,"value",fold_name))) %>% 
    mutate(traintest = ifelse(Species == 1,"Pr","Ab")) %>% 
    mutate_at(vars(fold_name), funs(ifelse((.) == TRUE, "train", "test"))) %>% 
    mutate_at(vars(fold_name), funs(str_c((.),traintest)))
  
  train_data <- fold_sf %>% 
    filter(fold_ref != fold_num) %>% 
    as(.,"Spatial") %>% 
    as.data.frame() %>% 
    select(lon,lat, fold_name) %>% 
    dplyr::rename(x = lon, y = lat)

  spp_ME <- maxent(
    x = envstack, 
    p = train_data %>% 
      filter(train_data[[fold_name]] %in% "trainPr") %>% 
      select(x, y),  
    a = train_data %>% 
      filter(train_data[[fold_name]] %in% "trainAb") %>% 
      select(x, y),
    removeDuplicates=TRUE, 
    path = fold_dir, 
    args = c("randomtestpoints=30", "betamultiplier=1", 
             "linear=true", # need to see if there is a way to specify test and train data sets in args 
             "quadratic=true", "product=true", "threshold=true", 
             "hinge=true", "threads=2", "responsecurves=true", 
             "jackknife=true","askoverwrite=false")
  )
  
  return(spp_ME)
}

## Generate data for Maxent function arguments
fold_data <- list()
fold_ref <- list()

for(i in 1:nrow(foldtable)){
  
  if(foldtable[i,"assignment"] == "environ"){
    fold_data[[i]] <- eb_folds
    fold_ref[[i]] <- eb$foldID
    
    } else if (foldtable[i,"assignment"] == "sb_check"){
      fold_data[[i]] <- sb_check_folds
      fold_ref[[i]] <- sb_list$sb_check$foldID
      
    } else if (foldtable[i,"assignment"] == "sb_sys"){
      fold_data[[i]] <- sb_sys_folds
      fold_ref[[i]] <- sb_list$sb_sys$foldID
      
    } else if(foldtable[i,"assignment"] == "sb_ran"){
      fold_data[[i]] <- sb_ran_folds
      fold_ref[[i]] <- sb_list$sb_ran$foldID
    }
  }
fold_name <- as.list(str_replace(foldtable$foldID,"fold","RUN"))
fold_num <- as.list(as.numeric(str_replace(foldtable$foldID,"fold","")))
fold_dir <- folder_dir

# Yay! It works...
maxlist <- pmap(
  .l = list(fold_data, fold_ref, fold_name, fold_num, fold_dir),
  .f = maxent_fold
  )

# test <- 1
# fold_data <- fold_data[[test]]
# fold_ref <- fold_ref[[test]]
# fold_name <- fold_name[[test]]
# fold_num <- fold_num[[test]]
# fold_dir <- folder_dir[[test]]


# Testing the model -------------------------------------------------------

## Use "evaluate" to get the reliability of the model 
## Generated with the "occtrain" data against the "occtest" data. AUC > 0.8 is acceptable

eval_models <- function(fold_data, fold_ref, fold_name, fold_num, maxlist) {
  
  # fold_sf <- PB_data %>% 
  #   mutate(fold_ref = block_data$foldID) %>% 
  #   bind_cols(as_tibble(block_data$biomodTable)) %>% 
  #   mutate(traintest = ifelse(Species == 1,"Pr","Ab")) %>% 
  #   mutate_at(vars(RUN1:!!endcol), funs(ifelse((.) == TRUE, "train", "test"))) %>% 
  #   mutate_at(vars(RUN1:!!endcol), funs(str_c((.),traintest)))
  
  fold_sf <- PB_data %>% 
    mutate(fold_ref = fold_ref) %>% 
    bind_cols(as_tibble(fold_data)) %>% 
    rename_at(vars(contains("value")),funs(str_replace(.,"value",fold_name))) %>% 
    mutate(traintest = ifelse(Species == 1,"Pr","Ab")) %>% 
    mutate_at(vars(fold_name), funs(ifelse((.) == TRUE, "train", "test"))) %>% 
    mutate_at(vars(fold_name), funs(str_c((.),traintest)))
  
  # test_data <- fold_sf %>% 
  #   filter(fold_ref == fold_num) %>% 
  #   as(.,"Spatial") %>% 
  #   as.data.frame() %>% 
  #   select(lon,lat, !!fold_name) %>% 
  #   dplyr::rename(x = lon, y = lat)
  
  test_data <- fold_sf %>% 
    filter(fold_ref == fold_num) %>% 
    as(.,"Spatial") %>% 
    as.data.frame() %>% 
    select(lon,lat, fold_name) %>% 
    dplyr::rename(x = lon, y = lat)
  
  eval_ME <- dismo::evaluate(
    model = maxlist, 
    x = envstack, 
    p = test_data %>% 
      filter(test_data[[fold_name]] %in% "testPr") %>% 
      select(x, y), 
    a = test_data %>% 
      filter(test_data[[fold_name]] %in% "testAb") %>% 
      select(x, y)
    )
    
    return(eval_ME)
 }


eval_list <- pmap(.l = list(fold_data, fold_ref, fold_name, fold_num, maxlist),
                  .f = eval_models
                  )
eval_list

# Diagnostics -------------------------------------------------------------

## Create folder for diagnostics
diag_dir <- glue("{maxres_dir}/diagnostics")
dir.create(diag_dir)

## Thresholds - dismo::threshold()
  # kappa: the threshold at which kappa is highest ("max kappa")
  # spec_sens: the threshold at which the sum of the sensitivity (true pos rate) and specificity (true neg rate) is highest
  # no_omission: the highest threshold at which there is no omission
  # prevalence: modeled prevalence is closest to observed prevalence
  # equal_sens_spec: equal sensitivity and specificity
  # sensitivty: fixed (specified) sensitivity
foldtable

map_df(eval_list, threshold) %>% 
  # mutate(model = glue("Model{c(1:length(eval_list))} thresholds")) %>% 
  mutate(model = foldtable$fold_dir) %>% 
  select(model, everything()) %>% 
  mutate(auc = map_dbl(eval_list, "auc"),
         n_pres = map_dbl(eval_list, "np"),
         n_abs = map_dbl(eval_list, "na"),
         cor = map_dbl(eval_list, "cor")) %>%
  select(model, auc:cor, everything()) %>% 
  as_tibble() %>% 
  arrange(desc(auc)) %>% 
  print(n = 7) %>% 
  write_csv(glue("{diag_dir}/thresholds & auc.csv"))
  
pdf(glue("{diag_dir}/auc_plots.pdf"))
par(mfrow = c(2,2))
for (i in 1:nrow(foldtable)){
  plot(eval_list[[i]], "ROC", sub=foldtable$fold_dir[[i]])
}
dev.off()

for (i in 1:nrow(foldtable)){
  png(glue("{diag_dir}/auc_plot_{foldtable$fold_dir[[i]]}.png"))
  plot(eval_list[[i]], "ROC", sub=foldtable$fold_dir[[i]])
  dev.off()
}

#Create a density plots of presence and absence data and boxplots
par(mfrow = c(2,2))
for (i in 1:nrow(foldtable)){
  density(eval_list[[i]])
}

# Make a box plot of model evaluation data, i.e., the model predictions for known presence and absence points.
pdf(glue("{diag_dir}/predic_boxplots.pdf"))
par(mfrow = c(2,2))
for (i in 1:nrow(foldtable)){
  boxplot(eval_list[[i]], main  = quo_name(foldtable$fold_dir[[i]]))
}
dev.off()

# Model predictions - full ------------------------------------------------

## Import SA boundary for plotting
geo_proj <- query$Value[which(query$Input == "Geographic projection")]
aeaproj <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

if (geo_proj == "Yes") {
  za <- st_read("data input/PR_SA.shp",crs = latlongCRS) %>% 
    st_transform(aeaproj) 
} else {
  za <- st_read("data input/PR_SA.shp",crs = latlongCRS)
}

## Create range predictions folder
pred_pdf_dir <- glue("{maxres_dir}/spatial_predictions_pdf")
dir.create(pred_pdf_dir)

## Create raster directory 
pred_ras_dir <- glue("{maxres_dir}/spatial_predictions_raster")
dir.create(pred_ras_dir)

## Create PNG directory 
pred_png_dir <- glue("{maxres_dir}/spatial_predictions_png")
dir.create(pred_png_dir)

# TODO Move these pred functions to functions folder in own script

pred_rang_full <- function(x,fold_name,output){
  
  pred_range <- dismo::predict(x, envstack, progress = "text") 
  
  if (output == "pdf") {
    plot(pred_range, xlab="Lon", ylab="Lat", las=1, main= glue("{sppselect} - {quo_name(fold_name)}")) 
    plot(occ_points$geometry,pch=16, cex=0.8, add = T)  
  }
  
  if (output == "raster") {
    writeRaster(pred_range, 
                filename = glue("{pred_ras_dir}/{sppselect}_range_{quo_name(fold_name)}.tif"), 
                format="GTiff", 
                overwrite=TRUE) 
  }

  else if (output == "png") {
    
    p <- rasterVis::levelplot(pred_range, 
                              main= glue("{sppselect} - {quo_name(fold_name)}"),
                              contour = FALSE, 
                              margin = FALSE,
                              col.regions = rev(terrain.colors(40)))+
      layer(sp.points(as(occ_points,"Spatial"),pch = 19, cex = 1.5, col = "black"))+
      layer(sp.lines(as(za, "Spatial"), lwd = 2.5, col = "black"))
    
    p <- arrangeGrob(grobs = list(p), nrow = 1)
    ggsave(
      glue("{pred_png_dir}/{sppselect}_range_{quo_name(fold_name)}.png"),
      p,
      width = 16, height = 9)
 
  }
  
}

## PDF
pdf(glue("{pred_pdf_dir}/pred_full.pdf"))
par(mfrow = c(2,2))
pwalk(list(maxlist, foldtable$fold_dir,list("pdf")),
      pred_rang_full)
dev.off()

## Rasters
pwalk(list(maxlist, foldtable$fold_dir,list("raster")),
      pred_rang_full)

# PNG 
pmap(list(maxlist, foldtable$fold_dir,list("png")),
      pred_rang_full)


# Model predictions - threshold -------------------------------------------
pred_rang_thresh <- function(x,fold_name,pred_thresh,output){

  pred_range <- dismo::predict(x, envstack, progress = "text") 
  pred_thresh_mat <- matrix(c(-pred_thresh, pred_thresh, 0), ncol=3, byrow=TRUE)
  pred_range_thresh <- reclassify(pred_range, pred_thresh_mat)
  
  if (output == "pdf") {
    plot(pred_range_thresh, xlab="Lon", ylab="Lat", las=1, main= glue("{quo_name(fold_name)} - Thresh {pred_thresh}")) 
    plot(occ_points$geometry,pch=16, cex=0.8, add = T)
    
  }
  
  if (output == "raster") {
    writeRaster(pred_range, 
                filename = glue("{pred_ras_dir}/{sppselect}_thresh_{quo_name(fold_name)}.tif"), 
                format="GTiff", 
                overwrite=TRUE) 
    }
  }

## Using kappa
pred_thresh <- map_df(eval_list, threshold) %>% 
  select(kappa) %>% 
  pull() %>% 
  as.list()

## Manual (alternative)
pred_thresh <- list(0.6)

## Plot
pdf(glue("{pred_pdf_dir}/pred_kappa_thresh.pdf"))
par(mfrow = c(2,2))
pwalk(list(maxlist, foldtable$fold_dir,pred_thresh,list("pdf")),
      pred_rang_thresh)
dev.off()

## Raster 
pwalk(list(maxlist, foldtable$fold_dir,pred_thresh,list("raster")),
      pred_rang_thresh)

# Model predictions - mean probability  -----------------------------------

## Mean probability over the study extent the can be used as the threshold 
## (above which species is considered "present" and below which species is considered "absent")\
## raster::reclassify = reclassify "probability of presence" raster using the calculated threshold value as the "zero" point

pred_mean <- function(x,fold_name,output){
  pred_range <- dismo::predict(x, envstack, progress = "text") 
  pred_mean <- cellStats(pred_range, stat="mean", na.rm=TRUE)
  pred_mean_mat <- matrix(c(-pred_mean, pred_mean, 0), ncol=3, byrow=TRUE)
  pred_range_mean <- reclassify(pred_range, pred_mean_mat)

  if (output == "pdf") {
    plot(pred_range_mean, xlab="Lon", ylab="Lat", las=1, main= glue("{quo_name(fold_name)} - Mean probablility")) 
    plot(occ_points$geometry,pch=16, cex=0.8, add = T)
  }
  
  if (output == "raster") {
    writeRaster(pred_range, 
                filename = glue("{pred_ras_dir}/{sppselect}_mean_{quo_name(fold_name)}.tif"), 
                format="GTiff", 
                overwrite=TRUE) 
  }
}

pdf(glue("{pred_pdf_dir}/pred_mean.pdf"))
par(mfrow = c(2,2))
pwalk(list(maxlist,foldtable$fold_dir,list("pdf")),
      pred_mean)
dev.off()

## Raster 
pwalk(list(maxlist,foldtable$fold_dir,list("raster")),
      pred_mean)

# Write query to folder ---------------------------------------------------
write_csv(query, path = glue("{maxres_dir}/USER_QUERY.csv"))

# Write workspace ---------------------------------------------------------
save.image(file = glue("{maxres_dir}/maxent_sdm_output_data.RData"))


# Write blocking data workspace -------------------------------------------
save(list = c("sb_list","sb_ran_folds", 
              "sb_check_folds","sb_sys_folds", 
              "eb_folds","foldtable"), 
     file = glue("data output/sdm data processing/{sppselect}/blockCV_data.RData"))

# Overlay urban landscape -------------------------------------------------

## LEAVE THIS OUT FOR NOW ##

# aeaproj <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
# latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# za <- st_read("data input/PR_SA.shp",crs = latlongCRS) %>% 
#   st_transform(aeaproj) 
# 
# urblc <- st_read("C:/Users/DominicH/Documents/GIS data/Environmental data/Urban shapefile/RasterT_Reclass3.shp") %>% 
#   st_transform(aeaproj)
# 
# rasex <- pred_range@extent
# 
# urb <- urblc %>% 
#   st_crop(xmin = rasex@xmin, xmax = rasex@xmax, ymin = rasex@ymin , ymax = rasex@ymax) %>% 
#   st_buffer(dist = 100)
# 
# urb <- urb %>% 
#   st_union()
# 
# urbspdf <- as(urb, "Spatial")
# rm(urblc)
# 
# pdf(paste0("data output/sdm maxent results/",sppselect,"/",sppselect,"_range_map_urban.pdf"),width = 16, height = 9)
# mapTheme <- rasterTheme(region=rev(terrain.colors(10))) # brewer.pal(8,"Greens")
# plt <- levelplot(pred_range, margin=F, par.settings=mapTheme)
# plt + layer(sp.lines(as(za,"Spatial"),col="black", lwd=1)) +
#   layer(sp.polygons(urbspdf,fill = alpha("black", 0.3),col="black", lwd=0.2)) #check sp.lines
# dev.off()

