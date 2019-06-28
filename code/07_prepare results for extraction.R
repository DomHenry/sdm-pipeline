# Notes -------------------------------------------------------------------

# Will have to write shapefile with different thresholds than can be visualised in Google Earth (for closer inspection and field site selection) -i.e. in KML format. Should also include cocc points - but can actually do this all in QGIS with the plugin?

# What about a leaflet map for each species?? can host these separately on Rpubs or GitHub - would make it easier to explore potential fieldwork sites.

# TODO Add the tables of test and train samples for each fold (use the eb[["records"]] values). Add them to the ensemble diagnostic plots (we'll thus be able to see why certain folds perform so badly)

# List of objects to include in report ------------------------------------

# 1. Frequency distribution of occ records (shaded against total)[both from 1940 & 1970]
# 2. Summary table of records with dates and errors.
# 3. Overll species occurence plot with SA map inset showing decadal information
# 4. Plot with prediction buffer
# 5. Diagnostic plots - AUC
# 6. Table of user query - showing input parameters
# 7. Maxent map, ensemble CA, mean and CV maps (need to plot points over maps - add this to ensemble code)
# 8. Include threat status plot
# 9. Overlay of urban cover - to guide surveys away from urban centers.

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
library(gt)

# Import query details ----------------------------------------------------
query <- read_xlsx("data input/SDM_query.xlsx") %>% 
  print

sppselect <- query$Value[which(query$Input == "Species")]

# Create species folder ---------------------------------------------------

rmark_dir <- glue("data output/rmarkdown documents/{sppselect}")

if(dir.exists(rmark_dir)) {
  print("Folder exists")
} else {
  dir.create(rmark_dir)
  print("Folder created")
}

# Import species data -----------------------------------------------------
load(glue("data output/sdm data processing/{sppselect}/sdm_input_data.RData"))
load(glue("data output/sdm data processing/{sppselect}/blockCV_data.RData"))

# Import spatial and temporal summary plots -------------------------------
copyfrom <- c(glue("data output/temporal occurrence plots/temp_freq_{sppselect}.png"),
              glue("data output/spatial occurrence plots/national_{sppselect}.png"),
              glue("data output/spatial occurrence plots/inset_{sppselect}.png"),
              glue("data output/spatial occurrence plots/decade_inset_{sppselect}.png"))

file.copy(copyfrom, rmark_dir, recursive=TRUE)

# Import SDM occ data summary figure --------------------------------------
copyfrom <- glue("data output/sdm data processing/{sppselect}/fig1.png")
file.copy(copyfrom, rmark_dir, recursive=TRUE)

# Import the threat status plot -------------------------------------------
copyfrom <- c("data output/spatial occurrence plots/fig6 - threat_status_facet.png")
file.copy(copyfrom,rmark_dir, recursive=TRUE)

# Import MaxEnt prediction plots ------------------------------------------

## Select top model & use it to select figs
auc_table <- read_csv(glue("data output/sdm maxent results/{sppselect}/diagnostics/thresholds & auc.csv"))
topmod <- auc_table$model[[1]]

copyfrom <- glue(
  "data output/sdm maxent results/{sppselect}/spatial_predictions_png/{sppselect}_range_{topmod}.png"
  )
file.copy(copyfrom, rmark_dir, recursive=TRUE)

# Import Maxent AUC plots -------------------------------------------------
copyfrom <- glue("data output/sdm maxent results/{sppselect}/diagnostics/auc_plot_{topmod}.png")
file.copy(copyfrom, rmark_dir, recursive=TRUE)

# Import ensemble diagnostics -------------------------------------------
copyfrom <- c(
  glue("data output/sdm ensemble results/{str_replace(sppselect, ' ', '.')}/diagnostics/avg_model_scores1.png"),
  glue("data output/sdm ensemble results/{str_replace(sppselect, ' ', '.')}/diagnostics/variable_importance.png")
  )

file.copy(copyfrom,rmark_dir, recursive=TRUE)

# Import ensemble maps ----------------------------------------------------

## TODO Perhaps write a query to extract specific maps

copyfrom <- c(
    glue("data output/sdm ensemble results/{str_replace(sppselect, ' ', '.')}/ensemble_maps/{str_replace(sppselect, ' ', '.')}_EMcvByROC.png"),
    glue("data output/sdm ensemble results/{str_replace(sppselect, ' ', '.')}/ensemble_maps/{str_replace(sppselect, ' ', '.')}_EMmeanByROC.png"),
    glue("data output/sdm ensemble results/{str_replace(sppselect, ' ', '.')}/ensemble_maps/{str_replace(sppselect, ' ', '.')}_EMcaByROC.png")
)

file.copy(copyfrom,rmark_dir, recursive=TRUE)

# Import EB and SB tables -------------------------------------------------
foldtable
write_csv(foldtable, glue("{rmark_dir}/foldtable.csv"))

# copyfrom <- c(glue("data output/sdm data processing/{sppselect}/blockCV_results/eb_fold_table.csv"),
#               glue("data output/sdm data processing/{sppselect}/blockCV_results/sb_fold_table.csv"))
# 
# file.copy(copyfrom,rmark_dir, recursive=TRUE)

# Import rmarkdown template into folder -----------------------------------
copyfrom <- c("code/SDM report.Rmd")
file.copy(copyfrom,rmark_dir, recursive=TRUE)

# Import blockCV plots ----------------------------------------------------
foldtable$fold_dir
copyfrom <- glue("data output/sdm data processing/{sppselect}/blockCV_results/tt_plots_{foldtable$fold_dir}.png")
file.copy(copyfrom,rmark_dir, recursive=TRUE)

