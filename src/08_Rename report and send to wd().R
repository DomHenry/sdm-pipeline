library(tidyverse)
library(readxl)
library(glue)

# Import query details ----------------------------------------------------
query <- read_xlsx("data input/SDM_query.xlsx") %>% print
sppselect <- query$Value[which(query$Input == "Species")]
spp_short <- str_c(str_sub(word(sppselect,1),1,2),str_sub(word(sppselect,2),1,3))

original <- glue("data output/rmarkdown documents/{sppselect}/SDM_report.html")
new <- glue("data output/rmarkdown documents/{sppselect}/SDM_report_{sppselect}.html")
file.rename(from = original, to = new)

# Create sdm report folder ------------------------------------------------
report_dir <- c("sdm reports")

if(dir.exists(report_dir)) {
  print("Folder exists")
} else {
  dir.create(report_dir)
  print("Folder created")
}

file.copy(from = new, to = report_dir, overwrite = TRUE) 

# Temporarily copy files needed for fieldwork analysis --------------------

## Raster data - rename file first
original <- glue("data output/sdm ensemble results/{sppselect}/ensemble_maps/{spp_short}_EMcaByROC.tif")
new <- glue("data output/sdm ensemble results/{sppselect}/ensemble_maps/{sppselect}_EMcaByROC.tif")
file.rename(from = original, to = new)
file.copy(new, report_dir, recursive=TRUE)

## Occ points shapefile
copyfrom <- dir(glue("data output/sdm data processing/{sppselect}"), 
    pattern = c(".dbf|.shp|.prj|.shx"),
    full.names = TRUE)

file.copy(copyfrom, report_dir, recursive=TRUE)

## Raster data - best Maxent Model
auc_table <- read_csv(glue("data output/sdm maxent results/{sppselect}/diagnostics/thresholds & auc.csv"))
topmod <- auc_table$model[[1]]
copyfrom <- glue("data output/sdm maxent results/{sppselect}/spatial_predictions_raster/{sppselect}_range_{topmod}.tif")

file.copy(copyfrom, report_dir, recursive=TRUE)

print("SUCCESSFULLY COMPLETED")
