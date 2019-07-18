# Source steps 3 - 7 ------------------------------------------------------
source("src/03_SDM import data.R")
rm(list = ls())
source("src/04_blockCV.R")
rm(list = ls())
source("src/05_SDM MaxEnt.R")
rm(list = ls())
source("src/06_SDM ensembles.R")
rm(list = ls())
source("src/07_prepare results for extraction.R")
rm(list = ls())

# Render Rmd file ---------------------------------------------------------
query <- readxl::read_xlsx("data input/SDM_query.xlsx") %>% print
sppselect <- query$Value[which(query$Input == "Species")]
rmarkdown::render(input = glue::glue("data output/rmarkdown documents/{sppselect}/SDM report.Rmd"),
                  output_format = "html_document",
                  output_dir = glue::glue("data output/rmarkdown documents/{sppselect}"))
rm(list = ls())

# Source step 8 -----------------------------------------------------------
source("src/08_Rename report and send to wd().R")
