library(tidyverse)
library(readxl)
library(glue)
# Import query details ----------------------------------------------------
query <- read_xlsx("data input/SDM_query.xlsx") %>% 
  print

sppselect <- query$Value[which(query$Input == "Species")]

original <- glue("data output/rmarkdown documents/{sppselect}/SDM_report.html")
new <- glue("data output/rmarkdown documents/{sppselect}/SDM_report_{sppselect}.html")

file.rename(from = original, to = new)
file.copy(from = new, to = getwd(), overwrite = TRUE)
