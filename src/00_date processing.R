# Description -------------------------------------------------------------

## Thu Jan 10 10:47:30 2019 
## Import the original amphibian database, subset to include only RL, NT and DD 
## species. Remove duplicate records then work on dates. Create 4 DFs with
## different date combinations (full date, month & year, year, no valid date).
## Write workspace with DF that has a column with full date and NA for any
## other date that contains errors in day, month or year. Also write csvs
## for remaing 3 date combinations.

library(tidyverse)
library(lubridate)
library(gghighlight)
library(gridExtra)
library(ggpubr)
library(sf)
library(viridis)
library(gt)
library(glue)

walk(dir("src/functions/", full.names = TRUE),
    source)

# Import data -------------------------------------------------------------

## Red-listed endemic species
rl <- low_rm_space(read_csv("data input/vertebrate_threatend_endemic.csv")) %>% 
  rename(rls = `regional_red-list_status`) %>% 
  filter(class == "Amphibia")

## Original database from John Measey (records up until 2015)
amph_all <- low_rm_space(read_csv("data input/SA_Amphibians_All_data_full.csv")) %>% 
  mutate(genus = ifelse(genus == "Amietophrynus","Sclerophrys",genus)) %>% # New genus for Leopard Toads
  mutate(scientificname = ifelse(str_detect(scientificname,"Amietophrynus"),
                                 str_replace(scientificname,"Amietophrynus", "Sclerophrys"), 
                                 scientificname))

## Check for duplicate rows upfront
print(glue::glue("{nrow(distinct(amph_all))/nrow(amph_all)*100}% UNIQUE RECORDS"))

## Create IUCN species vector
spp <- sort(rl$latin_name)

# Select columns and fiter RL species -------------------------------------
amph <- amph_all %>% 
  select(objectid, order,family,genus,scientificname,decimallatitude,decimallongitude,coordinateprecision,
         basisofrecord,year,month,day,coord_notes,qds,errors,error_notes) %>% 
  filter(scientificname %in% spp)

# Investigate duplicates --------------------------------------------------
distinct(amph %>% select(-objectid)) #Check for duplicate rows again when objectid column is removed

print(glue::glue("{round(nrow(distinct(amph %>% select(-objectid)))/nrow(amph %>% select(-objectid))*100,2)}% UNIQUE RECORDS"))

## Create a folder for errors 
err_dir <- c("data output/occurence record errors")
dir.create(err_dir)

### See example for various ways to filter duplicates ###
test <- tibble(ob = c("id1","id2","id3","id4"),
               val1 = c(1,2,2,1),
               val2 = c(1,3,4,1)) %>% 
  group_by_at(vars(-ob))

test
test %>% filter(n() > 1) # Keep non-distinct only
test %>% filter(n() == 1) # Exclude any non-distinct
test %>% filter(row_number() == 1) # Select the first row of duplicated values
###

## Write duplicates to file (non-distinct values)
amph %>% 
  group_by_at(vars(-objectid)) %>% 
  filter(n() > 1) %>%  # keep non-distinct values only (i.e. duplicates)
  arrange(order,family,genus,scientificname,
          decimallatitude,decimallongitude,
          basisofrecord,coord_notes) %>% 
  write_csv(glue("{err_dir}/duplicate_records.csv"))

## Remove duplicates from remainder of analysis (select distinct rows) 
amph <- amph %>% 
  group_by_at(vars(-objectid)) %>% #i.e. remove objectID before grouping (equivalent group_by_at(vars(order:error_notes))
  filter(row_number() == 1) %>% 
  ungroup()

# DF with full date (no errors) -------------------------------------------
amph_dmy <- amph %>% 
  filter(!(year == 0 | is.na(year) | year < 1800 |
             month == 0 | is.na(month) | month > 12 | 
             day == 0 | is.na(day) | day > 31)) %>% 
  filter(!(month == 2 & day > 28)) %>% # Remove dates like 30th of Feb and 31st of Nov
  filter(!(month == 11 & day > 30)) %>% 
  mutate(date = dmy(str_c(day,month,year,sep = "-")))

# DF with valid month and year (assign day = 1) ---------------------------
amph_my <- amph %>% 
  filter(!(year == 0 | is.na(year) | year < 1800 |
             month == 0 | is.na(month) | month > 12)) %>% 
  mutate(day = 1) %>% 
  mutate(date = dmy(str_c(day,month,year,sep = "-")))

# DF with valid year (assign month = 1 and day = 1) -----------------------
amph_y <- amph %>% 
  filter(!(year == 0 | is.na(year) | year < 1800)) %>% 
  mutate(day = 1, month = 1) %>% 
  mutate(date = dmy(str_c(day,month,year,sep = "-")))

# DF with no date information ---------------------------------------------
amph_nd <- amph %>% 
  filter(year == 0 | is.na(year) | year < 1800 |
           month == 0 | is.na(month) | month > 12 | 
           day == 0 | is.na(day) | day > 31 |
           month == 2 & day >28 |
           month == 11 & day > 30)

## Check
nrow(amph) == nrow(amph_dmy) + nrow(amph_nd)
nrow(amph) == nrow(amph_dmy) + nrow(amph_my) + nrow(amph_y)

# Create table summarising date time relevant records ---------------------
amph_date_table <- amph_nd %>% 
  group_by(scientificname) %>%
  tally %>% 
  arrange %>% 
  left_join(rl %>% select(latin_name,rls), by = c("scientificname" = "latin_name")) %>% 
  rename(no_date = n) %>% 
  full_join(amph_dmy %>% 
              filter(scientificname %in% unique(amph_nd$scientificname)) %>% 
              group_by(scientificname) %>% 
              tally %>% 
              arrange %>% 
              rename(with_date = n),
            by = "scientificname") %>% 
  mutate(total_records = no_date + with_date,
         prop_no_date = round((no_date/(total_records))*100,2)) %>% 
  print(n = 29)


# Merge DFs ---------------------------------------------------------------
amph <- full_join(amph, amph_dmy)

# Create validity variables for each date component -----------------------
amph <- amph %>% 
  mutate(year_check = ifelse(year == 0 | is.na(year) | year < 1800, "invalid","valid")) %>% 
  mutate(month_check = ifelse(month == 0 | is.na(month) | month > 12, "invalid","valid")) %>% 
  mutate(day_check = ifelse(day == 0 | is.na(day) | day > 31 , "invalid","valid")) 

# Write csvs --------------------------------------------------------------

## Create output directory
out_dir <- c("data output/occurence record dates")
dir.create(out_dir)

write_csv(amph_date_table, glue("{out_dir}/occ_records_date_summary.csv"))
write_csv(amph_my, glue("{out_dir}/amph_occ_records_month_year.csv"))
write_csv(amph_y, glue("{out_dir}/amph_occ_records_year.csv"))
write_csv(amph_nd, glue("{out_dir}/amph_occ_records_no_date.csv"))

# Amph date table in GT package -------------------------------------------
amph_gt <- amph_date_table %>% 
  gt() %>% 
  tab_header(
    title = md("**Breakdown of occurence records and date errors**"),
    subtitle = md(glue("*Includes data for {length(spp)} Amphibian species*"))) %>% 
  tab_source_note(source_note = md("*Analysis based on database provided by John Measey*")) %>% 
  cols_move_to_start(columns = vars(scientificname,rls,total_records,with_date,no_date, prop_no_date)) %>% 
  cols_label(scientificname = md("**Species**"),
             rls = md("**Red-list status**"),
             total_records = md("**Total records**"),
             with_date = md("**Valid date**"),
             prop_no_date = md("**No date information (%)**"),
             no_date = md("**No date information**")) 


amph_gt <- amph_gt %>% # Need to do this in two steps for some reason
  tab_style(style = cells_styles(text_style = "italic"),
            locations = cells_data(columns = vars(scientificname))) %>% 
  tab_style(style = cells_styles(text_align =  "center"),
            locations = cells_data(columns = vars(total_records,with_date,no_date,prop_no_date)))

# Can't find a way to automatically write this html to file (can do it via the Viewer tab)

# Write workspaces --------------------------------------------------------
save(list = c("amph","amph_all","rl","spp","err_dir"), file = "data output/amph_data_clean.RData")
