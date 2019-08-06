# Failed species & reason -------------------------------------------------

# By increasing the raster buffer and doing the raster mask it seems that I've solved the error:
## Error in envBlock(rasterLayer = envstack, speciesData = PB_data, species = "Species",  : The input raster layer does not cover all the species points ##

# By settting randomtestpoints arg to 0 in maxent I've solved the following error:
# Warning: Skipping species because it has 0 test samples

# Afrixalus knysnae -------------------------------------------------------
# There are 14 raster layers
## Error in { : 
#     task 1 failed - "cannot take a sample larger than the population when 'replace = FALSE'"

## Also a coastal species (works with 280 points though.)

# Arthroleptella drewesii -------------------------------------------------
## ERROR 1: Failed at 04 spatialAutoRange() - "cannot take a sample larger than the population when 'replace = FALSE'". This has to do with the number of points specified (only way to deal with this for now is a custom input.)

# Arthroleptella landdrosia -----------------------------------------------
## ERROR 1: Failed at 04 spatialAutoRange() - "cannot take a sample larger than the population when 'replace = FALSE'". This has to do with the number of points specified (only way to deal with this for now is a custom input.)

# Arthroleptella lightfooti -----------------------------------------------
# NOT RUNNING THIS UNTIL WE HAVE MORE FINE SCALE DATA

#  Breviceps bagginsi -----------------------------------------------------
# ERROR 1: Failed at 04 spatialAutoRange() - "cannot take a sample larger than the population when 'replace = FALSE'". This has to do with the number of points specified (only way to deal with this for now is a custom input.)

# Breviceps branchi -------------------------------------------------------
# NOT RUNNING - only one valid data point at this stage


# ALL other cacosternum species -------------------------------------------

# Need to go back and find out why they are not showing up in the occ_records_date file and why some of the decade inset maps are missing - This could be due to no valid date information for any records.
