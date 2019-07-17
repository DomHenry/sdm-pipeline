# Failed species & reason -------------------------------------------------

##TODO --- NEED TO CREATE A REPEX OF THE FAILING BLOCKCV STEPS (POST TO GITHUB) ----

# Afrixalus knysnae -------------------------------------------------------
## Failed at 04 at sbfunc() - occurence records are highly clumped and there were only 3 blocks in which they occurred. Therefore the k = 5 argument gave an error. Solution is to reduced the number of blocks (i.e. k = 3) but this is hard to do in an automatic way.

# Arthroleptella drewesii -------------------------------------------------
## ERROR 1: Failed at 04 spatialAutoRange() - "cannot take a sample larger than the population when 'replace = FALSE'". This has to do with the number of points specified (only way to deal with this for now is a custom input.)
## ERROR 2: Error in envBlock(rasterLayer = envstack, speciesData = PB_data, species = "Species",  : The input raster layer does not cover all the species points ==> Need to go back and see what's going on (I think it has something to do with the species occuring on the coast)


# Arthroleptella landdrosia -----------------------------------------------
## ERROR 1: Failed at 04 spatialAutoRange() - "cannot take a sample larger than the population when 'replace = FALSE'". This has to do with the number of points specified (only way to deal with this for now is a custom input.)
## ERROR 2: Error in envBlock(rasterLayer = envstack, speciesData = PB_data, species = "Species",  : The input raster layer does not cover all the species points

# Arthroleptella lightfooti -----------------------------------------------
# NOT RUNNING THIS UNTIL WE HAVE MORE FINE SCALE DATA

# Arthroleptella rugosa ---------------------------------------------------
# Error in envBlock(rasterLayer = envstack, speciesData = PB_data, species = "Species",  : The input raster layer does not cover all the species points.

# Arthroleptella subvoce --------------------------------------------------
# Error in envBlock(rasterLayer = envstack, speciesData = PB_data, species = "Species",  : The input raster layer does not cover all the species points.


#  Breviceps bagginsi -----------------------------------------------------
# ERROR 1: Failed at 04 spatialAutoRange() - "cannot take a sample larger than the population when 'replace = FALSE'". This has to do with the number of points specified (only way to deal with this for now is a custom input.)

# ERROR 2: from 05 maxent_fold() - Error in file(con, "r") : cannot open the connection
# In addition: Warning messages:
#   1: Calling `as_tibble()` on a vector is discouraged, because the behavior is likely to change in the future. Use `tibble::enframe(name = NULL)` instead.
# This warning is displayed once per session. 
# 2: In file(con, "r") :
#   Error in file(con, "r") : cannot open the connection 


# Breviceps branchi -------------------------------------------------------
# NOT RUNNING - only one valid data point at this stage



# Breviceps gibbosus ------------------------------------------------------

# FAILED AT ENSEMBLE STAGE! Models are exhibiting really poor fit (so need to manually adjust the eval.metric.quality.threshold to a lower value so that some models are actually included in the output!



