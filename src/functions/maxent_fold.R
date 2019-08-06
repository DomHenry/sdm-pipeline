# Maxent function  --------------------------------------------------------

# Run maxent model for each input blockCV fold

maxent_fold <- function(fold_data, fold_ref, fold_name, fold_num, fold_dir) {
  
  fold_sf <- PB_data %>% 
    mutate(fold_ref = fold_ref) %>% 
    bind_cols(as_tibble(fold_data)) %>% 
    rename_at(vars(contains("value")),list(~str_replace(.,"value",fold_name))) %>% 
    mutate(traintest = ifelse(Species == 1,"Pr","Ab")) %>% 
    mutate_at(vars(fold_name), list(~ifelse((.) == TRUE, "train", "test"))) %>% 
    mutate_at(vars(fold_name), list(~str_c((.),traintest)))
  
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
    args = c("randomtestpoints=0", "betamultiplier=1", 
             "linear=true", # need to see if there is a way to specify test and train data sets in args 
             "quadratic=true", "product=true", "threshold=true", 
             "hinge=true", "threads=2", "responsecurves=true", 
             "jackknife=true","askoverwrite=false")
  )
  
  return(spp_ME)
}