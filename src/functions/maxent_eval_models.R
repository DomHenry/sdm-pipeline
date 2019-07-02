# Evaluate maxent models --------------------------------------------------

maxent_eval_models <- function(fold_data, fold_ref, fold_name, fold_num, maxlist) {
  
  fold_sf <- PB_data %>% 
    mutate(fold_ref = fold_ref) %>% 
    bind_cols(as_tibble(fold_data)) %>% 
    rename_at(vars(contains("value")),list(~str_replace(.,"value",fold_name))) %>% 
    mutate(traintest = ifelse(Species == 1,"Pr","Ab")) %>% 
    mutate_at(vars(fold_name), list(~ifelse((.) == TRUE, "train", "test"))) %>% 
    mutate_at(vars(fold_name), list(~str_c((.),traintest)))
  
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
