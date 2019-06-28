ttplots <- function(block_data, fold_name, fold_num, sb_id, endcol, plot_base){
  
  bd <- block_data
  
  fold_sf <- PB_data %>% 
    mutate(fold_ref = bd$foldID) %>% 
    bind_cols(as_tibble(bd$biomodTable)) %>% 
    mutate(traintest = ifelse(Species == 1,"Pr","Ab")) %>% 
    mutate_at(vars(RUN1:!!endcol), funs(ifelse((.) == TRUE, "train", "test"))) %>% 
    mutate_at(vars(RUN1:!!endcol), funs(str_c((.),traintest)))
  
  train_data <- fold_sf %>% 
    filter(fold_ref != fold_num) %>% 
    as(.,"Spatial") %>% 
    as.data.frame() %>% 
    select(lon,lat, !!fold_name) %>% 
    dplyr::rename(x = lon, y = lat)
  
  test_data <- fold_sf %>% 
    filter(fold_ref == fold_num) %>% 
    as(.,"Spatial") %>% 
    as.data.frame() %>% 
    select(lon,lat, !!fold_name) %>% 
    dplyr::rename(x = lon, y = lat)
  
  train_plot <- plot_base$plots + 
    geom_point(data = train_data, aes(x=x, y=y, col = !!fold_name, size = !!fold_name), alpha=0.9) +
    scale_size_manual(values = c(1,4), guide = FALSE) +
    scale_color_manual(name = "", labels = c("Background", "Presence"),values=c("black","blue")) +
    labs(title = glue("Training data set - Fold {fold_num}"), subtitle = "")
  
  test_plot <- plot_base$plots + 
    geom_point(data = test_data, aes(x=x, y=y, col = !!fold_name, size = !!fold_name), alpha=0.9) +
    scale_size_manual(values = c(1,4), guide = FALSE) +
    scale_color_manual(name = "", labels = c("Background", "Presence"),values=c("black","blue")) +
    labs(title = glue("Testing data set - Fold {fold_num}"), subtitle = "")
  
  if(sb_id == "eb"){
    ggarrange(train_plot,test_plot, ncol = 2, nrow = 1)
    ggsave(glue("{block_dir}/tt_plots_environ_fold{fold_num}.pdf"),width = 16, height = 9)
    ggsave(glue("{block_dir}/tt_plots_environ_fold{fold_num}.png"),width = 16, height = 9)
  }
  
  else {
    ggarrange(train_plot,test_plot, ncol = 2, nrow = 1)
    ggsave(glue("{block_dir}/tt_plots_{sb_id}_fold{fold_num}.pdf"),width = 16, height = 9)
    ggsave(glue("{block_dir}/tt_plots_{sb_id}_fold{fold_num}.png"),width = 16, height = 9)
  }
  
}
