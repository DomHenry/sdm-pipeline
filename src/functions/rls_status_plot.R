rls_status_plot <- function(x,rlscat,spp_rm){
  
  nspp <- rl %>% 
    group_by(rls) %>% 
    tally %>% 
    filter(rls == rlscat) %>% 
    select(n) %>% 
    pull
  
  ggplot(x %>% filter(!scientificname %in% spp_rm))+
    geom_sf(color = "red",size = 1.5)+
    gghighlight(rls == rlscat)+ 
    geom_sf(data = sa_merge, fill = alpha("grey",0))+
    labs(title = glue::glue("{rlscat} species occurrence records"),
         subtitle = glue::glue("Total of {nspp} species"))
}