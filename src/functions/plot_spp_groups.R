plot_spp_groups <- function(x){
  
  ggplot() +
    geom_sf(data = za, fill = alpha("grey",0.5))+
    geom_sf(data = amphsf %>% filter(scientificname %in% spp[x]), 
            aes(col = scientificname), size = 1.5)+
    scale_color_viridis_d()+
    theme_bw()+
    theme(legend.text = element_text(size = 14),
          legend.title = element_blank(),
          axis.text = element_text(size = 14))
}