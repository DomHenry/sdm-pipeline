# Plot occurence points for each species per decade -----------------------

plot_single_spp_decade <- function(x){
  ggplot()+
    geom_sf(data = sa,fill = alpha("grey",0.5))+
    geom_sf(data =  amphsf %>% filter(scientificname %in% spp[x]),
            aes(color = decade,fill = decade))+
    scale_color_viridis_d(guide = FALSE)+
    scale_fill_viridis_d()+
    guides(fill = guide_legend(title="Decade"))+
    theme(legend.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          strip.text = element_text(size = 14),
          axis.text = element_text(size = 14))+
    facet_wrap(~scientificname)
}
