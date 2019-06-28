
# Plot highlighted species records against all records --------------------
occ_date_high_plot <- function(x, spp_ref,startdate){
  x %>% 
    filter(date > startdate,
           !scientificname %in% "Sclerophrys pantherinus") %>% 
    ggplot(aes(x = year,fill = scientificname))+
    geom_bar() +
    gghighlight(scientificname %in% spp[spp_ref])+
    ylab("Count")+
    theme(axis.text.x = element_text(angle = 90, size = 14),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 14))+
    coord_cartesian(ylim = c(0,250))+
    facet_wrap(~scientificname)
  
}
