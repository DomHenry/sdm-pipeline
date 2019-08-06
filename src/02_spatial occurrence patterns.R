# Description -------------------------------------------------------------

## Thu Jan 10 10:47:30 2019 
## Spatial analysis of amphibian occurence records

library(tidyverse)
library(lubridate)
library(gghighlight)
library(gridExtra)
library(ggpubr)
library(sf)
library(viridis)
library(mapview)
library(leaflet)
library(leaflet.esri)
library(glue)

# Import data -------------------------------------------------------------
load("data output/amph_data_clean.RData")
map(dir("src/functions/",full.names = TRUE),
    source)

latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
za <- st_read("data input/PR_SA.shp",crs = latlongCRS)

# Remove species with no data from species RL vector ----------------------
spp <- spp[spp %in% unique(amph$scientificname)]

# Plot :: All species occurence - split into groups of 4 ------------------
vec <- 1:length(spp)
sppgrp <- 4

# TODO Rename these figures with more informative names (instead of fig 1 etc)
dir.create(glue::glue("data output/spatial occurrence plots"))

pdf("data output/spatial occurrence plots/fig 1 - species_groups_map.pdf",width = 16, height = 9)
map(split(vec, ceiling(seq_along(vec)/sppgrp)),
    plot_spp_groups)
dev.off()

# Plot :: Separate species occurrence -------------------------------------
spp_len <- c(1:length(spp))
plists <- split(spp_len, ceiling(seq_along(spp_len)/4))

pdf("data output/spatial occurrence plots/fig2 - indiv_spp.pdf",width = 16, height = 9)
map2(plists,"pdf",
    plot_single_spp)
dev.off()

## Plot single PNGs
map2(spp_len,"png",
     safely(plot_single_spp))

# Plot :: Species distribution and inset ----------------------------------
pdf("data output/spatial occurrence plots/fig3 - spp_range_facet.pdf",width = 16, height = 9)
map2(spp_len,"pdf",
    spp_range_inset)
dev.off()

## Plot single PNGs
map2(spp_len,"png",
     safely(spp_range_inset))

# Plot :: Decadal plot separate species -----------------------------------
amphsf <- amphsf %>% 
  mutate(decade = as.factor(year(floor_date(date, years(10)))))

pdf("data output/spatial occurrence plots/fig4 - group_spp_decade.pdf",width = 16, height = 9)
spp_len <- c(1:length(spp))
plists <- split(spp_len, ceiling(seq_along(spp_len)/4))
map(plists,plot_single_spp_decade)
dev.off()

# Plot :: Decadal plot separate species inset -----------------------------
pdf("data output/spatial occurrence plots/fig5 - indiv_spp_decade_facet.pdf",width = 16, height = 9)
map2(spp_len, "pdf",
    safely(spp_range_inset_decade))
dev.off()

## Plot single PNGs
map2(spp_len, "png",
     safely(spp_range_inset_decade))

# Leaflet map -------------------------------------------------------------
decadepal <-  colorFactor(viridisLite::viridis(length(unique(amphsf$decade))), domain = unique(amphsf$decade))
esriBasemapLayers # List of available layers

amphbasemap <- leaflet(amphsf) %>% 
  addEsriBasemapLayer(key = esriBasemapLayers$Topographic, options = list(detectRetina=TRUE), group = "Topographic") %>% 
  addEsriBasemapLayer(key = esriBasemapLayers$Terrain, options = list(detectRetina=TRUE), group = "Terrain") %>% 
  addPolygons(data = sa_merge,
              color = "grey",
              fillOpacity = 0.3, stroke = FALSE)

# Try use purrr::walk and turn this into a function
for(i in 1:length(spp)){
  
  amphbasemap <- amphbasemap %>% 
    addCircleMarkers(data = amphsf %>% filter(scientificname %in% spp[i]), 
                     group = spp[i],
                     popup = ~ paste0(scientificname," \n", year),
                     radius = 5,
                     color = ~decadepal(decade),
                     stroke = FALSE, fillOpacity = 0.9)
  
}

amphbasemap %>% 
  addLayersControl(overlayGroups = spp,
                   options = layersControlOptions(collapsed = FALSE),
                   baseGroups = c("Topographic","Terrain")) %>% 
  addScaleBar(position = "bottomleft") %>% 
  addLegend("bottomleft",
            pal = decadepal,
            values = amphsf$decade,
            title = "Decade",
            opacity = 1) %>% 
  hideGroup(spp[2:length(spp)]) 



### Example of using purrr in leaflet ###
# library(leaflet.esri)
# basemaps <- esriBasemapLayers
# 
# # Continental US
# l <- leaflet() %>% setView(-98.35,39.5,3)
# 
# purrr::walk(basemaps,
#             function(basemap) {
#               l <<- l %>% addEsriBasemapLayer(key=basemap, autoLabels=TRUE,
#                                               group=basemap,
#                                               options = list(detectRetina=TRUE))
#             })
# 
# l %>%
#   addLayersControl(baseGroups = names(basemaps))


# Threat status plots -----------------------------------------------------
rl %>% 
  group_by(rls) %>% 
  tally 

amphsf <- amphsf %>% 
  left_join(rl %>% select(latin_name,rls), by = c("scientificname" = "latin_name")) 

x <- list(amphsf)
rlscat <- list("CR","EN","VU","NT")
spp_rm <- list("Sclerophrys pantherinus")  # Remove Western Leopard Toad

rlsplots <- pmap(list(x,rlscat,spp_rm),
      rls_status_plot)

pdf("data output/spatial occurrence plots/fig6 - threat_status_facet.pdf", width = 16, height = 9)
grid.arrange(grobs = rlsplots, ncol = 2)
dev.off()

p <- arrangeGrob(grobs = rlsplots, ncol = 2)
ggsave("data output/spatial occurrence plots/fig6 - threat_status_facet.png",
       p,
       width = 16, 
       height = 9)
# END ---------------------------------------------------------------------
