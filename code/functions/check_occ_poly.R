# Check whether points fall within expert polygon -------------------------

# Might need to think about adding user defined buffer distances
check_occ_poly <- function(sppname) {
  
  ref <- which(polynames %in% glue("{sppname}.shp"))
  
  if(length(ref) == 0){
    
    occ <- NULL
    
  } else {
    
    poly <- st_read(polylist[[ref]])
    print(glue("COORDINATE SYSTEM of polygon: {st_crs(poly)}"))
    
    b1 <- st_buffer(poly,5000) %>% # # 0.01 arc_degree ~ 1km therefire 0.05 ~ 5km
      st_sym_difference(poly) 
    
    b2 <- st_buffer(poly,10000) %>% # 10km
      st_sym_difference(b1) %>% 
      st_sym_difference(poly)
    
    b3 <- st_buffer(poly,20000) %>% # 20km
      st_sym_difference(b1) %>% 
      st_sym_difference(b2) %>% 
      st_sym_difference(poly)
    
    occ <- amphsf %>% 
      filter(scientificname == sppname) 
    
    if (nrow(occ) == 0) {
      
      occ <- NULL
      
    } else {
      
      st_crs(occ) <- "+proj=longlat"
      occ <- occ %>% 
        st_transform(st_crs(poly))
      
      occ <- occ %>% 
        mutate(core = st_intersects(occ, poly, sparse = FALSE)) %>% 
        mutate(buff1 = st_intersects(occ,b1, sparse = FALSE)) %>% 
        mutate(buff2 = st_intersects(occ, b2, sparse = FALSE)) %>% 
        mutate(buff3 = st_intersects(occ, b3, sparse = FALSE))
      
      # ggplot() +
      #   geom_sf(data = b3, fill = alpha("red",0.2))+
      #   geom_sf(data = b2, fill = alpha("green",0.2))+
      #   geom_sf(data = b1, fill = alpha("blue",0.2))+
      #   geom_sf(data = poly, fill = alpha("grey",0.9))+
      #   geom_sf(data = occ, size = 1.5)

      # st_geometry(occ) <- NULL # remove geometry, coerce to data.frame
    }  
  }
  return(occ)
}


