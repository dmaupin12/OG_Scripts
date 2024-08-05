
#Company merge
file_list <- list.files("Op_Non_Unknown_Acreage/output", recursive = F, pattern = "*shp$", full.names = TRUE)


x=shapefile_list[20]

x=x[1]

x=do.call(rbind,x)




read_trans_sf<-function(i){
  x=read_sf(i)
  #st_crs(x)='+proj=longlat +datum=WGS84'
  x=st_transform(x,crs="+init=epsg:32039")
  
  print(x)
  x=x%>%select(Status,SNAME,Basin,Company)
  
 
}



shapefile_list <- lapply(file_list, read_trans_sf)


shapefile_list %>%
  purrr::discard(~ nrow(.x) == 0) -> shapefile_list


all_shapefiles <- do.call(rbind, shapefile_list)

write_sf(all_shapefiles,"Op_Non_Unknown_Acreage/Combined_Acreage/Combined_Acreage.shp")


rm(list=ls())










