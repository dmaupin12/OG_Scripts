library(sf)
library(tidyverse)


sf::sf_use_s2(F)

wells<-read_sf("BD_ALL_WELLS.shp")

sections<-read_sf("sections_rowId.shp")

st_crs(sections) <-"+proj=longlat +datum=WGS84"

sections<-sections%>%mutate(area=st_area(geometry))%>%arrange(desc(area))%>%
  filter(row_id!=0)


wells_int<- st_intersection(wells,st_transform(sections,st_crs(wells)))
         
wells_int<-wells_int%>%mutate(length=as.numeric(st_length(geometry)))               
           
wells_int<-wells_int%>%filter(length>2000)%>%distinct(row_id,.keep_all=T)


op_sections<-left_join(sections,st_drop_geometry(wells_int)%>%select(UWI,row_id))

op_sections<-op_sections%>%filter(!is.na(UWI))



write_sf(op_sections,'operated_sections.shp')
