library(sf)
library(tidyverse)


setwd('//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/00_Regional Type Curves/01 Delaware/02 Geo Support/Final Polygons/')



file_list=list.files(, pattern="*.shp", full.names=TRUE)


file_list<-file_list[grepl('.shp',file_list)]

file_list<-file_list[!grepl('.xml',file_list)]

origList <- lapply(file_list, st_read)

uavln<-origList[[10]]
uavln<-data.frame(uavln)
uavln<-st_as_sf(uavln)
uavln<-st_zm(uavln)
uavln<- st_buffer(uavln,dist=100)

uavln<-uavln%>%group_by(Name)%>%
  mutate(geometry=st_union(st_buffer(geometry,dist=100)))%>%
  st_cast('MULTIPOLYGON')

ggplot(uavln)+geom_sf(aes(fill=Name))+facet_wrap(~Name)+
  theme_minimal()+theme(legend.position='')

#use this to select certain column in order to rbind the data
x=origList[[1]]
x=colnames(x)

i=file_list[1]

read_fctn<-function(i){
  shp=st_read(i)
  shp=shp%>%select(x)
  shp=st_zm(shp)
  #shp=shp%>%st_cast('POLYGON')
}

listOfShp <- lapply(file_list, read_fctn)


combinedShp <- do.call("rbind", listOfShp)


combinedShp$Zone=gsub('(.*)_\\w+', '\\1', combinedShp$Name)


uavln=combinedShp%>%filter(Zone=='UAVLN')

uavln=uavln[8,]

final_df<-data.frame()

create_polygon_fctn<-function(i){
  poly_sf<-combinedShp%>%
    filter(i==Name)%>%
    st_buffer(dist=20)%>%
    mutate(geometry=st_union(geometry))%>%
    st_cast('POLYGON')%>%
    st_cast('LINESTRING')%>%
    st_polygonize()%>%
    st_collection_extract()

  final_df<<-rbind(final_df,poly_sf)
  
  }

lapply(unique(combinedShp$Name),create_polygon_fctn)

ggplot(final_df%>%group_by(Zone)%>%mutate(row_id=row_number()))+geom_sf(aes(fill=as.character(row_id)))+
  facet_wrap(~Zone)+theme_minimal()+theme(legend.position = "")

write_fctn<-function(i){
  shpf=final_df%>%filter(Zone==i)
  i=sub(" ", "_", i)
  st_write(shpf,paste('//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/TC_Areas/DB_TC_UPDATE_7.1.2024/individual/a/',i,'.shp',sep=""),append=F)
}

lapply(unique(final_df$Zone),write_fctn)



write_sf(final_df,'//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/TC_Areas/Delaware_TC_Areas.shp')


