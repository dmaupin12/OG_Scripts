#Basin Merge extended line
all_shp <- rbindlist(mapply(
  c,
  (
    list.files(
      path = "Delaware_Inventory/Basin_Inventory/",
      pattern = "*.shp",
      full.names = TRUE
    ) %>%
      lapply(
        read_sf
      )
  ),
  (
    list.files(
      path = "Delaware_Inventory/Basin_Inventory/",
      pattern = "*.shp",
      full.names = TRUE
    ) %>%
      basename() %>%
      as.list()
  ),
  SIMPLIFY = FALSE
),
fill = T)



all_shp<-all_shp%>%select(-V1)%>%mutate(Tier=ifelse(grepl("T2|T3|BASIN|Basin",TC_Area),"Tier_3","Tier_1"))

all_shp<-st_as_sf(all_shp)
all_shp<-all_shp%>%mutate(length=as.numeric(st_length(all_shp)))

delaware<-all_shp%>%filter(length>3000)

delaware<- delaware%>%mutate(id=row_number())

all_shp <- rbindlist(mapply(
  c,
  (
    list.files(
      path = "Midland_Inventory/Basin_Inventory",
      pattern = "*.shp",
      full.names = TRUE
    ) %>%
      lapply(
        read_sf
      )
  ),
  (
    list.files(
      path = "Midland_Inventory/Basin_Inventory/",
      pattern = "*.shp",
      full.names = TRUE
    ) %>%
      basename() %>%
      as.list()
  ),
  SIMPLIFY = FALSE
),
fill = T)

all_shp<-all_shp%>%select(-V1)%>%mutate(Tier=ifelse(grepl("T2|T3|BASIN|Basin",TC_Area),"Tier_3","Tier_1"))

all_shp<-all_shp%>%mutate(length=as.numeric(st_length(geometry)))
midland<-all_shp%>%filter(length>3000)

#st_crs(delaware)<-"+init=epsg:32039"

midland<-st_as_sf(midland)
#st_crs(midland)<-"+init=epsg:32039"

midland<-st_transform(midland,st_crs(delaware))


midland<-midland%>%select(Company,reservoir,TC_Area,Tier,length)
midland<- midland%>%mutate(id=row_number())

delaware<-delaware%>%select(id,Company,reservoir,TC_Area,Tier,length)

basin_lines<-rbind(delaware,midland)
  
basin_lines<-basin_lines%>%mutate(id=row_number())

#plot(basin_lines[,1])

write_sf(basin_lines,'Basin_Line_Inventory/Combined/basin_lines.shp')


rm(list=ls())

