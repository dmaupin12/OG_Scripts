left = function (string,char){ substr(string,1,char) }

sticks<- read_sf("Op_Non_Unknown_Acreage/pdp/BD_ALL_WELLS.shp")


# db_tier1<- read_sf("Tier 1/DB_T1.shp")
# mb_tier1<- read_sf("Tier 1/MB_T1.shp")

#######

sf_use_s2(FALSE)


tier_outline<-read_sf('Delaware_Inventory/PermianBasin_Tiers_BD/BD_TIERS_T1_T3_update.shp')

tier_outline<-tier_outline%>%mutate(id=ifelse(grepl("T2",TC_Area),'a','b'))

tier_outline_slice<-tier_outline

tier_outline<-tier_outline%>%filter(id!= "a")

tier_outline<-tier_outline%>%arrange(id)



SBSG<- tier_outline%>%filter(LZ=='SBSG',!grepl("T3|T2|Basin",TC_Area))
SBSG<- SBSG%>% group_by(LZ)%>%
  summarise(geometry=st_union(geometry))

SBSG_basin<- tier_outline%>%filter(LZ=='SBSG',grepl("T3|T2|Basin",TC_Area))
SBSG_basin<- SBSG_basin%>% group_by(LZ)%>%
  summarise(geometry=st_union(geometry))

plot(SBSG_basin)

TBSG<- tier_outline%>%filter(LZ=='TBSG',!grepl("T3|T2|Basin",TC_Area))
TBSG<- TBSG%>% group_by(LZ)%>%
  summarise(geometry=st_union(geometry))


WFMPASD<- tier_outline%>%filter(LZ=='WFMP A SD',!grepl("T3|T2|Basin",TC_Area))
WFMPASD<- WFMPASD%>% group_by(LZ)%>%
  summarise(geometry=st_union(geometry))

WFMPASH<- tier_outline%>%filter(LZ %in% c('WFMP A SH'),!grepl("T3|T2|Basin",TC_Area))
WFMPASH<- WFMPASH%>%mutate(LZ='WFMP A SH')%>% group_by(LZ)%>%
  summarise(geometry=st_union(geometry))


AVALN<- tier_outline%>%filter(grepl('AVLN',LZ),!grepl("T3|T2|Basin",TC_Area))
AVALN<- AVALN%>%mutate(LZ='AVLN')%>%
  summarise(geometry=st_union(geometry))

LSBY<- tier_outline%>%filter(grepl('LSBY',LZ),!grepl("T3|T2|Basin",TC_Area))
LSBY<- LSBY%>% group_by(LZ)%>%
  summarise(geometry=st_union(geometry))

MSBY<- tier_outline%>%filter(grepl('MSBY',LZ),!grepl("T3|T2|Basin",TC_Area))
MSBY<- MSBY%>% group_by(LZ)%>%
  summarise(geometry=st_union(geometry))

WFMPA<- tier_outline%>%filter(LZ=='WFMP A',!grepl("T3|T2|Basin",TC_Area))
WFMPA<- WFMPA%>% group_by(LZ)%>%
  summarise(geometry=st_union(geometry))

WFMPB<- tier_outline%>%filter(LZ=='WFMP B',!grepl("T3|T2|Basin",TC_Area), grepl("Tier1+",TC_Area))
WFMPB<- WFMPB%>% group_by(LZ)%>%
  summarise(geometry=st_union(geometry))

WFMPB_DB<- tier_outline%>%filter(LZ=='WFMP B',!grepl("T3|T2|Basin",TC_Area), !grepl("Tier1+",TC_Area))
WFMPB_DB<- WFMPB_DB%>% group_by(LZ)%>%st_buffer(.,dist=0)%>%
  summarise(geometry=st_union(geometry))


 WFMPC<- tier_outline%>%filter(OBJECTID_1 %in% c(118,119))
 WFMPC<- WFMPC%>% group_by(LZ)%>%
   summarise(geometry=st_union(geometry))


tier_outlineOriginalFINAL<-raster::bind(as_Spatial(MSBY),as_Spatial(LSBY))
tier_outlineOriginalFINAL<-raster::bind(tier_outlineOriginalFINAL,as_Spatial(WFMPA))
tier_outlineOriginalFINAL<-raster::bind(tier_outlineOriginalFINAL,as_Spatial(WFMPB))
tier_outlineOriginalFINAL<-raster::bind(tier_outlineOriginalFINAL,as_Spatial(WFMPB_DB))
tier_outlineOriginalFINAL<-raster::bind(tier_outlineOriginalFINAL,as_Spatial(AVALN))
tier_outlineOriginalFINAL<-raster::bind(tier_outlineOriginalFINAL,as_Spatial(SBSG))
tier_outlineOriginalFINAL<-raster::bind(tier_outlineOriginalFINAL,as_Spatial(TBSG))
tier_outlineOriginalFINAL<-raster::bind(tier_outlineOriginalFINAL,as_Spatial(WFMPASD))
tier_outlineOriginalFINAL<-raster::bind(tier_outlineOriginalFINAL,as_Spatial(WFMPASH))
tier_outlineOriginalFINAL<-raster::bind(tier_outlineOriginalFINAL,as_Spatial(WFMPC))

tier_outlineOriginalFINAL<-st_as_sf(tier_outlineOriginalFINAL)



tier_outlineOriginalFINAL<- st_transform(tier_outlineOriginalFINAL,crs=st_crs(sticks))


sticks<- read_sf("Op_Non_Unknown_Acreage/pdp/BD_ALL_WELLS.shp")


sticks<-sticks%>%mutate(Reservr=ifelse(grepl("WFMP A SH",LANDING),"WFMP A SH",
                                       ifelse(grepl("WFMP A SD",LANDING),'WFMP A SD',
                                              ifelse(LANDING=='AVLN MID','MAVLN',
                                                     ifelse(LANDING=='AVLN LWR','LAVLN',
                                                            ifelse(LANDING=='AVLN UPR','UAVLN',Reservr))))))


######mewbourne survey sticks

mew_shp<- read_sf('Op_Non_Unknown_Acreage/pdp/env_planned_mew.shp')

mew_zone<-read.xlsx('Op_Non_Unknown_Acreage/pdp/MEWBOURNE SURVEYS WITH FORMATION.xlsx') 

mew_shp$UWI<-substr(mew_shp$api14_unfm, 1, 10)      
mew_shp<-mew_shp%>%select(UWI)%>%left_join(mew_zone%>%mutate(UWI=as.character(UWI))%>%select(UWI,Reservr))
mew_shp<-mew_shp%>%filter(!is.na(Reservr))

mew_shp <- st_transform(mew_shp, "+init=epsg:32039")

sticks <- st_transform(sticks, "+init=epsg:32039")

sticks<-rbind(sticks%>%select(UWI,Reservr),mew_shp)

sticks$API10<- left(sticks$UWI,10)
#####Missing NM Wells
zone_check<- openxlsx::read.xlsx("zone.xlsx")


zone_check<-zone_check%>%filter(!(API10 %in% c(sticks$API10)))

pdp_missing<- read.csv("Op_Non_Unknown_Acreage/pdp/rseg_missing_wells.csv")
pdp_missing$API10<- left(pdp_missing$Unformatted_API_UWI,10)
pdp_missing<- pdp_missing%>%select(API10,Longitude,Latitude,Longitude_BH,Latitude_BH)
pdp_missing<-pdp_missing%>%distinct(API10,.keep_all=T)
zone_check<- zone_check%>%distinct(API10,.keep_all=T)
zone_check<-left_join(zone_check,pdp_missing)
zone_check<-zone_check%>%filter(!is.na(Longitude),!is.na(Longitude_BH))


surface<- zone_check%>%select(API10,Longitude,Latitude)
bottom<- zone_check%>%select(API10,Longitude_BH,Latitude_BH)%>%rename(Longitude=Longitude_BH, Latitude=Latitude_BH)
combined_df<- full_join(surface,bottom)
combined_df<-combined_df%>%arrange(API10)%>%group_by(API10)%>%mutate(Count=max(row_number()))%>%filter(Count==2)
combined_df<-st_as_sf(combined_df,coords=c("Longitude","Latitude"))
combined_df<- combined_df%>%group_by(API10)%>%arrange(API10)%>%
  summarise() %>%
  st_cast("MULTIPOINT")%>%
  st_cast("MULTILINESTRING")

combined_df<-left_join(combined_df,zone_check[,1:4])

combined_df<-combined_df%>%filter(!is.na(geometry))

st_crs(combined_df) <- "+proj=longlat +datum=WGS84"

combined_df <- st_transform(combined_df, "+init=epsg:32039")
combined_df<-combined_df%>%rename(UWI=API10)
sticks<-rbind(sticks%>%select(-API10),combined_df%>%select(UWI,Reservr))



###############3





sticks<- st_transform(sticks,st_crs(tier_outlineOriginalFINAL))
sticks_int<- st_intersection(sticks,tier_outlineOriginalFINAL)

sticks_int<-sticks_int%>%filter(Reservr==LZ)

sticks_int$Tier="Tier1"

sticks<- left_join(sticks,st_drop_geometry(sticks_int)%>%select(UWI,Tier))
sticks<-sticks%>%distinct(UWI,.keep_all=T)

sticks<-sticks%>%mutate(Tier=ifelse(is.na(Tier),"Tier2+",Tier))


write_sf(sticks,'Op_Non_Unknown_Acreage/pdp/Tier1_2plus_PDP.shp')
