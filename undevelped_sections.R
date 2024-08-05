library(tidyverse)
library(openxlsx)
library(sf)
library(ggplot2)

sf::sf_use_s2(FALSE)


sections<- read_sf("sections/sections_rowId.shp")

pdp<- read_sf("pdp/BD_ALL_WELLS.shp")


sections<- st_transform(sections,st_crs(pdp))

pdp_sections<- st_intersection(pdp,sections)


pdp_sections<- pdp_sections%>%mutate(length= as.numeric(st_length(geometry)))%>%
  group_by(row_id)%>%filter(length== max(length))%>%
  ungroup()%>%filter(length>2000)


sections_undeveloped<- left_join(sections,st_drop_geometry(pdp_sections)%>%
    select(row_id,UWI10,length))%>%
    filter(is.na(UWI10))

plot(sections_undeveloped[,1])

###intersect endeavor acreage
options(scipen=999)
endeavorO<- read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/ACQUISITIONS/Endeavor_Screening/Acreage Shapefile/clean_EndeavorUpdate_2022_09.shp")

endeavor<- read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Permian Inventory/Op_Non_Unknown_Acreage/Combined_Acreage/Combined_Acreage.shp")

endeavor<-endeavor%>%filter(Basin=='Midland')%>%arrange(Company)
pioneer<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Permian Inventory/Op_Non_Unknown_Acreage/EndeavorFinalAcreage/PIONEER_update.shp")
endeavor<-rbind(endeavor,pioneer)

fasken<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Permian Inventory/Op_Non_Unknown_Acreage/Misc_ShapeFiles_InHouse/FASKEN_20180731.shp")
fasken<-fasken%>%mutate(SNAME='TEXAS',Basin='Midland',Company='FASKEN',Status='Operated')%>%
  select(SNAME,Basin,Status,Company)

#endeavor<-endeavorO%>%mutate(SNAME='TEXAS',Basin='Midland',Company='ENDEAVOR')

endeavor<-rbind(endeavorO%>%mutate(SNAME='TEXAS',Basin='Midland',Company='ENDEAVOR')%>%
                  select(SNAME,Basin,Status,Company),endeavor%>%filter(!grepl('FASKEN',Company)))

endeavor<- rbind(endeavor,st_transform(fasken,st_crs(endeavor)))

endeavor<- endeavor%>%group_by(Company,Status)%>%
  summarise(geometry=st_union(geometry))


# pdp_end<- st_intersection(pdp,endeavor)
# pdp_end<- pdp%>%filter(UWI %in% c(pdp_end$UWI))
# write_sf(pdp_end,'pdp_end.shp')





####for converting polyline to polygon and filtering out missing polygons
# xyL = data.frame(st_coordinates(st_cast(endeavor$geometry,"MULTIPOINT")))
# xyL<- xyL%>%group_by(L1)%>%mutate(row_num= max(row_number()))%>%ungroup()%>%arrange(row_num)%>%
#   distinct(L1,.keep_all = T)
# endeavor<- endeavor%>%mutate(L1= row_number())
# endeavor<- left_join(endeavor,xyL%>%select(L1,row_num))
# endeavor<- endeavor%>%filter(row_num>3)%>%select(L1,row_num,row_id,Status)
# endeavor_poly<- endeavor%>%st_cast(.,'POLYGON')
# write_sf(endeavor_poly,"//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/ACQUISITIONS/Endeavor_Screening/Acreage Shapefile/clean_EndeavorUpdate_2022_09.shp")

endeavor<- st_intersection(acreage,st_transform(sections,st_crs(sections)))
# endeavor<- endeavor%>%
#   summarise(geometry= st_union(geometry))
endeavor<-endeavor%>%mutate(area=as.numeric(st_area(geometry))/43560)
endeavor<-st_buffer(endeavor,dist=0)

sections<-sections%>%mutate(sect_area=as.numeric(st_area(geometry))/43560)
endeavor<-left_join(endeavor,st_drop_geometry(sections)%>%select(row_id,sect_area))

endeavor_tbl<-st_drop_geometry(endeavor)%>%group_by(Company,row_id,sect_area)%>%summarise(area=sum(area))%>%
  mutate(percent=area/sect_area)

endeavor_union<- endeavor%>%group_by(Company,row_id)%>%
     summarise(geometry= st_union(geometry))

endeavor_union<-left_join(endeavor_union,endeavor_tbl)

#### 85 PERCENT  CUTOFF----
endeavor_union<-endeavor_union%>%filter(percent>.84)

endeavor_undeveloped<- st_intersection(endeavor_union,sections_undeveloped)

endeavor_undeveloped<- endeavor_undeveloped%>%group_by(Company,row_id)%>%
  summarise(geometry=st_union(geometry))

tier1<- read_sf("//Conoco.net/ho_shared/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Individual Work/DM Work_COP/2023-03-07 Pioneer Undeveloped Sections/tiers.shp")

tier1<-tier1%>%mutate(id=row_number())%>%filter(id==2)

endeavor_undeveloped<-st_buffer(endeavor_undeveloped,dist=0)

endeavor_undeveloped<-endeavor_undeveloped%>%mutate(area_update=as.numeric(st_area(geometry)))



##############3Need to figure out totals!!!!!!!!!!!!!!!1
endeavor_undeveloped<-endeavor_undeveloped%>%filter(area_update>0)%>%arrange(desc(area_update))

write_sf(st_buffer(endeavor_undeveloped,dist=0),'company_undeveloped.shp')

endeavor_undeveloped_tier1<-st_intersection(endeavor_undeveloped,tier1)
endeavor_undeveloped_tier1<-st_buffer(endeavor_undeveloped_tier1,dist=0)

sec_count=st_drop_geometry(st_intersection(sections_undeveloped,tier1))%>%distinct(row_id,.keep_all=T)%>%
  mutate(sec_count=max(row_number()))

sec_count<- sec_count[1,6]


st_drop_geometry(endeavor_undeveloped_tier1)%>%group_by(Company,row_id)%>%
  distinct(Company,row_id,.keep_all=T)%>%ungroup()%>%group_by(Company)%>%summarise(count=max(row_number()))%>%
  arrange(desc(count))%>%mutate(perc=count/sec_count$sec_count)


write_sf(st_buffer(endeavor_undeveloped_tier1,dist=0),'Company_Perc_85.shp')

write_sf(st_buffer(sections_undeveloped,dist=0),'Undeveloped_Sections.shp')


########Intersect_Adjacent Acreage----

acreage<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Permian Inventory/Op_Non_Unknown_Acreage/Combined_Acreage/Combined_Acreage.shp")


pioneer<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Permian Inventory/Op_Non_Unknown_Acreage/EndeavorFinalAcreage/PIONEER_update.shp")
pioneer<-pioneer%>%select(Company,Basin,Status,SNAME)
pioneer<-st_transform(pioneer,st_crs(acreage))
endeavor<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Permian Inventory/Op_Non_Unknown_Acreage/EndeavorFinalAcreage/EndeavorUpdate_2022_09.shp")
endeavor<-endeavor%>%select(Company,Basin,Status,SNAME)
endeavor<-st_transform(endeavor,st_crs(acreage))
endeavor<-st_buffer(endeavor,dist=0)%>%group_by(Company,Status,SNAME)%>%
  summarise(geometry=st_union(geometry))

pxd_end<-rbind(pioneer%>%select(-Basin),endeavor)
pxd_end<-pxd_end%>%mutate(Status_SNAME=paste(Company,Status,SNAME))

acreage<-acreage%>%select(Company,Status,SNAME)

acreage<-rbind(acreage,pxd_end%>%select(-Status_SNAME))

crownrock<- read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/ACQUISITIONS/Project Derby/5.0 Land/5.1 Shapefiles/5.1.1 Acreage/5.1.1.1 CMTC_DSUs/CMTC_DSUs/CMTC_DSUs.shp")


crownrock<- crownrock%>%select(geometry)%>%mutate(Company="CrownRock_Int",Status='Operated',SNAME='TEXAS')%>%select(Company,Status,SNAME)
crownrock<-st_transform(crownrock,st_crs(acreage))
crownrock<-st_buffer(crownrock,dist=0)%>%group_by(Company,Status,SNAME)%>%summarise(geometry=st_union(geometry))

acreage<- rbind(acreage,crownrock)

acreage<-acreage[1028:1034,]

acreage<- acreage%>%group_by(Company)%>%
  summarise(geometry=st_union(geometry))

cop<- read_sf("//Conoco.net/ho_shared/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/BD_ANALYST/F_AcrGIS/BD_Base_Map/Shapes/COP_Acreage/COP_20230613.shp")

cop<- st_transform(cop,st_crs(endeavor))

sections<- st_buffer(sections,dist=0)
sections<- st_transform(sections,st_crs(endeavor))

end_df<-data.frame()

touching_fctn<-function(i){
  endeavor<-acreage%>%filter(Company==i)
  
  endeavor_int<- st_intersection(sections,endeavor)
  
  endeavor_int<- endeavor_int%>%mutate(area=as.numeric(st_area(geometry))/43560)%>%filter(area>40)
  
  endeavor_int<- sections%>%filter(row_id %in% c(endeavor_int$row_id))
  
  
  cop_int<- st_intersection(sections,st_buffer(cop,dist=0))
  cop_int<-st_buffer(cop_int,dist=0)
  
  cop_int<- cop_int%>%mutate(area=as.numeric(st_area(geometry))/43560)%>%filter(area>40)
  
  cop_int<- sections%>%filter(row_id %in% c(cop_int$row_id))
  
  
  
  cop_int<- cop_int%>%mutate(row_id=row_number())
  endeavor_int<- endeavor_int%>%mutate(row_id=row_number())
  
  touching_list=data.frame(st_relate(cop_int,endeavor_int, pattern = "****1****")) # sides, not corners, internals
  
  endeavor_touching<- endeavor_int%>%filter(row_id %in% c(touching_list$col.id))
  
  combined<-rbind(cop%>%select(geometry),endeavor%>%select(geometry))
  
  endeavor_touching<- st_intersection(st_buffer(endeavor_touching,dist=0),st_buffer(combined,dist=0))
  
  endeavor_touching<-st_buffer(endeavor_touching,dist=0)
  
  endeavor_touching<- st_difference(endeavor_touching,st_buffer(cop,dist=0))
  endeavor_touching<-st_buffer(endeavor_touching,dist=0)
  
  endeavor_touching<- st_transform(endeavor_touching,st_crs(endeavor))
 
  endeavor_touching<-endeavor_touching%>%mutate(Company=i)
  end_df<-rbind(end_df,endeavor_touching)
  
  end_df<<-end_df
  
  
}

lapply(unique(acreage$Company),touching_fctn)





touch_final<- end_df%>%mutate(id_check=row_number())

within=st_is_within_distance(st_centroid(touch_final),cop, dist = 2800)

within2=data.frame(within)




touch_final<-touch_final%>%filter(id_check %in% c(within2$row.id))


touch_final<-touch_final%>%group_by(Company)%>%
  summarise(geometry=st_union(geometry))%>%
  mutate(area=as.numeric(st_area(geometry)))%>%
  mutate(area=area/43560)



basin_outline<-read_sf("//conoco.net/ho_shared/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/Basin_Outline/Basin_Outlines.shp")

basin_outline<- st_transform(basin_outline,st_crs(touch_final))
basin_outline<-basin_outline[7,]

basin_outline<- st_cast(basin_outline,'POLYGON')
basin_outline<-st_buffer(basin_outline,dist=0)

final_table_filter<-st_difference(touch_final,basin_outline)


final_table_filter<-final_table_filter%>%group_by(Company)%>%
  summarise(geometry=st_union(geometry))%>%
  mutate(area=as.numeric(st_area(geometry)))%>%
  mutate(area=area/43560)




write_sf(final_table_filter,'cop_touch_pxd_end_crwn.shp')

getwd()







plotly::ggplotly(ggplot(cop)+
                   geom_sf(fill='yellow',color=NA)+
                   geom_sf(data=touch_final%>%filter(Company=='CHEVRON'),fill='maroon',color=NA))


endeavor_touching%>%
  summarise(geometry=st_union(geometry))%>%
  mutate(area=as.numeric(st_area(geometry))/43560)



acreage_int<- st_intersection(st_buffer(cop,dist=0),st_buffer(acreage,dist=0))
acreage_int<-st_buffer(acreage_int,dist=0)

acreage_int<-acreage_int%>%group_by(Company)%>%
  summarise(geometry=st_union(geometry))%>%
  mutate(area=as.numeric(st_area(geometry))/43560)

write_sf(acreage_int,'cop_permian_int.shp')
