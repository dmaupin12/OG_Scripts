library(sf)
library(tidyverse)

sf::sf_use_s2(FALSE)

devon<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/ACQUISITIONS/2024 M&A/Acg_Update/Devon.shp")

marathon<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/ACQUISITIONS/2024 M&A/Archive SHP/MARATHON OIL.shp")

acreage<-rbind(devon[,-c(5:7)],marathon)

sections<- read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Permian Inventory/Op_Non_Unknown_Acreage/sections_grid/sections_rowId.shp")

endeavorO<- acreage%>%filter(Company=='MARATHON OIL')%>%select(Company)
endeavor<-endeavorO

options(scipen=999)

endeavor<-endeavor%>%group_by(Company)%>%
  summarise(geometry=st_union(geometry))

endeavor<- st_intersection(endeavor,st_transform(sections,st_crs(endeavor)))

endeavor<-endeavor%>%group_by(Company,row_id)%>%
  summarise(geometry=st_union(geometry))

endeavor<-st_transform(endeavor,'+init=epsg:32039')
sections<-st_transform(sections,'+init=epsg:32039')

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
endeavor_union<-endeavor_union%>%filter(area>20)


cop<-read_sf("//Conoco.net/ho_shared/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/BD_ANALYST/SHAPE_DROP/COP_ACREAGE_NEW!/COP_Acreage.shp")

cop<-st_transform(cop,'+init=epsg:32039')

####Touching Script

acreage<-endeavor_union


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

lapply(unique(endeavor$Company),touching_fctn)


touch_final<- end_df%>%mutate(id_check=row_number())

within=st_is_within_distance(st_centroid(touch_final),cop, dist = 2800)

within2=data.frame(within)




touch_final<-touch_final%>%filter(id_check %in% c(within2$row.id))


touch_final<-touch_final%>%
  #group_by(Company)%>%
  #summarise(geometry=st_union(geometry))%>%
  mutate(area=as.numeric(st_area(geometry)))%>%
  mutate(area=area/43560)





basin_outline<-read_sf("//conoco.net/ho_shared/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/Basin_Outline/Basin_Outlines.shp")

basin_outline<- st_transform(basin_outline,st_crs(touch_final))
basin_outline<-basin_outline[7,]

basin_outline<- st_cast(basin_outline,'POLYGON')
basin_outline<-st_buffer(basin_outline,dist=0)

final_table_filter<-st_difference(touch_final,basin_outline)


final_table_filter<-final_table_filter%>%
  #group_by(Company)%>%
  #summarise(geometry=st_union(geometry))%>%
  mutate(area=as.numeric(st_area(geometry)))%>%
  mutate(area=area/43560)



# plotly::ggplotly(ggplot(cop)+
#                    geom_sf(fill='yellow',color=NA)+
#                    geom_sf(data=touch_final,fill='maroon',color=NA))


######MANUAL CHANGE UNHASH AND UPDATE ROW IDS
# eer_touching_manual_filter_from_spotfire<-final_table_filter%>%
#   filter((!row_id %in% c(272,
#                        388,
#                        462,
#                        510,
#                        561,
#                        687,
#                        688,
#                        689,
#                        787,
#                        911,
#                        928,
#                        932,
#                        933,
#                        985)))


# 
# eer_touching_manual_filter_from_spotfire<-eer_touching_manual_filter_from_spotfire%>%
#   group_by(Company)%>%
#   summarise(geometry=st_union(geometry))%>%
#   mutate(area=as.numeric(st_area(geometry)))%>%
#   mutate(area=area/43560)




write_sf(final_table_filter,'//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/ACQUISITIONS/2024 M&A/Intersect_Adjacent_Maps/MARATHON_touching_FINAL.shp')


endeavor<-endeavorO
endeavor<-st_transform(endeavor,'+init=epsg:32039')

eer_intersect<-st_intersection(st_buffer(endeavor,dist=0),st_buffer(cop,dist=0))


eer_intersect<-eer_intersect%>%group_by(Company)%>%
  summarise(geometry=st_union(geometry))%>%
  mutate(area=as.numeric(st_area(geometry)))%>%
  mutate(area=area/43560)

write_sf(eer_intersect,'//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/ACQUISITIONS/2024 M&A/Intersect_Adjacent_Maps/MARATHON_intersect.shp')



#####Touching Sections

# 
# endeavor<-endeavorO
# 
# endeavor<-endeavor%>%mutate(Company='EER')%>%select(Company)
# 
# endeavor<-st_transform(endeavor,'+init=epsg:32039')
# 
# 
# endeavor<-endeavor%>%mutate(area=as.numeric(st_area(geometry))/43560)%>%filter(area>20)
# endeavor_exp<-rmapshaper::ms_explode(endeavor)
# 
# endeavor_exp<-endeavor_exp%>%mutate(area=as.numeric(st_area(geometry))/43560)%>%filter(area>20)
# 
# endeavor_exp<-endeavor_exp%>%mutate(join_id=row_number())
# 
# endeavor_int<- st_intersection(st_buffer(endeavor_exp,dist=-25),st_transform(sections,st_crs(sections)))
# 
# endeavor_int<-endeavor_int%>%mutate(area_int=as.numeric(st_area(geometry))/43560)%>%filter(area_int>20)
# 
# endeavor_int<-left_join(endeavor_exp,st_drop_geometry(endeavor_int%>%select(join_id,row_id)))
# 
# options(scipen=999)
# 
# endeavor_group_sf<-endeavor_int%>%
#   #group_by(Company,row_id)%>%
#   summarise(geometry=st_union(geometry))
# 
# 
# 
# endeavor_group_sf<-st_buffer(endeavor_group_sf,dist=0)
# 
# endeavor_group_sf<-st_intersection(sections,endeavor_group_sf)
# 
# endeavor_group_sff<-endeavor_group_sf%>%
#   mutate(area=as.numeric(st_area(geometry))/43560)%>%
#   filter(area>25)%>%
#   group_by(row_id)%>%
#   summarise(geometry=st_union(geometry))%>%
#   mutate(area=as.numeric(st_area(geometry))/43560)%>%
#   filter(area>100)
# 
# 
# 
# endeavor_group_sff<-endeavor_group_sff%>%mutate(row_touching_id=row_number())
# 
# touching_list=data.frame(st_relate(endeavor_group_sff, pattern = "****1****")) # sides, not corners, internals
# 
# touching_list<-touching_list%>%filter(row.id !=col.id)
# 
# eer_filter_touching_df<-endeavor_group_sff%>%filter(row_touching_id%in% c(touching_list$col.id))
# 
# xplode_final<-rmapshaper::ms_explode(eer_filter_touching_df)
# 
# 
# xplode_final<-xplode_final%>%
#   mutate(area=as.numeric(st_area(geometry))/43560)%>%
#   filter(area>20)
# 
# 
# write_sf(xplode_final,'eer_filter_touching_df3.shp')
# 
# 
# ###############explode original
# 
# 
# 
# 
# test<-endeavor_exp%>%filter(area>100)
# parts <- st_cast(st_union(test),"POLYGON")
# 
# clust <- unlist(st_intersects(st_buffer(test,dist=-50,endCapStyle = 'FLAT'), st_buffer(parts,dist=-50,endCapStyle = 'FLAT')))
# 
# diss <- cbind(test, clust)
# 
# diss<-diss%>%group_by(clust)%>%mutate(Clust_Count=max(row_number()))
# 
# diss_final<-diss%>%
#   group_by(clust,Clust_Count) %>%
#   summarize(geometry=st_union(geometry))
# 
# diss_final<-diss_final%>%mutate(area=as.numeric(st_area(geometry))/43560)
# 
# write_sf(diss_final,'diss.shp')
# 
# 
# x=read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/ACQUISITIONS/Project Eclipse/VDR/1_Land/1.3_Shapefiles/1.3.3_DU_Shapefile/EER_DUs10262023.shp")
# 
# sum(x$GROSSAC)
# 
# 


##Sticks on COP Acreage

dvn_manual_inventory<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/ACQUISITIONS/2024 M&A/Inventory Master/DVN/INVENTORY_POST_DUC_REMOVAL")

mro_manual_inventory<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/ACQUISITIONS/2024 M&A/Inventory Master/MRO/INVENTORY_POST_DUC_REMOVAL")

inventory_manual<-rbind(dvn_manual_inventory,mro_manual_inventory)

cop_int<-st_intersection(inventory_manual,st_buffer(st_transform(cop,st_crs(inventory_manual)),dist=0))

cop_tbl<-st_drop_geometry(cop_int)%>%group_by(company,Status)%>%
  summarise(Count=max(row_number()))
cop_tbl<-cop_tbl%>%filter(Status != "Unknown")       

pivot_wider(cop_tbl,names_from=Status,values_from=Count)                  

setwd("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/ACQUISITIONS/2024 M&A/")

mro<-read_sf("Intersect_Adjacent_Maps/DEVON_touching_FINAL.shp")

mro<-st_union(mro)
mro<-st_as_sf(mro)%>%mutate(row_id=row_number())

mro<-mro%>%mutate(area=as.numeric(st_area(x))/43560)


plot(mro)
