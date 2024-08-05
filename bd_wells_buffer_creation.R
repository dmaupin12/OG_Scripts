library(sf)
library(tidyverse)
library(purrr)


sf::sf_use_s2(FALSE)

setwd("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/Well_Sticks_Zone/")


left = function (string,char) {
  substr(string,1,char)
}


wells<-read_sf("BD_ALL_WELLS.shp")


nm_missing<-read_sf("env_shp-Wells-Trajectories-0cc58_2024-01-08/env_shp-Wells-Trajectories-0cc58_2024-01-08.shp")

nm_missing<-nm_missing%>%select(api14_unfm,envoperatr,wellname,county,TVD,longitude,latitude,bh_long,bh_lat,latlength,
                                completdte,vintage,env_basin)

#CHANGE TO API12
nm_missing$UWI=left(nm_missing$api14_unfm,12)
nm_missing$OPERATO=nm_missing$envoperatr
nm_missing$WELLNAM=nm_missing$wellname
nm_missing$WELLNUM=nm_missing$wellname
nm_missing$COUNTY=nm_missing$county
nm_missing$meanTVD=nm_missing$TVD
nm_missing$srf_lng=nm_missing$longitude
nm_missing$srf_ltt=nm_missing$latitude
nm_missing$bh_lngt=nm_missing$bh_long
nm_missing$bh_lttd=nm_missing$bh_lat
nm_missing$WllbrPs="no_data"
nm_missing$Directn="no_data"
nm_missing$UWI10=left(nm_missing$api14_unfm,10)
nm_missing$length=nm_missing$latlength
nm_missing$COMPLET=nm_missing$completdte
nm_missing$year=nm_missing$vintage
nm_missing$API10=nm_missing$UWI10

nm_missing<-nm_missing[,c(13:31)]

nm_missing_zone<-openxlsx::read.xlsx("00_NM_Add_Zone.xlsx")
nm_missing_zone$api_12<-left(nm_missing_zone$api_12,12)
nm_missing_zone<-nm_missing_zone[,c(1,6)]
colnames(nm_missing_zone)[2]='Reservr'

nm_missing<-left_join(nm_missing,nm_missing_zone,by=c("UWI"='api_12'))
nm_missing$LANDING=nm_missing$Reservr
nm_missing$BUSINES=nm_missing$env_basin
nm_missing$Tank= nm_missing$Reservr
nm_missing$Tank=as.character(nm_missing$Tank)
nm_missing$Reservr=nm_missing$Reservr

nm_missing<-st_transform(nm_missing,st_crs(wells))

nm_missing$meanTVD<-as.double(nm_missing$meanTVD)
nm_missing$length<-as.double(nm_missing$length)
nm_missing$year<-as.double(nm_missing$year)

#wells<-wells%>%select(-Source)
nm_missing<-nm_missing%>%select(colnames(wells))

###ADD in Tank from original file

#tank_df<- st_drop_geometry(wells)%>%select(Reservr,Tank)%>%distinct(Reservr,Tank)

nm_missing<-nm_missing%>%mutate(Tank=ifelse(grepl('BS1',Tank),'BS1',
                                            ifelse(grepl('BS2',Tank),'BS2',
                                                   ifelse(grepl('WFMPB',Tank),'WFMP B',
                                                          ifelse(grepl('AVLN',Tank),'AVLN',
                                                          ifelse(grepl('WFMP SD',Tank),'WFMP A',Tank))))))


wells<-rbind(wells%>%mutate(Source='Internal'),nm_missing%>%mutate(Source='Enverus'))

wells$API10<-left(wells$UWI,10)

wells$UWI<-as.character(wells$UWI)

wells<-wells%>%distinct(UWI,.keep_all=T)

##Add alias and need to move column to beginning of data.frame
sticks<- sticks%>%mutate(Alias=ifelse(grepl("shell|COG|CONCHO",Alias,ignore.case = T),'CONOCOPHILLIPS',
                                      ifelse(grepl("xto",Alias,ignore.case = T),'EXXON',
                                             ifelse(grepl("OXY|CROWNROCK|CROWNQUEST",Alias,ignore.case = T),'OCCIDENTAL',
                                                    ifelse(grepl("CENTENNIAL|COLGATE|EARTHSTONE",Alias,ignore.case = T),'CDEV_PERMIAN RESOURCES',
                                                           ifelse(grepl('PIONEER',Alias,ignore.case=T) & COUNTY %in% c("WARD","WINKLER","REEVES","PECOS"),"CONTINENTAL RESOURCES",
                                                                  ifelse(grepl("PIONEER",Alias),"EXXON",
                                                                         ifelse(grepl("WPX|FELIX",Alias),"DEVON",
                                                                                ifelse(grepl("LAREDO|HENRY|MAPLE|TALL CITY|DRIFTWOOD",Alias),"VITAL",
                                                                                       ifelse(grepl("PIEDRA|BLACK SWAN|BLACKBEARD|PETROLEGACY",Alias),"OVINTIV",
                                                                                              ifelse(grepl("ADVANCE ENERGY",Alias),"MATADOR",
                                                                                                     ifelse(grepl("TITUS|CALLON",Alias),"APA CORP",Alias))))))))))))




st_write(wells,'BD_ALL_WELLS.shp',append=F)




wells<-wells%>%rename(Reservoir=Reservr)%>%mutate(Reservoir=trimws(Reservoir))


res_list<-unique(wells$Tank)

buff_fctn<-function(i){
  wells_f<-wells%>%filter(Tank==i)
  
  #wells_f<-wells_f%>%mutate(valid=st_is_valid(geometry))
  
  wells_f500<-st_buffer(wells_f,dist=500,endCapStyle='FLAT')
  
  write_sf(wells_f500,paste("Buffers/",i,"_500_buffer.shp",sep=""))
  
  wells_f1000<-st_buffer(wells_f,dist=1000,endCapStyle='FLAT')
  
  st_write(wells_f1000,paste("Buffers/",i,"_1000_buffer.shp",sep=""),append=F)
  
  
  
  print(paste("done",i,"!!"))
}


buff_fctn_purr<-purrr::possibly(buff_fctn,otherwise = NA)

lapply(res_list,buff_fctn_purr)

########Add Type Curve Information remove SNAME CNAME
type_curve<- read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/TC_Areas/Permian_TC_Areas.shp")


wells<-wells%>%mutate(join_TC_ARea=ifelse(grepl('SD',Reservoir),'WFMP A SD',
                                          ifelse(grepl('WFMP A CARB|WFMP A LWR|WFMP A MID|WFMP A SHALE|WFMP A UPR|WFMP UNC|WFMP WEDGE',Reservoir) & BUSINES %in% c("DBBU","DELAWARE"),'WFMP A SH',
                                                 ifelse(grepl('WFMP A CARB|WFMP A LWR|WFMP A MID|WFMP A SHALE|WFMP A UPR|WFMP UNC|WFMP WEDGE',Reservoir) & BUSINES %in% c("MBBU","MIDLAND"),'WFMP A',
                                                 ifelse(grepl('WFMP B',Reservoir),'WFMP B',
                                                        ifelse(grepl('WFMP C',Reservoir),'WFMP C',
                                                               ifelse(grepl('JOMILL',Reservoir),'JMILL',
                                                                      ifelse(grepl('LSBY',Reservoir),'LSBY',
                                                                             ifelse(grepl('MSBY',Reservoir),'MSBY',
                                                                                    ifelse(grepl('USBY',Reservoir),'USBY',
                                                               ifelse(grepl('AVLN LWR',Reservoir),'LAVLN',
                                                                      ifelse(grepl('AVLN MID',Reservoir),'MAVLN',
                                                                             ifelse(grepl('AVLN UPR',Reservoir),'UAVLN',
                                                               ifelse(grepl('BS1',Reservoir),'BS1',
                                                                     ifelse(grepl('BS2',Reservoir),'BS2',
                                                                           ifelse(grepl('BS3S',Reservoir),'BS3S',
                                                                                 ifelse(grepl('BS3C',Reservoir),'BS3C',
                                                                                       ifelse(grepl('WFMP D',Reservoir),'WFMP D',Reservoir))))))))))))))))))

x=data.frame(unique(wells$join_TC_ARea))

wells_int<-st_intersection(wells,st_transform(st_buffer(type_curve,dist=0),st_crs(wells)))%>%
  filter(join_TC_ARea==zone)%>%
  mutate(length_int=as.numeric(st_length(geometry)))%>%
  group_by(UWI)%>%
  filter(length_int==max(length_int))%>%select(-SNAME,-CNAME)%>%
  st_drop_geometry()

wells_tc<-left_join(wells,wells_int)

write_sf(wells_tc,'BD_ALL_WELLS_TC_AREA.shp')

wells_tc<-read_sf("BD_ALL_WELLS_TC_AREA.shp")

mround <-
  function(number, multiple)
    multiple * round(number / multiple)



wells_tc$Lat_TC = mround(wells_tc$length / 5000, .5)*100  

CoSLookup=openxlsx::read.xlsx("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/ACQUISITIONS/2024 M&A/CoS_lookup.xlsx")

CoSLookup<-CoSLookup%>%mutate(origin=ifelse(origin=='DVN','DEVON','MARATHON'))

wells_tcc<-left_join(wells_tc,CoSLookup,by=c("Lat_TC"="Lat_TC","j_TC_AR"="zone","COUNTY"="County","OPERATO"="origin","TC_Area"="TC_Area"))

dvn_mro_pdp<-wells_tcc%>%filter(OPERATO %in% c("DEVON","MARATHON"))

openxlsx::write.xlsx(st_drop_geometry(dvn_mro_pdp),"//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/ACQUISITIONS/2024 M&A/dvn_mro_pdp_CoS/dvn_mro_pdp.xlsx")




##Spacing Same Zone
ss_spacing=data.table::fread("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Individual Work/DM Work_COP/Spacing/Final_Tables/Same_Zone_Spacing_Output.csv")


ss_spacing<-ss_spacing%>%mutate(Yr_Qtr=lubridate::quarter(Completion_Date,with_year = T))


ss_spacing<-ss_spacing%>%mutate(Operator_Short=ifelse(grepl("FELIX|WPX|DEVON",Operator_Short),"DEVON",Operator_Short))

ss_spacing<-ss_spacing%>%mutate(Zone_Alias=ifelse(grepl("WFMP SD|WFMP A",Zone),"WFMP A",
                                                  ifelse(grepl("AVLN",Zone),"AVLN",
                                                         ifelse(grepl("WFMP B",Zone),"WFMP B",
                                                                ifelse(grepl("BS2",Zone),"BS2",
                                                                ifelse(grepl("BS1",Zone),"BS1",Zone))))))

dvn_gg<-ss_spacing%>%filter(Operator_Short=='DEVON',Yr_Qtr>2017,Zone_Alias%in% c('WFMP A','AVLN','BS2'))%>%group_by(Yr_Qtr,Zone_Alias)%>%
  summarise(spacing=mean(Hypotenuse_90Day))%>%
  ggplot(.)+geom_step(aes(Yr_Qtr,spacing,color=Zone_Alias),size=1)+
  theme_minimal()+
  theme(legend.position='bottom')+
  labs(color='')+
  scale_color_manual(values=c("red","blue","black"))

setwd("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/ACQUISITIONS/2024 M&A/")

ggsave(plot=dvn_gg,filename="dvn_spacing.png",width=13.26,height=6.28)


