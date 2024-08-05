library(sf)
library(tidyverse)


setwd('//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/Well_Sticks_Zone/')



left = function (string,char) {
  substr(string,1,char)
}




wells<-read_sf("BD_ALL_WELLS.shp")


nm_missing<-read_sf("env_shp-Wells-Trajectories-0cc58_2024-01-08/env_shp-Wells-Trajectories-0cc58_2024-01-08.shp")

nm_missing<-nm_missing%>%select(api14_unfm,envoperatr,wellname,county,TVD,longitude,latitude,bh_long,bh_lat,latlength,
                                completdte,vintage)

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

nm_missing<-nm_missing[,c(14:30)]

nm_missing_zone<-openxlsx::read.xlsx("00_NM_Add_Zone.xlsx")
nm_missing_zone$api_12<-left(nm_missing_zone$api_12,12)
nm_missing_zone<-nm_missing_zone[,c(1,6)]
colnames(nm_missing_zone)[2]='Reservr'

nm_missing<-left_join(nm_missing,nm_missing_zone,by=c("UWI"='api_12'))
nm_missing$LANDING=nm_missing$Reservr
nm_missing$BUSINES=nm_missing$Reservr
nm_missing$Tank=nm_missing$Reservr
nm_missing$Reservr=nm_missing$Reservr

nm_missing<-st_transform(nm_missing,st_crs(wells))

nm_missing$meanTVD<-as.double(nm_missing$meanTVD)
nm_missing$length<-as.double(nm_missing$length)
nm_missing$year<-as.double(nm_missing$year)

#wells<-wells%>%select(-Source)
nm_missing<-nm_missing%>%select(colnames(wells))

wells<-rbind(wells%>%mutate(Source='Internal'),nm_missing%>%mutate(Source='Enverus'))

wells$API10<-left(wells$UWI,10)

wells$UWI<-as.character(wells$UWI)

wells<-wells%>%distinct(UWI,.keep_all=T)


##Add in Type Curves

type_curves<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/TC_Areas/Permian_TC_Areas.shp")


#unique(wells_int$Tank)
unique(type_curves$zone)

wells_int<-st_intersection(wells,st_transform(st_buffer(type_curves,dist=0),st_crs(wells)))

wells_int<-wells_int%>%mutate(Tank=ifelse(grepl('WFMP C|WFMP D',Tank) & Basin=='DB','WFMP C/D',Tank))

wells_int<-wells_int%>%mutate(Tank=ifelse(grepl('WFMP SD',Reservr),'WFMP A SD',
                                  ifelse(Reservr %in% c('WFMP A','WFMP A SHALE','WFMP A SH','WFMP A CARB','WFMP A LWR','WFMP A MID','WFMP A UPR') & Basin=='DB','WFMP A SH',
                                         ifelse(grepl('WFMP A',Reservr) & Basin=='MB','WFMP A',
                                         ifelse(Reservr %in% c("AVLN",'AVLN LWR','LAVLN'),'LAVLN',
                                                ifelse(Reservr %in% c('AVLN MID','MAVLN'),'MAVLN',
                                                       ifelse(Reservr %in% c('AVLN UPR','UAVLN'),'UAVLN',
                                                              ifelse(grepl('BS1',Reservr),'BS1',
                                                                     ifelse(grepl('WFMP B|WFMPB',Reservr),'WFMP B',
                                                                            ifelse(grepl('BS2',Reservr),'BS2',Tank))))))))))%>%arrange(Tank)


#data.table::fwrite(st_drop_geometry(wells_int),'wells_int.csv')

wells_int<-wells_int%>%filter(Tank==zone)
wells_int<-wells_int%>%mutate(length_int=as.numeric(st_length(geometry)))%>%
  group_by(UWI)%>%filter(length_int==max(length_int))


wells_int<-st_drop_geometry(wells_int)%>%select(UWI,zone:Drll_Md)%>%distinct(UWI,.keep_all=T)

wells_int<-wells_int[,1:5]

wellsF<-left_join(wells,wells_int)

wellsF<-wellsF%>%group_by(UWI)%>%arrange(desc(BUSINES))%>%distinct(UWI,.keep_all=T)

wellsF<-wellsF%>%mutate(Tank=ifelse(Tank=='BS1S','BS1',
                                    ifelse(Tank=='BS2S','BS2',
                                           ifelse(Tank=='LAVLN','AVLN',
                                                  ifelse(Tank=='MAVLN','AVLN',
                                                         ifelse(Tank=='UAVLN','AVLN',
                                                                ifelse(Tank=='WFMP SD','WFMP A',Tank
                                                                       )))))))

wellsF<-wellsF%>%group_by(API10)%>%arrange(desc(length))


st_write(wellsF,'BD_ALL_WELLS.shp',delete_layer=T)

#####wellsF=wellsF%>%filter(UWI=='300154515900')

#x=wells_final%>%filter(UWI=='300154515900')

############################################################

#setwd('//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Individual Work/DM Work_COP/2024-07-22 BS2_BS3C PROJECT/')


di_wells<-openxlsx::read.xlsx("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Individual Work/DM Work_COP/2024-07-22 BS2_BS3C PROJECT/Wells Table.xlsx")

di_wells$Spud.Date<-as.Date(di_wells$Spud.Date,origin='1899-12-30')

di_wells$Completion.Date<-as.Date(di_wells$Completion.Date,origin='1899-12-30')

di_wells<-di_wells%>%group_by(API10)%>%arrange(desc(Spud.Date))%>%distinct(API10,.keep_all=T)

di_wellsf<-di_wells%>%select(API10,Spud.Date,Completion.Date)

sections<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Permian Inventory CoS Join/Op_Non_Unknown_Acreage/sections_grid/sections_rowId.shp")

sticks<-wellsF

sticks<-left_join(sticks,di_wellsf)

sticks<-sticks%>%mutate(COMPLET=ifelse(is.na(COMPLET),Completion.Date,COMPLET))

sticks<-sticks%>%mutate(COMPLET=ifelse(is.na(COMPLET),Spud.Date,COMPLET))

sticks$COMPLET<-as.Date(sticks$COMPLET)

zone<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Individual Work/DM Work_COP/Spacing/zone.csv")

zone<-zone%>%rename(Tank2=Tank)
zone<-zone%>%select(Tank2, UWI,LANDING_ZONE)

sticks<-left_join(sticks,zone)

sticks<-sticks%>%mutate(Reservr=ifelse(is.na(Reservr),LANDING_ZONE,Reservr),
                        Tank=ifelse(is.na(Tank),Tank2,Tank))%>%select(-Tank2,-LANDING_ZONE)


sections<-st_transform(sections,st_crs(sticks))



create_date_periods <- function(dates, time_period = 28) {
  # TODO: add some error checking
  
  # create a vector to hold the results
  return_vector <- structure(rep(NA_real_, length(dates)), class = "Date")
  
  # if any date in the vector is still missing, keep going
  while(any(is.na(return_vector))) {
    
    # set minimum date amongst the values that are missing
    min_date <- min(dates[is.na(return_vector)])
    
    # if the date falls in range of interest, set it to the minimum date
    return_vector[dates >= min_date & dates <= min_date + time_period] <- min_date
  }
  
  return(return_vector)
}


sections<- sections%>%mutate(area=as.numeric(st_area(geometry)))
sections<-sections%>%mutate(row_id=row_number())

wells_int<-st_intersection(sticks,sections)

wells_int<-wells_int%>%mutate(int_length=as.numeric(st_length(geometry)))
wells_int<-wells_int%>%group_by(UWI)%>%
  #mutate(max_length=max(int_length))
  filter(int_length==max(int_length)| int_length>2300)%>%ungroup()

wells_intf<-wells_int%>%arrange(row_id)%>%rename(section_id=row_id)%>%arrange(section_id)%>%group_by(section_id)%>%
  mutate(section_count=max(row_number()))%>%
  ungroup()%>%
  group_by(UWI)%>%
  filter(section_count==max(section_count))%>%
  distinct(UWI,.keep_all=T)
#mutate(count=row_number())%>%filter(count==1)


wells_final<-left_join(sticks,st_drop_geometry(wells_intf)%>%select(UWI,section_id))


wells_finalF <-wells_final%>% mutate(Clust_ID = paste(OPERATO, "_", section_id, sep ="")) %>%
  group_by(Clust_ID) %>%
  filter(!is.na(COMPLET))  %>%
  mutate(PeriodClust =dense_rank(create_date_periods(COMPLET, time_period = 120)))%>%
  mutate(DateClust =create_date_periods(COMPLET, time_period = 120))

wells_finalF <-wells_finalF %>%ungroup()%>%mutate(Project= paste(Clust_ID,PeriodClust,sep="_"))


final_tbl<-left_join(sticks,st_drop_geometry(wells_finalF)%>%select(Project,UWI,section_id))


project_summary <- st_drop_geometry(final_tbl) %>%filter(!is.na(Tank))%>%
  group_by(Project) %>%
  summarize(bs2_count = sum(Tank == "BS2"), 
            bs3c_count = sum(Tank == "BS3C"),
            avln_count = sum(Tank == "AVLN"),
            bs1_count = sum(Tank == "BS1"),
            bs3s_count = sum(Tank == "BS3S"),
            wfmpa_count = sum(Tank == "WFMP A"),
            wfmpb_count = sum(Tank == "WFMP B"),
            wfmpc_count = sum(Tank == "WFMP C"),
            wfmpd_count = sum(Tank == "WFMP D"),
            msby_count= sum(Tank=='MSBY'),
            lsby_count= sum(Tank=='LSBY'),
            jmill_count= sum(Tank=='JMILL'),
            dean_count= sum(Tank=='DEAN')) %>%
  ungroup()


project_summary <- project_summary %>%
  mutate(bs2_bs3c_prj = bs2_count > 2 & bs3c_count > 2,
         bs2_bs3c_bs3s_prj = bs2_count > 2 & bs3c_count > 2 & bs3s_count > 2,
         bs2_bs3c_bs3s_wfmpa_prj = bs2_count > 2 & bs3c_count > 2 & bs3s_count > 2 & wfmpa_count > 2,
         bs1_avln_prj = bs1_count > 2 & avln_count > 2,
         bs3s_wfmpa_prj = bs3s_count > 2 & wfmpa_count > 2,
         bs3s_wfmpa_wfmpb_prj = bs3s_count > 2 & wfmpa_count > 2 & wfmpb_count > 2,
         bs3s_wfmpa_wfmpb_wfmpc_OR_wfmpd_prj = bs3s_count > 2 & wfmpa_count > 2 & wfmpb_count > 2 & (wfmpc_count > 2 | wfmpd_count > 2),
         bs1_avln__bs2_prj = bs1_count > 2 & avln_count > 2 & bs2_count>2,
         bs1_avln__bs2_bs3c_prj = bs1_count > 2 & avln_count > 2 & bs2_count>2 & bs3c_count>2,
         wfmpa_wfmpbs_prj = wfmpa_count > 2 & wfmpb_count > 2,
         dean_wfmpa_prj = wfmpa_count > 2 & dean_count > 2,
         dean_lsby_prj = lsby_count > 2 & dean_count > 2,
         wfmpa_wfmpb_wfmpc_wfmpd_prj = wfmpa_count > 2 & wfmpb_count > 2 & wfmpc_count >2 & wfmpd_count >2,
         wfmpc_wfmpd_prj = wfmpc_count >2 & wfmpd_count >2,
         lsby_wfmpa_wfmpb_prj = wfmpa_count > 2 & wfmpb_count > 2 & lsby_count > 2,
         msby_lsby_wfmpa_wfmpb_prj = wfmpa_count > 2 & wfmpb_count > 2 & lsby_count > 2 & msby_count >2,
         msby_lsbya_prj = lsby_count > 2 & msby_count > 2,
         jmill_msby_lsby_prj = lsby_count > 2 & msby_count > 2 & jmill_count > 2,
         msby_lsbya_prj = lsby_count > 2 & msby_count > 2, 
         jmill_msby_prj = jmill_count >2 & msby_count > 2,
         wfmpb_wfmpc_prj=wfmpb_count >2 & wfmpc_count >2,
         avln_bs1_bs2_bs3c_bs3s_wfmpa_prj= bs1_count >2 & avln_count >2 & bs2_count >2 & bs3c_count >2 & bs3s_count > 2 & wfmpa_count >2,
         avln_bs1_bs2_bs3s_wfmpa_prj= bs1_count >2 & avln_count >2 & bs2_count >2 & bs3s_count > 2 & wfmpa_count > 2
  )

# Join this information back to the original dataframe
final_tbl <- final_tbl %>%
  left_join(project_summary %>% select(Project, bs2_bs3c_prj:avln_bs1_bs2_bs3s_wfmpa_prj))


section_summary <- st_drop_geometry(final_tbl) %>%filter(!is.na(Tank))%>%
  group_by(section_id) %>%
  summarize(bs2_count = sum(Tank == "BS2"), 
            bs3c_count = sum(Tank == "BS3C"),
            avln_count = sum(Tank == "AVLN"),
            bs1_count = sum(Tank == "BS1"),
            bs3s_count = sum(Tank == "BS3S"),
            wfmpa_count = sum(Tank == "WFMP A"),
            wfmpb_count = sum(Tank == "WFMP B"),
            wfmpc_count = sum(Tank == "WFMP C"),
            wfmpd_count = sum(Tank == "WFMP D"),
            msby_count= sum(Tank=='MSBY'),
            lsby_count= sum(Tank=='LSBY'),
            jmill_count= sum(Tank=='JMILL'),
            dean_count= sum(Tank=='DEAN')) %>%
  ungroup()


section_summary <-section_summary %>%
  mutate(bs2_bs3c_sec = bs2_count > 2 & bs3c_count > 2,
         bs2_bs3c_bs3s_sec = bs2_count > 2 & bs3c_count > 2 & bs3s_count > 2,
         bs2_bs3c_bs3s_wfmpa_sec = bs2_count > 2 & bs3c_count > 2 & bs3s_count > 2 & wfmpa_count > 2,
         bs1_avln_sec = bs1_count > 2 & avln_count > 2,
         bs3s_wfmpa_sec = bs3s_count > 2 & wfmpa_count > 2,
         bs3s_wfmpa_wfmpb_sec = bs3s_count > 2 & wfmpa_count > 2 & wfmpb_count > 2,
         bs3s_wfmpa_wfmpb_wfmpc_OR_wfmpd_sec = bs3s_count > 2 & wfmpa_count > 2 & wfmpb_count > 2 & (wfmpc_count > 2 | wfmpd_count > 2),
         bs1_avln__bs2_sec = bs1_count > 2 & avln_count > 2 & bs2_count>2,
         bs1_avln__bs2_bs3c_sec = bs1_count > 2 & avln_count > 2 & bs2_count>2 & bs3c_count>2,
         wfmpa_wfmpbs_sec = wfmpa_count > 2 & wfmpb_count > 2,
         dean_wfmpa_sec = wfmpa_count > 2 & dean_count > 2,
         dean_lsby_sec = lsby_count > 2 & dean_count > 2,
         wfmpa_wfmpb_wfmpc_wfmpd_sec = wfmpa_count > 2 & wfmpb_count > 2 & wfmpc_count >2 & wfmpd_count >2,
         wfmpc_wfmpd_sec = wfmpc_count >2 & wfmpd_count >2,
         lsby_wfmpa_wfmpb_sec = wfmpa_count > 2 & wfmpb_count > 2 & lsby_count > 2,
         msby_lsby_wfmpa_wfmpb_sec = wfmpa_count > 2 & wfmpb_count > 2 & lsby_count > 2 & msby_count >2,
         msby_lsbya_sec = lsby_count > 2 & msby_count > 2,
         jmill_msby_lsby_sec = lsby_count > 2 & msby_count > 2 & jmill_count > 2,
         msby_lsbya_sec = lsby_count > 2 & msby_count > 2, 
         jmill_msby_sec = jmill_count >2 & msby_count > 2,
         wfmpb_wfmpc_sec=wfmpb_count >2 & wfmpc_count >2,
         avln_bs1_bs2_bs3c_bs3s_wfmpa_sec= bs1_count >2 & avln_count >2 & bs2_count >2 & bs3c_count >2 & bs3s_count > 2 & wfmpa_count >2,
         avln_bs1_bs2_bs3s_wfmpa_sec= bs1_count >2 & avln_count >2 & bs2_count >2 & bs3s_count > 2 & wfmpa_count > 2
         )

# Join this information back to the original dataframe
final_tbl <- final_tbl %>%
  left_join(section_summary %>% select(section_id, bs2_bs3c_sec:avln_bs1_bs2_bs3s_wfmpa_sec))



#write_sf(final_tbl,'db_codev.shp')


final_tbl_xlsx<-st_drop_geometry(final_tbl) %>%  relocate(sort(names(st_drop_geometry(final_tbl))))

final_tbl_xlsx<-final_tbl_xlsx%>%relocate(contains('prj'),.after = last_col())
final_tbl_xlsx<-final_tbl_xlsx%>%relocate(contains('sec'),.after = last_col())
final_tbl_xlsx<-final_tbl_xlsx%>%relocate(section_id,.after = year)

final_tbl_xlsx$UWI<-as.character(final_tbl_xlsx$UWI)

wells_final<-left_join(wellsF,final_tbl_xlsx)


write_sf(final_tbl%>%select(UWI),'codev_sptfre.shp')


openxlsx::write.xlsx(final_tbl_xlsx,'codev_dev.xlsx')


#############################################################



wells<-wells%>%rename(Reservoir=Reservr)%>%mutate(Reservoir=trimws(Reservoir))


res_list<-unique(wells$Reservoir)

i='WFMP B'

buff_fctn<-function(i){
  wells_f<-wells%>%filter(Reservoir==i)
  
  wells_f500<-st_buffer(wells_f,dist=500,endCapStyle='FLAT')
  
  write_sf(wells_f500,paste("Buffers/",i,"_500_buffer.shp",sep=""))
  
  wells_f1000<-st_buffer(wells_f,dist=1000,endCapStyle='FLAT')
  
  write_sf(wells_f1000,paste("Buffers/",i,"_1000_buffer.shp",sep=""))
  
  
  
  print(paste("done",i,"!!"))
}



lapply(res_list,buff_fctn)