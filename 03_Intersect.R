left = function (string,char){ substr(string,1,char) }

wells<- fread("Op_Non_Unknown_Acreage/alias/env_csv-Wells-ce021_2024-05-14.csv")
wells$Unformatted_API_UWI<-as.character(wells$Unformatted_API_UWI)

glimpse(wells)
add_to_filter_vec<-wells%>%select(ENVOperator)%>%group_by(ENVOperator)%>%summarise(count=max(row_number()))%>%arrange(desc(count))
add_to_filter_vec<-add_to_filter_vec[1:150,]


add_to_filter_vec<- add_to_filter_vec%>%mutate(ENVOperator=ifelse(grepl("shell|COG|CONCHO|PDEH",ENVOperator,ignore.case = T),'CONOCOPHILLIPS',
                                      ifelse(grepl("ENDEAVOR|DIAMONDBACK",ENVOperator,ignore.case = T),'DIAMONDBACK',
                                             ifelse(grepl("KAISER-FRANCIS|GBK CORP",ENVOperator,ignore.case = T),'KAISER-FRANCIS',
                                                    ifelse(grepl("xto",ENVOperator,ignore.case = T),'EXXON',
                                                           ifelse(grepl("OXY|CROWNROCK|CROWNQUEST",ENVOperator,ignore.case = T),'OCCIDENTAL',
                                                                  ifelse(grepl("CENTENNIAL|COLGATE|EARTHSTONE|PERMIAN RESOURCES",ENVOperator,ignore.case = T),'CDEV_PERMIAN RESOURCES',
                                                                        ifelse(grepl("PIONEER",ENVOperator),"EXXON",
                                                                                       ifelse(grepl("WPX|FELIX",ENVOperator),"DEVON",
                                                                                              ifelse(grepl("PDC",ENVOperator),"CHEVRON",
                                                                                                     ifelse(grepl("LAREDO|HENRY|MAPLE|TALL CITY|DRIFTWOOD",ENVOperator),"VITAL",
                                                                                                            ifelse(grepl("PIEDRA|BLACK SWAN|PETROLEGACY",ENVOperator),"OVINTIV",
                                                                                                                   ifelse(grepl("ADVANCE ENERGY",ENVOperator),"MATADOR",
                                                                                                                          ifelse(grepl("TITUS|CALLON",ENVOperator),"APA CORP",ENVOperator))))))))))))))






sticks<- read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/Well_Sticks_Zone/BD_ALL_WELLS.shp")

sticks<-sticks%>%filter(Reservr != 'DVNN')

sticks$API10<- left(sticks$UWI,10)


#####Missing NM Wells
zone_check<-fread("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Individual Work/DM Work_COP/Spacing/zone.csv")
zone_check$API10<-left(zone_check$UWI,10)

zone_check<-zone_check%>%filter(!(API10 %in% c(sticks$API10)))

pdp_missingO<- read.csv("Op_Non_Unknown_Acreage/pdp/rseg_missing_wells.csv")
pdp_missingO$API10<- left(pdp_missingO$Unformatted_API_UWI,10)
pdp_missing<-pdp_missingO
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

combined_df<-left_join(combined_df,zone_check[,c(2:5)])

combined_df<-combined_df%>%filter(!is.na(geometry))

st_crs(combined_df) <- "+proj=longlat +datum=WGS84"

combined_df <- st_transform(combined_df, "+init=epsg:32039")
combined_df<-combined_df%>%rename(UWI=API10)
combined_df<-left_join(combined_df,pdp_missingO%>%select(ENVOperator,API10,County),by=c('UWI'='API10'))
combined_df<-combined_df%>%distinct(UWI,.keep_all=T)
sticks<-rbind(sticks%>%select(UWI,OPERATO,Reservr,COUNTY,Tank),combined_df%>%rename(Reservr=LANDING_ZONE)%>%select(UWI,Reservr,ENVOperator,County,Tank)%>%rename(OPERATO=ENVOperator,COUNTY=County))


sticks$API10<-left(sticks$UWI,10)
sticks<- left_join(sticks,wells%>%select(Unformatted_API_UWI,ENVOperator),
                   by=c("API10"="Unformatted_API_UWI"))

sticks<- sticks%>%mutate(Alias=ifelse(is.na(ENVOperator),OPERATO,ENVOperator))




sticks<- sticks%>%mutate(Alias=ifelse(grepl("shell|COG|CONCHO",Alias,ignore.case = T),'CONOCOPHILLIPS',
                                      ifelse(grepl("ENDEAVOR|DIAMONDBACK",Alias,ignore.case = T),'DIAMONDBACK',
                                             ifelse(grepl("KAISER-FRANCIS|GBK CORP",Alias,ignore.case = T),'KAISER-FRANCIS',
                                                    ifelse(Alias=="SEM",'SEQUITUR ENERGY',
                                                    ifelse(grepl("xto",Alias,ignore.case = T),'EXXON',
                                      ifelse(grepl("OXY|CROWNROCK|CROWNQUEST",Alias,ignore.case = T),'OCCIDENTAL',
                                      ifelse(grepl("CENTENNIAL|COLGATE|EARTHSTONE|PERMIAN RESOURCES",Alias,ignore.case = T),'CDEV_PERMIAN RESOURCES',
                                      ifelse(grepl('PIONEER',Alias,ignore.case=T) & COUNTY %in% c("WARD","WINKLER","REEVES","PECOS"),"CONTINENTAL RESOURCES",
                                      ifelse(grepl("PIONEER",Alias),"EXXON",
                                      ifelse(grepl("WPX|FELIX",Alias),"DEVON",
                                      ifelse(grepl("PDC",Alias),"CHEVRON",
                                      ifelse(grepl("LAREDO|HENRY|MAPLE|TALL CITY|DRIFTWOOD",Alias),"VITAL",
                                      ifelse(grepl("PIEDRA|BLACK SWAN|PETROLEGACY",Alias),"OVINTIV",
                                      ifelse(grepl("ADVANCE ENERGY",Alias),"MATADOR",
ifelse(grepl("TITUS|CALLON",Alias),"APA CORP",Alias))))))))))))))))

###Enverus survey sticks added 8.17.2023
#####Look to incorporate all enverus surveys
colnames(sticks)
mewbourne_survey_update<-read_sf('Op_Non_Unknown_Acreage/pdp/enverus surveys/env_shp-Wells-Trajectories-16131_2023-08-18/env_shp-Wells-Trajectories.shp')
mewbourne_survey_update<-mewbourne_survey_update%>%select(api14_unfm,envoperatr,envintrval,county)
colnames(mewbourne_survey_update)[1]='UWI'
colnames(mewbourne_survey_update)[2]='OPERATO'
colnames(mewbourne_survey_update)[3]='Reservr'
colnames(mewbourne_survey_update)[4]='COUNTY'
#colnames(mewbourne_survey_update)[5]='Tank'
#colnames(mewbourne_survey_update)[6]='API'

mewbourne_survey_update$API10<-mewbourne_survey_update$UWI
mewbourne_survey_update$ENVOperator<-mewbourne_survey_update$OPERATO
mewbourne_survey_update$Alias<-mewbourne_survey_update$OPERATO
mewbourne_survey_update$Tank<-mewbourne_survey_update$Reservr

mewbourne_survey_update<-st_transform(mewbourne_survey_update,st_crs(sticks))
sticks<-rbind(sticks,mewbourne_survey_update)

#Sections
sections<-read_sf("Op_Non_Unknown_Acreage/sections_grid/sections_grid.shp")
#update centennial and colgate land to permian resources
#use this for grid shapefile
#st_crs(sections) <- "+init=epsg:2257"

#use this for sections
st_crs(sections) <- "+proj=longlat +datum=WGS84"
sections <- st_transform(sections, "+init=epsg:32039")

sections<-sections%>%mutate(row_id=row_number())
sections<- sections%>%select(row_id)



st_crs(sticks) <- "+init=epsg:32039"

###State
state<- read_sf("Op_Non_Unknown_Acreage/state/nm_tx_state.shp")
st_crs(state) <- "+proj=longlat +datum=WGS84"
state <- st_transform(state, "+init=epsg:32039")



###Basin Outline
Delaware<- read_sf("Op_Non_Unknown_Acreage/basin_outline/Delaware.shp")
Delaware$Basin<-"Delaware" 

Midland<- read_sf("Op_Non_Unknown_Acreage/basin_outline/Midland.shp")  
cbp<-read_sf("Op_Non_Unknown_Acreage/basin_outline/cbp.shp")
cbp<-st_cast(cbp,'POLYGON')
Midland<-rbind(Midland%>%select(geometry),st_transform(cbp%>%select(geometry),st_crs(Midland)))%>%mutate(id="a")
st_crs(Midland) <- "+proj=longlat +datum=WGS84"
Midland <- st_transform(Midland, "+init=epsg:2257")
Midland<-st_buffer(Midland,dist=2000)
Midland<-Midland%>%group_by(id)%>%summarise(geometry=st_union(geometry))
Midland <- st_transform(Midland, st_crs(Delaware))%>%select(geometry)
Midland$Basin<-"Midland" 

basin<-rbind(Delaware%>%select(Basin),Midland)
basin<-basin%>%select(Basin)

st_crs(basin) <- "+proj=longlat +datum=WGS84"
basin<- st_transform(basin, "+init=epsg:32039")



#Acreage
acreage<- read_sf("Op_Non_Unknown_Acreage/DI_Company_Acreage/env_shp-OperatorLand-Operators-34709_2024-05-28/env_shp-OperatorLand-Operators-34709_2024-05-28.shp")

acreage<-acreage%>%rename(OpCompany=ENV_Op)%>%select(OpCompany)

st_crs(acreage) <- "+proj=longlat +datum=WGS84"
acreage <- st_transform(acreage, "+init=epsg:32039")

acreage_tble<-data.frame()

acreage<-acreage%>%arrange(OpCompany)



###COP INTERNAL
cop<-read_sf("//Conoco.net/ho_shared/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/BD_ANALYST/SHAPE_DROP/COP_ACREAGE_NEW!/COP_Acreage.shp")

cop<-cop%>%select(geometry)%>%mutate(OpCompany='CONOCOPHILLIPS')
cop<-cop %>% relocate(geometry, .after = last_col())

cop<-st_transform(cop,st_crs(acreage))

acreage<-acreage%>%filter(!(grepl("CONOCOPHILLIPS|ENDEAVOR",OpCompany)))


endeavor<-read_sf("Op_Non_Unknown_Acreage/endeavor/EER_DUs10262023.shp")

endeavor<-endeavor%>%select(geometry)%>%mutate(OpCompany='DIAMONDBACK ENERGY')
endeavor<-endeavor %>% relocate(geometry, .after = last_col())

endeavor<-st_transform(endeavor,st_crs(acreage))

acreage<-rbind(acreage,endeavor)





list_filter=st_drop_geometry(sticks)%>%group_by(Alias)%>%summarise(id=max(row_number()))%>%arrange(desc(id))
#list_filter= unique(list_filter[1:170,1])
list_filter=list_filter$Alias




list_filter<- c(additional_companies,list_filter)


acreage<- acreage%>%mutate(OpCompany= ifelse(grepl("HANDONG XINCHAO ENERGY CORP",OpCompany),"SURGE OPERATING",
                                             ifelse(grepl("ENDEAVOR",OpCompany),"DIAMONDBACK ENERGY",
                                                    ifelse(grepl("PDC",OpCompany),"CHEVRON",
                                                    ifelse(grepl("ADVANCE",OpCompany),"MATADOR",
                                                    ifelse(grepl("CROWNROCK|CROWNQUEST",OpCompany),"OCCIDENTAL PETROLEUM",
                                                    ifelse(grepl("DOUBLE EAGLE",OpCompany),"PIONEER NATURAL RESOURCES",
                                                           ifelse(grepl("PIONEER",OpCompany),"EXXON MOBIL",
                                                                  ifelse(grepl("CALLON",OpCompany),"APA CORP",
                                             ifelse(grepl("CENTENNIAL|COLGATE|PERMIAN RESOURCES",OpCompany),'CDEV_PERMIAN RESOURCES',
                                              ifelse(grepl('VITAL|LAREDO|HENRY RESOURCES|MAPLE|TALL CITY|DRIFTWOOD',OpCompany),'VITAL',
                                                                  ifelse(grepl('PIEDRA|BLACK SWAN|PETROLEGACY',OpCompany),'OVINTIV',
                                                           ifelse(grepl('EARTHSTONE',OpCompany),'CDEV_PERMIAN RESOURCES',
                                                           OpCompany)))))))))))))



acreage<-rbind(acreage,cop)


acreagef<-acreage%>%filter(grepl(paste(list_filter,sep="|",collapse="|"), OpCompany))
  
acreagef<-acreagef%>%filter(!grepl('UNITED|MINERAL|MINERALS|PARTNERS|INVESTMENT|INVESTMENTS|
                                   LP|ROYALTY|ROYALTIES|TRUST|FEDERAL LAND|WATER|MIDSTREAM|
                                   UNIVERSITY LAND|POTASH|SINOCHEM',OpCompany,ignore.case=T))

area_filter_df<-acreagef%>%mutate(area=as.numeric(st_area(.))/43560)%>%
  st_drop_geometry()%>%group_by(OpCompany)%>%
  summarise(area=sum(area))%>%ungroup()%>%arrange(desc(area))

area_filter_df<-area_filter_df[1:100,]


id=unique(area_filter_df$OpCompany)

additional_companies<- c("CHARGER","CRESCENT","MURCHISON","POINT ENERGY","TAP ROCK",'MEWBOURNE')

id=c(id,additional_companies)
vec=id

#id='SEM'

#id=sort(vec_master)[44]
op_function<-function(id){
  
  
  #Drilling Info clip  
  acreage_f<-acreage%>%filter(grepl(id,OpCompany,ignore.case = T))
  acreage_f<-st_buffer(acreage_f,dist=0)
  
  acreage_ff<-acreage_f
  acreage_f<-st_union(acreage_f)
  
  
  acreage_clip<-st_intersection(sections,acreage_f)
  acreage_clip<-acreage_clip%>%select(row_id)
  
  
  #Sticks Filter
  sticks_filter<- st_intersection(sticks,sections)%>%
    mutate(length_stick_sec=as.numeric(st_length(geometry)))%>%filter(length_stick_sec>1500)
  
  
  #Sticks clip
  sections_clip_sticks_int<-st_intersection(sections,sticks_filter%>%select(Alias))
  
  sections_clip_sticks_int<-sections_clip_sticks_int%>%group_by(row_id,Alias)%>%mutate(CountMaxAlias=max(row_number()))
  
  max_operator<-st_drop_geometry(sections_clip_sticks_int)%>%ungroup()%>%group_by(row_id)%>%
    filter(CountMaxAlias==max(CountMaxAlias)|grepl('CONOCOPHILLIPS',Alias))%>%distinct(Alias,.keep_all=T)
  #max_operator<-max_operatorO
  
  
  if(nchar(word(id,start=1))<3 & str_count(id,'\\w+')<2){
    max_operator<-max_operator%>%filter(grepl(stringr::word(id,start=1),stringr::word(Alias,start=1)))
  }else if(nchar(word(id,start=1))<3 & str_count(id,'\\w+')>2){
    max_operator<-max_operator%>%filter(grepl(stringr::word(id,start=1, end=2),stringr::word(Alias,start=1, end=2)))
  }else{
    max_operator<-max_operator%>%filter(grepl(stringr::word(id,1),stringr::word(Alias,1)))
  }
    

    #view(unique(x))
  
    ####OLD METHOD TO GET TO OPERATED SECTIONS
  # sections_clip_sticks<-sections_clip_sticks_int%>%mutate(row_id2=row_number())%>%group_by(row_id2)%>%mutate(rank=ifelse(grepl(Alias,id,ignore.case = T),1,2))%>%
  #   filter(rank==1)
  
  ##NEW METHOD FOR OPERATED SECTIONS
  sections_clip_sticks<-sections_clip_sticks_int%>%filter(row_id %in% c(max_operator$row_id))
  
    
  sections_clip_sticks<-sections_clip_sticks%>%ungroup()%>%
    mutate(length_int=as.numeric(st_length(geometry)))%>%filter(length_int>500)
  sections_clip_sticks<-st_drop_geometry(sections_clip_sticks)%>%distinct(row_id,.keep_all=T)
  
  sections_clip_sticks<-sections%>%filter(row_id %in% sections_clip_sticks$row_id)
  
    
  sections_clip_sticks_combined<-
    if(nrow(sections_clip_sticks)>0 & nrow(acreage_clip)>0){
      combined<-raster::bind(as_Spatial(sections_clip_sticks%>%mutate(source='Operated')),
                             as_Spatial(acreage_clip%>%mutate(source='DI')))
      combined<-st_as_sf(combined)
      combined<-combined%>%arrange(desc(source))%>%distinct(row_id,.keep_all=T)
    
      }else if(nrow(sections_clip_sticks)>0 & nrow(acreage_clip)<1)
    {
      combined<-sections_clip_sticks %>%mutate(source='Operated')
    } else{
      combined<-acreage_clip %>%mutate(source='DI')
    }
  
      
    #Non operated

  sections_clip_sticks<-acreage_clip%>%filter(!(row_id %in% max_operator$row_id))
  
  sections_clip_sticks<-st_drop_geometry(sections_clip_sticks)%>%mutate(source2='NonOperated')%>%distinct(row_id,.keep_all=T)
  
  sections_clip_sticks_combined<-sections_clip_sticks_combined%>%left_join(sections_clip_sticks)
  
    
  sections_clip_sticks_combined<-sections_clip_sticks_combined%>%group_by(row_id)%>%mutate(source3=ifelse(source=='DI' & source2=='NonOperated','NonOperated',source))
  
  sections_clip_sticks_combined<-sections_clip_sticks_combined%>%mutate(source3= ifelse(source=='DI' & is.na(source2),'Unknown',source3))
  
  sections_clip_sticks_combined<-sections_clip_sticks_combined%>%select(row_id,source3)%>%rename(Status=source3)
  
  sections_clip_sticks_combined<-sections_clip_sticks_combined%>%mutate(Company=id)
  
  sections_clip_sticks_combined<-if (id=='DIAMONDBACK ENERGY') {
    sections_clip_sticks_combined<-sections_clip_sticks_combined%>%mutate(Status=ifelse(Status=='Unknown','Operated',Status))
  }else{
    sections_clip_sticks_combined
  }
  
  sections_clip_final_summary<-sections_clip_sticks_combined%>%
    group_by(Company,Status)%>%
    summarize(geometry = st_union(geometry))
  
  area_operated<- sections_clip_final_summary%>%mutate(area=as.numeric(st_area(geometry)))%>%st_drop_geometry(.)%>%summarise(area=sum(area)/43560)
  area_operated<-area_operated$area
  area_operated<-if (length(area_operated)>1 | is_empty(area_operated)==FALSE) {
    area_operated
  }else{
    1
  }
  
  
  
  
  area_di<- acreage%>%filter(grepl(word(id,1),OpCompany,ignore.case = T))
  area_di<-st_buffer(area_di,dist=0)
  area_di<-area_di%>%mutate(area=as.numeric(st_area(geometry)))%>%st_drop_geometry(.)%>%summarise(area=sum(area)/43560)
  area_di<-area_di$area
  
  
  sections_clip_final_summary<- if (area_operated/area_di >2) {
    sections_clip_final_summary
  }else if (area_operated > 20000) {
    sections_clip_final_summary
    #st_intersection(sections_clip_final_summary,acreage_ff)
  }else{
    acreage_ff%>%mutate(Status='Unknown')
  }
  
  
  
  sections_clip_final_summary<-st_buffer(sections_clip_final_summary,dist=0)
  
  
  
  sections_clip_final_summary<-st_buffer(sections_clip_final_summary,dist=0)
  
  #plotly::ggplotly(ggplot()+geom_sf(data=sections_clip_final_summary_int,aes(fill=Status),color=NA))
  
  ##intersect dvn acreage
  
  
  
  sections_clip_final_summary_int<-st_intersection(sections_clip_final_summary,state%>%select(SNAME))
  
  sections_clip_final_summary_int<- sections_clip_final_summary_int%>%group_by(Status,SNAME)%>%
    summarise(geometry=st_union(geometry))
  
  sections_clip_final_summary_int<-sections_clip_final_summary_int%>%mutate(Status=ifelse(SNAME=='NEW MEXICO' & Status == "Unknown","Operated",Status))
  
  
  sections_clip_final_summary_int<- sections_clip_final_summary_int%>%group_by(Status,SNAME)%>%
    summarise(geometry=st_union(geometry))
  
  #plot(sections_clip_final_summary_int)
  
  
  
  #ggplot(sections_clip_final_summary_int)+geom_sf(data=basin)+geom_sf()+theme_minimal()
  #Add Basin identifier
  
  sections_clip_final_summary_int_basin<-st_intersection(sections_clip_final_summary_int,basin)
  
  sections_clip_final_summary_int<-left_join(sections_clip_final_summary_int,st_drop_geometry(sections_clip_final_summary_int_basin))
  
  sections_clip_final_summary_int<-  sections_clip_final_summary_int%>%mutate(Basin= ifelse(is.na(Basin),"Other",Basin))
  
  sections_clip_final_summary_int$Company<-id
  
  zzz<<-sections_clip_final_summary_int
  
  
  id= if (id=='CDEV_PERMIAN RESOURCES') {
    "PERMIAN RESOURCES"
  }else{
    id
  }
  
  if (id=='DIAMONDBACK ENERGY') {
    fang<-st_difference(sections_clip_final_summary_int,st_union(endeavor))
    endeavor<-endeavor%>%select(geometry)
    endeavor$Company=unique(sections_clip_final_summary_int$Company)
    endeavor$Status="Operated_Endeavor"  
    endeavor$SNAME="TEXAS"
    endeavor$Basin="Midland"
    sections_clip_final_summary_int<-rbind(fang,endeavor)
    sections_clip_final_summary_int<-st_buffer(sections_clip_final_summary_int,dist=0)
    
  }else{
    sections_clip_final_summary_int
    }
  
  write_sf(sections_clip_final_summary_int,paste("Op_Non_Unknown_Acreage/output/",id,".shp",sep=""),delete_layer=TRUE)
  
  print(paste(id,"done!!"))
  
}



#vec[c(6,7,31)]

op_functionPURR<-purrr::possibly(op_function,"NA")


add_to_filter_vec<-add_to_filter_vec%>%mutate(row_id=row_number())

add_to_filter_vec<-add_to_filter_vec%>%mutate(ENVOperator=ifelse(ENVOperator=='PERMIAN RESOURCES','CDEV_PERMIAN RESOURCES',ENVOperator))



vec_final=data.frame()
filt_vec_fctn<-function(i){

  add_to_filter_vec_f<-add_to_filter_vec%>%filter(row_id==i)
  
  add_to_filter_vec_f<-add_to_filter_vec_f%>%mutate(alias2=
if(nchar(word(ENVOperator,start=1))<3 & str_count(ENVOperator,'\\w+')<2){
  stringr::word(ENVOperator,start=1)
}else if(nchar(word(ENVOperator,start=1))<3 & str_count(ENVOperator,'\\w+')>2){
  stringr::word(ENVOperator,start=1, end=2)
}else{
  stringr::word(ENVOperator,1)
}
)
  vec_final<<-rbind(vec_final,add_to_filter_vec_f)
}


lapply(unique(add_to_filter_vec$row_id),filt_vec_fctn)

original_vec<-as_tibble(vec)


vec2_pre_df<-data.frame()

filter_original_fctn<-function(i){
  original_vec_f<-original_vec%>%filter(value==i)%>%distinct(value)
  original_vec_f_df<-original_vec_f
  original_vec_f<-original_vec_f$value
  
  operator_check<-if(nchar(word(original_vec_f,start=1))<3 & str_count(original_vec_f,'\\w+')<2){
    stringr::word(original_vec_f,start=1)
  }else if(nchar(word(original_vec_f,start=1))<3 & str_count(original_vec_f,'\\w+')>2){
    stringr::word(original_vec_f,start=1, end=2)
  }else{
    stringr::word(original_vec_f,1)
  }

original_vec_f_df$alias2=operator_check

vec2_pre_df<<-rbind(vec2_pre_df,original_vec_f_df)
}

lapply(unique(original_vec$value),filter_original_fctn)


vec_final2<-vec_final%>%filter(!(alias2 %in% c(vec2_pre_df$alias2)))

vec_master<-c(vec,vec_final2$ENVOperator)
view(vec_master)

lapply(vec_master,op_functionPURR)


#view(data.frame(vec))
rm(list=ls())

gc()
####All Operated Sections

#Sticks clip
# sections_clip_sticks_int<-st_intersection(sections,sticks)
# sections_clip_sticks<-sections_clip_sticks_int
# sections_clip_sticks<-st_drop_geometry(sections_clip_sticks)%>%distinct(row_id,.keep_all=T)
# 
# sections_clip_sticks<-sections%>%filter(row_id %in% sections_clip_sticks$row_id)
# 
# write_sf(sections_clip_sticks,'all_operated_sections.shp')
# 
