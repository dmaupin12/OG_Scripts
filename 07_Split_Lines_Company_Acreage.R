source("Functions/inventory_functions.R")

source("Midland_Inventory/06_Setup.R")

###Read In Acreage----
acreage<-read_sf("Op_Non_Unknown_Acreage/Combined_Acreage/Combined_Acreage.shp")

#acreage<-acreage%>%filter(grepl('DIAMONDBACK',Company))


# acreage0<-read_sf("Op_Non_Unknown_Acreage/output/CONOCOPHILLIPS.shp")
# acreage<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Individual Work/DM Work_COP/2023-12-12 EER Inventory Same Zone Reduce/eer_acreage/EER_DUs10262023.shp")
# acreage<-acreage%>%select(geometry)%>%mutate(Status='Operated',SNAME='TEXAS',Basin='Midland',Company='ENDEAVOR')


#Diamondback Specific on unknown
# acreage<-acreage%>%filter(grepl('DEVON|DIAMONDBACK|MARATHON',Company,ignore.case=T))
# 
# acreage<-acreage%>%mutate(Status=ifelse(grepl('DIAMONDBACK',Company) & Status=='Unknown','Operated',Status),
#                           S_SNAME=ifelse(S_SNAME=="DIAMONDBACK ENERGY Unknown TEXAS",'DIAMONDBACK ENERGY Operated TEXAS',S_SNAME))

acreage<-acreage%>%mutate(S_SNAME=paste(Company,Status,SNAME))

acreage<-acreage%>%group_by(Company,Status,SNAME,S_SNAME)%>%
  summarise(geometry=st_union(geometry))


######################
#########################
###########################
#ONLY RUN THIS PORTION OF CODE WHEN UPDATING INTERSECT CODE FOR COMPANY ACREAGE
# 
# acreage<-acreage%>%filter(!grepl('CONOCOPHILLIPS',Company))
# 
# ###Add in Internal COP acreage
# cop_internal<- read_sf("Op_Non_Unknown_Acreage/cop_op_non_op/op_non_op.shp")
# cop_internal<-cop_internal%>%mutate(STATUS=ifelse(STATUS=='Undetermined','Unknown',ifelse(STATUS=='Non Op',"NonOperated",STATUS)))
# cop_internal<-cop_internal%>%select(geometry,STATUS)%>%rename(Status=STATUS)%>%
#   mutate(Company='CONOCOPHILLIPS',SNAME='Texas_NM',Basin='Delaware')
# 
# cop_internal_mb<-cop_internal%>%
#   mutate(Company='CONOCOPHILLIPS',SNAME='Texas_NM',Basin='Midland')
# 
# cop_internal<-rbind(cop_internal,cop_internal_mb)
# 
# cop_internal<-st_transform(cop_internal,st_crs(acreage))
# 
# acreage<-rbind(acreage,cop_internal)
# 
# acreage<- st_buffer(acreage,dist=0)%>%mutate(valid=st_is_valid(geometry))%>%filter(valid==TRUE)%>%
#   mutate(Status=ifelse(Status %in% c("Operated","Operated+"),"Operated",Status))%>%
#   group_by(Company,Status,SNAME)%>%
#   summarise(geometry=st_union(geometry))
# 
# acreage<- acreage%>%mutate(valid=st_is_valid(geometry),area=st_area(geometry))%>%filter(valid==TRUE, as.numeric(area)>100)%>%ungroup()%>%
#   st_buffer(.,dist=1)%>%
#   st_make_valid(.)

# 
# 
# ###Add in Endeavor and Pioneer acreage manually changed
# pioneer<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Permian Inventory/Op_Non_Unknown_Acreage/EndeavorFinalAcreage/PIONEER_update.shp")
# pioneer<-pioneer%>%select(Company,Basin,Status,SNAME)
# pioneer<-st_transform(pioneer,st_crs(acreage))
# endeavor<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Permian Inventory/Op_Non_Unknown_Acreage/EndeavorFinalAcreage/EndeavorUpdate_2022_09.shp")
# endeavor<-endeavor%>%select(Company,Basin,Status,SNAME)
# endeavor<-st_transform(endeavor,st_crs(acreage))
# endeavor<-st_buffer(endeavor,dist=0)%>%group_by(Company,Status,SNAME)%>%
#   summarise(geometry=st_union(geometry))
# 
# pxd_end<-rbind(pioneer%>%select(-Basin),endeavor)
# pxd_end<-pxd_end%>%mutate(Status_SNAME=paste(Company,Status,SNAME))
# 
# acreage<-acreage%>%select(Company,Status,SNAME,Status_SNAME)
# acreage<-rbind(acreage,pxd_end)
# 
# ##Add Fasken Oil and Ranch
# fasken<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Permian Inventory/Op_Non_Unknown_Acreage/Misc_ShapeFiles_InHouse/FASKEN_20180731.shp")
# fasken<-st_buffer(fasken,dist=0)%>%mutate(Company='FASKEN',Status='Operated',SNAME='TEXAS')%>%group_by(Company,Status,SNAME)%>%
#   summarise(geometry=st_union(geometry))%>%mutate(Status_SNAME='FASKEN Operated TEXAS')
# fasken<- st_transform(fasken,st_crs(acreage))
# acreage<-rbind(acreage,fasken)
# 
# 
# ###Add COP Barnett and CLR Midland Acg
# 
# cop_brnt<- read_sf("Op_Non_Unknown_Acreage/Misc_ShapeFiles_InHouse/CFILIP_BRNT.shp")
# cop_brnt<-st_buffer(cop_brnt,dist=0)%>%mutate(Company='CFILIP_BRNT',SNAME='TEXAS')%>%
#   mutate(row_id=row_number())%>%mutate(Status=ifelse(row_id==1,'Unknown','Operated'))%>%select(-row_id)%>%
#   group_by(Company,Status,SNAME)%>%
#   summarise(geometry=st_union(geometry))%>%
#   mutate(Status_SNAME=paste(Company,Status,SNAME))
# cop_brnt<- st_transform(cop_brnt,st_crs(acreage))
# acreage<-rbind(acreage,cop_brnt)
# 
# clr_add<-read_sf("Op_Non_Unknown_Acreage/Misc_ShapeFiles_InHouse/clr_clip.shp")
# clr_add<-st_buffer(clr_add,dist=0)%>%mutate(Company='CONTINENTAL RESOURCES',Status='Operated',SNAME='TEXAS')%>%
#   group_by(Company,Status,SNAME)%>%
#   summarise(geometry=st_union(geometry))%>%
#   mutate(Status_SNAME=paste(Company,Status,SNAME))
# clr_add<- st_transform(clr_add,st_crs(acreage))
# acreage<-rbind(acreage,clr_add)
# 
# 
# 
# civitas_add<-read_sf("//conoco.net/ho_shared/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/ACQUISITIONS/Hunt 2021/VDR/1_Cannonball/1_Land/1.1_Leasehold, DSU and PDP Wells Shapefiles/1.1.1_Cannonball_DSU_Shapefile/Cannonball_DSUs_Shapefile")
# st_crs(civitas_add) <- "+init=epsg:32039"
# 
# civitas_add<-st_buffer(st_zm(civitas_add),dist=0)%>%
#   mutate(Status=ifelse(grepl("Non-Op",Op_NonOp),"NonOperated",ifelse(grepl("Operated",Op_NonOp),"Operated",Op_NonOp)))%>%
#   mutate(Company='CIVITAS',SNAME='TEXAS')%>%
#   group_by(Company,Status,SNAME)%>%
#   summarise(geometry=st_union(geometry))%>%
#   mutate(Status_SNAME=paste(Company,Status,SNAME))
# civitas_add<- st_transform(civitas_add,st_crs(acreage))
# acreage<-rbind(acreage%>%select(-S_SNAME),civitas_add)
# 
# acreage<-st_buffer(acreage,dist=0)
# 
# write_sf(acreage,"Op_Non_Unknown_Acreage/Combined_Acreage/Combined_Acreage.shp")

operator_list<- unique(acreage$Company)


area<- acreage%>%mutate(area=as.numeric(st_area(geometry)/10000))%>%st_drop_geometry()
area<-area%>%arrange(desc(area))

area<- area%>%group_by(Company)%>%filter(area==max(area))%>%distinct(Company,area)

area<-area%>%ungroup()%>%arrange(desc(area))
#area<-area[1:100,]
#area<-area[-c(11,24,26),]

acreage<-acreage%>%rename(Status_SNAME=S_SNAME)
acreage_list<-acreage%>%mutate(area=as.numeric(st_area(geometry)/10000))%>%st_drop_geometry()%>%arrange(desc(area))


acreage_list<-unique(acreage_list$Status_SNAME)



###Read in combined lines----
all_shp<-read_sf("Basin_Line_Inventory/Combined/basin_lines.shp")


x<-c('BRNT','BRNT','LSBY','MSBY',	'MSBY','WFMP A',	'WFMP A','WFMP B',	'WFMP B','JMILL',	'JMILL','WFMP D',	'WFMP D','WFMP C',"AVLN","LAVLN","MAVLN","BS2","BS3S","UAVLN","WFMP A SD","WFMP A SH","BS1","WFMP B","WFMP C","BS3C")
y= c('BRNT|WDFD','BRNT|WDFD','LSBY|WFMP A|DEAN|WFMP B','MSBY|JMILL',	'MSBY|JMILL','LSBY|WFMP A|WFMP B|DEAN',	'LSBY|WFMP A|WFMP B|DEAN','LSBY|WFMP A|WFMP B|DEAN',	'LSBY|WFMP A|WFMP B|DEAN','MSBY|JMILL|LSBY',	'MSBY|JMILL|LSBY','WFMP D',	'WFMP D','WFMP C|WFMP B',"BS1|AVLN","BS1|AVLN","BS1|AVLN","BS2|BS3C","WFMP A|BS3S|BS3C","BS1|AVLN","WFMP A|BS3S","WFMP A|BS3S|WFMP B","BS1|AVLN","WFMP A|WFMP B","WFMP B|WFMP C|WFMP D","BS2|BS3S|BS3C")
a=c('MIDLAND_BASIN','MIDLAND_BASIN',"MIDLAND_BASIN","MIDLAND_BASIN","MIDLAND_BASIN","MIDLAND_BASIN","MIDLAND_BASIN","MIDLAND_BASIN","MIDLAND_BASIN","MIDLAND_BASIN","MIDLAND_BASIN","MIDLAND_BASIN","MIDLAND_BASIN","MIDLAND_BASIN","DELAWARE_BASIN","DELAWARE_BASIN","DELAWARE_BASIN","DELAWARE_BASIN","DELAWARE_BASIN","DELAWARE_BASIN","DELAWARE_BASIN","DELAWARE_BASIN","DELAWARE_BASIN","DELAWARE_BASIN","DELAWARE_BASIN","DELAWARE_BASIN")
z=data.frame(x,y,a)
z=unique(z)
z=z%>%arrange(x)

all_shp<-all_shp%>%left_join(z%>%rename(reservoir=x,pdp_filter=y,Company=a))
all_shp<-all_shp%>%filter(!is.na(pdp_filter))


lines <- read_sf('Op_Non_Unknown_Acreage/pdp/BD_ALL_WELLS.shp')
lines$Tier= 'Tier1'
lines <- st_transform(lines, "+init=epsg:32039")
lines <- lines %>% mutate(length = st_length(lines))
lines$length <- as.numeric(lines$length)
lines <- lines %>% filter(length > 1000)

##Filter Zone

# i='LAVLN'
# y='DELAWARE_BASIN'

pre_df<- data.frame()

pdp_filter_fctn<- function(i,y){

all_shpf<-all_shp%>%filter(reservoir==i,Company==y)
lines<-lines%>%rename(Reservr=Tank,Tank=Reservr)
lines<-lines%>%mutate(Reservr=ifelse(Reservr %in% c("LAVLN","MAVLN","AVLN",'UAVLN') & 
i %in% c("LAVLN","MAVLN","AVLN",'UAVLN'),i,Reservr))
lines<-lines%>%mutate(Reservr=ifelse(Reservr %in% c("WFMP A SH",'WFMP A SD','WFMP A') & 
                                       i %in% c("WFMP A SH",'WFMP A SD','WFMP A'),i,Reservr))


  ###Single Zone update----
# lines_res<-lines%>%filter(Reservr==i)
# tier_res<-tier_outlineOriginal%>%filter(LZ==i)  
# lines_res_int<-st_intersection(lines_res,st_transform(tier_res,st_crs(lines_res)))
# lines_res_int<-lines_res_int%>%group_by(UWI)%>%filter(spacing_parameter==min(spacing_parameter))%>%
#   select(UWI,spacing_parameter)%>%st_drop_geometry()
# lines_res<-left_join(lines_res,lines_res_int)%>%mutate(spacing_parameter= ifelse(is.na(spacing_parameter),1280,spacing_parameter))
# lines_res<-lines_res%>%mutate(spacing_parameter=ifelse(spacing_parameter==1280,1180,spacing_parameter))
# lines_res<-lines_res%>%mutate(spacing_parameter=ifelse(spacing_parameter!=1180,spacing_parameter-20,spacing_parameter))
# lines_res<-lines_res%>%rename(buffer_distance=spacing_parameter)
# lines<-lines_res

###Unhash for original----
lines<- lines%>%mutate(buffer_distance= ifelse(grepl(i,Reservr),1000,500))
lines<- lines%>%mutate(buffer_distance= ifelse(i %in% c("LSBY","WFMP A","WFMP B")& Reservr %in% c("LSBY","WFMP A",'WFMP B'),1000,500))
lines<- lines%>%mutate(buffer_distance= ifelse(Tier=="Tier1" & Reservr != i,500,buffer_distance))
lines<-lines%>%mutate(buffer_distance=ifelse(i==Reservr,1000,buffer_distance))
lines<-lines%>%mutate(buffer_distance=ifelse(is.na(Reservr),500,buffer_distance))
lines <- st_buffer(lines,lines$buffer_distance, endCapStyle = 'FLAT')
lines <- as_Spatial(lines)


lines <-
  rgeos::gBuffer(lines,
                 width = 1,
                 capStyle = 'FLAT',
                 byid = TRUE)
lines = st_as_sf(lines)


#lines_zone <- lines %>% filter(grepl(unique(all_shpf$reservoir), Reservr, ignore.case = T))
####Unhash for original
lines_zone <- lines %>% filter(grepl(unique(all_shpf$pdp_filter), Reservr, ignore.case = T))



lines_zone <- st_union(lines_zone)
lines_zone <- st_as_sf(lines_zone)

#write_sf(lines_zone,paste("Midland_Inventory/reservoir_output/",i, ".shp", sep = ""))

all_shpf<- st_transform(all_shpf,st_crs(lines_zone))


# ggplot()+
#   theme_minimal()+geom_sf(data=lines_zone,color='red')+
#   geom_sf(data=acreage_clip,color='blue')

acreage_clip <- st_difference(all_shpf, lines_zone)
acreage_clip <- st_collection_extract(acreage_clip, "LINESTRING")
acreage_clip <- st_cast(acreage_clip, 'LINESTRING')

acreage_clip<- acreage_clip%>%mutate(length=as.numeric(st_length(acreage_clip)))

pre_df<<- rbind(pre_df,acreage_clip)

print('done!')
}

vec<-all_shp#%>%filter(Company=='MIDLAND_BASIN')
vec<-vec%>%select(reservoir,Company)%>%rename(i=reservoir,y=Company)%>%distinct(i,y)

pmap(vec,pdp_filter_fctn)




pre_dfO<-pre_df
################################

#acreage<-acreage%>%mutate(Status_SNAME=paste(Company,Status,SNAME))

#operator_list<- unique(acreage$Company)
#acreage_list<-unique(acreage$Status_SNAME)

#acreage_list=acreage_list[137:139]

#i=acreage_list[1]

sections<-read_sf("Op_Non_Unknown_Acreage/sections_grid/sections_grid.shp")
st_crs(sections) <- "+proj=longlat +datum=WGS84"
sections <- st_transform(sections, "+init=epsg:32039")


lines<-st_as_sf(pre_df)

lines<-lines %>% group_by(id,Company,reservoir,Tier) %>% summarize(m = mean(row_number())) %>% st_cast("MULTILINESTRING")

write_sf(lines,'Final/pre_extend.shp')

#lines<-st_intersection(lines,acreage%>%filter(grepl('CONOCOPHILLIPS',Company)))

#lines_Test<-lines

#i=acreage_list[2]
#remove(i)

acreage_list=acreage_list[!grepl("MINERALS|MINERAL|PARTNER|PROPERTIES|INVESTMENTS|ROYALTY|ROYALTIES", acreage_list)]

#acreage_list<-acreage_list[39:200]

for (i in acreage_list) {
  
  acreagef<-acreage%>%filter(Status_SNAME==i)
  
  company<-unique(acreagef$Company)
  
  acreagef<- st_buffer(acreagef,dist=0)
  
  acreagef<-st_as_sf(acreagef)
  
  # #lines <- st_transform(lines, "+init=epsg:2257")
  # #ggplot(acreagef)+geom_sf()+geom_sf(data=lines,color='red')  
  # 
  # acreagef= st_cast(acreagef,'MULTIPOLYGON')
  # 
  # acreagef=st_collection_extract(acreagef,'POLYGON')
  # 
  # #acreagef=rmapshaper::ms_explode(acreagef)
  # 
  # acreagef<- st_intersection(acreagef,sections)
  
  #acreagef=acreagef%>%mutate(area=as.numeric(st_area(geometry))/43560)%>%filter(area>200)
  
  if(nrow(acreagef)==0){
    print("skip")
  }else{
  
  acreagef<-acreagef%>%summarise()
  

    combined_lines<-st_intersection(lines,acreagef)
    
    combined_lines<-combined_lines%>%mutate(length=as.numeric(st_length(geometry)))%>%arrange(desc(length))

    combined_lines<-combined_lines%>%mutate(id=row_number())
    
    pt<-combined_lines#%>%st_cast("LINESTRING")%>%mutate(id=row_number())
    
    # pt<-combined_lines[1:3,]%>%st_cast('MULTIPOINT')%>%st_cast('POINT')%>%sfc_as_cols()%>%
    #   group_by(id,Company,reservoir,Tier)%>%mutate(row_id=row_number())%>%filter(row_id==min(row_id)|row_id==max(row_id))%>%
    #   summarise(id=mean(id))%>%
    #   st_cast('LINESTRING')%>%mutate(length=as.numeric(st_length(geometry)))
    #     
    # pt <- combined_lines[1,] %>% group_by(id,Company,reservoir,Tier) %>% mutate(m = mean(row_number())) %>% st_cast("LINESTRING")%>%
    #   mutate(length=as.numeric(st_length(geometry)),row_id=row_number())%>%ungroup()%>%
    #   mutate(row_id=row_number())
    # 
    # first_point = st_line_sample(pt, sample = c(0))
    # first_point <- do.call(rbind, st_geometry(first_point)) %>%
    #   as_tibble() %>% setNames(c("sf_longitude", "sf_latitude"))%>%mutate(row_id=row_number())#%>%filter(row_id==min(row_id))
    # 
    # last_point = st_line_sample(pt, sample = c(1))
    # last_point <- do.call(rbind, st_geometry(last_point)) %>%
    #   as_tibble() %>% setNames(c("bh_longitude", "bh_latitude"))%>%mutate(row_id=row_number())#%>%filter(row_id==max(row_id))
    # 
    # comb_pt<-left_join(first_point,last_point)
    # 
    # pt_comb<-left_join(st_drop_geometry(pt),comb_pt)%>%group_by(id,Company,reservoir,Tier)%>%mutate(row_id=row_number())%>%
    #   filter(row_id==min(row_id)|row_id==max(row_id))
    # 
    # first_pt<-pt_comb%>%select(id,row_id,sf_longitude,sf_latitude,Company,reservoir,Tier)%>%group_by(id,Company,reservoir,Tier)%>%
    #   filter(row_id==min(row_id))%>%rename(longitude=sf_longitude,latitude=sf_latitude)%>%select(-row_id)
    # 
    # last_pt<-pt_comb%>%select(id,row_id,bh_longitude,bh_latitude,Company,reservoir,Tier)%>%group_by(id,Company,reservoir,Tier)%>%
    #   filter(row_id==max(row_id))%>%rename(longitude=bh_longitude,latitude=bh_latitude)%>%select(-row_id)
    # 
    # pt_full<-full_join(first_pt,last_pt)%>%arrange(id)
    # 
    # pt<-st_as_sf(pt_full%>%filter(!is.na(longitude)),coords=c('longitude','latitude'))%>%group_by(id,Company,reservoir,Tier)%>%
    #   summarize(m=mean(id))%>%st_cast('LINESTRING')%>%mutate(length=st_length(geometry))
    # 
    # pt<-left_join(pt,st_drop_geometry(combined_lines)%>%select(Company,reservoir,Tier,id)%>%distinct(id,.keep_all=T))
    
    #     
    if(nrow(combined_lines)<1){
      polygon="blank_pt"
    } else{
  

      pt<-pt%>%ungroup()%>%mutate(length=as.numeric(st_length(geometry)))%>%
        mutate(Lat_TC = mround(length / 5300, .25))%>%select(-m)

      pt<- pt%>%mutate(id=row_number())
      
      st_crs(pt)<-st_crs(combined_lines)
      
  #lines_clip<- st_intersection(pt,acreagef)
  

  
  #write_sf(lines_clip,"pre_df.shp")
  
  #plotly::ggplotly(ggplot(acreagef)+geom_sf(aes(fill=Status),color=NA)+geom_sf(data=x,aes(fill=Company),color=NA))
  
  if(nrow(pt)==0){
    print("no_lines")
  }else
  {
  
    mround <-
      function(number, multiple)
        multiple * round(number / multiple)
    
    if(grepl("ENDEAVOR",i)==T){
      pt=st_collection_extract(pt,'LINESTRING')
    }else{
      "continue"
    }

    pt=st_segments(pt)
    
  
    pt<-pt%>%mutate(length=as.numeric(st_length(pt))) 
    pt$Lat_TC = mround(pt$length / 5300, .25)
    
    pt<-pt%>%mutate(id=row_number())
    
    #split into 15000 length
    a<-pt%>%filter((Lat_TC>4.9 & Lat_TC<6.25)|Lat_TC>7.9)
    
    
    
    #split into 2 pieces Update this and next code to 3.25----
    b<-pt%>%filter((Lat_TC>3 & Lat_TC<4.9))
    
    #keep as is
    c<-pt%>%filter(Lat_TC<3.25)
    
    #split into 3 pieces
    d<-pt%>%filter(Lat_TC>6 & Lat_TC%%3!=0 & Lat_TC<8)
    
    
    
    #c Keep the same
    c = if (nrow(c) == 0) {
      c
    } else {
      c %>%
        mutate(length = as.numeric(st_length(c))) %>%
        mutate(id = paste("new", row_number()))
    }
    
    
    #a
    combined_lines_split = if (nrow(a) == 0) {
      a
    } else {
      a_split<-split_lines(a,max_length=15900,id="id")
      a<-left_join(st_drop_geometry(a),a_split)
      a<-st_as_sf(a)
      a<-a%>%select(-split_fID)
    }
    
    combined_lines_split = if (nrow(combined_lines_split) ==0) {
      combined_lines_split
    } else {
      combined_lines_split %>%
        mutate(length = as.numeric(st_length(combined_lines_split))) %>%
        mutate(id = paste("new", row_number()))
    }
    
    
    #b
    combined_lines_split_two_parts = if (nrow(b) == 0) {
      b
    } else {
      first_half=st_linesubstring(b, 0,.5)
      second_half=st_linesubstring(b, .5,1)
      rbind(first_half,second_half)
    }
    
    combined_lines_split_two_parts = if (nrow(combined_lines_split_two_parts) ==
                                         0) {
      combined_lines_split_two_parts
    } else {
      combined_lines_split_two_parts %>%
        mutate(length = as.numeric(st_length(combined_lines_split_two_parts))) %>%
        mutate(id = paste("new2", row_number()))
    }
    ####
    combined_lines_split_three_parts = if (nrow(d) == 0) {
      d
    } else {
      
      first_third=st_linesubstring(d, 0,.33)
      second_third=st_linesubstring(d, .333,.66)
      third_third=st_linesubstring(d, .666,1)
      
      rbind(first_third,second_third,third_third)
    }
    
    combined_lines_split_three_parts = if (nrow(combined_lines_split_three_parts) ==
                                           0) {
      combined_lines_split_three_parts
    } else {
      combined_lines_split_three_parts %>%
        mutate(length = as.numeric(st_length(combined_lines_split_three_parts))) %>%
        mutate(id = paste("new3", row_number()))
    }
    
    
    
    
    combined_lines_split <-if ((nrow(combined_lines_split)==0 | nrow(combined_lines_split)>0) & nrow(combined_lines_split_two_parts)==0) {
      combined_lines_split
    }else if(nrow(combined_lines_split)==0 & nrow(combined_lines_split_two_parts)>0){
      combined_lines_split_two_parts
    } else if (nrow(combined_lines_split)>0 & nrow(combined_lines_split_two_parts)>0){
      st_as_sf(raster::bind(
        as_Spatial(combined_lines_split),
        as_Spatial(combined_lines_split_two_parts)
      ))
    }
    
    
    combined_lines_split <-if ((nrow(combined_lines_split)==0 | nrow(combined_lines_split)>0) & nrow(combined_lines_split_three_parts)==0) {
      combined_lines_split
    }else if(nrow(combined_lines_split)==0 & nrow(combined_lines_split_three_parts)>0){
      combined_lines_split_three_parts
    } else if (nrow(combined_lines_split)>0 & nrow(combined_lines_split_three_parts)>0){
      st_as_sf(raster::bind(
        as_Spatial(combined_lines_split),
        as_Spatial(combined_lines_split_three_parts)
      ))
    }
    
    
    combined_lines_split <-if ((nrow(combined_lines_split)==0 | nrow(combined_lines_split)>0) & nrow(c)==0) {
      combined_lines_split
    }else if(nrow(combined_lines_split)==0 & nrow(c)>0){
      c
    } else if (nrow(combined_lines_split)>0 & nrow(c)>0){
      st_as_sf(raster::bind(
        as_Spatial(combined_lines_split),
        as_Spatial(c)
      ))
    }
    
    
    
    
    combined_lines_split<-combined_lines_split%>%mutate(length=as.numeric(st_length(combined_lines_split))) 
    combined_lines_split$Lat_TC = mround(combined_lines_split$length / 5000, .25)
    
    combined_lines_split<-combined_lines_split%>%mutate(id=row_number())
    
    combined_lines_split<-combined_lines_split%>%ungroup()%>%mutate(length=as.numeric(st_length(combined_lines_split)))
    combined_lines_split$Lat_TC = mround(combined_lines_split$length / 5000, .25)
    combined_lines_split<-as.data.frame(combined_lines_split)
    
    
    lookup <- c(geometry = "result")
    
    combined_lines_split<-combined_lines_split%>%rename(any_of(lookup))
    
    
    
    final_lines<- combined_lines_split%>%select(Company,length,Lat_TC,reservoir,Tier,geometry)
    #%>%filter(Lat_TC>.9)
    final_lines$Lat_TC<-as.factor(final_lines$Lat_TC)
    
    final_lines<-st_as_sf(final_lines)
    
    final_lines<-final_lines%>%ungroup()%>%mutate(length2=as.numeric(st_length(final_lines)))
    final_lines$Lat_TC2 = mround(final_lines$length2 / 5300, .25)

  } 
  
  if (nrow(final_lines)==0) {
    "no lines"
  }else{
  
  final_lines$Company= company
  
  final_lines<-final_lines%>%mutate(row_id=row_number())
  
  # vec=unique(final_lines$row_id)
  # 
  # all_shp_small=data.frame()
  # 
  # all_shp<-final_lines
  # 
  # shorten_line=function(i){
  #   all_shp2=all_shp%>%filter(row_id==i)  
  #   all_shp2=st_as_sf(all_shp2)
  #   
  #   if(all_shp2$length<5000){
  #   all_shp2  
  #   }else{
  #   
  #   sf::st_geometry(all_shp2) <- st_extend_shorten_line(sf::st_geometry(all_shp2), -175)
  #   }
  #   
  #   all_shp2<-all_shp2%>%mutate(length=as.numeric(st_length(geometry)))
  #   
  #   print(i)
  #   
  #   all_shp_small<-rbind(all_shp_small,all_shp2)
  #   
  # }
  # 
  # final=lapply(vec,shorten_line)
  # 
  # final=bind_rows(final)
  # 
  
  #plotly::ggplotly(ggplot(acreagef)+geom_sf()+geom_sf(data=pt,color='red'))
  
  output<<-final_lines%>%select(-length2,-Lat_TC2)
  
  #plotly::ggplotly(ggplot(output)+geom_sf(color='green')+theme_minimal())
  
  write_sf(final_lines,paste("Final/Final/",i,".shp",sep=""))
  
  }
}
  }
  
}






rm(list=ls())





