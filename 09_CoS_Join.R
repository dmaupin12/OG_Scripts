mround <-
  function(number, multiple)
    multiple * round(number / multiple)


basin<-read_sf("Op_Non_Unknown_Acreage/basin_outline/Delaware.shp")
#state<-read_sf("Op_Non_Unknown_Acreage/state/nm_tx_state.shp")%>%select(SNAME)
state <- tigris::counties(state = c("Texas","New Mexico"), cb = TRUE) %>%
  st_as_sf()

state<-state%>%rename(SNAME=STUSPS,COUNTY=NAME)


CoS=as_tibble(read.xlsx("CoS_Link_Table/CoS_Join_Tbl.xlsx"))

glimpse(CoS[,1:10])



inventory<-read_sf("Final/Combined/combined_inventory_4.21.2024.shp")

inventory<-inventory%>%mutate(row_id=row_number())

inventory$Lat_TC = mround(inventory$length / 5000, .5)*100

st_crs(inventory)<-"+init=epsg:32039"
inventory_int<-st_intersection(inventory,st_transform(basin%>%select(geometry)%>%mutate(Basin='DB'),st_crs(inventory)))
inventory_int<-st_drop_geometry(inventory_int)%>%select(row_id,Basin)
inventory<-left_join(inventory,inventory_int)
inventory<-inventory%>%mutate(Basin=ifelse(is.na(Basin),"MB",Basin))

state<-st_transform(state,st_crs(inventory))
state<-state%>%select(COUNTY,SNAME)

inventory<-inventory%>%mutate(row_id=row_number())
inventory_st<-st_intersection(inventory,state)

inventory_st<-inventory_st%>%mutate(length_st=as.numeric(st_length(geometry)))

inventory_st<-inventory_st%>%group_by(row_id)%>%filter(length_st==max(length_st))

tier_outline<-read_sf('Delaware_Inventory/PermianBasin_Tiers_BD/Permian_TC_Areas.shp')

inventory_st<-st_intersection(inventory_st,st_buffer(tier_outline,dist=0))

inventory_st<-inventory_st%>%filter(reservr==zone)%>%mutate(length_int=as.numeric(st_length(geometry)))%>%
  group_by(row_id)%>%filter(length_int==max(length_int))

inventory_st<-inventory_st%>%ungroup()%>%distinct(row_id,.keep_all=T)

inventory<-left_join(inventory,st_drop_geometry(inventory_st)%>%select(row_id,TC_Area,SNAME,COUNTY))

inventory$Well.Name=paste(inventory$Basin,"_",inventory$SNAME,"_",inventory$reservr,"_","TCA",inventory$TC_Area,"_",inventory$Lat_TC,sep="")

COS2=CoS%>%select(ALIAS,`CoS.$.WTI`)

COS2$ALIAS<-str_squish(COS2$ALIAS)

COS2$ALIAS<-gsub(" ","",COS2$ALIAS)

COS2<-COS2%>%distinct(ALIAS,.keep_all = T)

inventory$Well.Name<-str_squish(inventory$Well.Name)
inventory$Well.Name<-gsub(" ","",inventory$Well.Name)
inventory$Well.Name<-gsub("  ","",inventory$Well.Name)
inventory$Well.Name<-gsub(" ","",inventory$Well.Name)

inventory<-inventory%>%mutate(Lat_TC=ifelse(Lat_TC>300,300,Lat_TC))

inventory_final<-left_join(inventory,COS2%>%select(ALIAS,`CoS.$.WTI`),by=c("Well.Name"="ALIAS"))

inventory_final_db_missing<-inventory_final%>%filter(Basin=='DB' & is.na(`CoS.$.WTI`))

inventory_final_db_missing$Well.Name=paste(inventory_final_db_missing$Basin,"_",inventory_final_db_missing$reservr,"_","TCA",inventory_final_db_missing$TC_Area,"_",inventory_final_db_missing$Lat_TC,sep="")

inventory_final_db_missing$Well.Name<-str_squish(inventory_final_db_missing$Well.Name)
inventory_final_db_missing$Well.Name<-gsub(" ","",inventory_final_db_missing$Well.Name)
inventory_final_db_missing$Well.Name<-gsub("  ","",inventory_final_db_missing$Well.Name)
inventory_final_db_missing$Well.Name<-gsub(" ","",inventory_final_db_missing$Well.Name)




COS2$ALIAS<-str_replace(COS2$ALIAS,"NM_","")
COS2$ALIAS<-str_replace(COS2$ALIAS,"TX_","")

inventory_final_db_missing<-left_join(inventory_final_db_missing[,-17],COS2%>%select(ALIAS,`CoS.$.WTI`),by=c("Well.Name"="ALIAS"))

inventory_final2<-inventory_final%>%filter(!(Basin=='DB' & is.na(`CoS.$.WTI`)))

inventory_final3<-rbind(inventory_final2,inventory_final_db_missing)

write_sf(inventory_final3,"CoS_Inventory_Join_Final/CoS_Inventory_Final_4.23.24.shp")








