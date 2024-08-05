#source("Midland_Inventory/00_Setup.R")



library(ggalluvial)

##NEEDED FOR WEBSHOT
Sys.setenv(CHROMOTE_CHROME = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
chromote::find_chrome()


permian_outline<- read_sf("Basin_Outline/Basin_Outlines.shp")

single_line_cos<-read.xlsx("CoS_Link_Table/CoS_Join_Tbl.xlsx")


brnt<-read_sf('//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Permian Inventory CoS Join/Midland_Inventory/PermianBasin_Tiers_BD/BRNT Play Outline230913.shp')


tier_outlines<- read_sf("Delaware_Inventory/PermianBasin_Tiers_BD/Permian_TC_Areas.shp")
tier_outlinesO<-tier_outlines

tier_outlines<-st_buffer(tier_outlines,dist=0)%>%group_by(zone,Basin)%>%summarise(geometry=st_union(geometry))%>%st_buffer(.,dist=0)

tier_outlines<-tier_outlines%>%rename(LZ=zone)

tier_outlines <- st_transform(tier_outlines, "+init=epsg:32039")
brnt<-st_transform(brnt,st_crs(tier_outlines))

state_county <- tigris::counties(state = c("Texas","New Mexico"), cb = TRUE) %>%
  st_as_sf()

acreage<-read_sf("Op_Non_Unknown_Acreage/Combined_Acreage/Combined_Acreage.shp")

gross_area<-acreage%>%mutate(area=as.numeric(st_area(geometry)))
gross_area_sum<- st_drop_geometry(gross_area)%>%distinct(area,.keep_all=T)%>%
  group_by(Company)%>%summarise(area=sum(area)/43560)



di_acreage<-read_sf("Op_Non_Unknown_Acreage/DI_Company_Acreage/PermianCompanyAcreageDIShape.shp")
st_crs(di_acreage) <- "+proj=longlat +datum=WGS84"
di_acreage <- st_transform(di_acreage, "+init=epsg:32039")
di_acreage <-di_acreage %>%mutate(Status='DI',SNAME='DI',Basin='Basin')%>%select(Status,SNAME,Basin,OpCompany)%>%
  rename(Company=OpCompany)

di_acreage <-st_buffer(di_acreage,dist=0)
di_acreage <-di_acreage%>%group_by(Company)%>%summarize(geometry = st_union(geometry))


#acreage<-rbind(acreage,di_acreage)
inventory<-read_sf("CoS_Inventory_Join_Final/CoS_Inventory_Final_4.23.24.shp")


##Filter to 55 CoS----
inventory<-inventory%>%filter(`CoS_$_WTI`<55.001| is.na(`CoS_$_WTI`))
#inventory<-inventory%>%filter(Company !='DEVON ENERGY')

inventoryO<-inventory


#inventoryO0<-inventoryO0%>%mutate(reservoir %in% c("WFMP A SH","WFMP A SD","BS3S")


#inventory<-inventory%>%filter(grepl("DIAMONDBACK",Company))


inventory<- inventory%>%mutate(Company=ifelse(grepl("TAP ROCK|VENCER|HIBERNIA",Company),'CIVITAS',Company))%>%
  mutate(Company=ifelse(grepl("PIONEER",Company),'EXXON MOBIL',Company))#%>%
  #mutate(Company=ifelse(grepl("CDEV_PERMIAN",Company),'PERMIAN',Company))

inventory<-inventory%>%filter(!grepl("TEXAS PACIFIC|FRANCO NEVADA|PEGASUS|FREEHOLD|COLLINS|POM LEGACY",Company))


###Wide Basin Outline

wide_basin<-read_sf("Op_Non_Unknown_Acreage/basin_outline/WIDE_BASIN_OUTLINE.shp")

#inventory<-st_intersection(inventory,st_transform(wide_basin%>%select(Name),st_crs(inventory)))

inventory_filter<-st_drop_geometry(inventory)%>%
  group_by(Company)%>%summarise(count=max(row_number()))%>%filter(count>200)%>%distinct(Company)

inventory<-inventory%>%filter(Company %in% c(inventory_filter$Company))

mround <-
  function(number, multiple)
    multiple * round(number / multiple)



inventory<- inventory%>%rename(reservoir=reservr)%>%filter(!(Company=='DEVON ENERGY' & Basin=='MB'))

top_x<- st_drop_geometry(inventory)%>%group_by(Company)%>%summarize(Top_N=max(row_number()))%>%arrange(desc(Top_N))

inventory<-inventory%>%group_by(Company)%>%mutate(Top10=max(row_number()))%>%filter(Top10>1)

inventory<-inventory%>%filter(!(Company=='CHEVRON'& Status2=='Unknown'))

inventory<-inventory%>%mutate(reservoirO=reservoir)

inventory2<-inventory
inventory2<- inventory2%>%mutate(reservoir= ifelse(grepl('AVLN',reservoir),'AVLN',reservoir))

inventory2<-inventory2%>%mutate(Status= ifelse(grepl("NonOperated",Status),'NonOperated',ifelse(grepl("Unknown",Status),'Unknown','Operated')))


inventory<-inventory%>%mutate(reservoir= ifelse(grepl('WFMP A|SD|SH',reservoir),"WFMP A",
                                                ifelse(grepl('AVLN',reservoir),'AVLN',reservoir)))

inventory<-inventory%>%mutate(reservoir=ifelse(reservoir=='D',"WFMP D",reservoir))%>%mutate(color=
  ifelse(reservoir=='WFMP A','#88FFFF',
  ifelse(reservoir=='WFMP B','#46B3FF',
  ifelse(reservoir=='BS2','#FF0241',
  ifelse(reservoir=='AVLN','#EECCFF',
  ifelse(reservoir=='LSBY','#3BEA41',
  ifelse(reservoir=='MSBY','#99F46B',
  ifelse(reservoir=='BS1','#FF88FF',
  ifelse(reservoir=='WFMP C','#0B3BFF',
  ifelse(reservoir=='JMILL','#E0FB9E',
  ifelse(reservoir=='BRNT','#177B70',
  ifelse(reservoir=='WFMP D','#2F02BF',
  ifelse(reservoir=='BS3C','turquoise',
  ifelse(reservoir=='BS3S','#DF0006',reservoir))))))))))))))




###Company Vector Start----

company_vector=st_drop_geometry(inventory)%>%select(Company)%>%distinct(Company)
company_vector=company_vector$Company
company_vector<- company_vector[c(2,30,4,5,6,8,9,13,14,15,16,19,20)]
company_vector<-company_vector
company_vectorO=company_vector



map_function<-function(i){
  
  di_acreage_filter= acreage%>%group_by(Company)%>%filter(grepl(Company,i))
  
  acreagef<-acreage%>%filter(i==Company)
  cop<- acreage%>%filter(grepl('CONOCOPHILLIPS',Company))
  cop_DI<- di_acreage%>%group_by(Company)%>%filter(grepl('CONOCOPHILLIPS',Company))
  state_county<-st_transform(state_county,st_crs(acreagef))
  inventoryf<-inventory%>%filter(i==Company)
  
  bbox <- st_as_sfc(st_bbox(acreagef))
  bbox_centroid<-st_centroid(bbox)
  bbox_centroid<-st_as_sf(bbox_centroid)
  bbox<-st_segments(bbox)
  bbox<-st_as_sf(bbox)
  max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
  
  square=buffer_square(bbox_centroid,max_dist*2)
  st_crs(square) <- "+init=epsg:32039"
  bbox<-st_as_sf(square)
  bbox<-st_bbox(bbox)
  inventoryf<-inventoryf%>%group_by(reservoir)%>%mutate(count=max(row_number()))
  
  ggplot(state_county) +
    geom_sf(data = permian_outline, fill = 'grey') +
    geom_sf(fill = NA) +
    geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
    geom_sf(data = acreagef, fill = 'gold', color = NA) +
    geom_sf(data = cop, fill = 'light grey', color = NA) +
    geom_sf(data = cop_DI, fill = 'light grey', color = NA) +
    geom_sf(data = inventoryf, color = 'black') +
    geom_sf_text(data = state_county, aes(label = NAME), size = 3) +
    coord_sf(
      xlim = c(bbox[1], bbox[3]),
      ylim = c(bbox[2], bbox[4]),
      expand = T,
      lims_method = 'box'
    ) +
    theme_void()+
    theme( plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
      colour = "black",
      size = 1
    )
    
    )
}


map_plot<-lapply(company_vector,map_function)

#i=company_vectorO
##Bar Plot
bar_functionOrig<-function(i){

  inventoryf<-inventory%>%filter(i==Company)
  inventoryf<-inventoryf%>%mutate(reservoir=ifelse(reservoir %in% c("WFMP A","BS3S"),"WFMP A/BS3S",reservoir))
  inventoryf<-inventoryf%>%group_by(reservoir)%>%mutate(count=max(row_number()),
                                                       color=ifelse(Status2=='Unknown','dark green',
                                                                   ifelse(Status2=='Operated','orange','light blue')))
  
  inventoryf$Status2=fct_relevel(inventoryf$Status2,c("Operated","NonOperated","Unknown"))

  
  
  if(nrow(data.frame(unique(inventoryf$Status2)))==3){
    
    Colors<-c('turquoise','#fac132','#fa1b8e')
    names(Colors) <- levels(inventoryf$Status2)
    colScale <- scale_fill_manual(name = "Status",values = Colors)
  }else{
    Colors<-c('turquoise','#fac132')
    names(Colors) <- levels(inventoryf$Status2)
    colScale <- scale_fill_manual(name = "Status",values = Colors)
  }
  
  inventoryff<-st_drop_geometry(inventoryf)%>%group_by(reservoir,Basin,Status2)%>%
    summarise(Count=max(row_number()))%>%ungroup()%>%
    group_by(reservoir,Basin)%>%
    mutate(CountMax=sum(Count))

    ggplot(inventoryff,aes(reorder_within(reservoir,-CountMax,Basin),Count,fill=Status2,label=Count))+geom_col()+
    facet_wrap(~Basin,scales='free_x')+
    theme_minimal()+theme(axis.text.x=element_text(size=7,angle = 45, vjust = 0.5, hjust=1),
                          legend.position = "top",
                          legend.key.size = unit(0.3, "cm"),
                          legend.title = element_blank(),
                          legend.justification="left",
                          legend.margin=margin(0,0,0,0),
                          legend.box.margin=margin(-1,-1,-1,-1)
    )+xlab("")+ylab("Count")+
    scale_fill_discrete(name="")+
    scale_fill_manual(values=Colors)+
    #scale_fill_identity()+
    scale_y_continuous(labels=comma)+
    guides(fill = guide_legend(reverse = F))+
    geom_text(stat = "stratum", aes(stratum = Status2),size=2)+
    scale_x_reordered()

  
}


bar_plotOrig<-lapply(company_vector,bar_functionOrig)



company_vector=company_vectorO
i=company_vectorO[1]
remove(i)
##Table
table_function<-function(i){
   inventoryf<-inventory%>%filter(i==Company)

   inventoryf<-inventoryf%>%mutate(reservoir=ifelse(reservoirO %in% c("WFMP A SH","WFMP A SD","BS3S"),"WFMP A/BS3S Tank",reservoir))
  
    lat_count<-st_drop_geometry(inventoryf)%>%
     #filter(Tier=="Tier1")%>%
     filter(Lat_TC>180)%>%
     group_by(reservoir,Basin)%>%
     summarise(`2 Mile+`=max(row_number()))
   
   inventoryf<-inventoryf%>%mutate(`CoS_$_WTI`=ifelse(is.na(`CoS_$_WTI`),55,`CoS_$_WTI`))
   ###CoS Bin Join
   inventoryf$bins <- cut(inventoryf$`CoS_$_WTI`, breaks=c(0,40,45,50,55,200), 
                          labels=c("x≤$40","$40<x≤$45","$45<x≤$50","$50<x≤$55","$55+"))
   
   cos_bins<-st_drop_geometry(inventoryf)%>%group_by(reservoir,bins,Basin)%>%summarise(CoSCount=max(row_number()))
  
   cos_bins<-cos_bins%>%pivot_wider(names_from=bins,values_from=CoSCount)%>%
     relocate(-`$50<x≤$55`)
   
   inventory_summary<-st_drop_geometry(inventoryf)%>%
    group_by(reservoir,Basin)%>%
    summarise(count=max(row_number()))%>%
    ungroup()%>%
     arrange(count)%>%
     select(-count)%>%
     #left_join(lat_count)%>%
     #left_join(lat_countT1)%>%
     left_join(cos_bins)
     #mutate(`2 Mile+`= ifelse(is.na(`2 Mile+`),0,`2 Mile+`))
   
   sum_fctn=function(i){
     sum(na.rm=T)
   }
  
   max_cols=ncol(inventory_summary)
   inventory_summary<-inventory_summary%>%
     mutate_at(c(3:max_cols), ~replace(., is.na(.), 0))
  
  name=data.frame(company_vectorO)%>%mutate(row_id=row_number())%>%filter(company_vectorO==i)
  
  inventory_summary%>%
     mutate(Total=rowSums(pick(3:max_cols), na.rm = TRUE))%>%
     group_by(Basin,reservoir) |>
     summarise(across(c(2:max_cols-1), sum))%>%
    gt()%>%
    
     summary_rows(
       columns = c(2:max_cols+1),
       fns =  list(label = "Total", fn = "sum"),
       side = "bottom"
     )  |>
     grand_summary_rows(columns = c(2:max_cols+1),
                        fns = list(label = "Grand Total", fn = "sum"))%>%
      
    gtExtras::gt_theme_espn()%>%
    tab_options(column_labels.font.weight = 'bold',column_labels.font.size = 14,table.font.size = 11,
                table.width = 750,data_row.padding = px(1))%>%
     tab_style(
       style = list(cell_text(weight = "bold")),
       locations = cells_body(columns = vars(reservoir,Total))
     )%>%
  gtsave(.,paste("table_image/",name$row_id,".png",sep=""))


}


table_img<- lapply(company_vectorO,table_function)
# 
# table_img <- list.files("table_image/", pattern = "png")
# table_img<-paste("table_image/",table_img,sep="")
#
# get all images in a list
table_plot <- map(table_img, image_read)


####Pie Chart
pie_function<-function(i){
  inventoryf<-inventory%>%filter(i==Company)
  inventory_TC<-st_drop_geometry(inventoryf)%>%mutate(TC_Binned=ifelse(Lat_TC<1.5*100,"1 Mile",
                                                                     ifelse(Lat_TC>1.49*100 & Lat_TC< 2*100,"1.5 Mile",
                                                                            ifelse(Lat_TC>1.9*100 & Lat_TC< 2.5*100,"2 Mile",">2 Mile"))))%>%arrange(length)%>%
  group_by(TC_Binned)%>%summarise(Count=max(row_number()))%>%ungroup()%>%mutate(TC_Binned=factor(TC_Binned,levels=c("1 Mile","1.5 Mile","2 Mile",">2 Mile","3 Mile")))%>%
  mutate(CumCount=cumsum(Count),Percentage=round(Count/max(CumCount)*100,0))


inventory_TC$TC_Binned=fct_rev(inventory_TC$TC_Binned)

inventory_TC$hsize=2

ggplot(inventory_TC, aes(x = hsize, y = Count, fill = TC_Binned)) +
  geom_col() +
  geom_text(aes(label = paste(comma(Count)," (",percent(Percentage/100),")",sep="")),
            position = position_stack(vjust = 0.5),fontface='bold',size=2) +
  
  coord_polar(theta = "y") +
  xlim(c(0.2, unique(inventory_TC$hsize) + 0.5))+theme_void()+
  theme(legend.position='bottom',legend.text = element_text(size=9),legend.key.size = unit(0.5, "cm"),legend.title = element_blank())+
  guides(fill = guide_legend(reverse=TRUE))+
  scale_fill_manual(values=c('light blue','red','orange','grey'))

}


pie_plot<-lapply(company_vector,pie_function)



bar_function<-function(i){
  

  inventoryf<-inventory%>%filter(i==Company)
  inventoryf<-st_drop_geometry(inventoryf)%>%group_by(reservoir,color,Basin)%>%summarise(count=max(row_number()))
  
  inventoryf<-inventoryf%>%ungroup()%>%group_by(Basin)%>%arrange(Basin,count)%>%
    mutate(res_basin=paste(reservoir,Basin))%>%
    mutate(IDENTIFY = factor(res_basin, unique(res_basin))) 
  inventoryf$reservoir=as.factor(inventoryf$reservoir)
  inventoryf$reservoir<-fct_reorder(inventoryf$reservoir,as.numeric(inventoryf$IDENTIFY))
  
  inventoryf<- inventoryf%>%mutate(count_scale=count)%>%
  mutate_at(vars("count_scale"), scale)
  #inventoryf<-inventoryf%>%mutate(count_scale=ifelse(count_scale==min(count_scale),min(count_scale)+.5,count_scale))
  
  ggplot(inventoryf)+
    geom_bar(aes(IDENTIFY,count,fill=factor(Basin)),stat='identity')+
    theme_minimal()+theme(axis.text.x=element_text(size=7),
                          legend.position = "top",
                          legend.key.size = unit(0.3, "cm"),
                          legend.title = element_blank(),
                          legend.justification="left",
                          legend.margin=margin(0,0,0,0),
                          legend.box.margin=margin(-1,-1,-1,-1)
    )+xlab("")+ylab("Cumulative Count")+
    scale_fill_manual(values=c('red','blue'))+
    scale_x_discrete(labels=inventoryf$reservoir)+
    scale_y_continuous(labels=comma)+
    guides(fill = guide_legend(reverse = F),alpha='none')+
    geom_text(
      aes(IDENTIFY,count,label = scales::comma(count), 
          vjust = 0,hjust=.5),
      size = 3, fontface = "bold", family = "Spline Sans",angle=0
    )+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  
}


bar_plot<-lapply(company_vector,bar_function)


####Inventory Detail Slides----


##Avalon Map Detail Plot
avln_map_fctn<- function(i){
acreagef<- acreage%>%filter(i==Company)
inventory_filter<-inventory2%>%filter(reservoir=="AVLN" & i==Company) 

tier_outlines_filter=tier_outlines%>%filter(grepl('AVLN',LZ))%>%st_buffer(dist=10)%>%st_union()

outline_filter<-tier_outlines_filter
di_acreage_filter= di_acreage%>%group_by(Company)%>%filter(grepl(Company,i))

if (nrow(inventory_filter)==0) {

  ggplot()+geom_blank()+theme_void()
  
}else{
  
  bbox <- st_as_sfc(st_bbox(outline_filter))
  bbox_centroid<-st_centroid(bbox)
  bbox_centroid<-st_as_sf(bbox_centroid)
  bbox<-st_segments(bbox)
  bbox<-st_as_sf(bbox)
  max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
  
  basin_id='DB'
  expand_wide=if(basin_id=='DB'){
    1.8
  }else{
    1.5
  }
  expand_long=if(basin_id=='DB'){
    1.8*1.8/1.2
  }else{
    2.25
  }
  
  square=buffer_rectangle(bbox_centroid,x_length=max_dist*expand_wide,y_length=max_dist*expand_long)
  st_crs(square) <- "+init=epsg:32039"
  bbox<-st_as_sf(square)
  bbox<-st_bbox(bbox)
  
  ggplot(outline_filter)+
    #geom_sf(data=tier2_outlines_filter,fill='grey',alpha=.1)+
    geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
    geom_sf(data=acreagef,fill='gold',color=NA)+
    geom_sf(data=tier_outlines_filter,fill='grey',alpha=.2)+
    geom_sf(data=state_county,fill=NA)+
    geom_sf(data=inventory_filter,size=1.05,color='red')+
    geom_sf_text(data=state_county,aes(label=NAME),size=3)+
    coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T,lims_method = 'box')+  
    theme_void()+scale_color_identity()+theme(legend.position = 'none', 
    plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
      colour = "black",
      size = 1
    )
    )


}
}

avln_map<-lapply(company_vector,avln_map_fctn)

avln_plot_fctn<-function(i){
  inventory_filter<-inventory2%>%filter(grepl("AVLN",reservoir) & i==Company) 
  
  inventory_filter$bins <- cut(inventory_filter$`CoS_$_WTI`, breaks=c(0,30,35,40,45,50,200), labels=c("x≤30","30<x≤35","35<x≤40","40<x≤45","45<x≤50","55+"))
  
  
  inventory_aggregate<- st_drop_geometry(inventory_filter)%>%group_by(bins)%>%summarise(Count=max(row_number()))
  
  
  
  
  ggplot(inventory_aggregate) + geom_bar(aes(x = bins, y = Count), stat = 'identity')+
    theme_minimal()+labs(x="")+
    geom_text(aes(x = bins, y = Count,label=Count),color="white",size=3,position=position_stack(vjust=0.5))+
    theme(
      text = element_text(size = 8),
      legend.position = "top",
      legend.key.size = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.justification="left",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-1,-1,-1,-1)
    ) +
    scale_x_discrete(guide = guide_axis(angle = 45)) 
}

avln_plot<-lapply(company_vector,avln_plot_fctn)


sbsg_map_fctn<- function(i){
  acreagef<- acreage%>%filter(i==Company)
  inventory_filter<-inventory2%>%filter(reservoir=="BS2" & i==Company) 
  
  tier_outlines_filter=tier_outlines%>%filter(grepl('BS2',LZ))%>%st_buffer(dist=300)%>%st_union()
  
  outline_filter<-tier_outlines_filter
  di_acreage_filter= di_acreage%>%group_by(Company)%>%filter(grepl(Company,i))
  
  if (nrow(inventory_filter)==0) {
    
    ggplot()+geom_blank()+theme_void()
    
  }else{
    
    bbox <- st_as_sfc(st_bbox(outline_filter))
    bbox_centroid<-st_centroid(bbox)
    bbox_centroid<-st_as_sf(bbox_centroid)
    bbox<-st_segments(bbox)
    bbox<-st_as_sf(bbox)
    max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
    
    basin_id='DB'
    expand_wide=if(basin_id=='DB'){
      1.2*1.2
    }else{
      1.5
    }
    expand_long=if(basin_id=='DB'){
      1.8*1.2
    }else{
      2.25
    }
    
    square=buffer_rectangle(bbox_centroid,x_length=max_dist*expand_wide,y_length=max_dist*expand_long)
    st_crs(square) <- "+init=epsg:32039"
    bbox<-st_as_sf(square)
    bbox<-st_bbox(bbox)
    
    ggplot(outline_filter)+
      #geom_sf(data=tier2_outlines_filter,fill='grey',alpha=.1)+
      geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
      geom_sf(data=acreagef,fill='gold',color=NA)+
      geom_sf(data=tier_outlines_filter,fill='grey',alpha=.2)+
      geom_sf(data=state_county,fill=NA)+
      geom_sf(data=inventory_filter,size=1.05,color='red')+
      geom_sf_text(data=state_county,aes(label=NAME),size=3)+
      coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T,lims_method = 'box')+  
      theme_void()+scale_color_identity()+theme(legend.position = 'none', 
                                                plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
                                                  colour = "black",
                                                  size = 1
                                                )
      )
    
    
  }
}

sbsg_map<-lapply(company_vector,sbsg_map_fctn)

sbsg_plot_fctn<-function(i){
  inventory_filter<-inventory2%>%filter(grepl("BS2",reservoir) & i==Company) 
  
  inventory_filter$bins <- cut(inventory_filter$`CoS_$_WTI`, breaks=c(0,30,35,40,45,50,200), labels=c("x≤30","30<x≤35","35<x≤40","40<x≤45","45<x≤50","55+"))
  
  
  inventory_aggregate<- st_drop_geometry(inventory_filter)%>%group_by(bins)%>%summarise(Count=max(row_number()))
  
  
  
  
  ggplot(inventory_aggregate) + geom_bar(aes(x = bins, y = Count), stat = 'identity')+
    theme_minimal()+labs(x="")+
    geom_text(aes(x = bins, y = Count,label=Count),color="white",size=3,position=position_stack(vjust=0.5))+
    theme(
      text = element_text(size = 8),
      legend.position = "top",
      legend.key.size = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.justification="left",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-1,-1,-1,-1)
    ) +
    scale_x_discrete(guide = guide_axis(angle = 45))
}

sbsg_plot<-lapply(company_vector,sbsg_plot_fctn)

# tbsg_map_fctn<- function(i){
#   acreagef<- acreage%>%filter(i==Company)
#   inventory_filter<-inventory2%>%filter(reservoir=="BS3S" & i==Company) 
#   inventory_filter<-inventory_filter%>%mutate(color=ifelse(Tier=='Tier_3','red','black'))
#   tier_outlines_filter=tier_outlines%>%filter(LZ=="BS3S")
#   
#   tier_outlines_filter<-st_intersection(tier_outlines_filter,outline_filter)
#   #inventory_filter<-st_intersection(inventory_filter,outline_filter)
#   di_acreage_filter= di_acreage%>%group_by(Company)%>%filter(grepl(Company,i))
#   
#   if (nrow(inventory_filter)==0) {
#     
#     ggplot()+geom_blank()+theme_void()
#     
#   }else{
#     
#     bbox <- st_as_sfc(st_bbox(outline_filter))
#     bbox_centroid<-st_centroid(bbox)
#     bbox_centroid<-st_as_sf(bbox_centroid)
#     bbox<-st_segments(bbox)
#     bbox<-st_as_sf(bbox)
#     max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
#     
#     basin_id='DB'
#     expand_wide=if(basin_id=='DB'){
#       1.2
#     }else{
#       1.5
#     }
#     expand_long=if(basin_id=='DB'){
#       1.8
#     }else{
#       2.25
#     }
#     
#     square=buffer_rectangle(bbox_centroid,x_length=max_dist*expand_wide,y_length=max_dist*expand_long)
#     st_crs(square) <- "+init=epsg:32039"
#     bbox<-st_as_sf(square)
#     bbox<-st_bbox(bbox)
#     
#     ggplot(outline_filter)+
#       #geom_sf(data=tier2_outlines_filter,fill='grey',alpha=.1)+
#       geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
#       geom_sf(data=acreagef,fill='gold',color=NA)+
#       geom_sf(data=tier_outlines_filter,fill='grey',alpha=.2)+
#       geom_sf(data=state_county,fill=NA)+
#       geom_sf(data=inventory_filter,aes(color=color))+
#       geom_sf_text(data=state_county,aes(label=NAME),size=3)+
#       coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T,lims_method = 'box')+  
#       theme_void()+scale_color_identity()+theme(legend.position = 'none', 
#                                                 plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
#                                                   colour = "black",
#                                                   size = 1
#                                                 )
#       )
#     
#     
#   }
# }
# 
# tbsg_map<-lapply(company_vector,tbsg_map_fctn)
# 
# tbsg_plot_fctn<-function(i){
#   inventory_filter<-inventory2%>%filter(reservoir=="BS3S" & i==Company) 
#   inventory_filter<-inventory_filter%>%mutate(color=ifelse(Tier=='Tier_3','red','black'))
#   inventory_filter$Tier<-as.factor(inventory_filter$Tier)
#   inventory_filter$Tier<- forcats::fct_relevel(inventory_filter$Tier,levels= c("Tier1","Tier_3"))
#   
#   inventory_aggregate<- st_drop_geometry(inventory_filter)%>%group_by(Status,Tier)%>%summarise(Count=max(row_number()),Foot=sum(length))%>%
#     mutate(color=ifelse(grepl("Tier_3",Tier),'red','black'))%>%ungroup()%>%group_by(Status)%>%arrange(Status,Tier)%>%mutate(label_y=cumsum(Count)-.5*Count)
#   
#   
#   if (length(unique(inventory_aggregate$Tier))==1 & any(unique(inventory_aggregate$Tier)=='Tier1')==FALSE){
#     ggplot(inventory_aggregate) + geom_bar(aes(x = Status, y = Count, fill =
#                                                  color), stat = 'identity',position=position_stack(reverse=T)) +
#       scale_fill_identity(guide = 'legend',labels = c("Tier_2+"),name = 'Legend')+
#       scale_y_continuous(labels=comma)+theme_minimal()+xlab("")+
#       theme(
#         text = element_text(size = 8),
#         legend.position = "top",
#         legend.key.size = unit(0.3, "cm"),
#         legend.title = element_blank(),
#         legend.justification="left",
#         legend.margin=margin(0,0,0,0),
#         legend.box.margin=margin(-1,-1,-1,-1)
#       )+
#       geom_text(aes(x = Status,y=label_y, label = Count),fontface='bold',size=3,colour = "white")
#     
#   }else {
#     ggplot(inventory_aggregate) + geom_bar(aes(x = Status, y = Count, fill =
#                                                  color), stat = 'identity',position=position_stack(reverse=T)) + scale_fill_identity(
#                                                    guide = 'legend',
#                                                    labels = c("Tier_1", "Tier_2+"),
#                                                    name = 'Legend')+
#       scale_y_continuous(labels=comma)+theme_minimal()+xlab("")+
#       theme(
#         text = element_text(size = 8),
#         legend.position = "top",
#         legend.key.size = unit(0.3, "cm"),
#         legend.title = element_blank(),
#         legend.justification="left",
#         legend.margin=margin(0,0,0,0),
#         legend.box.margin=margin(-1,-1,-1,-1)
#       )+
#       geom_text(aes(x = Status,y=label_y, label = Count),fontface='bold',size=3,colour = "white")
#     
#   }  
# }
# 
# tbsg_plot<-lapply(company_vector,tbsg_plot_fctn)

# wfmp_sd_map_fctn<- function(i){
#   acreagef<- acreage%>%filter(i==Company)
#   inventory_filter<-inventory2%>%filter(reservoir=="WFMP A SD" & i==Company) 
#   inventory_filter<-inventory_filter%>%mutate(color=ifelse(Tier=='Tier_3','red','black'))
#   tier_outlines_filter=tier_outlines%>%filter(LZ=="WFMP A SD")
#   
#   # area2= if(area %in% c("TBSG","WFMP A SD")){
#   #   'WFMP A SH'
#   # }else{
#   #   "AVLN"}
#   
#   area2='WFMP A SH'
#   tier2_outlines_filter<-tier2_outlines%>%filter(area2==LZ)
#   outline_filter<-outline%>%filter("DB"==basin)
#   tier_outlines_filter<-st_intersection(tier_outlines_filter,outline_filter)
#   #inventory_filter<-st_intersection(inventory_filter,outline_filter)
#   di_acreage_filter= di_acreage%>%group_by(Company)%>%filter(grepl(Company,i))
#   
#   if (nrow(inventory_filter)==0) {
#     
#     ggplot()+geom_blank()+theme_void()
#     
#   }else{
#     
#     bbox <- st_as_sfc(st_bbox(outline_filter))
#     bbox_centroid<-st_centroid(bbox)
#     bbox_centroid<-st_as_sf(bbox_centroid)
#     bbox<-st_segments(bbox)
#     bbox<-st_as_sf(bbox)
#     max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
#     
#     basin_id='DB'
#     expand_wide=if(basin_id=='DB'){
#       1.2
#     }else{
#       1.5
#     }
#     expand_long=if(basin_id=='DB'){
#       1.8
#     }else{
#       2.25
#     }
#     
#     square=buffer_rectangle(bbox_centroid,x_length=max_dist*expand_wide,y_length=max_dist*expand_long)
#     st_crs(square) <- "+init=epsg:32039"
#     bbox<-st_as_sf(square)
#     bbox<-st_bbox(bbox)
#     
#     ggplot(outline_filter)+
#       geom_sf(data=tier2_outlines_filter,fill='grey',alpha=.1)+
#       geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
#       geom_sf(data=acreagef,fill='gold',color=NA)+
#       geom_sf(data=tier_outlines_filter,fill='grey',alpha=.2)+
#       geom_sf(data=state_county,fill=NA)+
#       geom_sf(data=inventory_filter,aes(color=color))+
#       geom_sf_text(data=state_county,aes(label=NAME),size=3)+
#       coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T,lims_method = 'box')+  
#       theme_void()+scale_color_identity()+theme(legend.position = 'none', 
#                                                 plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
#                                                   colour = "black",
#                                                   size = 1
#                                                 )
#       )
#     
#     
#   }
# }
# 
# wfmp_sd_map<-lapply(company_vector,wfmp_sd_map_fctn)
# 
# 
# 
# wfmp_sd_plot_fctn<-function(i){
#   inventory_filter<-inventory2%>%filter(reservoir=="WFMP A SD" & i==Company) 
#   inventory_filter<-inventory_filter%>%mutate(color=ifelse(Tier=='Tier_3','red','black'))
#   inventory_filter$Tier<-as.factor(inventory_filter$Tier)
#   inventory_filter$Tier<- forcats::fct_relevel(inventory_filter$Tier,levels= c("Tier1","Tier_3"))
#   
#   inventory_aggregate<- st_drop_geometry(inventory_filter)%>%group_by(Status,Tier)%>%summarise(Count=max(row_number()),Foot=sum(length))%>%
#     mutate(color=ifelse(grepl("Tier_3",Tier),'red','black'))%>%ungroup()%>%group_by(Status)%>%arrange(Status,Tier)%>%mutate(label_y=cumsum(Count)-.5*Count)
#   
#   
#   if (length(unique(inventory_aggregate$Tier))==1 & any(unique(inventory_aggregate$Tier)=='Tier1')==FALSE){
#     ggplot(inventory_aggregate) + geom_bar(aes(x = Status, y = Count, fill =
#                                                  color), stat = 'identity',position=position_stack(reverse=T)) +
#       scale_fill_identity(guide = 'legend',labels = c("Tier_2+"),name = 'Legend')+
#       scale_y_continuous(labels=comma)+theme_minimal()+xlab("")+
#       theme(
#         text = element_text(size = 8),
#         legend.position = "top",
#         legend.key.size = unit(0.3, "cm"),
#         legend.title = element_blank(),
#         legend.justification="left",
#         legend.margin=margin(0,0,0,0),
#         legend.box.margin=margin(-1,-1,-1,-1)
#       )+
#       geom_text(aes(x = Status,y=label_y, label = Count),fontface='bold',size=3,colour = "white")
#     
#   }else {
#     ggplot(inventory_aggregate) + geom_bar(aes(x = Status, y = Count, fill =
#                                                  color), stat = 'identity',position=position_stack(reverse=T)) + scale_fill_identity(
#                                                    guide = 'legend',
#                                                    labels = c("Tier_1", "Tier_2+"),
#                                                    name = 'Legend')+
#       scale_y_continuous(labels=comma)+theme_minimal()+xlab("")+
#       theme(
#         text = element_text(size = 8),
#         legend.position = "top",
#         legend.key.size = unit(0.3, "cm"),
#         legend.title = element_blank(),
#         legend.justification="left",
#         legend.margin=margin(0,0,0,0),
#         legend.box.margin=margin(-1,-1,-1,-1)
#       )+
#       geom_text(aes(x = Status,y=label_y, label = Count),fontface='bold',size=3,colour = "white")
#     
#   }  
# }
# 
# wfmp_sd_plot<-lapply(company_vector,wfmp_sd_plot_fctn)

wfmp_sh_map_fctn<- function(i){
  acreagef<- acreage%>%filter(i==Company)
  inventory_filter<-inventory2%>%filter(reservoir %in% c("WFMP A SD","WFMP A SH","BS3S") & i==Company) 
  inventory_filter$reservoir<-'WFMP A/BS3S Tank'
  tier_outlines_filter=tier_outlines%>%filter(LZ %in% c("WFMP A SD","WFMP A SH","BS3S"))%>%st_buffer(dist=100)%>%st_union()
  
  
  outline_filter<-tier_outlines_filter
  di_acreage_filter= di_acreage%>%group_by(Company)%>%filter(grepl(Company,i))
  
  if (nrow(inventory_filter)==0) {
    
    ggplot()+geom_blank()+theme_void()
    
  }else{
    
    bbox <- st_as_sfc(st_bbox(outline_filter))
    bbox_centroid<-st_centroid(bbox)
    bbox_centroid<-st_as_sf(bbox_centroid)
    bbox<-st_segments(bbox)
    bbox<-st_as_sf(bbox)
    max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
    
    basin_id='DB'
    expand_wide=if(basin_id=='DB'){
      1.2*1.2
    }else{
      1.5
    }
    expand_long=if(basin_id=='DB'){
      1.8*1.2
    }else{
      2.25
    }
    
    square=buffer_rectangle(bbox_centroid,x_length=max_dist*expand_wide,y_length=max_dist*expand_long)
    st_crs(square) <- "+init=epsg:32039"
    bbox<-st_as_sf(square)
    bbox<-st_bbox(bbox)
    
    ggplot(outline_filter)+
      #geom_sf(data=tier2_outlines_filter,fill='grey',alpha=.1)+
      geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
      geom_sf(data=acreagef,fill='gold',color=NA)+
      geom_sf(data=tier_outlines_filter,fill='grey',alpha=.2)+
      geom_sf(data=state_county,fill=NA)+
      geom_sf(data=inventory_filter,size=1.05,color='red')+
      geom_sf_text(data=state_county,aes(label=NAME),size=3)+
      coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T,lims_method = 'box')+  
      theme_void()+scale_color_identity()+theme(legend.position = 'none', 
                                                plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
                                                  colour = "black",
                                                  size = 1
                                                )
      )
    
    
    
  }
}

wfmp_sh_map<-lapply(company_vector,wfmp_sh_map_fctn)



wfmp_sh_plot_fctn<-function(i){
   inventory_filter<-inventory2%>%filter(reservoir %in% c("WFMP A SH","WFMP A SD","BS3S") & i==Company) 
  
  inventory_filter$bins <- cut(inventory_filter$`CoS_$_WTI`, breaks=c(0,30,35,40,45,50,200), labels=c("x≤30","30<x≤35","35<x≤40","40<x≤45","45<x≤50","55+"))
  
  
  inventory_aggregate<- st_drop_geometry(inventory_filter)%>%group_by(bins)%>%summarise(Count=max(row_number()))
  
  
  
  
  ggplot(inventory_aggregate) + geom_bar(aes(x = bins, y = Count), stat = 'identity')+
    theme_minimal()+labs(x="")+
    geom_text(aes(x = bins, y = Count,label=Count),color="white",size=3,position=position_stack(vjust=0.5))+
    theme(
      text = element_text(size = 8),
      legend.position = "top",
      legend.key.size = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.justification="left",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-1,-1,-1,-1)
    ) +
    scale_x_discrete(guide = guide_axis(angle = 45))
}

wfmp_sh_plot<-lapply(company_vector,wfmp_sh_plot_fctn)

fbsg_map_fctn<- function(i){
  acreagef<- acreage%>%filter(i==Company)
  inventory_filter<-inventory2%>%filter(reservoir=="BS1" & i==Company) 
  
  tier_outlines_filter=tier_outlines%>%filter(grepl('BS1',LZ))%>%st_buffer(dist=500)%>%st_union()
  
  outline_filter<-tier_outlines_filter
  di_acreage_filter= di_acreage%>%group_by(Company)%>%filter(grepl(Company,i))
  
  if (nrow(inventory_filter)==0) {
    
    ggplot()+geom_blank()+theme_void()
    
  }else{
    
    bbox <- st_as_sfc(st_bbox(outline_filter))
    bbox_centroid<-st_centroid(bbox)
    bbox_centroid<-st_as_sf(bbox_centroid)
    bbox<-st_segments(bbox)
    bbox<-st_as_sf(bbox)
    max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
    
    basin_id='DB'
    expand_wide=if(basin_id=='DB'){
      1.2
    }else{
      1.5
    }
    expand_long=if(basin_id=='DB'){
      1.8
    }else{
      2.25
    }
    
    square=buffer_rectangle(bbox_centroid,x_length=max_dist*expand_wide,y_length=max_dist*expand_long)
    st_crs(square) <- "+init=epsg:32039"
    bbox<-st_as_sf(square)
    bbox<-st_bbox(bbox)
    
    ggplot(outline_filter)+
      #geom_sf(data=tier2_outlines_filter,fill='grey',alpha=.1)+
      geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
      geom_sf(data=acreagef,fill='gold',color=NA)+
      geom_sf(data=tier_outlines_filter,fill='grey',alpha=.2)+
      geom_sf(data=state_county,fill=NA)+
      geom_sf(data=inventory_filter,size=1.05,color='red')+
      geom_sf_text(data=state_county,aes(label=NAME),size=3)+
      coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T,lims_method = 'box')+  
      theme_void()+scale_color_identity()+theme(legend.position = 'none', 
                                                plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
                                                  colour = "black",
                                                  size = 1
                                                )
      )
    
    
  }
}

fbsg_map<-lapply(company_vector,fbsg_map_fctn)

fbsg_plot_fctn<-function(i){
  inventory_filter<-inventory2%>%filter(grepl("BS1",reservoir) & i==Company) 
  
  inventory_filter$bins <- cut(inventory_filter$`CoS_$_WTI`, breaks=c(0,30,35,40,45,50,200), labels=c("x≤30","30<x≤35","35<x≤40","40<x≤45","45<x≤50","55+"))
  
  
  inventory_aggregate<- st_drop_geometry(inventory_filter)%>%group_by(bins)%>%summarise(Count=max(row_number()))
  
  
  
  
  ggplot(inventory_aggregate) + geom_bar(aes(x = bins, y = Count), stat = 'identity')+
    theme_minimal()+labs(x="")+
    geom_text(aes(x = bins, y = Count,label=Count),color="white",size=3,position=position_stack(vjust=0.5))+
    theme(
      text = element_text(size = 8),
      legend.position = "top",
      legend.key.size = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.justification="left",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-1,-1,-1,-1)
    ) +
    scale_x_discrete(guide = guide_axis(angle = 45))
}

fbsg_plot<-lapply(company_vector,fbsg_plot_fctn)
i=company_vector

wfmp_c_map_fctn<- function(i){
  acreagef<- acreage%>%filter(i==Company)
  inventory_filter<-inventory2%>%filter(reservoir=="WFMP C" & i==Company & Basin=='DB') 
  
  tier_outlines_filter=tier_outlines%>%filter(grepl('WFMP C',LZ) & Basin=='DB')%>%st_buffer(dist=10)%>%st_union()
  
  outline_filter<-tier_outlines_filter
  di_acreage_filter= di_acreage%>%group_by(Company)%>%filter(grepl(Company,i))
  if (nrow(inventory_filter)==0) {
    
    ggplot()+geom_blank()+theme_void()
    
  }else{
    
    bbox <- st_as_sfc(st_bbox(outline_filter))
    bbox_centroid<-st_centroid(bbox)
    bbox_centroid<-st_as_sf(bbox_centroid)
    bbox<-st_segments(bbox)
    bbox<-st_as_sf(bbox)
    max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
    
    basin_id='DB'
    expand_wide=if(basin_id=='DB'){
      1.2*1.2
    }else{
      1.5
    }
    expand_long=if(basin_id=='DB'){
      1.8*1.2
    }else{
      2.25
    }
    
    square=buffer_rectangle(bbox_centroid,x_length=max_dist*expand_wide,y_length=max_dist*expand_long)
    st_crs(square) <- "+init=epsg:32039"
    bbox<-st_as_sf(square)
    bbox<-st_bbox(bbox)
    
    ggplot(outline_filter)+
      #geom_sf(data=tier2_outlines_filter,fill='grey',alpha=.1)+
      geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
      geom_sf(data=acreagef,fill='gold',color=NA)+
      geom_sf(data=tier_outlines_filter,fill='grey',alpha=.2)+
      geom_sf(data=state_county,fill=NA)+
      geom_sf(data=inventory_filter,size=1.05,color='red')+
      geom_sf_text(data=state_county,aes(label=NAME),size=3)+
      coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T,lims_method = 'box')+  
      theme_void()+scale_color_identity()+theme(legend.position = 'none', 
                                                plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
                                                  colour = "black",
                                                  size = 1
                                                )
      )
    
    
    
  }
}

wfmp_c_map<-lapply(company_vector,wfmp_c_map_fctn)

wfmp_c_plot_fctn<-function(i){
  inventory_filter<-inventory2%>%filter(reservoir=='WFMP C',Basin=='DB', i==Company) 
  
  inventory_filter$bins <- cut(inventory_filter$`CoS_$_WTI`, breaks=c(0,30,35,40,45,50,200), labels=c("x≤30","30<x≤35","35<x≤40","40<x≤45","45<x≤50","55+"))
  
  
  inventory_aggregate<- st_drop_geometry(inventory_filter)%>%group_by(bins)%>%summarise(Count=max(row_number()))
  
  
  
  
  ggplot(inventory_aggregate) + geom_bar(aes(x = bins, y = Count), stat = 'identity')+
    theme_minimal()+labs(x="")+
    geom_text(aes(x = bins, y = Count,label=Count),color="white",size=3,position=position_stack(vjust=0.5))+
    theme(
      text = element_text(size = 8),
      legend.position = "top",
      legend.key.size = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.justification="left",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-1,-1,-1,-1)
    ) +
    scale_x_discrete(guide = guide_axis(angle = 45))
}

wfmp_c_plot<-lapply(company_vector,wfmp_c_plot_fctn)

bs3c_map_fctn<- function(i){
  acreagef<- acreage%>%filter(i==Company)
  inventory_filter<-inventory2%>%filter(reservoir=="BS3C" & i==Company) 
  
  tier_outlines_filter=tier_outlines%>%filter(grepl('BS3C',LZ))%>%st_buffer(dist=50)%>%st_union()
  
  outline_filter<-tier_outlines_filter
  di_acreage_filter= di_acreage%>%group_by(Company)%>%filter(grepl(Company,i))
  
  
  if (nrow(inventory_filter)==0) {
    
    ggplot()+geom_blank()+theme_void()
    
  }else{
    
    bbox <- st_as_sfc(st_bbox(outline_filter))
    bbox_centroid<-st_centroid(bbox)
    bbox_centroid<-st_as_sf(bbox_centroid)
    bbox<-st_segments(bbox)
    bbox<-st_as_sf(bbox)
    max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
    
    basin_id='DB'
    expand_wide=if(basin_id=='DB'){
      1.2
    }else{
      1.5
    }
    expand_long=if(basin_id=='DB'){
      1.8
    }else{
      2.25
    }
    
    square=buffer_rectangle(bbox_centroid,x_length=max_dist*expand_wide,y_length=max_dist*expand_long)
    st_crs(square) <- "+init=epsg:32039"
    bbox<-st_as_sf(square)
    bbox<-st_bbox(bbox)
    
    ggplot(outline_filter)+
      #geom_sf(data=tier2_outlines_filter,fill='grey',alpha=.1)+
      geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
      geom_sf(data=acreagef,fill='gold',color=NA)+
      geom_sf(data=tier_outlines_filter,fill='grey',alpha=.2)+
      geom_sf(data=state_county,fill=NA)+
      geom_sf(data=inventory_filter,size=1.05,color='red')+
      geom_sf_text(data=state_county,aes(label=NAME),size=3)+
      coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T,lims_method = 'box')+  
      theme_void()+scale_color_identity()+theme(legend.position = 'none', 
                                                plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
                                                  colour = "black",
                                                  size = 1
                                                )
      )
    
  }
}

bs3c_map<-lapply(company_vector,bs3c_map_fctn)

bs3c_plot_fctn<-function(i){
  inventory_filter<-inventory2%>%filter(grepl("BS3C",reservoir) & i==Company) 
  
  inventory_filter$bins <- cut(inventory_filter$`CoS_$_WTI`, breaks=c(0,30,35,40,45,50,200), labels=c("x≤30","30<x≤35","35<x≤40","40<x≤45","45<x≤50","55+"))
  
  
  inventory_aggregate<- st_drop_geometry(inventory_filter)%>%group_by(bins)%>%summarise(Count=max(row_number()))
  
  
  
  
  ggplot(inventory_aggregate) + geom_bar(aes(x = bins, y = Count), stat = 'identity')+
    theme_minimal()+labs(x="")+
    geom_text(aes(x = bins, y = Count,label=Count),color="white",size=3,position=position_stack(vjust=0.5))+
    theme(
      text = element_text(size = 8),
      legend.position = "top",
      legend.key.size = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.justification="left",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-1,-1,-1,-1)
    ) +
    scale_x_discrete(guide = guide_axis(angle = 45))
}

bs3c_plot<-lapply(company_vector,bs3c_plot_fctn)


wfmp_b_db_map_fctn<- function(i){
  acreagef<- acreage%>%filter(i==Company)
  inventory_filter<-inventory2%>%filter(reservoir=="WFMP B" & i==Company & Basin=='DB') 
  
  tier_outlines_filter=tier_outlines%>%filter(grepl('WFMP B',LZ) & Basin=='DB')%>%st_buffer(dist=705)%>%st_union()
  
  outline_filter<-tier_outlines_filter
  di_acreage_filter= di_acreage%>%group_by(Company)%>%filter(grepl(Company,i))
  
  if (nrow(inventory_filter)==0) {
    
    ggplot()+geom_blank()+theme_void()
    
  }else{
    
    bbox <- st_as_sfc(st_bbox(outline_filter))
    bbox_centroid<-st_centroid(bbox)
    bbox_centroid<-st_as_sf(bbox_centroid)
    bbox<-st_segments(bbox)
    bbox<-st_as_sf(bbox)
    max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
    
    basin_id='DB'
    expand_wide=if(basin_id=='DB'){
      1.2*1.3
    }else{
      1.5
    }
    expand_long=if(basin_id=='DB'){
      1.8*1.3
    }else{
      2.25
    }
    
    square=buffer_rectangle(bbox_centroid,x_length=max_dist*expand_wide,y_length=max_dist*expand_long)
    st_crs(square) <- "+init=epsg:32039"
    bbox<-st_as_sf(square)
    bbox<-st_bbox(bbox)
    
    ggplot(outline_filter)+
      #geom_sf(data=tier2_outlines_filter,fill='grey',alpha=.1)+
      geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
      geom_sf(data=acreagef,fill='gold',color=NA)+
      geom_sf(data=tier_outlines_filter,fill='grey',alpha=.2)+
      geom_sf(data=state_county,fill=NA)+
      geom_sf(data=inventory_filter,size=1.05,color='red')+
      geom_sf_text(data=state_county,aes(label=NAME),size=3)+
      coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T,lims_method = 'box')+  
      theme_void()+scale_color_identity()+theme(legend.position = 'none', 
                                                plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
                                                  colour = "black",
                                                  size = 1
                                                )
      )
    
    
  }
}

wfmp_b_db_map<-lapply(company_vector,wfmp_b_db_map_fctn)

wfmp_b_db_plot_fctn<-function(i){
  inventory_filter<-inventory2%>%filter(reservoir=='WFMP B',Basin=='DB', i==Company) 
  
  inventory_filter$bins <- cut(inventory_filter$`CoS_$_WTI`, breaks=c(0,30,35,40,45,50,200), labels=c("x≤30","30<x≤35","35<x≤40","40<x≤45","45<x≤50","55+"))
  
  
  inventory_aggregate<- st_drop_geometry(inventory_filter)%>%group_by(bins)%>%summarise(Count=max(row_number()))
  
  
  
  
  ggplot(inventory_aggregate) + geom_bar(aes(x = bins, y = Count), stat = 'identity')+
    theme_minimal()+labs(x="")+
    geom_text(aes(x = bins, y = Count,label=Count),color="white",size=3,position=position_stack(vjust=0.5))+
    theme(
      text = element_text(size = 8),
      legend.position = "top",
      legend.key.size = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.justification="left",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-1,-1,-1,-1)
    ) +
    scale_x_discrete(guide = guide_axis(angle = 45))
}

wfmp_b_db_plot<-lapply(company_vector,wfmp_b_db_plot_fctn)


###Midland Detail Maps----


jmill_map_fctn<- function(i){
  acreagef<- acreage%>%filter(i==Company)
  inventory_filter<-inventory2%>%filter(reservoir=="JMILL" & i==Company) 
  
  tier_outlines_filter=tier_outlines%>%filter(grepl('JMILL',LZ))%>%st_buffer(dist=500)%>%st_union()
  
  outline_filter<-tier_outlines_filter
  di_acreage_filter= di_acreage%>%group_by(Company)%>%filter(grepl(Company,i))
  
  
  if (nrow(inventory_filter)==0) {
    
    ggplot()+geom_blank()+theme_void()
    
  }else{
    
    bbox <- st_as_sfc(st_bbox(outline_filter))
    bbox_centroid<-st_centroid(bbox)
    bbox_centroid<-st_as_sf(bbox_centroid)
    bbox<-st_segments(bbox)
    bbox<-st_as_sf(bbox)
    max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
    
    basin_id='MB'
    expand_wide=if(basin_id=='DB'){
      1.2
    }else{
      1.5
    }
    expand_long=if(basin_id=='DB'){
      1.8
    }else{
      2.25
    }
    
    square=buffer_rectangle(bbox_centroid,x_length=max_dist*expand_wide,y_length=max_dist*expand_long)
    st_crs(square) <- "+init=epsg:32039"
    bbox<-st_as_sf(square)
    bbox<-st_bbox(bbox)
    
    ggplot(outline_filter)+
      #geom_sf(data=tier2_outlines_filter,fill='grey',alpha=.1)+
      geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
      geom_sf(data=acreagef,fill='gold',color=NA)+
      geom_sf(data=tier_outlines_filter,fill='grey',alpha=.2)+
      geom_sf(data=state_county,fill=NA)+
      geom_sf(data=inventory_filter,size=1.05,color='blue')+
      geom_sf_text(data=state_county,aes(label=NAME),size=3)+
      coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T,lims_method = 'box')+  
      theme_void()+scale_color_identity()+theme(legend.position = 'none', 
                                                plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
                                                  colour = "black",
                                                  size = 1
                                                )
      )
    
  }
}

jmill_map<-lapply(company_vector,jmill_map_fctn)

jmill_plot_fctn<-function(i){
  inventory_filter<-inventory2%>%filter(grepl("JMILL",reservoir) & i==Company) 
  
  inventory_filter$bins <- cut(inventory_filter$`CoS_$_WTI`, breaks=c(0,30,35,40,45,50,200), labels=c("x≤30","30<x≤35","35<x≤40","40<x≤45","45<x≤50","55+"))
  
  
  inventory_aggregate<- st_drop_geometry(inventory_filter)%>%group_by(bins)%>%summarise(Count=max(row_number()))
  
  
  
  
  ggplot(inventory_aggregate) + geom_bar(aes(x = bins, y = Count), stat = 'identity')+
    theme_minimal()+labs(x="")+
    geom_text(aes(x = bins, y = Count,label=Count),color="white",size=3,position=position_stack(vjust=0.5))+
    theme(
      text = element_text(size = 8),
      legend.position = "top",
      legend.key.size = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.justification="left",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-1,-1,-1,-1)
    ) +
    scale_x_discrete(guide = guide_axis(angle = 45))
}

jmill_plot<-lapply(company_vector,jmill_plot_fctn)

msby_map_fctn<- function(i){
  acreagef<- acreage%>%filter(i==Company)
  inventory_filter<-inventory2%>%filter(reservoir=="MSBY" & i==Company) 
  
  tier_outlines_filter=tier_outlines%>%filter(grepl('MSBY',LZ))%>%st_buffer(dist=500)%>%st_union()
  
  outline_filter<-tier_outlines_filter
  di_acreage_filter= di_acreage%>%group_by(Company)%>%filter(grepl(Company,i))
  
  
  if (nrow(inventory_filter)==0) {
    
    ggplot()+geom_blank()+theme_void()
    
  }else{
    
    bbox <- st_as_sfc(st_bbox(outline_filter))
    bbox_centroid<-st_centroid(bbox)
    bbox_centroid<-st_as_sf(bbox_centroid)
    bbox<-st_segments(bbox)
    bbox<-st_as_sf(bbox)
    max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
    
    basin_id='MB'
    expand_wide=if(basin_id=='DB'){
      1.2
    }else{
      1.5
    }
    expand_long=if(basin_id=='DB'){
      1.8
    }else{
      2.25
    }
    
    square=buffer_rectangle(bbox_centroid,x_length=max_dist*expand_wide,y_length=max_dist*expand_long)
    st_crs(square) <- "+init=epsg:32039"
    bbox<-st_as_sf(square)
    bbox<-st_bbox(bbox)
    
    ggplot(outline_filter)+
      #geom_sf(data=tier2_outlines_filter,fill='grey',alpha=.1)+
      geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
      geom_sf(data=acreagef,fill='gold',color=NA)+
      geom_sf(data=tier_outlines_filter,fill='grey',alpha=.2)+
      geom_sf(data=state_county,fill=NA)+
      geom_sf(data=inventory_filter,size=1.05,color='blue')+
      geom_sf_text(data=state_county,aes(label=NAME),size=3)+
      coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T,lims_method = 'box')+  
      theme_void()+scale_color_identity()+theme(legend.position = 'none', 
                                                plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
                                                  colour = "black",
                                                  size = 1
                                                )
      )
    
  }
}

msby_map<-lapply(company_vector,msby_map_fctn)

msby_plot_fctn<-function(i){
  inventory_filter<-inventory2%>%filter(grepl("MSBY",reservoir) & i==Company) 
  
  inventory_filter$bins <- cut(inventory_filter$`CoS_$_WTI`, breaks=c(0,30,35,40,45,50,200), labels=c("x≤30","30<x≤35","35<x≤40","40<x≤45","45<x≤50","55+"))
  
  
  inventory_aggregate<- st_drop_geometry(inventory_filter)%>%group_by(bins)%>%summarise(Count=max(row_number()))
  
  
  
  
  ggplot(inventory_aggregate) + geom_bar(aes(x = bins, y = Count), stat = 'identity')+
    theme_minimal()+labs(x="")+
    geom_text(aes(x = bins, y = Count,label=Count),color="white",size=3,position=position_stack(vjust=0.5))+
    theme(
      text = element_text(size = 8),
      legend.position = "top",
      legend.key.size = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.justification="left",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-1,-1,-1,-1)
    ) +
    scale_x_discrete(guide = guide_axis(angle = 45))
}

msby_plot<-lapply(company_vector,msby_plot_fctn)

lsby_map_fctn<- function(i){
  acreagef<- acreage%>%filter(i==Company)
  inventory_filter<-inventory2%>%filter(reservoir=="LSBY" & i==Company) 
  
  tier_outlines_filter=tier_outlines%>%filter(grepl('LSBY',LZ))%>%st_buffer(dist=500)%>%st_union()
  
  outline_filter<-tier_outlines_filter
  di_acreage_filter= di_acreage%>%group_by(Company)%>%filter(grepl(Company,i))
  
  
  if (nrow(inventory_filter)==0) {
    
    ggplot()+geom_blank()+theme_void()
    
  }else{
    
    bbox <- st_as_sfc(st_bbox(outline_filter))
    bbox_centroid<-st_centroid(bbox)
    bbox_centroid<-st_as_sf(bbox_centroid)
    bbox<-st_segments(bbox)
    bbox<-st_as_sf(bbox)
    max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
    
    basin_id='MB'
    expand_wide=if(basin_id=='DB'){
      1.2
    }else{
      1.5
    }
    expand_long=if(basin_id=='DB'){
      1.8
    }else{
      2.25
    }
    
    square=buffer_rectangle(bbox_centroid,x_length=max_dist*expand_wide,y_length=max_dist*expand_long)
    st_crs(square) <- "+init=epsg:32039"
    bbox<-st_as_sf(square)
    bbox<-st_bbox(bbox)
    
    ggplot(outline_filter)+
      #geom_sf(data=tier2_outlines_filter,fill='grey',alpha=.1)+
      geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
      geom_sf(data=acreagef,fill='gold',color=NA)+
      geom_sf(data=tier_outlines_filter,fill='grey',alpha=.2)+
      geom_sf(data=state_county,fill=NA)+
      geom_sf(data=inventory_filter,size=1.05,color='blue')+
      geom_sf_text(data=state_county,aes(label=NAME),size=3)+
      coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T,lims_method = 'box')+  
      theme_void()+scale_color_identity()+theme(legend.position = 'none', 
                                                plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
                                                  colour = "black",
                                                  size = 1
                                                )
      )
    
  }
}

lsby_map<-lapply(company_vector,lsby_map_fctn)


lsby_plot_fctn<-function(i){
  inventory_filter<-inventory2%>%filter(grepl("LSBY",reservoir) & i==Company) 
  
  inventory_filter$bins <- cut(inventory_filter$`CoS_$_WTI`, breaks=c(0,30,35,40,45,50,200), labels=c("x≤30","30<x≤35","35<x≤40","40<x≤45","45<x≤50","55+"))
  
  
  inventory_aggregate<- st_drop_geometry(inventory_filter)%>%group_by(bins)%>%summarise(Count=max(row_number()))
  
  
  
  
  ggplot(inventory_aggregate) + geom_bar(aes(x = bins, y = Count), stat = 'identity')+
    theme_minimal()+labs(x="")+
    geom_text(aes(x = bins, y = Count,label=Count),color="white",size=3,position=position_stack(vjust=0.5))+
    theme(
      text = element_text(size = 8),
      legend.position = "top",
      legend.key.size = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.justification="left",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-1,-1,-1,-1)
    ) +
    scale_x_discrete(guide = guide_axis(angle = 45))
}

lsby_plot<-lapply(company_vector,lsby_plot_fctn)



wfmp_a_map_fctn<- function(i){
  acreagef<- acreage%>%filter(i==Company)
  inventory_filter<-inventory2%>%filter(reservoir=="WFMP A" & i==Company) 
  
  tier_outlines_filter=tier_outlines%>%filter(grepl('WFMP A',LZ) & Basin=='MB')%>%st_buffer(dist=500)%>%st_union()
  
  outline_filter<-tier_outlines_filter
  di_acreage_filter= di_acreage%>%group_by(Company)%>%filter(grepl(Company,i))
  
  
  if (nrow(inventory_filter)==0) {
    
    ggplot()+geom_blank()+theme_void()
    
  }else{
    
    bbox <- st_as_sfc(st_bbox(outline_filter))
    bbox_centroid<-st_centroid(bbox)
    bbox_centroid<-st_as_sf(bbox_centroid)
    bbox<-st_segments(bbox)
    bbox<-st_as_sf(bbox)
    max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
    
    basin_id='MB'
    expand_wide=if(basin_id=='DB'){
      1.2
    }else{
      1.5
    }
    expand_long=if(basin_id=='DB'){
      1.8
    }else{
      2.25
    }
    
    square=buffer_rectangle(bbox_centroid,x_length=max_dist*expand_wide,y_length=max_dist*expand_long)
    st_crs(square) <- "+init=epsg:32039"
    bbox<-st_as_sf(square)
    bbox<-st_bbox(bbox)
    
    ggplot(outline_filter)+
      #geom_sf(data=tier2_outlines_filter,fill='grey',alpha=.1)+
      geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
      geom_sf(data=acreagef,fill='gold',color=NA)+
      geom_sf(data=tier_outlines_filter,fill='grey',alpha=.2)+
      geom_sf(data=state_county,fill=NA)+
      geom_sf(data=inventory_filter,size=1.05,color='blue')+
      geom_sf_text(data=state_county,aes(label=NAME),size=3)+
      coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T,lims_method = 'box')+  
      theme_void()+scale_color_identity()+theme(legend.position = 'none', 
                                                plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
                                                  colour = "black",
                                                  size = 1
                                                )
      )
    
  }
}

wfmp_a_map<-lapply(company_vector,wfmp_a_map_fctn)

wfmp_a_plot_fctn<-function(i){
  inventory_filter<-inventory2%>%filter(reservoir=='WFMP A' & i==Company) 
  
  inventory_filter$bins <- cut(inventory_filter$`CoS_$_WTI`, breaks=c(0,30,35,40,45,50,200), labels=c("x≤30","30<x≤35","35<x≤40","40<x≤45","45<x≤50","55+"))
  
  
  inventory_aggregate<- st_drop_geometry(inventory_filter)%>%group_by(bins)%>%summarise(Count=max(row_number()))
  
  
  
  
  ggplot(inventory_aggregate) + geom_bar(aes(x = bins, y = Count), stat = 'identity')+
    theme_minimal()+labs(x="")+
    geom_text(aes(x = bins, y = Count,label=Count),color="white",size=3,position=position_stack(vjust=0.5))+
    theme(
      text = element_text(size = 8),
      legend.position = "top",
      legend.key.size = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.justification="left",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-1,-1,-1,-1)
    ) +
    scale_x_discrete(guide = guide_axis(angle = 45))
}

wfmp_a_plot<-lapply(company_vector,wfmp_a_plot_fctn)

wfmp_b_map_fctn<- function(i){
  acreagef<- acreage%>%filter(i==Company)
  inventory_filter<-inventory2%>%filter(reservoir=="WFMP B" & i==Company & Basin=='MB') 
  
  tier_outlines_filter=tier_outlines%>%filter(grepl('WFMP B',LZ) & Basin=='MB')%>%st_buffer(dist=500)%>%st_union()
  
  outline_filter<-tier_outlines_filter
  di_acreage_filter= di_acreage%>%group_by(Company)%>%filter(grepl(Company,i))
  
  
  if (nrow(inventory_filter)==0) {
    
    ggplot()+geom_blank()+theme_void()
    
  }else{
    
    bbox <- st_as_sfc(st_bbox(outline_filter))
    bbox_centroid<-st_centroid(bbox)
    bbox_centroid<-st_as_sf(bbox_centroid)
    bbox<-st_segments(bbox)
    bbox<-st_as_sf(bbox)
    max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
    
    basin_id='MB'
    expand_wide=if(basin_id=='DB'){
      1.2
    }else{
      1.5
    }
    expand_long=if(basin_id=='DB'){
      1.8
    }else{
      2.25
    }
    
    square=buffer_rectangle(bbox_centroid,x_length=max_dist*expand_wide,y_length=max_dist*expand_long)
    st_crs(square) <- "+init=epsg:32039"
    bbox<-st_as_sf(square)
    bbox<-st_bbox(bbox)
    
    ggplot(outline_filter)+
      #geom_sf(data=tier2_outlines_filter,fill='grey',alpha=.1)+
      geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
      geom_sf(data=acreagef,fill='gold',color=NA)+
      geom_sf(data=tier_outlines_filter,fill='grey',alpha=.2)+
      geom_sf(data=state_county,fill=NA)+
      geom_sf(data=inventory_filter,size=1.05,color='blue')+
      geom_sf_text(data=state_county,aes(label=NAME),size=3)+
      coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T,lims_method = 'box')+  
      theme_void()+scale_color_identity()+theme(legend.position = 'none', 
                                                plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
                                                  colour = "black",
                                                  size = 1
                                                )
      )
    
  }
}

wfmp_b_map<-lapply(company_vector,wfmp_b_map_fctn)

wfmp_b_plot_fctn<-function(i){
  inventory_filter<-inventory2%>%filter(reservoir=='WFMP B' & i==Company & Basin=='MB') 
  
  inventory_filter$bins <- cut(inventory_filter$`CoS_$_WTI`, breaks=c(0,30,35,40,45,50,200), labels=c("x≤30","30<x≤35","35<x≤40","40<x≤45","45<x≤50","55+"))
  
  
  inventory_aggregate<- st_drop_geometry(inventory_filter)%>%group_by(bins)%>%summarise(Count=max(row_number()))
  
  
  
  
  ggplot(inventory_aggregate) + geom_bar(aes(x = bins, y = Count), stat = 'identity')+
    theme_minimal()+labs(x="")+
    geom_text(aes(x = bins, y = Count,label=Count),color="white",size=3,position=position_stack(vjust=0.5))+
    theme(
      text = element_text(size = 8),
      legend.position = "top",
      legend.key.size = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.justification="left",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-1,-1,-1,-1)
    ) +
    scale_x_discrete(guide = guide_axis(angle = 45))
}

wfmp_b_plot<-lapply(company_vector,wfmp_b_plot_fctn)



wfmp_c_mb_map_fctn<- function(i){
  acreagef<- acreage%>%filter(i==Company)
  inventory_filter<-inventory2%>%filter(reservoir=="WFMP C" & i==Company & Basin=='MB') 
  
  tier_outlines_filter=tier_outlines%>%filter(grepl('WFMP C',LZ) & Basin=='MB')%>%st_buffer(dist=500)%>%st_union()
  
  outline_filter<-tier_outlines_filter
  di_acreage_filter= di_acreage%>%group_by(Company)%>%filter(grepl(Company,i))
  
  
  if (nrow(inventory_filter)==0) {
    
    ggplot()+geom_blank()+theme_void()
    
  }else{
    
    bbox <- st_as_sfc(st_bbox(outline_filter))
    bbox_centroid<-st_centroid(bbox)
    bbox_centroid<-st_as_sf(bbox_centroid)
    bbox<-st_segments(bbox)
    bbox<-st_as_sf(bbox)
    max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
    
    basin_id='MB'
    expand_wide=if(basin_id=='DB'){
      1.2
    }else{
      1.5
    }
    expand_long=if(basin_id=='DB'){
      1.8
    }else{
      2.25
    }
    
    square=buffer_rectangle(bbox_centroid,x_length=max_dist*expand_wide,y_length=max_dist*expand_long)
    st_crs(square) <- "+init=epsg:32039"
    bbox<-st_as_sf(square)
    bbox<-st_bbox(bbox)
    
    ggplot(outline_filter)+
      #geom_sf(data=tier2_outlines_filter,fill='grey',alpha=.1)+
      geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
      geom_sf(data=acreagef,fill='gold',color=NA)+
      geom_sf(data=tier_outlines_filter,fill='grey',alpha=.2)+
      geom_sf(data=state_county,fill=NA)+
      geom_sf(data=inventory_filter,size=1.05,color='blue')+
      geom_sf_text(data=state_county,aes(label=NAME),size=3)+
      coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T,lims_method = 'box')+  
      theme_void()+scale_color_identity()+theme(legend.position = 'none', 
                                                plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
                                                  colour = "black",
                                                  size = 1
                                                )
      )
    
  }
}

wfmp_c_mb_map<-lapply(company_vector,wfmp_c_mb_map_fctn)

wfmp_c_mb_plot_fctn<-function(i){
  inventory_filter<-inventory2%>%filter(reservoir=='WFMP C' & i==Company & Basin=='MB') 
  
  inventory_filter$bins <- cut(inventory_filter$`CoS_$_WTI`, breaks=c(0,30,35,40,45,50,200), labels=c("x≤30","30<x≤35","35<x≤40","40<x≤45","45<x≤50","55+"))
  
  
  inventory_aggregate<- st_drop_geometry(inventory_filter)%>%group_by(bins)%>%summarise(Count=max(row_number()))
  
  
  
  
  ggplot(inventory_aggregate) + geom_bar(aes(x = bins, y = Count), stat = 'identity')+
    theme_minimal()+labs(x="")+
    geom_text(aes(x = bins, y = Count,label=Count),color="white",size=3,position=position_stack(vjust=0.5))+
    theme(
      text = element_text(size = 8),
      legend.position = "top",
      legend.key.size = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.justification="left",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-1,-1,-1,-1)
    ) +
    scale_x_discrete(guide = guide_axis(angle = 45))
}

wfmp_c_mb_plot<-lapply(company_vector,wfmp_c_plot_fctn)






wfmp_d_map_fctn<- function(i){
  acreagef<- acreage%>%filter(i==Company)
  inventory_filter<-inventory2%>%filter(reservoir=="WFMP D" & i==Company & Basin=='MB') 
  
  tier_outlines_filter=tier_outlines%>%filter(grepl('WFMP D',LZ) & Basin=='MB')%>%st_buffer(dist=500)%>%st_union()
  
  outline_filter<-tier_outlines_filter
  di_acreage_filter= di_acreage%>%group_by(Company)%>%filter(grepl(Company,i))
  
  
  if (nrow(inventory_filter)==0) {
    
    ggplot()+geom_blank()+theme_void()
    
  }else{
    
    bbox <- st_as_sfc(st_bbox(outline_filter))
    bbox_centroid<-st_centroid(bbox)
    bbox_centroid<-st_as_sf(bbox_centroid)
    bbox<-st_segments(bbox)
    bbox<-st_as_sf(bbox)
    max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
    
    basin_id='MB'
    expand_wide=if(basin_id=='DB'){
      1.2
    }else{
      1.5
    }
    expand_long=if(basin_id=='DB'){
      1.8
    }else{
      2.25
    }
    
    square=buffer_rectangle(bbox_centroid,x_length=max_dist*expand_wide,y_length=max_dist*expand_long)
    st_crs(square) <- "+init=epsg:32039"
    bbox<-st_as_sf(square)
    bbox<-st_bbox(bbox)
    
    ggplot(outline_filter)+
      #geom_sf(data=tier2_outlines_filter,fill='grey',alpha=.1)+
      geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
      geom_sf(data=acreagef,fill='gold',color=NA)+
      geom_sf(data=tier_outlines_filter,fill='grey',alpha=.2)+
      geom_sf(data=state_county,fill=NA)+
      geom_sf(data=inventory_filter,size=1.05,color='blue')+
      geom_sf_text(data=state_county,aes(label=NAME),size=3)+
      coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T,lims_method = 'box')+  
      theme_void()+scale_color_identity()+theme(legend.position = 'none', 
                                                plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
                                                  colour = "black",
                                                  size = 1
                                                )
      )
    
  }
}

wfmp_d_map<-lapply(company_vector,wfmp_d_map_fctn)

wfmp_d_plot_fctn<-function(i){
  inventory_filter<-inventory2%>%filter(reservoir=='WFMP D' & i==Company & Basin=='MB') 
  
  inventory_filter$bins <- cut(inventory_filter$`CoS_$_WTI`, breaks=c(0,30,35,40,45,50,200), labels=c("x≤30","30<x≤35","35<x≤40","40<x≤45","45<x≤50","55+"))
  
  
  inventory_aggregate<- st_drop_geometry(inventory_filter)%>%group_by(bins)%>%summarise(Count=max(row_number()))
  
  
  
  
  ggplot(inventory_aggregate) + geom_bar(aes(x = bins, y = Count), stat = 'identity')+
    theme_minimal()+labs(x="")+
    geom_text(aes(x = bins, y = Count,label=Count),color="white",size=3,position=position_stack(vjust=0.5))+
    theme(
      text = element_text(size = 8),
      legend.position = "top",
      legend.key.size = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.justification="left",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-1,-1,-1,-1)
    ) +
    scale_x_discrete(guide = guide_axis(angle = 45))
}


wfmp_d_plot<-lapply(company_vector,wfmp_d_plot_fctn)




brnt_map_fctn<- function(i){
  acreagef<- acreage%>%filter(i==Company)
  inventory_filter<-inventory2%>%filter(reservoir=="BRNT" & i==Company) 
  
  tier_outlines_filter=brnt%>%st_buffer(dist=50)%>%st_union()
  
  outline_filter<-tier_outlines_filter
  di_acreage_filter= di_acreage%>%group_by(Company)%>%filter(grepl(Company,i))
  
  
  if (nrow(inventory_filter)==0) {
    
    ggplot()+geom_blank()+theme_void()
    
  }else{
    
    bbox <- st_as_sfc(st_bbox(outline_filter))
    bbox_centroid<-st_centroid(bbox)
    bbox_centroid<-st_as_sf(bbox_centroid)
    bbox<-st_segments(bbox)
    bbox<-st_as_sf(bbox)
    max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
    
    basin_id='MB'
    expand_wide=if(basin_id=='DB'){
      1.2
    }else{
      1.5*1.1
    }
    expand_long=if(basin_id=='DB'){
      1.8
    }else{
      2.25*1.1
    }
    
    square=buffer_rectangle(bbox_centroid,x_length=max_dist*expand_wide,y_length=max_dist*expand_long)
    st_crs(square) <- "+init=epsg:32039"
    bbox<-st_as_sf(square)
    bbox<-st_bbox(bbox)
    
    ggplot(outline_filter)+
      #geom_sf(data=tier2_outlines_filter,fill='grey',alpha=.1)+
      geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
      geom_sf(data=acreagef,fill='gold',color=NA)+
      geom_sf(data=tier_outlines_filter,fill='grey',alpha=.2)+
      geom_sf(data=state_county,fill=NA)+
      geom_sf(data=inventory_filter,size=1.05,color='blue')+
      geom_sf_text(data=state_county,aes(label=NAME),size=3)+
      coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T,lims_method = 'box')+  
      theme_void()+scale_color_identity()+theme(legend.position = 'none', 
                                                plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
                                                  colour = "black",
                                                  size = 1
                                                )
      )
    
  }
}

brnt_map<-lapply(company_vector,brnt_map_fctn)

brnt_plot_fctn<-function(i){
  inventory_filter<-inventory2%>%filter(reservoir=='BRNT' & i==Company & Basin=='MB') 
  
  inventory_filter$bins <- cut(inventory_filter$`CoS_$_WTI`, breaks=c(0,30,35,40,45,50,200), labels=c("x≤30","30<x≤35","35<x≤40","40<x≤45","45<x≤50","55+"))
  
  
  inventory_aggregate<- st_drop_geometry(inventory_filter)%>%group_by(bins)%>%summarise(Count=max(row_number()))
  
  
  
  
  ggplot(inventory_aggregate) + geom_bar(aes(x = bins, y = Count), stat = 'identity')+
    theme_minimal()+labs(x="")+
    geom_text(aes(x = bins, y = Count,label=Count),color="white",size=3,position=position_stack(vjust=0.5))+
    theme(
      text = element_text(size = 8),
      legend.position = "top",
      legend.key.size = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.justification="left",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-1,-1,-1,-1)
    ) +
    scale_x_discrete(guide = guide_axis(angle = 45))
}


brnt_plot<-lapply(company_vector,brnt_plot_fctn)

####CoS Summary----

cos_fctn<-function(i){
  inventory_filter<-inventory2%>%filter(i==Company) 
  
  inventory_filter<-inventory_filter%>%mutate(reservoir=ifelse(reservoir %in% c("WFMP A SH","WFMP A SD","BS3S"),"WFMP A/BS3 Tank",reservoir))
  
  
  inventory_filter$bins <- cut(inventory_filter$`CoS_$_WTI`, breaks=c(0,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120))
  inventory_filter<-inventory_filter%>%filter(`CoS_$_WTI`<55.0001)
  
  #inventory_filter$bins<-fct_reorder(inventory_filter$bins,`CoS_$_WTI`)
  
  inventory_aggregate<- st_drop_geometry(inventory_filter)%>%group_by(reservoir,bins)%>%summarise(Count=max(row_number()))
  inventory_total<- st_drop_geometry(inventory_filter)%>%group_by(reservoir)%>%summarise(Count=max(row_number()))
  
  cols <- c("(0,35]" = "darkgreen", "(35,40]" = "#32a852", "(40,45]" = "darkorange", "(45,50]" = "#d61515","(50,55]" = "darkred")
  

  
  ggplot(inventory_aggregate) + geom_bar(aes(x = reservoir, y = Count,fill=fct_rev(bins)), stat = 'identity')+
    theme_minimal()+labs(x="")+
    geom_text(data=inventory_total,aes(x = reservoir, y = Count,label=comma(Count)),color="black",size=4,vjust=-.5)+
    geom_text(aes(x = reservoir, y = Count,label=Count,fill=bins),color="white",size=3,position=position_stack(vjust=.65,reverse=T))+
    theme(
      text = element_text(size = 12),
      legend.position = "top",
      legend.key.size = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.justification="left",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-1,-1,-1,-1)
    ) +
    scale_y_continuous(labels = scales::comma)+
    scale_x_discrete(guide = guide_axis(angle = 0))+
    guides(fill = guide_legend(reverse = T))+
    scale_fill_manual(values=cols)

  
  
  }

cos_plot<-lapply(company_vectorO,cos_fctn)

#i=company_vectorO[1]
##Cake Plot
cake_fctn<-function(i){
  inventory_filter<-inventory2%>%filter(i==Company) 
  
  inventory_filter<-inventory_filter%>%mutate(reservoir=ifelse(reservoir %in% c("WFMP A SH","WFMP A SD","BS3S"),"WFMP A/BS3 Tank",reservoir))
  

  inventory_filter$bins <- cut(inventory_filter$`CoS_$_WTI`, breaks=c(0,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120))
  inventory_filter<-inventory_filter%>%filter(`CoS_$_WTI`<55.0001)
  
  #inventory_filter$bins<-fct_rev(inventory_filter$bins)
  inventory_total<- st_drop_geometry(inventory_filter)%>%ungroup()%>%arrange(`CoS_$_WTI`)%>%
    group_by(bins)%>%
    summarise(Count=max(row_number()))%>%ungroup()%>%mutate(CumCount=cumsum(Count))
  
  #inventory_total$`CoS_$_WTI`<-fct_reorder(as.factor(inventory_total$`CoS_$_WTI`),inventory_total$Count)
  
  inventory_label<-inventory_total%>%group_by(bins)%>%filter(CumCount==max(CumCount))%>%
    ungroup()%>%mutate(CumCount=((CumCount-lag(CumCount,default = 0))/2)+lag(CumCount,default = 0))
  
  inventory_total$right <- cumsum(inventory_total$Count) + 0*c(0:(nrow(inventory_total)-1))
  inventory_total$left <- inventory_total$right - inventory_total$Count 
  
  ggplot(inventory_total) + 
    geom_rect(aes(xmin = left, xmax = right,ymin=0, ymax = Count, fill = bins))+ 
    theme_minimal()+labs(x="Cumulative Inventory Count")+
    geom_text(data=inventory_label,aes(x = CumCount, y = Count/2,label=bins),color="black",size=3)+
    geom_text(data=inventory_label,aes(x = CumCount, y = Count,label=comma(Count)),color="black",size=3,vjust=-.5)+
    theme(
      text = element_text(size = 10),
      legend.position = "top",
      legend.key.size = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.justification="left",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-1,-1,-1,-1)
    ) +
    scale_y_continuous(labels = scales::comma)+
    #scale_x_discrete(guide = guide_axis(angle = 0))+
    guides(fill = guide_legend(reverse = F))+
    scale_fill_manual(values=c('darkgreen','#32a852',"darkorange",'#d61515','darkred','lightgrey','grey'))+
    ylim(0,max(inventory_total$Count)+20)
}


cake_plot<-lapply(company_vectorO,cake_fctn)


#i='APA CORP'

###Operated Percentage
operated_nonop_function<-function(i){
  inventoryf<-inventory%>%filter(i==Company)
  inventory_op_perc<-st_drop_geometry(inventoryf)%>%
    group_by(Status2)%>%summarise(Count=max(row_number()))%>%ungroup()%>%
    mutate(CumCount=cumsum(Count),Percentage=round(Count/max(CumCount)*100,0))
  
  
  inventory_op_perc$hsize=2
  
  inventory_op_perc$Status2<-fct_relevel(as_factor(inventory_op_perc$Status2),"Operated","NonOperated","Unknown")
  
  ggplot(inventory_op_perc, aes(x = hsize, y = Count, fill = Status2)) +
    geom_col() +
    geom_text(aes(label = paste(comma(Count)," (",percent(Percentage/100),")",sep="")),
              position = position_stack(vjust = 0.5),fontface='bold',size=3.5) +
    
    coord_polar(theta = "y") +
    xlim(c(0.2, unique(inventory_op_perc$hsize) + 0.5))+theme_void()+
    theme(legend.position='bottom',legend.text = element_text(size=10),legend.key.size = unit(0.5, "cm"),legend.title = element_blank())+
    guides(fill = guide_legend(reverse=F))+
    scale_fill_manual(values=c('turquoise','#fac132','#fa1b8e','grey'))
  
}

operated_nonop_plot<-lapply(company_vectorO,operated_nonop_function)

inventory_join_cos<-st_drop_geometry(inventory)%>%filter(Lat_TC==100)%>%group_by(reservoir,Basin)%>%
  distinct(reservoir,Basin,TC_Area,`CoS_$_WTI`)
  
####Tier Acreage

cop<- di_acreage%>%group_by(Company)%>%filter(grepl('CONOCOPHILLIPS',Company))


db_tier_acg_fctn<-function(i){
  db_tier<-tier_outlinesO%>%filter(Basin=='DB',zone==i)
  db_tier<-db_tier%>%left_join(.,
single_line_cos %>%
  select(Lateral.Length.ft,basin,Zone_Alias,TC_Area,`CoS.$.WTI`,Type.Curve)%>%
  filter(Lateral.Length.ft==5000),by=c("Basin"="basin","zone"="Zone_Alias","TC_Area"="TC_Area"))
  
  db_tier<-db_tier%>%mutate(`CoS.$.WTI`=ifelse(is.na(`CoS.$.WTI`),55,`CoS.$.WTI`))
  
  max_num= 10*round(max(db_tier$`CoS.$.WTI`)/10, 0)+5
  
  db_tier$bins <- cut(db_tier$`CoS.$.WTI`, breaks=c(0,40,45,50,55,max_num))
  outline_filter<-st_union(st_buffer(db_tier,dist=500))
  
  cop_int<-st_intersection(st_buffer(st_transform(cop,st_crs(outline_filter)),dist=0),outline_filter)
  cop_int_union<-st_union(cop_int)
  
  ######
  bbox <- st_as_sfc(st_bbox(cop_int_union))
  bbox_centroid<-st_centroid(bbox)
  bbox_centroid<-st_as_sf(bbox_centroid)
  bbox<-st_segments(bbox)
  bbox<-st_as_sf(bbox)
  max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
  
  basin_id='DB'
  expand_wide=if(basin_id=='DB'){
    1.2*1.8
  }else{
    1.5
  }
  expand_long=if(basin_id=='DB'){
    1.8*1.5
  }else{
    2.25
  }
  
  square=buffer_rectangle(bbox_centroid,x_length=max_dist*expand_wide,y_length=max_dist*expand_long)
  st_crs(square) <- "+init=epsg:32039"
  bbox<-st_as_sf(square)
  bbox<-st_bbox(bbox)
  
  
  
  Colors<-c('darkgreen','#32a852',"darkorange",'#d61515','lightgrey')
  names(Colors) <- levels(db_tier$bins)
  colScale <- scale_fill_manual(name = "bins",values = Colors)
  

  
  ggplot(db_tier)+geom_sf(aes(fill=bins),color=NA)+
    scale_fill_manual(values = Colors,name="")+
    geom_sf(data=state_county,fill=NA)+
    geom_sf_text(data=state_county,aes(label=NAME),size=3)+
    guides(fill = guide_legend(override.aes = list(size = 4),nrow=2)) +
    geom_sf(data=cop_int,fill='yellow',alpha=.45,color=NA)+
    coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T,lims_method = 'box')+  
    theme_void()+scale_color_identity()+
    theme(legend.position = 'bottom', 
plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
colour = "black",
size = 1
                                              )
    )
  
 }

#inventoryOO<-st_drop_geometry(inventory)%>%ungroup()%>%distinct(reservoir)
inventoryO<-st_drop_geometry(inventory)%>%
  filter(Basin=='DB')%>%
  ungroup()%>%distinct(reservoirO)%>%arrange(reservoirO)%>%filter(!(reservoirO %in% c("BS3S","WFMP A SD")))


x=st_drop_geometry(inventory)%>%
  filter(Basin=='DB')%>%
  ungroup()%>%distinct(reservoirO)%>%arrange(reservoirO)


db_tier_acg_plot<-lapply(x$reservoirO,db_tier_acg_fctn)



#i='WFMP B'

cos_40_fctn<-function(i){
  db_tier<-tier_outlinesO%>%filter(Basin=='DB',zone==i)
  db_tier<-db_tier%>%left_join(.,
                               single_line_cos %>%
                                 select(Lateral.Length.ft,basin,Zone_Alias,TC_Area,`CoS.$.WTI`,Type.Curve)%>%
                                 filter(Lateral.Length.ft==5000),by=c("Basin"="basin","zone"="Zone_Alias","TC_Area"="TC_Area"))
  
  db_tier<-db_tier%>%mutate(`CoS.$.WTI`=ifelse(is.na(`CoS.$.WTI`),100,`CoS.$.WTI`))
  max_num= 10*round(max(db_tier$`CoS.$.WTI`)/10, 0)
  min_num= 10*round(min(db_tier$`CoS.$.WTI`)/10, 0)+5
  db_tier$bins <- cut(db_tier$`CoS.$.WTI`, breaks=c(0,35,40,45,50,55,max_num))
  
  if(min_num>40.0001){
    min_num<-min_num
  }else{
    min_num<-40.001
  }
  
  db_tier<-db_tier%>%filter(`CoS.$.WTI`<min_num)
  
  
  
  
  db_tier<-db_tier%>%select(bins,TC_Area)
  acreage_int<-st_intersection(st_buffer(acreage%>%filter(Company !='DEVON ENERGY'),dist=0),st_buffer(db_tier,dist=0))
  acreage_int_acg<-acreage_int%>%select(Company,Status,TC_Area)%>%group_by(Company,Status,TC_Area)%>%
    mutate(gross_acg=as.numeric(st_area(geometry))/43560)%>%st_drop_geometry(.)%>%ungroup()%>%
    group_by(Company)%>%mutate(comp_gross=sum(gross_acg))%>%
    arrange(desc(comp_gross))
  acreage_int_acg<-acreage_int_acg%>%ungroup()%>%mutate(Rank=dense_rank(desc(comp_gross)))
  acreage_int_acg<-acreage_int_acg%>%filter(Rank<11)
  
  acreage_int<-acreage_int%>%filter(Company %in% c(acreage_int_acg$Company))
  acreage_int_acg<-unique(acreage_int_acg)
  acreage_intf<-st_drop_geometry(acreage_int)%>%
    ungroup()%>%group_by(Company,Status,TC_Area)%>%distinct(Company,Status,TC_Area,.keep_all=T)%>%
    left_join(.,acreage_int_acg)
  acreage_intf<-  acreage_intf%>%ungroup()%>%
    group_by(Company,Status,bins)%>%summarise(gross_acg)
  
  acreage_intf<-acreage_intf%>%mutate(Company=word(Company,start=1,end=1))
  
  acreage_intf$Status<-fct_relevel(acreage_intf$Status,c("Operated","NonOperated","Unknown"))

  acreage_intf<-acreage_intf%>%ungroup()%>%group_by(Company)%>%mutate(CompGross=sum(gross_acg))
  
  if(nrow(acreage_intf)<1){
    print(paste(i,'ign'))
    ggplot()
    
  }else{
  
  if(nrow(data.frame(unique(acreage_intf$Status)))==3){
    
  Colors<-c('turquoise','#fac132','#fa1b8e')
  names(Colors) <- levels(acreage_intf$Status)
  colScale <- scale_fill_manual(name = "Status",values = Colors)
  }else{
    Colors<-c('turquoise','#fac132')
    names(Colors) <- levels(acreage_intf$Status)
    colScale <- scale_fill_manual(name = "Status",values = Colors)
  }

    print(i)  
  ggplot(acreage_intf)+geom_bar(aes(fct_reorder(Company,desc(CompGross)),gross_acg,fill=fct_rev(Status)),stat='identity')+
    #geom_text(aes(fct_reorder(Company,desc(CompGross)),gross_acg,fill=fct_rev(Status),label=gross_acg/1000),color="black",size=3,position=position_stack(vjust=.65,reverse=T))+
    theme_minimal()+
    theme(
      text = element_text(size = 8),
      legend.position = "top",
      legend.key.size = unit(0.3, "cm"),
      legend.title = element_blank(),
      legend.justification="left",
      legend.margin=margin(0,0,0,-35),
      legend.box.margin=margin(-1,-1,-1,-1),
      axis.text.x = element_text(size = 6)
    ) +
    scale_y_continuous(labels = scales::comma)+
    scale_x_discrete(guide = guide_axis(angle = 65))+
    labs(y='Gross Acres',x="", caption=paste('CoS Max = $',min_num,sep=""))+
    guides(fill = guide_legend(reverse=F))+
    scale_fill_manual(values = Colors)
  
  
}

  }



cos_40_plot<-lapply(x$reservoirO,cos_40_fctn)




#i='WFMP A SH'
###cos map with operator sticks----
cos_40_sticks_fctn<-function(i){
  if(i=='WFMP A SH'){
    db_tier<-tier_outlinesO%>%filter(Basin=='DB',zone %in% c("WFMP A SH",'WFMP A SD','BS3S'))
    inventory_f<-inventory%>%filter(Basin=='DB',reservoirO %in% c("WFMP A SH",'WFMP A SD','BS3S'))
    }else{
  db_tier<-tier_outlinesO%>%filter(Basin=='DB',zone==i)
  inventory_f<-inventory%>%filter(Basin=='DB',reservoirO==i)
  }
  db_tier<-db_tier%>%st_buffer(dist=705)%>%st_union()
  

 inventory_f<-inventory_f%>%mutate(`CoS_$_WTI`=ifelse(is.na(`CoS_$_WTI`),100,`CoS_$_WTI`))
 inventory_f<-inventory_f%>%filter(`CoS_$_WTI`<55.001)
  
  max_num= 10*round(max(inventory_f$`CoS_$_WTI`)/10, 0)
  
  inventory_f$bins <- cut(inventory_f$`CoS_$_WTI`, breaks=c(0,35,40,45,50,55))
  
  cop_int<-st_intersection(st_buffer(st_transform(cop,st_crs(db_tier)),dist=0),db_tier)
  cop_int_union<-st_union(cop_int)
  
  ######
  bbox <- st_as_sfc(st_bbox(cop_int_union))
  bbox_centroid<-st_centroid(bbox)
  bbox_centroid<-st_as_sf(bbox_centroid)
  bbox<-st_segments(bbox)
  bbox<-st_as_sf(bbox)
  
  
  max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
  
  basin_id='DB'
  expand_wide=if(basin_id=='DB'){
    1.2*1.8
  }else{
    1.5
  }
  expand_long=if(basin_id=='DB'){
    1.8*1.5
  }else{
    2.25
  }
  
  square=buffer_square(bbox_centroid,max_dist*2)
  st_crs(square) <- "+init=epsg:32039"
  bbox<-st_as_sf(square)
  bbox<-st_bbox(bbox)
  
  
  
  Colors<-c('darkgreen','#32a852',"darkorange",'#d61515','darkred','lightgrey')
  names(Colors) <- levels(inventory_f$bins)
  colScale <- scale_fill_manual(name = "bins",values = Colors)
  
  
  ggplot(inventory_f)+
    geom_sf(data=cop_int,fill='yellow',alpha=.45,color=NA)+
    geom_sf(data=db_tier,fill='grey',alpha=.2)+
    geom_sf(aes(color=bins))+
    scale_color_manual(values = Colors,name="")+
    geom_sf(data=state_county,fill=NA)+
    geom_sf_text(data=state_county,aes(label=NAME),size=3)+
    guides(color = guide_legend(override.aes = list(size = 5,linewidth=3))) +
    coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = T,lims_method = 'box')+  
    theme_void()+theme(legend.position = 'bottom', 
  plot.margin = margin(0., 0., 0., 0., "cm"), plot.background = element_rect(
  colour = "black",
  size = 1
                                              )
    )
}


cos_40_sticks_plot<-lapply(inventoryO$reservoirO,cos_40_sticks_fctn)



##cos stick count
cos_zone_op_chart_fctn<-function(i){
  if(i=='WFMP A SH'){
    inventory_f<-inventory%>%filter(Basin=='DB',reservoirO %in% c("WFMP A SH",'WFMP A SD','BS3S'))
    inventory_f$reservoirO<-'WFMP A/BS3S Tank'
  }else{
  inventory_f<-inventory%>%filter(Basin=='DB',reservoirO==i)
  }

  if(nrow(inventory_f)==0){
    print('skip')
  }else{
  
  inventory_f<-inventory_f%>%mutate(`CoS_$_WTI`=ifelse(is.na(`CoS_$_WTI`),55,`CoS_$_WTI`))
  max_num= 10*round(max(inventory_f$`CoS_$_WTI`)/10, 0)
  min_num= 10*round(min(inventory_f$`CoS_$_WTI`)/10, 0)+5
  #inventory_f$bins <- cut(inventory_f$`CoS_$_WTI`, breaks=c(0,40,45,55,max_num))
  inventory_f$bins <- cut(inventory_f$`CoS_$_WTI`, breaks=c(0,40,45,55,max_num), 
                         labels=c("CoS:x≤$40","CoS:$40<x≤$45","CoS:$45<x≤$55","CoS:$55+"))
  
  
  if(min_num>40.0001){
    min_num<-min_num
  }else{
    min_num<-40.001
  }
  


  if(nrow(inventory_f)<1){
    print(paste(i,'ign'))
    ggplot()
    
  }else{
    
    if(nrow(data.frame(unique(inventory_f$Status2)))==3){
      
      Colors<-c('turquoise','#fac132','#fa1b8e')
      names(Colors) <- levels(inventory_f$Status2)
      colScale <- scale_fill_manual(name = "Status",values = Colors)
    }else{
      Colors<-c('turquoise','#fac132')
      names(Colors) <- levels(inventory_f$Status2)
      colScale <- scale_fill_manual(name = "Status",values = Colors)
    }
    
    print(i)  
    
    
    inventory_f<-inventory_f%>%mutate(Company=word(Company,start=1,end=1))
    
    inventory_f_sum<-st_drop_geometry(inventory_f)%>%
      filter(`CoS_$_WTI`<55)%>%
      group_by(Company,Status2,bins)%>%
      summarise(Count=max(row_number()))%>%ungroup()%>%group_by(Company,bins)%>%
      mutate(MaxCount=sum(Count))%>%
      ungroup()%>%group_by(bins)%>%arrange(bins,desc(MaxCount))%>%mutate(Rank=row_number())%>%
      ungroup()%>%group_by(bins)%>%mutate(RankFilter=dense_rank(desc(MaxCount)))
    
    inventory_f_sum<-inventory_f_sum%>%filter(RankFilter<11)
    
    inventory_f_sum$Status2<-fct_relevel(inventory_f_sum$Status2,c("Operated","NonOperated","Unknown"))
  
    
    ggplot(inventory_f_sum,aes(reorder_within(Company,-Rank,bins),Count,fill=Status2,label=Count))+
      geom_col()+
      geom_text(stat = "stratum", aes(stratum = Status2),size=3)+
      facet_wrap(~bins,ncol=1,scales='free_y')+coord_flip()+
      theme_minimal()+
      theme(panel.spacing.y = unit(-0.5, "lines"),
        text = element_text(size = 10),
        strip.text = element_text(size = 9),
        legend.position = "top",
        legend.key.size = unit(0.3, "cm"),
        legend.title = element_blank(),
        legend.justification="left",
        legend.margin=margin(0,0,0,-35),
        legend.box.margin=margin(-1,-1,-1,-1),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)
      ) +
      scale_y_continuous(labels = scales::comma)+
      #scale_x_discrete(guide = guide_axis(angle = 65))+
      labs(y='Count',x="", caption=paste('CoS Max = $',55,sep=""))+
      guides(fill = guide_legend(reverse=F))+
      scale_x_reordered()+
      scale_fill_manual(values = Colors)
    
    
  }
  
}
}

#inventoryO$reservr
cos_zone_op_chart_fctn_plot<-lapply(inventoryO$reservoirO,cos_zone_op_chart_fctn)



##cos stick count summary
cos_summary_total_fctn<-function(i){
  inventory_f<-inventory%>%filter(Basin==i)
  
  if(nrow(inventory_f)==0){
    print('skip')
  }else{
    
    inventory_f<-inventory_f%>%mutate(`CoS_$_WTI`=ifelse(is.na(`CoS_$_WTI`),55,`CoS_$_WTI`))
    max_num= 10*round(max(inventory_f$`CoS_$_WTI`)/10, 0)
    min_num= 10*round(min(inventory_f$`CoS_$_WTI`)/10, 0)+5
    #inventory_f$bins <- cut(inventory_f$`CoS_$_WTI`, breaks=c(0,40,45,55,max_num))
    inventory_f$bins <- cut(inventory_f$`CoS_$_WTI`, breaks=c(0,40,45,55,max_num), 
                            labels=c("CoS:x≤$40","CoS:$40<x≤$45","CoS:$45<x≤$55","CoS:$55+"))
    
    
    if(min_num>40.0001){
      min_num<-min_num
    }else{
      min_num<-40.001
    }
    
    
    
    if(nrow(inventory_f)<1){
      print(paste(i,'ign'))
      ggplot()
      
    }else{
      
      if(nrow(data.frame(unique(inventory_f$Status2)))==3){
        
        Colors<-c('turquoise','#fac132','#fa1b8e')
        names(Colors) <- levels(inventory_f$Status2)
        colScale <- scale_fill_manual(name = "Status",values = Colors)
      }else{
        Colors<-c('turquoise','#fac132')
        names(Colors) <- levels(inventory_f$Status2)
        colScale <- scale_fill_manual(name = "Status",values = Colors)
      }
      
      print(i)  
      
      
      inventory_f<-inventory_f%>%mutate(Company=word(Company,start=1,end=1))
      
      inventory_f_sum<-st_drop_geometry(inventory_f)%>%
        filter(`CoS_$_WTI`<55)%>%
        group_by(Company,Status2)%>%
        summarise(Count=max(row_number()))%>%ungroup()%>%group_by(Company)%>%
        mutate(MaxCount=sum(Count))%>%
        ungroup()%>%arrange(desc(MaxCount))%>%mutate(Rank=row_number())%>%
        ungroup()%>%mutate(RankFilter=dense_rank(desc(MaxCount)))
      
      inventory_f_sum<-inventory_f_sum%>%filter(RankFilter<25)
      
      inventory_f_sum$Status2<-fct_relevel(inventory_f_sum$Status2,c("Operated","NonOperated","Unknown"))
      
      
      p2<-ggplot(inventory_f_sum%>%filter(RankFilter>12),aes(fct_reorder(Company,Rank),Count,fill=Status2,label=Count))+
        geom_col()+
        geom_text(stat = "stratum", aes(stratum = Status2),size=3)+
        #geom_text(aes(fct_reorder(Company,Rank),MaxCount,label=MaxCount),size=2.9)+
        #facet_wrap(~bins,ncol=1,scales='free_y')+
        coord_flip()+
        theme_minimal()+
        theme(plot.background = element_rect('white',color='white'),
          panel.spacing.y = unit(-0.5, "lines"),
              text = element_text(size = 10),
              strip.text = element_text(size = 9),
              legend.position = "none",
              legend.key.size = unit(0.3, "cm"),
              legend.title = element_blank(),
              legend.justification="left",
              legend.margin=margin(0,0,0,-35),
              legend.box.margin=margin(-1,-1,-1,-1),
              axis.text.x = element_text(size = 8),
              axis.text.y = element_text(size = 8)
        ) +
        scale_y_continuous(labels = scales::comma)+
        #scale_x_discrete(guide = guide_axis(angle = 65))+
        labs(y='Count',x="", caption=paste('CoS Max = $',55,sep=""))+
        guides(fill = guide_legend(reverse=F))+
        scale_x_reordered()+
        scale_fill_manual(values = Colors)
      
      
      
      p1<-ggplot(inventory_f_sum,aes(fct_reorder(Company,Rank),Count,fill=Status2,label=Count))+
        geom_col()+
        geom_text(stat = "stratum", aes(stratum = Status2),size=3.5)+
        #facet_wrap(~bins,ncol=1,scales='free_y')+
        coord_flip()+
        theme_minimal()+
        theme(panel.spacing.y = unit(-0.5, "lines"),
              text = element_text(size = 10),
              strip.text = element_text(size = 9),
              legend.position = "top",
              legend.key.size = unit(0.3, "cm"),
              legend.title = element_blank(),
              legend.justification="left",
              legend.margin=margin(0,0,0,-35),
              legend.box.margin=margin(-1,-1,-1,-1),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12)
        ) +
        scale_y_continuous(labels = scales::comma)+
        #scale_x_discrete(guide = guide_axis(angle = 65))+
        labs(y='Count',x="", caption=paste('CoS Max = $',55,sep=""))+
        guides(fill = guide_legend(reverse=F))+
        scale_x_reordered()+
        scale_fill_manual(values = Colors)
    
      ggdraw()+
        draw_plot(p1)+
        draw_plot(p2,height=0.28,width=.65,x=0.35,y=0.68)
      
        
      
    }
    
  }
}

#inventoryO$reservr
cos_summary_total_plot<-lapply('DB',cos_summary_total_fctn)






#############################################Output Powerpoint slides----



pptx <- read_pptx("Charts/DO NO DELETE PPT TEMPLATE/template_update.pptx")


company_vector=data.frame(company_vector)%>%mutate(company_vec1=word(company_vector,start=1,end=1))

company_vector_df<-company_vector%>%
  mutate(company_vec1=ifelse(is.na(company_vec1),company_vector,company_vec1))

company_vector<-company_vector_df$company_vec1

company_vector<-company_vector

#company_vector<-company_vector[1:8]
#map_plot1<-map_plot[1:8]

#LOOP----

for (i in seq_along(map_plot)) {
  ##Top Ten Operators By Reservoir with CoS map
  if(i==1){
    pptx <- add_slide(pptx, layout = "summary", master = "Office Theme")
    pptx <- ph_with(pptx, paste('DB CoS Summary'), location = ph_location_label(ph_label = "Title 15") )
    pptx <- ph_with(pptx, cos_summary_total_plot[[1]], location = ph_location_label(ph_label = "Content Placeholder 2") )
    pptx <- add_slide(pptx, layout = "TopTenOpZone", master = "Office Theme")
    pptx <- ph_with(pptx, paste(inventoryO$reservoirO[1],':Top 10 Operators By Reservoir'), location = ph_location_label(ph_label = "Title 75") )
    pptx <- ph_with(pptx, cos_40_sticks_plot[[1]], location = ph_location_label(ph_label = "Content Placeholder 71") )
    pptx <- ph_with(pptx, cos_zone_op_chart_fctn_plot[[1]], location = ph_location_label(ph_label = "Content Placeholder 72") )
    pptx <- add_slide(pptx, layout = "TopTenOpZone", master = "Office Theme")
    pptx <- ph_with(pptx, paste(inventoryO$reservoirO[2],':Top 10 Operators By Reservoir'), location = ph_location_label(ph_label = "Title 75") )
    pptx <- ph_with(pptx, cos_40_sticks_plot[[2]], location = ph_location_label(ph_label = "Content Placeholder 71") )
    pptx <- ph_with(pptx, cos_zone_op_chart_fctn_plot[[2]], location = ph_location_label(ph_label = "Content Placeholder 72") )
    pptx <- add_slide(pptx, layout = "TopTenOpZone", master = "Office Theme")
    pptx <- ph_with(pptx, paste(inventoryO$reservoirO[3],':Top 10 Operators By Reservoir'), location = ph_location_label(ph_label = "Title 75") )
    pptx <- ph_with(pptx, cos_40_sticks_plot[[3]], location = ph_location_label(ph_label = "Content Placeholder 71") )
    pptx <- ph_with(pptx, cos_zone_op_chart_fctn_plot[[3]], location = ph_location_label(ph_label = "Content Placeholder 72") )
    pptx <- add_slide(pptx, layout = "TopTenOpZone", master = "Office Theme")
    pptx <- ph_with(pptx, paste(inventoryO$reservoirO[4],':Top 10 Operators By Reservoir'), location = ph_location_label(ph_label = "Title 75") )
    pptx <- ph_with(pptx, cos_40_sticks_plot[[4]], location = ph_location_label(ph_label = "Content Placeholder 71") )
    pptx <- ph_with(pptx, cos_zone_op_chart_fctn_plot[[4]], location = ph_location_label(ph_label = "Content Placeholder 72") )
    pptx <- add_slide(pptx, layout = "TopTenOpZone", master = "Office Theme")
    pptx <- ph_with(pptx, paste(inventoryO$reservoirO[5],':Top 10 Operators By Reservoir'), location = ph_location_label(ph_label = "Title 75") )
    pptx <- ph_with(pptx, cos_40_sticks_plot[[5]], location = ph_location_label(ph_label = "Content Placeholder 71") )
    pptx <- ph_with(pptx, cos_zone_op_chart_fctn_plot[[5]], location = ph_location_label(ph_label = "Content Placeholder 72") )
    pptx <- add_slide(pptx, layout = "TopTenOpZone", master = "Office Theme")
    pptx <- ph_with(pptx, paste(inventoryO$reservoirO[6],':Top 10 Operators By Reservoir'), location = ph_location_label(ph_label = "Title 75") )
    pptx <- ph_with(pptx, cos_40_sticks_plot[[6]], location = ph_location_label(ph_label = "Content Placeholder 71") )
    pptx <- ph_with(pptx, cos_zone_op_chart_fctn_plot[[6]], location = ph_location_label(ph_label = "Content Placeholder 72") )
    pptx <- add_slide(pptx, layout = "TopTenOpZone", master = "Office Theme")
    pptx <- ph_with(pptx, paste('WFMP A/BS3S Tank',':Top 10 Operators By Reservoir'), location = ph_location_label(ph_label = "Title 75") )
    pptx <- ph_with(pptx, cos_40_sticks_plot[[7]], location = ph_location_label(ph_label = "Content Placeholder 71") )
    pptx <- ph_with(pptx, cos_zone_op_chart_fctn_plot[[7]], location = ph_location_label(ph_label = "Content Placeholder 72") )
    pptx <- add_slide(pptx, layout = "TopTenOpZone", master = "Office Theme")
    pptx <- ph_with(pptx, paste(inventoryO$reservoirO[8],':Top 10 Operators By Reservoir'), location = ph_location_label(ph_label = "Title 75") )
    pptx <- ph_with(pptx, cos_40_sticks_plot[[8]], location = ph_location_label(ph_label = "Content Placeholder 71") )
    pptx <- ph_with(pptx, cos_zone_op_chart_fctn_plot[[8]], location = ph_location_label(ph_label = "Content Placeholder 72") )
    pptx <- add_slide(pptx, layout = "TopTenOpZone", master = "Office Theme")
    pptx <- ph_with(pptx, paste(inventoryO$reservoirO[9],':Top 10 Operators By Reservoir'), location = ph_location_label(ph_label = "Title 75") )
    pptx <- ph_with(pptx, cos_40_sticks_plot[[9]], location = ph_location_label(ph_label = "Content Placeholder 71") )
    pptx <- ph_with(pptx, cos_zone_op_chart_fctn_plot[[9]], location = ph_location_label(ph_label = "Content Placeholder 72") )}
  else{
    print('skip')
  }
  
  if(i==1){
    pptx <- add_slide(pptx, layout = "details", master = "Office Theme")
    pptx <- ph_with(pptx, paste('Tier 1 Acreage By Zone'), location = ph_location_label(ph_label = "Title Detail") )
    pptx<- ph_with(pptx,value=x$reservoirO[1],location= ph_location_label(ph_label= 'Text Placeholder 11'))
    pptx <- ph_with(pptx, db_tier_acg_plot[[1]], location = ph_location_label(ph_label = "Content Placeholder 11") )
    pptx <- ph_with(pptx, cos_40_plot[[1]], location = ph_location_label(ph_label = "Content Placeholder 16") )
    pptx<- ph_with(pptx,value=x$reservoirO[2],location= ph_location_label(ph_label= 'Text Placeholder 12'))
    pptx <- ph_with(pptx, db_tier_acg_plot[[2]], location = ph_location_label(ph_label = "Content Placeholder 12") )
    pptx <- ph_with(pptx, cos_40_plot[[2]], location = ph_location_label(ph_label = "Content Placeholder 17") )
    pptx<- ph_with(pptx,value=x$reservoirO[3],location= ph_location_label(ph_label= 'Text Placeholder 13'))
    pptx <- ph_with(pptx, db_tier_acg_plot[[3]], location = ph_location_label(ph_label = "Content Placeholder 13") )
    pptx <- ph_with(pptx, cos_40_plot[[3]], location = ph_location_label(ph_label = "Content Placeholder 18") )
    pptx<- ph_with(pptx,x$reservoirO[4],location= ph_location_label(ph_label= 'Text Placeholder 14'))
    pptx <- ph_with(pptx, db_tier_acg_plot[[4]], location = ph_location_label(ph_label = "Content Placeholder 14") )
    pptx <- ph_with(pptx, cos_40_plot[[4]], location = ph_location_label(ph_label = "Content Placeholder 19") )
    pptx<- ph_with(pptx,value=x$reservoirO[5],location= ph_location_label(ph_label= 'Text Placeholder 15'))
    pptx <- ph_with(pptx, db_tier_acg_plot[[5]], location = ph_location_label(ph_label = "Content Placeholder 15") )
    pptx <- ph_with(pptx, cos_40_plot[[5]], location = ph_location_label(ph_label = "Content Placeholder 20") )
  }
  if(i==1){
    pptx <- add_slide(pptx, layout = "details", master = "Office Theme")
    pptx <- ph_with(pptx, paste('Tier 1 Acreage By Zone'), location = ph_location_label(ph_label = "Title Detail") )
    pptx<- ph_with(pptx,value=x$reservoirO[6],location= ph_location_label(ph_label= 'Text Placeholder 11'))
    pptx <- ph_with(pptx, db_tier_acg_plot[[6]], location = ph_location_label(ph_label = "Content Placeholder 11") )
    pptx <- ph_with(pptx, cos_40_plot[[6]], location = ph_location_label(ph_label = "Content Placeholder 16") )
    pptx<- ph_with(pptx,value=x$reservoirO[7],location= ph_location_label(ph_label= 'Text Placeholder 12'))
    pptx <- ph_with(pptx, db_tier_acg_plot[[7]], location = ph_location_label(ph_label = "Content Placeholder 12") )
    pptx <- ph_with(pptx, cos_40_plot[[7]], location = ph_location_label(ph_label = "Content Placeholder 17") )
    pptx<- ph_with(pptx,value=x$reservoirO[8],location= ph_location_label(ph_label= 'Text Placeholder 13'))
    pptx <- ph_with(pptx, db_tier_acg_plot[[8]], location = ph_location_label(ph_label = "Content Placeholder 13") )
    pptx <- ph_with(pptx, cos_40_plot[[8]], location = ph_location_label(ph_label = "Content Placeholder 18") )
    pptx<- ph_with(pptx,value=x$reservoirO[9],location= ph_location_label(ph_label= 'Text Placeholder 14'))
    pptx <- ph_with(pptx, db_tier_acg_plot[[9]], location = ph_location_label(ph_label = "Content Placeholder 14") )
    pptx <- ph_with(pptx, cos_40_plot[[9]], location = ph_location_label(ph_label = "Content Placeholder 19") )
    pptx<- ph_with(pptx,value=x$reservoirO[10],location= ph_location_label(ph_label= 'Text Placeholder 15'))
    pptx <- ph_with(pptx, db_tier_acg_plot[[10]], location = ph_location_label(ph_label = "Content Placeholder 15") )
    pptx <- ph_with(pptx, cos_40_plot[[10]], location = ph_location_label(ph_label = "Content Placeholder 20") )
  }
  if(i==1){
    pptx <- add_slide(pptx, layout = "details", master = "Office Theme")
    pptx <- ph_with(pptx, paste('Tier 1 Acreage By Zone'), location = ph_location_label(ph_label = "Title Detail") )
    pptx<- ph_with(pptx,value=x$reservoirO[11],location= ph_location_label(ph_label= 'Text Placeholder 11'))
    pptx <- ph_with(pptx, db_tier_acg_plot[[11]], location = ph_location_label(ph_label = "Content Placeholder 11") )
    pptx <- ph_with(pptx, cos_40_plot[[11]], location = ph_location_label(ph_label = "Content Placeholder 16") )
  }
  pptx <- add_slide(pptx, layout = "4Layout", master = "Office Theme")
  pptx <- ph_with(pptx, map_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 60") )
  ###ratio
    info<-attr(readPNG(paste("table_image/",i,".png",sep=""), info=T), 'info') 
    dims<-info$dim/info$dpi
    width_dim=info$dim[1]
    height_dim=info$dim[2]
    ratio=height_dim/width_dim
    #.5788732
  pptx <- ph_with(pptx, value=external_img(paste("table_image/",i,".png",sep=""),width=7.1,height=7.1*ratio),
                  location = ph_location(left= 6.16,top=.90),use_loc_size=F)
  pptx <- ph_with(pptx, pie_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 100") )
  pptx <- ph_with(pptx, bar_plotOrig[[i]], location = ph_location_label(ph_label = "Content Placeholder 120") )
  pptx <- ph_with(pptx, paste(company_vector[i]), location = ph_location_label(ph_label = "Title Summary") )
  ##Delaware Basin Details First Slide----
  if(length(wfmp_sh_map[[i]]$data)>0){
  pptx <- add_slide(pptx, layout = "details", master = "Office Theme")
  if(length(avln_map[[i]]$data)>0){
  pptx<- ph_with(pptx,value='AVALON',location= ph_location_label(ph_label= 'Text Placeholder 11'))}
  pptx <- ph_with(pptx, avln_map[[i]], location = ph_location_label(ph_label = "Content Placeholder 11") )
  if(length(sbsg_map[[i]]$data)>0){
    pptx<- ph_with(pptx,value='Second Bone Spring',location= ph_location_label(ph_label= 'Text Placeholder 13'))}
  pptx <- ph_with(pptx, sbsg_map[[i]], location = ph_location_label(ph_label = "Content Placeholder 13") )
   if(length(fbsg_map[[i]]$data)>0){
     pptx<- ph_with(pptx,value='First Bone Spring',location= ph_location_label(ph_label= 'Text Placeholder 12'))}
  pptx <- ph_with(pptx, fbsg_map[[i]], location = ph_location_label(ph_label = "Content Placeholder 12") )
   if(length(bs3c_map[[i]]$data)>0){
     pptx<- ph_with(pptx,value='Third Bone Spring Carb.',location= ph_location_label(ph_label= 'Text Placeholder 14'))}
  pptx <- ph_with(pptx, bs3c_map[[i]], location = ph_location_label(ph_label = "Content Placeholder 14") )
  if(length(wfmp_sh_map[[i]]$data)>0){
    pptx<- ph_with(pptx,value='Wolfcamp A Shale',location= ph_location_label(ph_label= 'Text Placeholder 15'))}
  pptx <- ph_with(pptx, wfmp_sh_map[[i]], location = ph_location_label(ph_label = "Content Placeholder 15") )
  pptx <- ph_with(pptx, avln_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 16") )
  pptx <- ph_with(pptx, sbsg_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 18") )
  pptx <- ph_with(pptx, fbsg_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 17") )
  pptx <- ph_with(pptx, bs3c_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 19") )
  pptx <- ph_with(pptx, wfmp_sh_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 20") )
  pptx <- ph_with(pptx, paste(company_vector[i],'DELAWARE BASIN'), location = ph_location_label(ph_label = "Title Detail") )
  }
  ##Midland Basin Details First Slide----
  if(length(wfmp_a_map[[i]]$data)>0){
    pptx <- add_slide(pptx, layout = "details", master = "Office Theme")
    if(length(jmill_map[[i]]$data)>0){
      pptx<- ph_with(pptx,value='Jo Mill',location= ph_location_label(ph_label= 'Text Placeholder 11'))}
    pptx <- ph_with(pptx, jmill_map[[i]], location = ph_location_label(ph_label = "Content Placeholder 11") )
    if(length(msby_map[[i]]$data)>0){
      pptx<- ph_with(pptx,value='Middle Spraberry',location= ph_location_label(ph_label= 'Text Placeholder 12'))}
    pptx <- ph_with(pptx, msby_map[[i]], location = ph_location_label(ph_label = "Content Placeholder 12") )
    if(length(lsby_map[[i]]$data)>0){
      pptx<- ph_with(pptx,value='Lower Spraberry',location= ph_location_label(ph_label= 'Text Placeholder 13'))}
    pptx <- ph_with(pptx, lsby_map[[i]], location = ph_location_label(ph_label = "Content Placeholder 13") )
    if(length(wfmp_a_map[[i]]$data)>0){
      pptx<- ph_with(pptx,value='Wolfcamp A',location= ph_location_label(ph_label= 'Text Placeholder 14'))}
    pptx <- ph_with(pptx, wfmp_a_map[[i]], location = ph_location_label(ph_label = "Content Placeholder 14") )
    if(length(wfmp_a_map[[i]]$data)>0){
      pptx<- ph_with(pptx,value='Wolfcamp B',location= ph_location_label(ph_label= 'Text Placeholder 15'))}
    pptx <- ph_with(pptx, wfmp_b_map[[i]], location = ph_location_label(ph_label = "Content Placeholder 15") )
    pptx <- ph_with(pptx, jmill_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 16") )
    pptx <- ph_with(pptx, msby_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 17") )
    pptx <- ph_with(pptx, lsby_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 18") )
    pptx <- ph_with(pptx, wfmp_a_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 19") )
    pptx <- ph_with(pptx, wfmp_b_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 20") )
    pptx <- ph_with(pptx, paste(company_vector[i],'MIDLAND BASIN'), location = ph_location_label(ph_label = "Title Detail") )
  }
  
  ##Delaware/Midland Secondary Basin Details First Slide----
  if(length(wfmp_sh_map[[i]]$data)>0){
    pptx <- add_slide(pptx, layout = "details", master = "Office Theme")
     if(length(wfmp_c_map[[i]]$data)>0){
      pptx<- ph_with(pptx,value='Wolfcamp C',location= ph_location_label(ph_label= 'Text Placeholder 12'))
    pptx <- ph_with(pptx, wfmp_c_map[[i]], location = ph_location_label(ph_label = "Content Placeholder 12") )}
    if(length(wfmp_b_db_map[[i]]$data)>0){
      pptx<- ph_with(pptx,value='Wolfcamp B',location= ph_location_label(ph_label= 'Text Placeholder 11'))
      pptx <- ph_with(pptx, wfmp_b_db_map[[i]], location = ph_location_label(ph_label = "Content Placeholder 11") )}
    
    if(length(wfmp_d_map[[i]]$data)>0){
      pptx<- ph_with(pptx,value='Wolfcamp D',location= ph_location_label(ph_label= 'Text Placeholder 13'))
    pptx <- ph_with(pptx, wfmp_d_map[[i]], location = ph_location_label(ph_label = "Content Placeholder 13") )}
    # if(length(brnt_map[[i]]$data)>0){
    #   pptx<- ph_with(pptx,value='Barnett',location= ph_location_label(ph_label= 'Text Placeholder 13'))
    #   pptx <- ph_with(pptx, brnt_map[[i]], location = ph_location_label(ph_label = "Content Placeholder 13") )}
    # if(length(brnt_map[[i]]$data)>0){
    # pptx <- ph_with(pptx, brnt_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 16") )}  
    if(length(wfmp_c_map[[i]]$data)>0){
    pptx <- ph_with(pptx, wfmp_c_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 17") )}
    if(length(wfmp_b_db_map[[i]]$data)>0){
      pptx <- ph_with(pptx, wfmp_b_db_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 16") )}
    if(length(brnt_map[[i]]$data)>0){
    pptx <- ph_with(pptx, brnt_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 18") )}
    if(length(brnt_map[[i]]$data)>0){
      pptx <- ph_with(pptx, brnt_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 18") )
    }
    pptx <- ph_with(pptx, paste(company_vector[i],'DELAWARE/MIDLAND BASIN'), location = ph_location_label(ph_label = "Title Detail") )
  } else{
    pptx <- add_slide(pptx, layout = "details", master = "Office Theme")
    if(length(wfmp_d_map[[i]]$data)>0){
      pptx<- ph_with(pptx,value='Wolfcamp C',location= ph_location_label(ph_label= 'Text Placeholder 11'))
      pptx <- ph_with(pptx, wfmp_c_mb_map[[i]], location = ph_location_label(ph_label = "Content Placeholder 11") )
      pptx <- ph_with(pptx, wfmp_c_mb_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 16") )
      pptx<- ph_with(pptx,value='Wolfcamp D',location= ph_location_label(ph_label= 'Text Placeholder 12'))
      pptx <- ph_with(pptx, wfmp_d_map[[i]], location = ph_location_label(ph_label = "Content Placeholder 12") )
      pptx <- ph_with(pptx, wfmp_d_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 17") )
      pptx<- ph_with(pptx,value='Barnett',location= ph_location_label(ph_label= 'Text Placeholder 13'))
      pptx <- ph_with(pptx, brnt_map[[i]], location = ph_location_label(ph_label = "Content Placeholder 13") )
      pptx <- ph_with(pptx, brnt_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 18") )
      pptx <- ph_with(pptx, paste(company_vector[i],'MIDLAND BASIN'), location = ph_location_label(ph_label = "Title Detail") )
      }
  }
  
  
  if(length(cos_plot[[i]]$data)>0){
    pptx <- add_slide(pptx, layout = "CoS Layout", master = "Office Theme")
    pptx <- ph_with(pptx, paste(company_vector[i],'CoS Summary and Status Breakdown'), location = ph_location_label(ph_label = "Title 1") )
    pptx <- ph_with(pptx, cos_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 30") )
    pptx <- ph_with(pptx, cake_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 31") )
    pptx <- ph_with(pptx, operated_nonop_plot[[i]], location = ph_location_label(ph_label = "Content Placeholder 32") )
  }
  
    }
    
  



###Write PPT----

print(pptx, "Charts/Permian_Inventory_5.24.2024.pptx")



# ######SUMMARY SLIDES----
# 
# 
# delaware_chart=st_drop_geometry(inventory)%>%filter(Name=='DELAWARE')%>%group_by(Company)%>%summarise(count_max=max(row_number()))%>%
#   arrange(desc(count_max))%>%left_join(company_vector_df,by=c("Company"="company_vector"))
# 
# delaware_chart=delaware_chart[1:40,]
# 
# delaware_chart=delaware_chart%>%mutate(Company=as.factor(Company))
# delaware_chart$company_vec1<-fct_reorder(delaware_chart$company_vec1,-delaware_chart$count_max)
# 
# delaware_chart1<-ggplot(delaware_chart)+geom_bar(aes(x=company_vec1,y=count_max),stat='identity')+
#   theme_minimal()+
#   geom_bar(data=delaware_chart%>%filter(grepl('CONOCOPHILLIPS',company_vec1)),aes(x=company_vec1,y=count_max),stat='identity',fill='red')+
#   theme(axis.text.x = element_text(angle = 55, hjust = 1, vjust = 1,size=8))+
#   ylab('Gross Count')+xlab("")
#   
# delaware_chart2<-ggplot(delaware_chart[20:40,])+geom_bar(aes(x=company_vec1,y=count_max),stat='identity')+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 55, hjust = 1, vjust = 1,size=5),plot.background = element_rect('white',color='white'))+
#   ylab('')+xlab("")
# 
# delaware_chart_final<-delaware_chart1+inset_element(delaware_chart2,left=.45,bottom=0.22,right=1,top=1)
# 
# ggsave(filename='delaware_chart_final.png',plot=delaware_chart_final,width=13.26,height=6.28)
# 
# 
# 
# 
# midland_chart<-st_drop_geometry(inventory)%>%filter(reservoir!='BRNT')%>%
#   filter(Name=='MIDLAND',Company != 'FASKEN OIL & RANCH',Company!= 'CROWNQUEST OPERATING',
#          Company!= 'CROWNROCK LP')%>%
#   mutate(Company=ifelse(Company=='CROWNROCK_INT','CROWNROCK',Company))%>%
#   group_by(Company)%>%summarise(count_max=max(row_number()))%>%
#   mutate(count_max=ifelse(grepl('ENDEAVOR',Company),5628,count_max))%>%
#   mutate(count_max=ifelse(grepl('CROWNROCK',Company),910,count_max))%>%
#   arrange(desc(count_max))%>%left_join(company_vector_df,by=c("Company"="company_vector"))%>%
#   mutate(company_vec1=ifelse(is.na(company_vec1),'CROWNROCK',company_vec1))
# 
# midland_chart=midland_chart[1:45,]
# 
# midland_chart=midland_chart%>%mutate(Company=as.factor(company_vec1))
# midland_chart<-midland_chart%>%filter(!grepl('ONYXPOINT|ACCELERATE|BAYSWATER|SIERRA|RIVERBEND|SINOCHEM|ECOPETROL|NORTHERN|MC|ENCORE|ENERVEST|GRANITE|GRP|JOY',company_vec1))
# midland_chart$company_vec1<-fct_reorder(midland_chart$company_vec1,-midland_chart$count_max)
# 
# midland_chart<-midland_chart
# midland_chart<-midland_chart[c(1:16,21,22),]
# 
# midland_chart1<-ggplot(midland_chart)+
#   geom_bar(aes(x=company_vec1,y=count_max),stat='identity')+
#   theme_minimal()+
#   geom_bar(data=midland_chart%>%filter(grepl('CONOCOPHILLIPS',company_vec1)),aes(x=company_vec1,y=count_max),stat='identity',fill='red')+
#   geom_bar(data=midland_chart%>%filter(grepl('ENDEAVOR',company_vec1)),aes(x=company_vec1,y=count_max),stat='identity',fill='blue')+
#   geom_bar(data=midland_chart%>%filter(grepl('CROWNROCK',company_vec1)),aes(x=company_vec1,y=count_max),stat='identity',fill='yellow')+
#   theme(axis.text.x = element_text(angle = 55, hjust = 1, vjust = 1,size=8),plot.background = element_rect('white',color='white'))+
#   ylab('Gross Count')+xlab("")+
#   geom_text(data=midland_chart%>%filter(count_max>950),aes(x=company_vec1,y=count_max,label = comma(count_max)), stat = "identity", vjust = 1.25, colour = "white",fontface='bold')+
#   geom_text(data=midland_chart%>%filter(count_max<950),aes(x=company_vec1,y=count_max,label = comma(count_max)), stat = "identity", vjust = -.5, colour = "black")
# 
# 
# midland_chart2<-ggplot(midland_chart[20:40,])+geom_bar(aes(x=company_vec1,y=count_max),stat='identity')+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 55, hjust = 1, vjust = 1,size=5),plot.background = element_rect('white',color='white'))+
#   ylab('')+xlab("")
# 
# midland_chart_final<-midland_chart1+inset_element(midland_chart2,left=.45,bottom=0.22,right=1,top=1)
# 
# ggsave(filename='midland_chart_final.png',plot=midland_chart1,width=13.26,height=6.28)
# 
# 
# 
# tier_1=st_drop_geometry(inventory)%>%filter(Tier=="Tier1",Company != 'FASKEN OIL & RANCH')%>%group_by(Company,Name)%>%summarise(count_max=max(row_number()))%>%
#   arrange(desc(count_max))%>%left_join(company_vector_df,by=c("Company"="company_vector"))%>%
#   ungroup()%>%group_by(Company)%>%mutate(max_basin_count=sum(count_max))%>%
#   ungroup()%>%arrange(desc(max_basin_count))%>%
#   mutate(rank=dense_rank(-max_basin_count))
# 
# tier_1=tier_1%>%filter(rank<41)
# 
# tier_1=tier_1%>%mutate(Company=as.factor(Company))
# tier_1$company_vec1<-fct_reorder(tier_1$company_vec1,-tier_1$max_basin_count)
# 
# tier1_chart<-ggplot(tier_1)+geom_bar(aes(x=company_vec1,y=count_max,fill=Name),stat='identity')+
#   theme_minimal()+
#   geom_bar(data=tier_1%>%filter(grepl('CONOCOPHILLIPS',company_vec1)),aes(x=company_vec1,y=count_max,fill=Name),stat='identity',color='black')+
#   theme(axis.text.x = element_text(angle = 55, hjust = 1, vjust = 1,size=8),legend.position = 'bottom',
#         legend.box.spacing = unit(-50, "pt"))+
#   ylab('Gross Count')+xlab("")+labs(fill='Basin')+
#   scale_fill_manual(values=c('blue','orange'))
# 
# tier1_chart2<-ggplot(tier_1%>%filter(rank>19))+geom_bar(aes(x=company_vec1,y=count_max,fill=Name),stat='identity')+
#   theme_minimal()+
#   #geom_bar(data=tier_1%>%filter(grepl('CONOCOPHILLIPS',company_vec1)),aes(x=company_vec1,y=count_max,fill=Name),stat='identity',color='black')+
#   theme(axis.text.x = element_text(angle = 55, hjust = 1, vjust = 1,size=8),
#         ,plot.background = element_rect('white',color='white'),legend.position="")+
#   ylab('Gross Count')+xlab("")+
#   scale_fill_manual(values=c('blue','orange'))
# 
# 
# 
# tier1_chart_final<-tier1_chart+inset_element(tier1_chart2,left=.45,bottom=0.22,right=1,top=1)
# 
# ggsave(filename='tier1_chart_final.png',plot=tier1_chart_final,width=13.26,height=6.28)
# 
# lat_length_df<-st_drop_geometry(inventory)%>%left_join(company_vector_df,by=c("Company"="company_vector"))%>%
#   filter(Company != 'FASKEN OIL & RANCH')%>%
#   mutate(TC_Binned=ifelse(Lat_TC<1.5,"1 Mile",ifelse(Lat_TC>1.49 & Lat_TC< 2,"1.5 Mile",ifelse(Lat_TC>1.9 & Lat_TC< 2.5,"2 Mile",">2 Mile"))))%>%arrange(length)%>%
#   group_by(TC_Binned,company_vec1)%>%summarise(Count=max(row_number()))%>%ungroup()%>%
#   group_by(company_vec1)%>%
#   mutate(CumCount=sum(Count))%>%ungroup()%>%arrange(desc(CumCount))%>%
#   mutate(rank=dense_rank(-CumCount))%>%filter(rank<41)
# 
# lat_length_df$company_vec1<-fct_reorder(lat_length_df$company_vec1,-lat_length_df$CumCount)
# 
# lat_length_df$TC_Binned=fct_rev(fct_relevel(lat_length_df$TC_Binned,levels= c(">2 Mile","2 Mile","1.5 Mile","1 Mile")))
# 
# lat_length_chart<-ggplot(lat_length_df)+geom_bar(aes(x=company_vec1,y=Count,fill=TC_Binned),stat='identity')+
#   theme_minimal()+
#   geom_bar(data=lat_length_df,aes(x=company_vec1,y=Count,fill=TC_Binned),stat='identity',color='light grey')+
#   geom_bar(data=lat_length_df%>%filter(grepl('CONOCOPHILLIPS',company_vec1)),aes(x=company_vec1,y=Count,fill=TC_Binned),stat='identity',color='black')+
#   theme(axis.text.x = element_text(angle = 55, hjust = 1, vjust = 1,size=8),legend.position = 'bottom',
#         legend.box.spacing = unit(-50, "pt"))+
#   ylab('Gross Count')+xlab("")+labs(fill='Lateral Length')+
#   guides(fill = guide_legend(reverse=TRUE))+
#   scale_fill_manual(values=c('white','purple','orange','blue'))
# 
# 
# lat_length_chart2<-ggplot(lat_length_df%>%filter(rank>19))+geom_bar(aes(x=company_vec1,y=Count,fill=TC_Binned),stat='identity')+
#   theme_minimal()+
#   geom_bar(data=lat_length_df%>%filter(rank>19),aes(x=company_vec1,y=Count,fill=TC_Binned),stat='identity',color='light grey')+
#   #geom_bar(data=lat_length_df%>%filter(grepl('CONOCOPHILLIPS',company_vec1)),aes(x=company_vec1,y=Count,fill=TC_Binned),stat='identity',color='black')+
#   theme(axis.text.x = element_text(angle = 55, hjust = 1, vjust = 1,size=8),legend.position = ''
#         ,plot.background = element_rect('white',color='white'))+
#   ylab('Gross Count')+xlab("")+
#   guides(fill = guide_legend(reverse=TRUE))+
#   scale_fill_manual(values=c('white','purple','orange','blue'))
# 
# lat_length_chart_final<-lat_length_chart+inset_element(lat_length_chart2,left=.45,bottom=0.22,right=1,top=1)
# 
# ggsave(filename='lat_length_final.png',plot=lat_length_chart_final,width=13.26,height=6.28)
# 
# 
# 
# operated_chart=st_drop_geometry(inventory)%>%filter(Status2=='Operated',Company != 'FASKEN OIL & RANCH')%>%group_by(Company)%>%summarise(count_max=max(row_number()))%>%
#   arrange(desc(count_max))%>%left_join(company_vector_df,by=c("Company"="company_vector"))
# 
# operated_chart=operated_chart[1:40,]
# 
# operated_chart=operated_chart%>%mutate(Company=as.factor(company_vec1))
# operated_chart$company_vec1<-fct_reorder(operated_chart$company_vec1,-operated_chart$count_max)
# 
# operated_chart1=ggplot(operated_chart)+
#   geom_bar(aes(x=company_vec1,y=count_max),stat='identity')+
#   theme_minimal()+
#   geom_bar(data=operated_chart%>%filter(grepl('CONOCOPHILLIPS',company_vec1)),aes(x=company_vec1,y=count_max),stat='identity',fill='red')+
#   theme(axis.text.x = element_text(angle = 55, hjust = 1, vjust = 1,size=8))+
#   ylab('Gross Count')+xlab("")
# 
# operated_chart2<-ggplot(operated_chart[20:40,])+geom_bar(aes(x=company_vec1,y=count_max),stat='identity')+
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 55, hjust = 1, vjust = 1,size=5),plot.background = element_rect('white',color='white'))+
#   ylab('')+xlab("")
# 
# operated_final<-operated_chart1+inset_element(operated_chart2,left=.45,bottom=0.22,right=1,top=1)
# 
# ggsave(filename='opearted_chart_final.png',plot=operated_final,width=13.26,height=6.28)
# 
# 
# ###total permian
# permian_chart<-st_drop_geometry(inventory)%>%filter(reservoir!='BRNT')%>%
#   filter(Company != 'FASKEN OIL & RANCH',Company!= 'CROWNQUEST OPERATING',
#          Company!= 'CROWNROCK LP')%>%
#   mutate(Company=ifelse(Company=='CROWNROCK_INT','CROWNROCK',Company))%>%
#   group_by(Company)%>%summarise(count_max=max(row_number()))%>%
#   mutate(count_max=ifelse(grepl('ENDEAVOR',Company),5628,count_max))%>%
#   mutate(count_max=ifelse(grepl('CROWNROCK',Company),910,count_max))%>%
#   arrange(desc(count_max))%>%left_join(company_vector_df,by=c("Company"="company_vector"))%>%
#   mutate(company_vec1=ifelse(is.na(company_vec1),'CROWNROCK',company_vec1))
# 
# permian_chart=permian_chart[1:40,]
# 
# permian_chart=permian_chart%>%mutate(Company=as.factor(company_vec1))
# permian_chart$company_vec1<-fct_reorder(permian_chart$company_vec1,-permian_chart$count_max)
# 
# 
# permian_chart<-permian_chart%>%filter(!grepl('ONYXPOINT|ACCELERATE|BAYSWATER|SIERRA|RIVERBEND|SINOCHEM|ECOPETROL|NORTHERN|MC|ENCORE|ENERVEST|GRANITE|GRP|JOY',company_vec1))
# 
# permian_chart<-permian_chart[c(1:20,22:24),]
# 
# permian_chart1<-ggplot(permian_chart)+
#   geom_bar(aes(x=company_vec1,y=count_max),stat='identity')+
#   theme_minimal()+
#   geom_bar(data=permian_chart%>%filter(grepl('CONOCOPHILLIPS',company_vec1)),aes(x=company_vec1,y=count_max),stat='identity',fill='red')+
#   geom_bar(data=permian_chart%>%filter(grepl('ENDEAVOR',company_vec1)),aes(x=company_vec1,y=count_max),stat='identity',fill='blue')+
#   geom_bar(data=permian_chart%>%filter(grepl('CROWNROCK',company_vec1)),aes(x=company_vec1,y=count_max),stat='identity',fill='yellow')+
#   #geom_bar(data=permian_chart%>%filter(grepl('MEWBOURNE',company_vec1)),aes(x=company_vec1,y=count_max),stat='identity',fill='orange')+
#   theme(axis.text.x = element_text(angle = 55, hjust = 1, vjust = 1,size=8),plot.background = element_rect('white',color='white'))+
#   ylab('Gross Count')+xlab("")+
#   geom_text(data=permian_chart%>%filter(count_max>2000),aes(x=company_vec1,y=count_max,label = comma(count_max)), stat = "identity", vjust = 1.25, colour = "white",fontface='bold')+
#   geom_text(data=permian_chart%>%filter(count_max<2000),aes(x=company_vec1,y=count_max,label = comma(count_max)), stat = "identity", vjust = -.5, colour = "black")
# 
# ggsave(filename='permian_chart1_final.png',plot=permian_chart1,width=13.26,height=6.28)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###########DB_CoS_EUR_output----
# 
# #i='BS3S'
# 
# db_tier_acg_fctn<-function(i){
#   db_tier<-tier_outlinesO%>%filter(Basin=='DB',zone==i)
#   plot(db_tier[,1])
#   db_tier<-db_tier%>%left_join(.,
#                                single_line_cos %>%
#                                  select(Lateral.Length.ft,basin,Zone_Alias,TC_Area,`CoS.$.WTI`,Type.Curve,`Gross.2-Str.EUR.Mboe`)%>%
#                                  filter(Lateral.Length.ft==10000),by=c("Basin"="basin","zone"="Zone_Alias","TC_Area"="TC_Area"))
#   
#   #db_tier<-db_tier%>%mutate(`CoS.$.WTI`=ifelse(is.na(`CoS.$.WTI`),55,`CoS.$.WTI`))
#   
#   max_num= 10*round(max(db_tier$`CoS.$.WTI`)/10, 0)+10
#   
# 
#   
#   db_tier$bins <- cut(db_tier$`CoS.$.WTI`, breaks=c(0,40,45,50,55,max_num))
#   
#   #db_tier$bins <- cut(db_tier$`CoS.$.WTI`, breaks=c(0,40,45))
# 
#   
#   x='//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/TC_Areas/DB_CoS_Gross EUR Mboe/'
#     
#   write_sf(db_tier,paste(x,i,".shp",sep=""))
#   
# }
# 
# inventoryOO<-st_drop_geometry(inventory)%>%ungroup()%>%distinct(reservoir)
# inventoryO<-st_drop_geometry(inventory)%>%
#   filter(Basin=='DB')%>%
#   ungroup()%>%distinct(reservoirO)%>%filter(!(reservoirO %in% c("BS3S","WFMP A SD")))
# 
# x=st_drop_geometry(inventory)%>%
#   filter(Basin=='DB')%>%
#   ungroup()%>%distinct(reservoirO)%>%arrange(reservoirO)
# 
# db_tier_acg_plot<-lapply(x$reservoirO,db_tier_acg_fctn)
