library(sf)
library(tidyverse)
library(openxlsx)
library(buffeRs)
library(gridExtra)
library(officer)
library(magick)
library(nngeo)
library(gt)
library(png)
library(flextable)

# Sys.setenv(CHROMOTE_CHROME = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
# chromote::find_chrome()
# 
# options(webshot2.chrome = chromote::find_chrome())


setwd("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Individual Work/DM Work_COP/2024-07-31 Section Analysis Ex/")

production<-readRDS("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/ENV_PRD/spotfire_output/prod.rds")


cop<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/BD_ANALYST/SHAPE_DROP/COP_ACREAGE_NEW!/COP_20240819.shp")


permian_outline<- read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Permian Inventory CoS Join/Basin_Outline/Basin_Outlines.shp")



state_county <- tigris::counties(state = c("Texas","New Mexico"), cb = TRUE) %>%
  st_as_sf()

state_county<-st_transform(state_county,'+init=epsg:32039')


st_extend_line <- function(line, distance, end = "BOTH")
{
  if (!(end %in% c("BOTH", "HEAD", "TAIL")) | length(end) != 1) stop("'end' must be 'BOTH', 'HEAD' or 'TAIL'")
  
  M <- sf::st_coordinates(line)[,1:2]
  keep <- !(end == c("TAIL", "HEAD"))
  
  ends <- c(1, nrow(M))[keep]
  headings <- st_ends_heading(line)[keep]
  distances <- if (length(distance) == 1) rep(distance, 2) else rev(distance[1:2])
  
  M[ends,] <- M[ends,] + distances[keep] * c(cos(headings), sin(headings))
  newline <- sf::st_linestring(M)
  
  # If input is sfc_LINESTRING and not sfg_LINESTRING
  if (is.list(line)) newline <- sf::st_sfc(newline, crs = sf::st_crs(line))
  
  return(newline)
}



st_ends_heading <- function(line)
{
  M <- sf::st_coordinates(line)
  i <- c(2, nrow(M) - 1)
  j <- c(1, -1)
  
  headings <- mapply(i, j, FUN = function(i, j) {
    Ax <- M[i-j,1]
    Ay <- M[i-j,2]
    Bx <- M[i,1]
    By <- M[i,2]
    unname(atan2(Ay-By, Ax-Bx))
  })
  
  return(headings)
}




db_one_line<-read.xlsx("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/00_Regional Type Curves/01 Delaware/04 Engineering Support/DB_BOATTs_2024_03_12_SWE_OUTPUTS_FOR_IA.xlsx",sheet=4)
db_one_line<-db_one_line%>%select(LEASE,ROR,`GROSS.OIL,MB`,`GROSS.GAS,MMF`)

mb_one_line<-read.xlsx("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/00_Regional Type Curves/02 Midland/04 Engineering Support/Sent to IA/MB_BOATTs_2024_02_29_SWE_Outputs_for_IA.xlsx",sheet=4)
mb_one_line<-mb_one_line%>%select(LEASE,ROR,`GROSS.OIL,MB`,`GROSS.GAS,MMF`)

one_line_combined<-full_join(db_one_line,mb_one_line)


db_boat_prod<-read.xlsx("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/00_Regional Type Curves/01 Delaware/04 Engineering Support/DB_BOATTs_2024_03_12_SWE_OUTPUTS_FOR_IA.xlsx")
mb_boat_prod<-read.xlsx("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/00_Regional Type Curves/02 Midland/04 Engineering Support/Sent to IA/MB_BOATTs_2024_02_29_SWE_Outputs_for_IA.xlsx")
mb_boat_prod<-mb_boat_prod%>%select(-OUTDATE)


db_boat_prod<- db_boat_prod%>%mutate(LndngZn= str_replace(Unique.Drill.Model, "^[^_]*_", ""))
db_boat_prod<- db_boat_prod%>%mutate(LndngZn= str_replace(LndngZn, "^[^_]*_", ""))
db_boat_prod<- db_boat_prod%>%mutate(LndngZn=str_replace(LndngZn, "_.*$", ""))

db_boat_prod<-db_boat_prod%>%mutate(LndngZn= ifelse(LndngZn=='FBSG','BS1',
                                                    ifelse(LndngZn=='SBSG','BS2',
                                                           ifelse(LndngZn=='TBSG','BS3S',
                                                                  ifelse(LndngZn=='WFMPA','WFMP A',
                                                                         ifelse(LndngZn=='WMFPA','WFMP A',
                                                                                ifelse(LndngZn=='WFMPB','WFMP B',
                                                                                       ifelse(LndngZn=='WFMPC','WFMP C',LndngZn))))))))

db_boat_prod<-db_boat_prod%>%mutate(LndngZn=ifelse(grepl("WFMPA_SD|WMFPA_SD",Unique.Drill.Model),'WFMP A SD',LndngZn))


mb_boat_prod<- mb_boat_prod%>%mutate(LndngZn= str_replace(Unique.Drill.Model, "^[^_]*_", ""))
mb_boat_prod<- mb_boat_prod%>%mutate(LndngZn=str_replace(LndngZn, "_.*$", ""))


mb_boat_prod<-mb_boat_prod%>%mutate(LndngZn=ifelse(LndngZn=='JM','JMILL',
                                                   ifelse(LndngZn=='WCA','WFMP A',
                                                          ifelse(LndngZn=='WCB','WFMP B',
                                                                 ifelse(LndngZn=='WCC','WFMP C',
                                                                        ifelse(LndngZn=='WCD','WFMP D',LndngZn))))))


db_mb_boat_prod<-full_join(db_boat_prod,mb_boat_prod)
db_mb_boat_prod$Lat_TC<-str_replace(db_mb_boat_prod$Unique.Drill.Model, ".*_", "")
db_mb_boat_prod<-db_mb_boat_prod%>%mutate(Basin= str_replace(Unique.Drill.Model, "_.*$", ""))

db_mb_boat_prod$TC_Area<-str_replace(db_mb_boat_prod$Unique.Drill.Model, "_[^_]*$", "")
db_mb_boat_prod$TC_Area<-str_extract(db_mb_boat_prod$TC_Area, "[^_]+$")


db_mb_boat_prod<-db_mb_boat_prod%>%mutate(LndngZn=ifelse(LndngZn=="WFMP A" & Basin=='DB','WFMP A SH',LndngZn))

db_mb_boat_prod<-db_mb_boat_prod%>%mutate(TypeCurveLink=paste(Basin,LndngZn,TC_Area,Lat_TC,sep='_'))

db_mb_boat_prod<-db_mb_boat_prod%>%mutate(TypeCurveLink=stringr::str_to_upper(TypeCurveLink))


type_curve_join=db_mb_boat_prod%>%distinct(TypeCurveLink,Unique.Drill.Model)


sticks<-read_sf('//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/Well_Sticks_Zone/BD_ALL_WELLS.shp')
prj_sec<-read.xlsx("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/Well_Sticks_Zone/codev_dev.xlsx")


sticks<-left_join(sticks,prj_sec%>%select(UWI10,Project,section_id,length,Spud.Date)%>%arrange(desc(length))%>%
                    distinct(UWI10,.keep_all=T)%>%select(-length))

sticks$Spud.Date=as.Date(sticks$Spud.Date,origin='1899-12-30')

sticks<-sticks%>%mutate(zone=ifelse(is.na(TC_Jn_T),LndngZn,TC_Jn_T))
sticks<-sticks%>%mutate(zone=ifelse(is.na(zone),"UNK",zone))



sticks<-sticks%>%mutate(path_group=ifelse(TC_Jn_T=='BS3C','BS2',
                                          ifelse(TC_Jn_T=='BS3S','BS3S',
                                                 ifelse(TC_Jn_T=='WFMP A SD','WFMP A',
                                                        ifelse(TC_Jn_T=='WFMP A SH','WFMP A',
                                                 ifelse(TC_Jn_T=='WFMP B','WFMP A',
                                                        ifelse(TC_Jn_T=='DEAN','WFMP A',
                                                               ifelse(TC_Jn_T=='JMILL','MSBY',
                                                                      ifelse(TC_Jn_T=='JOMILL','MSBY',
                                                                      ifelse(TC_Jn_T=='LSBY','LSBY',
                                                                             ifelse(TC_Jn_T=='WFMP C','WFMP D',
ifelse(TC_Jn_T=='LSBY','LSBY',TC_Jn_T))))))))))))



sticks<-sticks%>%group_by(section_id)%>%mutate(sect_max_date=max(Spud.Date))%>%
  filter(sect_max_date>'2019-01-01')



zone_vec= c("AVLN","BS1","BS2","BS3C","BS3S","DEAN","JMILL",'LAVLN','LSBY','MAVLN','MSBY','UAVLN','WFMP A','WFMP A SD','WFMP A SH','WFMP B','WFMP C','WFMP C/D','WFMP D','UNK','USBY')
color_vec<-c('purple','#9BFAF1','yellow','orange','#CE0792','maroon','blue','dark green',
             'brown','#D4BF01','red','cyan','#76FE0F','#0F7EFE','red','turquoise','yellow','orange','#c9190c','grey','#FF69B4')



color_df<-data.frame(zone_vec,color_vec)




#sticks<-sticks%>%select(-color_vec)
sticks<-left_join(sticks,color_df%>%rename(zone=zone_vec))

mround <-
  function(number, multiple)
    multiple * round(number / multiple)


sticks$Lat_TC = mround(sticks$length / 5400, .50)*100

sticks<-sticks%>%mutate(Lat_TC=ifelse(Lat_TC>299,300,Lat_TC))


##Model Link

sticks<-sticks%>%mutate(TC_Jn_T=ifelse(grepl("WFMP A",LndngZn) & Basin=='DB','WFMP A SH',TC_Jn_T))
sticks<-sticks%>%mutate(TC_Jn_T=ifelse(grepl("WFMP SD",LndngZn),'WFMP A SD',TC_Jn_T))
sticks<-sticks%>%mutate(TC_Jn_T=ifelse(grepl("AVLN LWR",LndngZn),'LAVLN',TC_Jn_T))
sticks<-sticks%>%mutate(TC_Jn_T=ifelse(grepl("AVLN MID",LndngZn),'MAVLN',TC_Jn_T))
sticks<-sticks%>%mutate(TC_Jn_T=ifelse(grepl("JOMILL",LndngZn),'JMILL',TC_Jn_T))
sticks<-sticks%>%mutate(TC_Jn_T=ifelse(grepl("WFMP D",LndngZn) & Basin=='DB','WFMP C',TC_Jn_T))


sticks<-sticks%>%mutate(Basin=ifelse(County %in% c("CHAVES","CULBERSON","EDDY","JEFF DAVIS","LEA","LOVING","PECOS","REEVES","WARD","WINKLER"),"DB","MB"))


sticks<-sticks%>%mutate(TypeCurveLink=paste(Basin,TC_Jn_T,paste('TCA',TC_Area,sep=""),Lat_TC,sep='_'))

#sticks<-sticksOriginal
#sticksOriginal=sticks
#x<-sticks%>%filter(grepl('POKER LAKE',WELLNAM))





sticks<-sticks%>%mutate(Spud.Date=ifelse(is.na(Spud.Date),COMPLET,Spud.Date))
sticks$Spud.Date<-lubridate::as_date(sticks$Spud.Date)

sticks$year<-year(sticks$Spud.Date)  

sticks$WELLNAM <- gsub("[^a-zA-Z0-9]", " ", sticks$WELLNAM)


##add well number
env_well_num<-data.table::fread("env_wells.csv")
env_well_num<-env_well_num%>%select(Unformatted_API_UWI,WellNumber,LeaseName)%>%distinct(Unformatted_API_UWI,.keep_all=T)
env_well_num$Unformatted_API_UWI=as.character(env_well_num$Unformatted_API_UWI)
sticks<-left_join(sticks,env_well_num,by=c("UWI10"="Unformatted_API_UWI"))


sticks<-left_join(sticks,type_curve_join)

sticks<-left_join(sticks,one_line_combined,by=c("Unique.Drill.Model"="LEASE"))

sticks<-sticks%>%ungroup()%>%group_by(ENV_Opr)%>%
  arrange(desc(sect_max_date),section_id)%>%
  group_by(ENV_Opr,section_id)%>%
  mutate(start_id=row_number())%>%
  ungroup()%>%
  group_by(ENV_Opr)%>%
  mutate(start_id=cumsum(start_id == 1))



#y='MEWBOURNE'
####FINAL FUNCTION----
company_fctn<-function(y){

sticks_prep<-sticks%>%filter(grepl(y,ENV_Opr))


sticks_prep<-sticks_prep%>%group_by(section_id)%>%mutate(section_count=max(row_number()))%>%
  filter(section_count>1)


image_output_fctn<-function(i){

ex<-sticks_prep%>%arrange(desc(Spud_Dt))%>%filter(section_id==i)

ex<-ex%>%filter(Spud_Dt>"2015-01-01")

ex<-ex%>%group_by(UWI10)%>%filter(length==max(length))%>%ungroup()%>%distinct(UWI10,.keep_all = T)

single_stick<-ex%>%filter(Spud.Date==max(Spud.Date))%>%distinct(UWI10,.keep_all=T)

position=single_stick$WllbrPs

ex<-ex%>%filter(WllbrPs==single_stick$WllbrPs)

prod_filt<-production%>%filter(API10 %in% c(ex$UWI10))

max_length=max(ex$length)
max_length=max_length[1]

ex<-ex%>%mutate(row_id=row_number())%>%
  group_by(row_id)%>%
  mutate(geometry=ifelse(WllbrPs==position,st_extend_line(sf::st_geometry(geometry), (max_length-length)*2,end='BOTH'),
                         st_extend_line(sf::st_geometry(geometry), (max_length-length)*2,end='BOTH')))%>%ungroup()



ex<-ex%>%mutate(length_new=as.numeric(st_length(geometry)))

ex<-ex%>%filter(Spud.Date>"2010-12-31" & length>1500)

#single_stick<-ex%>%filter(length==min(length))%>%distinct(UWI10,.keep_all=T)

# Extract coordinates
coords <- st_coordinates(single_stick)
x1 <- coords[1, 1]
y1 <- coords[1, 2]
x2 <- coords[2, 1]
y2 <- coords[2, 2]

# Calculate the midpoint
x0 <- (x1 + x2) / 2
y0 <- (y1 + y2) / 2

# Calculate the direction vector
dx <- x2 - x1
dy <- y2 - y1

# Calculate the perpendicular direction vector
# A perpendicular vector (dx, dy) is (-dy, dx) or (dy, -dx)
perpendicular_dx <- -dy
perpendicular_dy <- dx

# Choose t values to define the extent of the perpendicular line
t1 <- -10
t2 <- 10

# Calculate points on the perpendicular line
x1_perpendicular <- x0 + t1 * perpendicular_dx
y1_perpendicular <- y0 + t1 * perpendicular_dy
x2_perpendicular <- x0 + t2 * perpendicular_dx
y2_perpendicular <- y0 + t2 * perpendicular_dy

# Create the perpendicular LINESTRING
perpendicular_linestring <- st_sfc(
  st_linestring(
    matrix(c(
      x1_perpendicular, y1_perpendicular,
      x2_perpendicular, y2_perpendicular
    ), ncol = 2, byrow = TRUE)
  ),
  crs = 32039
)

# Create a simple features data frame
perpendicular_sf <- st_sf(
  UWI = "Perpendicular",
  geometry = perpendicular_linestring
)


st_crs(ex)=st_crs(perpendicular_sf)

intersect_df=st_intersection(ex[,1],perpendicular_sf)

dist=st_distance(intersect_df,intersect_df,remove_self=T)
dist=data.frame(dist)
# Convert units to numeric for comparison
df_numeric <- as.data.frame(lapply(dist, as.numeric))

# Find the column with the maximum value
max_column <- colnames(dist)[which.max(sapply(df_numeric, max))]

dist=dist%>%select(max_column)

dist=dist[,1]

ex=cbind(ex,dist)
ex$dist=as.numeric(ex$dist)


labels_df <-ex %>% group_by(path_group)%>%
  mutate(count=max(row_number()))%>%
  filter(count>1)%>%
  rename(x=dist,y=meanTVD)%>%
  arrange(x, desc(y)) %>% 
  mutate(
    prev_x = lag(x, default = first(x)),
    prev_y = lag(y, default = first(y)),
    shift_x = x - prev_x,
    shift_y = y - prev_y
  ) %>% 
  pivot_longer(
    cols = matches("shift"),
    names_to = "direction",
    names_prefix = "shift_"
  ) %>% 
  filter(value != 0) %>% 
  mutate(
    label_x = case_when(
      direction == "x" ~ (x + prev_x)/2,
      direction == "y" ~ x
    ),
    label_y = case_when(
      direction == "x" ~ prev_y,
      direction == "y" ~ y - value/2
    )    
  ) %>% 
  mutate(label_text = as.character(abs(value))) %>% 
  mutate(angle = if_else(direction == "x", 0, 90))


labels_df <- 
  labels_df %>%group_by(path_group) %>%
  mutate(fix_y_label = direction == "y" & lag(direction) == "y" & 
           sign(value) != sign(lag(value)) & row_number() != 1) %>% 
  mutate(
    label_y    = ifelse(fix_y_label, label_y - lag(value)/2, label_y),
    label_text = ifelse(fix_y_label, as.character(abs(value + lag(value))), label_text)
  )

ex<-ex%>%
  group_by(path_group)%>%
  arrange(dist,meanTVD)%>%ungroup()%>%mutate(row_id=row_number())


zones_backfill=st_drop_geometry(ex)%>%group_by(zone)%>%filter(meanTVD==min(meanTVD))%>%
  select(zone,meanTVD)%>%distinct(zone,meanTVD)%>%arrange(meanTVD)

zones_backfill$meanTVD=zones_backfill$meanTVD-25

ex$zone<-fct_reorder(ex$zone,ex$meanTVD)

zones_backfill$zone<-fct_reorder(zones_backfill$zone,zones_backfill$meanTVD)

ex$year=as.character(ex$year)

ex$year<-fct_rev(ex$year)

labels_df<-labels_df%>%mutate(label_y2=ifelse(angle==0,label_y-25,label_y))
labels_df<-labels_df%>%mutate(label_x2=ifelse(angle!=0,label_x-25,label_x))

stick_caption=unique(ex$WELLNAM)
stick_caption=toString(paste0("'",stick_caption,"'") )

ex=ex%>%
  mutate(WellNumber=ifelse(!is.na(WellNumber),WellNumber,
                           ifelse(grepl("\\d{3}H$", WELLNAM)|grepl("\\d{2}H$", WELLNAM), sub(".*(\\d{3}H)$", "\\1", WELLNAM), NA)))

ex$WellNumber<-sub("H$", "", ex$WellNumber)


p1=ggplot(ex)+
  scale_y_reverse(labels=scales::comma)+
  scale_x_continuous(expand = c(0,0),labels=scales::comma)+
  #geom_hline(data=zones_backfill,aes(yintercept=meanTVD,color=zone))+
  geom_rect(data=zones_backfill,
            aes(fill=zone,xmin=-250,xmax=max(ex$dist)+250,ymax=max(ex$meanTVD)+300,ymin=meanTVD-50),alpha=0,show.legend = F)+
  geom_step(data=ex %>% group_by(path_group)%>%
              mutate(count=max(row_number()))%>%
              filter(count>1),
              aes(dist,meanTVD,group=path_group),size=.6,linetype='dashed')+
  #scale_fill_manual(values=with(color_df, setNames(color_vec, zone_vec)))+
  geom_text(data = labels_df%>%filter(as.numeric(label_text)>20)
            , aes(label_x2, label_y2, label = round(as.numeric(label_text),0), angle = angle),
            color = "black",size=4,check_overlap = T)+
  #geom_point(aes(dist,meanTVD,shape=year),color='black',size=7)+
  geom_point(aes(dist,meanTVD,fill=zone,shape=year),size=6)+
  geom_text(aes(dist,meanTVD,label=WellNumber),fontface = "bold",size=2,font='bold',max.overlaps = 50)+
  scale_shape_manual(values=c(21,22,23,24,25,1,2,3,4,5))+
  scale_fill_manual(values=with(color_df, setNames(color_vec, zone_vec)))+
  theme(legend.position='bottom')+ylab("")+xlab("")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_blank(),
        panel.background = element_rect(fill = 'white'),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.background=element_blank(),
        legend.key=element_rect(fill="white"),
        panel.grid.major.y = element_line(color = "darkgrey",linetype = 2),
        plot.caption = element_text(size=6, color="darkblue", face="italic"))+
guides(fill = guide_legend(order=1,nrow=2,override.aes = list(size =4,shape=21)))+
  guides(shape = guide_legend(order=2,nrow=2,override.aes = list(size = 4,fill='black')))+
  labs(caption = str_wrap(stick_caption,160))



####Add in Map----


bbox <- st_as_sfc(st_bbox(st_transform(ex,st_crs("+proj=longlat +datum=WGS84"))))
bbox_centroid<-st_centroid(bbox)
bbox_centroid<-st_as_sf(bbox_centroid)
bbox<-st_segments(bbox)
bbox<-st_as_sf(bbox)
max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))

bbox_centroid<-st_transform(bbox_centroid,'+init=epsg:32039')

square=buffer_square(bbox_centroid,max_dist*300)
st_crs(square) <- "+init=epsg:32039"
bbox<-st_as_sf(square)
state_countys=st_intersection(state_county,bbox)
bbox<-st_bbox(bbox)




#prod_filt<-left_join(prod_filt,zones_backfill%>%mutate(zone2=zone)%>%select(zone,zone2))

prod_filt<-prod_filt%>%
  filter(Cum_Oil_FF>0)%>%
  group_by(API10)%>%mutate(count=max(row_number()))%>%
  filter(count>1)

boat_filter<- db_mb_boat_prod%>%filter(TypeCurveLink %in% c(ex$TypeCurveLink))

boat_filter<-boat_filter%>%filter(`Gross.Oil,.Bbl`>0)
boat_filter<-boat_filter%>%group_by(TypeCurveLink)%>%mutate(Months_Online=row_number())
boat_filter$length<-as.numeric(boat_filter$Lat_TC)*50

boat_filter$`zone (2)`=boat_filter$LndngZn

boat_filter<-boat_filter%>%mutate(Model_Cum_Oil_FF=cumsum(`Gross.Oil,.Bbl`/length)*1000,
                                  Model_Cum_Gas_FF=cumsum(`Gross.Gas,.McF`/length)*1000)

max_period=max(prod_filt$Months_Online)+1

boat_filter<-boat_filter%>%filter(Months_Online<max_period)

boat_filter<-left_join(boat_filter,st_drop_geometry(ex)%>%select(TypeCurveLink,TC_Jn_T)%>%distinct(TypeCurveLink,.keep_all = T))

prod_filt<-left_join(prod_filt%>%select(UWI,API10,Months_Online,Cum_Oil_FF,Cum_Gas_FF),sticks%>%select(-UWI),
                       by=c('API10'='UWI10'))


prod_filt$ROR<-round(prod_filt$ROR*100,0)


boat_filter<-left_join(boat_filter,one_line_combined,by=c('Unique.Drill.Model'='LEASE'))

ror_df<-boat_filter%>%group_by(Unique.Drill.Model)%>%filter(Months_Online==max(Months_Online))%>%
  ungroup()%>%
  distinct(Unique.Drill.Model,.keep_all=T)%>%
  group_by(LndngZn)%>%filter(ROR==max(ROR))%>%
  ungroup()%>%
  distinct(LndngZn,.keep_all=T)

boat_filter<-boat_filter%>%filter(Unique.Drill.Model %in% c(ror_df$Unique.Drill.Model))


p2=if(nrow(prod_filt)==0){
  ggplot()
}else{
  p2=ggplot(prod_filt)+
  geom_line(data=boat_filter,aes(Months_Online,Model_Cum_Oil_FF,group=TypeCurveLink),color='darkgrey',linetype='dashed',size=1)+
  geom_line(aes(Months_Online,Cum_Oil_FF,color=TC_Jn_T,group=API10),size=1)+
  ggrepel::geom_text_repel(data=ror_df,aes(Months_Online,Model_Cum_Oil_FF,label=scales::percent(round(ROR*100,0)/100)),fontface='italic',size=3.5)+
  scale_color_manual(values=with(color_df, setNames(color_vec, zone_vec)))+
  ylab('Oil Per Thousand Foot')+xlab("")+theme_minimal()+facet_wrap(~TC_Jn_T,scales='free_y')+
  theme(legend.position='none')+scale_y_continuous(labels=scales::comma)
  
}


p3=if(nrow(prod_filt)==0){
  ggplot()
}else{
  p3=ggplot(prod_filt)+
  geom_line(data=boat_filter,aes(Months_Online,Model_Cum_Gas_FF,group=TypeCurveLink),color='darkgrey',linetype='dashed',size=1)+
  geom_line(aes(Months_Online,Cum_Gas_FF,color=TC_Jn_T,group=API10),size=1)+
  ggrepel::geom_text_repel(data=ror_df,aes(Months_Online,Model_Cum_Gas_FF,label=scales::percent(round(ROR*100,0)/100)),fontface='italic',size=3.5)+
  scale_color_manual(values=with(color_df, setNames(color_vec, zone_vec)))+
  ylab('Gas Per Thousand Foot')+xlab("")+theme_minimal()+facet_wrap(~TC_Jn_T,scales='free_y')+
  theme(legend.position='none')+scale_y_continuous(labels=scales::comma)+
  labs(caption='Models show longest lateral type curve based off of PDP for respective zone.')
}

name_df=ex%>%mutate(WELLNAM2=ifelse(grepl('FEDERAL',WELLNAM),'FED',WELLNAM),titles=stringr::word(WELLNAM2,start=1,2))
name_df=name_df%>%select(titles)%>%distinct(titles)%>%filter(!is.na(titles))

name_df=name_df[1,]
company=unique(ex$ENV_Opr)
company=company[1]

stick_caption=ex%>%arrange(desc(Spud.Date))%>%select(WELLNAM)
stick_caption=unique(ex$WELLNAM)
stick_caption=word(stick_caption, 1)
stick_caption=unique(stick_caption)
stick_caption_output=toString(paste0(stick_caption))
stick_caption_output=str_replace(stick_caption_output,",","__")
stick_caption_output=sub(" ", "_", stick_caption_output)
stick_caption_output=gsub("[[:blank:]]","",stick_caption_output)
  

lease_id=ex%>%
  mutate(LeaseName=ifelse(is.na(LeaseName),sub(" [^ ]*$", "", WELLNAM),LeaseName))%>%
  distinct(LeaseName,.keep_all = T)





map_plot=ggplot(state_county) +
  geom_sf(fill = NA) +
  # geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
  # geom_sf(data = acreagef, fill = 'gold', color = NA) +
  geom_sf(data = cop, fill = 'gold', color = NA,alpha=.65) +
  # geom_sf(data = cop_DI, fill = 'light grey', color = NA) +
  geom_sf(data = permian_outline, color = 'black') +
  geom_sf(data = ex, color = 'red') +
  ggrepel::geom_text_repel(data=lease_id%>%arrange(LeaseName)%>%
                             mutate(row_id=row_number())%>%
                             filter(row_id==min(row_id)|row_id==max(row_id)),aes(geometry=geometry,label=LeaseName),stat = "sf_coordinates",min.segment.length = 10,fontface='bold')+
  geom_sf_text(data = state_countys, aes(label = NAME), size = 3) +
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



#table data
table_data <-st_drop_geometry(ex) %>% arrange(desc(ROR))%>%
  select(Unique.Drill.Model,zone,ROR,GROSS.OIL.MB,GROSS.GAS.MMF)%>%
  rename(`Drill Model`=Unique.Drill.Model,IRR=ROR,`Oil MB`=GROSS.OIL.MB,`Gas MMF`=GROSS.GAS.MMF,Zone=zone)%>%
  distinct(`Drill Model`,.keep_all=T)%>%filter(!is.na(IRR))%>%
  mutate(IRR=round(IRR*100,0))%>%
  mutate(`Gas MMF`=round(`Gas MMF`,0),`Oil MB`=round(`Oil MB`,0))

ex$year=as.integer(as.character(ex$year))
max_date=ex%>%filter(year==max(as.numeric(year)))%>%distinct(year)
max_date=max_date$year

dir <- paste(getwd(),company,sep="/") 

if (!dir.exists(dir)) dir.create(dir)


p4=grid.arrange(arrangeGrob(p1,ncol=1),                             # First row with one plot spaning over 2 columns
                arrangeGrob(p2, p3, nrow = 2), # Second row with 2 plots in 2 different columns
                ncol = 2,widths=c(4,3))
ggsave(plot=p4,width=13.3,height=6.5,paste(dir,'/',max_date[1],"_",stick_caption_output,"_",i,"_gbd.png",sep=""))






if(nrow(table_data)<1){
  table_grob=ggplot()
  ggsave(plot=table_grob,width=6.89,height=5,paste(dir,'/',max_date[1],"_",stick_caption_output,"_",i,"_table.png",sep=""))

  }else{

    table_data <- flextable(table_data)
   
    table_grob<-theme_box(table_data) %>%  # Use a box theme as a starting point
      set_table_properties(width = 0.8, layout = "autofit") %>%  # Adjust table width
      bg(i = 1, bg = "#F0F0F0", part = "header") %>%  # Gray background for header
      color(i = 1, color = "black", part = "header") %>%  # Black text in the header
      color(color = "black", part = "body") %>%  # Black text in the body
      font(fontname = "Arial", part = "all") %>%  # Set font to Arial for all parts
      bold(i = 1, bold = TRUE, part = "header") %>%  # Bold the header
      border_remove() %>%  # Remove borders
      border_outer(border = fp_border(color = "gray70", width = 1)) %>%  # Add light gray outer border
      align(align = "center", part = "all")# Center-align text
    
    
    table_grob<-width(table_grob,j="Drill Model",width=2.2,unit='in')
    table_grob<-width(table_grob,j="Zone",width=1,unit='in')
    table_grob<-width(table_grob,j="IRR",width=1,unit='in')
    table_grob<-width(table_grob,j="Oil MB",width=1.195,unit='in')
    table_grob<-width(table_grob,j="Gas MMF",width=1.195,unit='in')
    table_grob <- colformat_num(table_grob, j = "IRR", suffix = "%", big.mark = "", digits = 0, scale = 100)
    table_grob <- align(table_grob,j="Drill Model",align='left',part='all')
    
    table_grob <- set_table_properties(table_grob, layout = "fixed")  # Fixed layout respects the set widths
    
    
    height=dim(table_grob)
    height=sum(height$heights)


gr=gen_grob(table_grob)
# Create a png device
png(filename = paste(dir,'/',max_date[1],"_",stick_caption_output,"_",i,"_table.png",sep=""), width = 6.59, height = height,units='in', res=300)
# Draw the flextable grob onto the device
grid::grid.draw(gr)
# Turn off the device to save the file
dev.off()


}




ggsave(plot=map_plot,width=6.6,height=6,paste(dir,'/',max_date[1],"_",stick_caption_output,"_",i,"_map.png",sep=""))




Sys.sleep(2)
}


image_output_fctn_possibly<-purrr::possibly(image_output_fctn)

section_to_apply<-st_drop_geometry(sticks_prep)%>%arrange(desc(Spud.Date))%>%
  distinct(section_id)

section_to_apply=unique(section_to_apply$section_id)

lapply(section_to_apply,image_output_fctn_possibly)
  
}

company_fctn_possibly<-purrr::possibly(company_fctn)

operator_vec=st_drop_geometry(sticks)%>%group_by(ENV_Opr)%>%mutate(count=max(row_number()))%>%arrange(desc(count))%>%
  filter(!grepl('COG|CONOCOPHILLIPS',ENV_Opr))%>%
  distinct(ENV_Opr)

operator_vec=operator_vec$ENV_Opr



lapply(operator_vec,company_fctn_possibly)




############create Power Point Slide Deck----
# 
# create_ppt_from_folders <- function(base_path) {
#   # List all directories (folders) in the base path
#   folders <- list.dirs(base_path, full.names = TRUE, recursive = FALSE)
#   #folder<-folders[2]
#   # Loop through each folder
#   for (folder in folders) {
#     folder_name <- basename(folder)
#     
#     ppt <- read_pptx("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Permian Inventory CoS Join/Charts/DO NO DELETE PPT TEMPLATE/template_update.pptx")
#     
#     # List all image files in the folder
#     image_files <- list.files(paste(base_path,folder_name,sep="/"), pattern = "\\.(png|jpg|jpeg|bmp|gif)$", full.names = TRUE)
#     #image_files<-rev(image_files)
#     
#     image_files_df<-data.frame(image_files)
#     image_files_df<-image_files_df%>%mutate(file_name= sub("^.*/", "", image_files))
#     image_files_df<-image_files_df%>%mutate(file_name= sub("\\..*$", "", file_name))
#     image_files_df<-image_files_df%>%mutate(source_image=   sub("^.*_", "", file_name))
#     image_files_df<-image_files_df%>%mutate(file_name= sub("_[^_]*$", "", file_name))
#     image_files_df<-image_files_df%>%arrange(desc(file_name))%>%
#       mutate(id=as.integer(gl(n(), 3, n())))
#     
#     
#     
#     image_files_df<-image_files_df%>%nest(.by=id)
#     
#     for (i in seq_along(image_files_df$id)) {
#       data_f<-image_files_df%>%filter(id==i)%>%unnest()
#       
#       gbd=data_f%>%filter(source_image=='gbd')%>%select(image_files)
#       maps=data_f%>%filter(source_image=='map')%>%select(image_files)
#       tables=data_f%>%filter(source_image=='table')%>%select(image_files)
#  
#       
#       # Add a slide with the "Title and Content" layout for each image
#       ppt <- add_slide(ppt, layout = "1_TopTenOpZone", master = "Office Theme")
#       
#       # Add title name
#       ppt <- ph_with(ppt, unique(sub("\\.[^.]*$", "", data_f$file_name)), location = ph_location_label(ph_label = "Title 85"))
#       
#       # Add the image to the slide with full-size dimensions
#       ppt <- ph_with(ppt, external_img(maps$image_files), location = ph_location_label(ph_label = "Content Placeholder 85"))
#       
#       info<-attr(readPNG(tables$image_files, info=T), 'info') 
#       dims<-info$dim/info$dpi
#       width_dim=info$dim[1]
#       height_dim=info$dim[2]
#       ratio=height_dim/width_dim
#       #.5788732
#       ppt <- ph_with(ppt, value=external_img(tables$image_files,width=6.59,height=7.1*ratio),
#                      location = ph_location(left= 6.5,top=1.07),use_loc_size=F)
#       
#       
#       # Add a slide with the "Title and Content" layout for each image
#       ppt <- add_slide(ppt, layout = "summary", master = "Office Theme")
#       
#       # Add the image name as the title of the slide
#       ppt <- ph_with(ppt, unique(sub("\\.[^.]*$", "", data_f$file_name)), location = ph_location_label(ph_label = "Title 15"))
#       ppt <- ph_with(ppt, external_img(gbd$image_files), location = ph_location_label(ph_label = "Content Placeholder 2"))
#       
#       
#       
#       
#     }
#     
#     
#     
#     
#     
#     # Save the PowerPoint file with the folder name
#     ppt_file <- file.path(base_path, paste0(folder_name, ".pptx"))
#     print(paste("Saving PowerPoint:", ppt_file))
#     
#     # Save the PowerPoint presentation
#     print(ppt, target = ppt_file)
#     
#     Sys.sleep(10)
#   }
# }
# 
# Specify the base path where your folders are located
base_path <- getwd()

# Run the main function to create PowerPoint decks
create_ppt_from_folders(base_path)




#######2025 PROJECTS BY PEERS----


prod_filter<-production%>%
  filter(Cum_Oil_FF>0)%>%
  group_by(API10)%>%mutate(max_period=max(row_number()))%>%
  filter(max_period>1 & max_period<4)%>%distinct(API10)


sticks$ENV_Opr <- sapply(strsplit(sticks$ENV_Opr, " "), `[`, 1)


sticks<-sticks%>%group_by(section_id)%>%
  mutate(CheckSec=any(UWI10 %in% c(prod_filter$API10)))%>%
  filter(CheckSec==TRUE)%>%
  mutate(ENV_Opr2='all')%>%
  ungroup()%>%
  rename(ENV_Opr=ENV_Opr2,ENV_Opr2=ENV_Opr)


#i=34737
####Project with New Production (3 Months) ONLY----
company_fctn_all<-function(y){
  
  sticks_prep<-sticks%>%filter(section_id==y)
  
  company<-sticks_prep%>%filter(Spud.Date==max(Spud.Date))%>%distinct(ENV_Opr2)
  
  
  sticks_prep<-sticks_prep%>%group_by(section_id)%>%mutate(section_count=max(row_number()))%>%
    filter(section_count>1)
  
  
  image_output_fctn<-function(i){
    
    ex<-sticks_prep%>%arrange(desc(Spud_Dt))%>%filter(section_id==i)
    
    ex<-ex%>%filter(Spud_Dt>"2015-01-01")
    
    ex<-ex%>%group_by(UWI10)%>%filter(length==max(length))%>%ungroup()%>%distinct(UWI10,.keep_all = T)
    
    single_stick<-ex%>%filter(Spud.Date==max(Spud.Date))%>%distinct(UWI10,.keep_all=T)
    
    position=single_stick$WllbrPs
    
    ex<-ex%>%filter(WllbrPs==single_stick$WllbrPs)
    
    prod_filt<-production%>%filter(API10 %in% c(ex$UWI10))
    
    max_length=max(ex$length)
    max_length=max_length[1]
    
    ex<-ex%>%mutate(row_id=row_number())%>%
      group_by(row_id)%>%
      mutate(geometry=ifelse(WllbrPs==position,st_extend_line(sf::st_geometry(geometry), (max_length-length)*2,end='BOTH'),
                             st_extend_line(sf::st_geometry(geometry), (max_length-length)*2,end='BOTH')))%>%ungroup()
    
    
    
    ex<-ex%>%mutate(length_new=as.numeric(st_length(geometry)))
    
    ex<-ex%>%filter(Spud.Date>"2010-12-31" & length>1500)
    
    #single_stick<-ex%>%filter(length==min(length))%>%distinct(UWI10,.keep_all=T)
    
    # Extract coordinates
    coords <- st_coordinates(single_stick)
    x1 <- coords[1, 1]
    y1 <- coords[1, 2]
    x2 <- coords[2, 1]
    y2 <- coords[2, 2]
    
    # Calculate the midpoint
    x0 <- (x1 + x2) / 2
    y0 <- (y1 + y2) / 2
    
    # Calculate the direction vector
    dx <- x2 - x1
    dy <- y2 - y1
    
    # Calculate the perpendicular direction vector
    # A perpendicular vector (dx, dy) is (-dy, dx) or (dy, -dx)
    perpendicular_dx <- -dy
    perpendicular_dy <- dx
    
    # Choose t values to define the extent of the perpendicular line
    t1 <- -10
    t2 <- 10
    
    # Calculate points on the perpendicular line
    x1_perpendicular <- x0 + t1 * perpendicular_dx
    y1_perpendicular <- y0 + t1 * perpendicular_dy
    x2_perpendicular <- x0 + t2 * perpendicular_dx
    y2_perpendicular <- y0 + t2 * perpendicular_dy
    
    # Create the perpendicular LINESTRING
    perpendicular_linestring <- st_sfc(
      st_linestring(
        matrix(c(
          x1_perpendicular, y1_perpendicular,
          x2_perpendicular, y2_perpendicular
        ), ncol = 2, byrow = TRUE)
      ),
      crs = 32039
    )
    
    # Create a simple features data frame
    perpendicular_sf <- st_sf(
      UWI = "Perpendicular",
      geometry = perpendicular_linestring
    )
    
    
    st_crs(ex)=st_crs(perpendicular_sf)
    
    intersect_df=st_intersection(ex[,1],perpendicular_sf)
    
    dist=st_distance(intersect_df,intersect_df,remove_self=T)
    dist=data.frame(dist)
    # Convert units to numeric for comparison
    df_numeric <- as.data.frame(lapply(dist, as.numeric))
    
    # Find the column with the maximum value
    max_column <- colnames(dist)[which.max(sapply(df_numeric, max))]
    
    dist=dist%>%select(max_column)
    
    dist=dist[,1]
    
    ex=cbind(ex,dist)
    ex$dist=as.numeric(ex$dist)
    
    
    labels_df <-ex %>% group_by(path_group)%>%
      mutate(count=max(row_number()))%>%
      filter(count>1)%>%
      rename(x=dist,y=meanTVD)%>%
      arrange(x, desc(y)) %>% 
      mutate(
        prev_x = lag(x, default = first(x)),
        prev_y = lag(y, default = first(y)),
        shift_x = x - prev_x,
        shift_y = y - prev_y
      ) %>% 
      pivot_longer(
        cols = matches("shift"),
        names_to = "direction",
        names_prefix = "shift_"
      ) %>% 
      filter(value != 0) %>% 
      mutate(
        label_x = case_when(
          direction == "x" ~ (x + prev_x)/2,
          direction == "y" ~ x
        ),
        label_y = case_when(
          direction == "x" ~ prev_y,
          direction == "y" ~ y - value/2
        )    
      ) %>% 
      mutate(label_text = as.character(abs(value))) %>% 
      mutate(angle = if_else(direction == "x", 0, 90))
    
    
    labels_df <- 
      labels_df %>%group_by(path_group) %>%
      mutate(fix_y_label = direction == "y" & lag(direction) == "y" & 
               sign(value) != sign(lag(value)) & row_number() != 1) %>% 
      mutate(
        label_y    = ifelse(fix_y_label, label_y - lag(value)/2, label_y),
        label_text = ifelse(fix_y_label, as.character(abs(value + lag(value))), label_text)
      )
    
    ex<-ex%>%
      group_by(path_group)%>%
      arrange(dist,meanTVD)%>%ungroup()%>%mutate(row_id=row_number())
    
    
    zones_backfill=st_drop_geometry(ex)%>%group_by(zone)%>%filter(meanTVD==min(meanTVD))%>%
      select(zone,meanTVD)%>%distinct(zone,meanTVD)%>%arrange(meanTVD)
    
    zones_backfill$meanTVD=zones_backfill$meanTVD-25
    
    ex$zone<-fct_reorder(ex$zone,ex$meanTVD)
    
    zones_backfill$zone<-fct_reorder(zones_backfill$zone,zones_backfill$meanTVD)
    
    ex$year=as.character(ex$year)
    
    ex$year<-fct_rev(ex$year)
    
    labels_df<-labels_df%>%mutate(label_y2=ifelse(angle==0,label_y-25,label_y))
    labels_df<-labels_df%>%mutate(label_x2=ifelse(angle!=0,label_x-25,label_x))
    
    stick_caption=unique(ex$WELLNAM)
    stick_caption=toString(paste0("'",stick_caption,"'") )
    
    ex=ex%>%
      mutate(WellNumber=ifelse(!is.na(WellNumber),WellNumber,
                               ifelse(grepl("\\d{3}H$", WELLNAM)|grepl("\\d{2}H$", WELLNAM), sub(".*(\\d{3}H)$", "\\1", WELLNAM), NA)))
    
    ex$WellNumber<-sub("H$", "", ex$WellNumber)
    
    
    company_name=ex%>%arrange(desc(Spud.Date))%>%distinct(ENV_Opr2)
    
    company_name=company_name[1,]
    
    
    
    p1=ggplot(ex)+
      scale_y_reverse(labels=scales::comma)+
      scale_x_continuous(expand = c(0,0),labels=scales::comma)+
      #geom_hline(data=zones_backfill,aes(yintercept=meanTVD,color=zone))+
      geom_rect(data=zones_backfill,
                aes(fill=zone,xmin=-250,xmax=max(ex$dist)+250,ymax=max(ex$meanTVD)+300,ymin=meanTVD-50),alpha=0,show.legend = F)+
      geom_step(data=ex %>% group_by(path_group)%>%
                  mutate(count=max(row_number()))%>%
                  filter(count>1),
                aes(dist,meanTVD,group=path_group),size=.6,linetype='dashed')+
      #scale_fill_manual(values=with(color_df, setNames(color_vec, zone_vec)))+
      geom_text(data = labels_df%>%filter(as.numeric(label_text)>20)
                , aes(label_x2, label_y2, label = round(as.numeric(label_text),0), angle = angle),
                color = "black",size=4,check_overlap = T)+
      #geom_point(aes(dist,meanTVD,shape=year),color='black',size=7)+
      geom_point(aes(dist,meanTVD,fill=zone,shape=year,color=ENV_Opr2),size=6)+
      geom_text(aes(dist,meanTVD,label=WellNumber),fontface = "bold",size=2,font='bold',max.overlaps = 50)+
      scale_shape_manual(values=c(21,22,23,24,25,1,2,3,4,5))+
      scale_color_manual(values=c('black','red','green'))+
      scale_fill_manual(values=with(color_df, setNames(color_vec, zone_vec)))+
      theme(legend.position='bottom')+ylab("")+xlab("")+
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_blank(),
            panel.background = element_rect(fill = 'white'),
            legend.title = element_text(size = 6),
            legend.text = element_text(size = 6),
            legend.background=element_blank(),
            legend.key=element_rect(fill="white"),
            panel.grid.major.y = element_line(color = "darkgrey",linetype = 2),
            plot.caption = element_text(size=6, color="darkblue", face="italic"))+
      guides(fill = guide_legend(order=1,nrow=2,override.aes = list(size =4,shape=21)))+
      guides(color=guide_legend(order=3,nrow=2,override.aes=list(size=4)))+
      guides(shape = guide_legend(order=2,nrow=2,override.aes = list(size = 4,fill='black')))+
      labs(caption = str_wrap(stick_caption,160),color="")
    
    
    
    ####Add in Map----
    
    
    bbox <- st_as_sfc(st_bbox(st_transform(ex,st_crs("+proj=longlat +datum=WGS84"))))
    bbox_centroid<-st_centroid(bbox)
    bbox_centroid<-st_as_sf(bbox_centroid)
    bbox<-st_segments(bbox)
    bbox<-st_as_sf(bbox)
    max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
    
    bbox_centroid<-st_transform(bbox_centroid,'+init=epsg:32039')
    
    square=buffer_square(bbox_centroid,max_dist*300)
    st_crs(square) <- "+init=epsg:32039"
    bbox<-st_as_sf(square)
    state_countys=st_intersection(state_county,bbox)
    bbox<-st_bbox(bbox)
    
    
    
    
    #prod_filt<-left_join(prod_filt,zones_backfill%>%mutate(zone2=zone)%>%select(zone,zone2))
    
    prod_filt<-prod_filt%>%group_by(API10)%>%mutate(count=max(row_number()))%>%
      filter(count>1)
    
    boat_filter<- db_mb_boat_prod%>%filter(TypeCurveLink %in% c(ex$TypeCurveLink))
    
    boat_filter<-boat_filter%>%filter(`Gross.Oil,.Bbl`>0)
    boat_filter<-boat_filter%>%group_by(TypeCurveLink)%>%mutate(Months_Online=row_number())
    boat_filter$length<-as.numeric(boat_filter$Lat_TC)*50
    
    boat_filter$`zone (2)`=boat_filter$LndngZn
    
    boat_filter<-boat_filter%>%mutate(Model_Cum_Oil_FF=cumsum(`Gross.Oil,.Bbl`/length)*1000,
                                      Model_Cum_Gas_FF=cumsum(`Gross.Gas,.McF`/length)*1000)
    
    max_period=13
    
    boat_filter<-boat_filter%>%filter(Months_Online<max_period)
    
    boat_filter<-left_join(boat_filter,st_drop_geometry(ex)%>%select(TypeCurveLink,TC_Jn_T)%>%distinct(TypeCurveLink,.keep_all = T))
    
    prod_filt<-left_join(prod_filt%>%select(UWI,API10,Months_Online,Cum_Oil_FF,Cum_Gas_FF),sticks%>%select(-UWI),
                         by=c('API10'='UWI10'))
    
    
    prod_filt$ROR<-round(prod_filt$ROR*100,0)
    
    
    boat_filter<-left_join(boat_filter,one_line_combined,by=c('Unique.Drill.Model'='LEASE'))
    
    boat_filter<-boat_filter#%>%filter(Unique.Drill.Model %in% c(ror_df$Unique.Drill.Model))%>%filter(Months_Online<13)
    
    prod_filt<-prod_filt%>%filter(Months_Online<13)
    
    ror_df<-boat_filter%>%group_by(Unique.Drill.Model)%>%filter(Months_Online==max(Months_Online))%>%
      ungroup()%>%
      distinct(Unique.Drill.Model,.keep_all=T)%>%
      group_by(LndngZn)%>%filter(ROR==max(ROR))%>%
      ungroup()%>%
      distinct(LndngZn,.keep_all=T)
    
    
    p2=if(nrow(prod_filt)==0){
      ggplot()
    }else{
      p2=ggplot(prod_filt)+
        geom_line(data=boat_filter,aes(Months_Online,Model_Cum_Oil_FF,group=TypeCurveLink),color='darkgrey',linetype='dashed',size=1)+
        geom_line(aes(Months_Online,Cum_Oil_FF,color=TC_Jn_T,group=API10),size=1)+
        ggrepel::geom_text_repel(data=ror_df,aes(Months_Online,Model_Cum_Oil_FF,label=scales::percent(round(ROR*100,0)/100)),fontface='italic',size=3.5)+
        scale_color_manual(values=with(color_df, setNames(color_vec, zone_vec)))+
        ylab('Oil Per Thousand Foot')+xlab("")+theme_minimal()+facet_wrap(~TC_Jn_T,scales='free_y')+
        theme(legend.position='none')+scale_y_continuous(labels=scales::comma)+
        scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))
      
    }
    
    
    p3=if(nrow(prod_filt)==0){
      ggplot()
    }else{
      p3=ggplot(prod_filt)+
        geom_line(data=boat_filter,aes(Months_Online,Model_Cum_Gas_FF,group=TypeCurveLink),color='darkgrey',linetype='dashed',size=1)+
        geom_line(aes(Months_Online,Cum_Gas_FF,color=TC_Jn_T,group=API10),size=1)+
        ggrepel::geom_text_repel(data=ror_df,aes(Months_Online,Model_Cum_Gas_FF,label=scales::percent(round(ROR*100,0)/100)),fontface='italic',size=3.5)+
        scale_color_manual(values=with(color_df, setNames(color_vec, zone_vec)))+
        ylab('Gas Per Thousand Foot')+xlab("")+theme_minimal()+facet_wrap(~TC_Jn_T,scales='free_y')+
        theme(legend.position='none')+scale_y_continuous(labels=scales::comma)+
        labs(caption='Models show longest lateral type curve based off of PDP for respective zone.')+
        scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12))
    }
    
    name_df=ex%>%mutate(WELLNAM2=ifelse(grepl('FEDERAL',WELLNAM),'FED',WELLNAM),titles=stringr::word(WELLNAM2,start=1,2))
    name_df=name_df%>%select(titles)%>%distinct(titles)%>%filter(!is.na(titles))
    
    name_df=name_df[1,]
    company=unique(ex$ENV_Opr)
    company=company[1]
    
    stick_caption=ex%>%arrange(desc(Spud.Date))%>%select(WELLNAM)
    stick_caption=unique(ex$WELLNAM)
    stick_caption=word(stick_caption, 1)
    stick_caption=unique(stick_caption)
    stick_caption_output=toString(paste0(stick_caption))
    stick_caption_output=str_replace(stick_caption_output,",","__")
    stick_caption_output=sub(" ", "_", stick_caption_output)
    stick_caption_output=gsub("[[:blank:]]","",stick_caption_output)
    stick_caption_output=gsub(",","",stick_caption_output)
    
    lease_id=ex%>%
      mutate(LeaseName=ifelse(is.na(LeaseName),sub(" [^ ]*$", "", WELLNAM),LeaseName))%>%
      distinct(LeaseName,.keep_all = T)
    
    
    
    
    map_plot=ggplot(state_county) +
      geom_sf(fill = NA) +
      # geom_sf(data=di_acreage_filter,fill='gold',color=NA)+
      # geom_sf(data = acreagef, fill = 'gold', color = NA) +
      geom_sf(data = cop, fill = 'gold', color = NA,alpha=.65) +
      # geom_sf(data = cop_DI, fill = 'light grey', color = NA) +
      geom_sf(data = permian_outline, color = 'black') +
      geom_sf(data = ex, color = 'red') +
      ggrepel::geom_text_repel(data=lease_id%>%arrange(LeaseName)%>%
                                 mutate(row_id=row_number())%>%
                                 filter(row_id==min(row_id)|row_id==max(row_id)),aes(geometry=geometry,label=LeaseName),stat = "sf_coordinates",min.segment.length = 10,fontface='bold')+
      geom_sf_text(data = state_countys, aes(label = NAME), size = 3) +
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
    
    
    
    #table data
    table_data <-st_drop_geometry(ex) %>% arrange(desc(ROR))%>%
      select(Unique.Drill.Model,zone,ROR,GROSS.OIL.MB,GROSS.GAS.MMF)%>%
      rename(`Drill Model`=Unique.Drill.Model,IRR=ROR,`Oil MB`=GROSS.OIL.MB,`Gas MMF`=GROSS.GAS.MMF,Zone=zone)%>%
      distinct(`Drill Model`,.keep_all=T)%>%filter(!is.na(IRR))%>%
      mutate(IRR=round(IRR*100,0))%>%
      mutate(`Gas MMF`=round(`Gas MMF`,0),`Oil MB`=round(`Oil MB`,0))
    
    ex$year=as.integer(as.character(ex$year))
    max_date=ex%>%filter(year==max(as.numeric(year)))%>%distinct(year)
    max_date=max_date$year
    
    dir <- paste(getwd(),company,sep="/") 
    
    if (!dir.exists(dir)) dir.create(dir)
    
    
    p4=grid.arrange(arrangeGrob(p1,ncol=1),                             # First row with one plot spaning over 2 columns
                    arrangeGrob(p2, p3, nrow = 2), # Second row with 2 plots in 2 different columns
                    ncol = 2,widths=c(4,3))
    ggsave(plot=p4,width=13.3,height=6.5,paste(dir,'/',company_name,max_date[1],"_",stick_caption_output,"_",i,"_gbd.png",sep=""))
    
    
    
    
    
    
    if(nrow(table_data)<1){
      table_grob=ggplot()
      ggsave(plot=table_grob,width=6.89,height=5,paste(dir,'/',company_name,max_date[1],"_",stick_caption_output,"_",i,"_table.png",sep=""))
      
    }else{
      
      table_data <- flextable(table_data)
      
      table_grob<-theme_box(table_data) %>%  # Use a box theme as a starting point
        set_table_properties(width = 0.8, layout = "autofit") %>%  # Adjust table width
        bg(i = 1, bg = "#F0F0F0", part = "header") %>%  # Gray background for header
        color(i = 1, color = "black", part = "header") %>%  # Black text in the header
        color(color = "black", part = "body") %>%  # Black text in the body
        font(fontname = "Arial", part = "all") %>%  # Set font to Arial for all parts
        bold(i = 1, bold = TRUE, part = "header") %>%  # Bold the header
        border_remove() %>%  # Remove borders
        border_outer(border = fp_border(color = "gray70", width = 1)) %>%  # Add light gray outer border
        align(align = "center", part = "all")# Center-align text
      
      
      table_grob<-width(table_grob,j="Drill Model",width=2.2,unit='in')
      table_grob<-width(table_grob,j="Zone",width=1,unit='in')
      table_grob<-width(table_grob,j="IRR",width=1,unit='in')
      table_grob<-width(table_grob,j="Oil MB",width=1.195,unit='in')
      table_grob<-width(table_grob,j="Gas MMF",width=1.195,unit='in')
      table_grob <- colformat_num(table_grob, j = "IRR", suffix = "%", big.mark = "", digits = 0, scale = 100)
      table_grob <- align(table_grob,j="Drill Model",align='left',part='all')
      
      table_grob <- set_table_properties(table_grob, layout = "fixed")  # Fixed layout respects the set widths
      
      
      height=dim(table_grob)
      height=sum(height$heights)
      
      
      gr=gen_grob(table_grob)
      # Create a png device
      png(filename = paste(dir,'/',company_name,max_date[1],"_",stick_caption_output,"_",i,"_table.png",sep=""), width = 6.59, height = height,units='in', res=300)
      # Draw the flextable grob onto the device
      grid::grid.draw(gr)
      # Turn off the device to save the file
      dev.off()
      
      
    }
    
    
    
    
    ggsave(plot=map_plot,width=6.6,height=6,paste(dir,'/',company_name,max_date[1],"_",stick_caption_output,"_",i,"_map.png",sep=""))
    
    
    
    
    Sys.sleep(2)
  }
  
  
  image_output_fctn_possibly<-purrr::possibly(image_output_fctn)
  
  section_to_apply<-st_drop_geometry(sticks_prep)%>%arrange(desc(Spud.Date))%>%
    distinct(section_id)
  
  section_to_apply=unique(section_to_apply$section_id)
  
  lapply(section_to_apply,image_output_fctn_possibly)
  
}

company_fctn_all_possibly<-purrr::possibly(company_fctn_all)

section_vec<-sticks%>%distinct(section_id)


lapply(section_vec$section_id,company_fctn_all_possibly)



create_ppt_from_folders <- function(base_path) {
  # List all directories (folders) in the base path
  folders <- list.dirs(base_path, full.names = TRUE, recursive = FALSE)
  #folder<-folders[2]
  # Loop through each folder
  for (folder in folders) {
    folder_name <- basename(folder)
    
    ppt <- read_pptx("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Permian Inventory CoS Join/Charts/DO NO DELETE PPT TEMPLATE/template_update.pptx")
    
    # List all image files in the folder
    image_files <- list.files(paste(base_path,folder_name,sep="/"), pattern = "\\.(png|jpg|jpeg|bmp|gif)$", full.names = TRUE)
    #image_files<-rev(image_files)
    
    image_files_df<-data.frame(image_files)
    image_files_df<-image_files_df%>%mutate(file_name= sub("^.*/", "", image_files))
    image_files_df<-image_files_df%>%mutate(file_name= sub("\\..*$", "", file_name))
    image_files_df<-image_files_df%>%mutate(source_image=   sub("^.*_", "", file_name))
    image_files_df<-image_files_df%>%mutate(file_name= sub("_[^_]*$", "", file_name))
    image_files_df<-image_files_df%>%arrange(desc(file_name))%>%
      mutate(id=as.integer(gl(n(), 3, n())))
    
    
    
    image_files_df<-image_files_df%>%nest(.by=id)
    
    for (i in seq_along(image_files_df$id)) {
      data_f<-image_files_df%>%filter(id==i)%>%unnest()
      
      gbd=data_f%>%filter(source_image=='gbd')%>%select(image_files)
      maps=data_f%>%filter(source_image=='map')%>%select(image_files)
      tables=data_f%>%filter(source_image=='table')%>%select(image_files)
      
      
      # Add a slide with the "Title and Content" layout for each image
      ppt <- add_slide(ppt, layout = "1_TopTenOpZone", master = "Office Theme")
      
      # Add title name
      ppt <- ph_with(ppt, unique(sub("\\.[^.]*$", "", data_f$file_name)), location = ph_location_label(ph_label = "Title 85"))
      
      # Add the image to the slide with full-size dimensions
      ppt <- ph_with(ppt, external_img(maps$image_files), location = ph_location_label(ph_label = "Content Placeholder 85"))
      
      info<-attr(readPNG(tables$image_files, info=T), 'info') 
      dims<-info$dim/info$dpi
      width_dim=info$dim[1]
      height_dim=info$dim[2]
      ratio=height_dim/width_dim
      #.5788732
      ppt <- ph_with(ppt, value=external_img(tables$image_files,width=6.59,height=7.1*ratio),
                     location = ph_location(left= 6.5,top=1.07),use_loc_size=F)
      
      
      # Add a slide with the "Title and Content" layout for each image
      ppt <- add_slide(ppt, layout = "summary", master = "Office Theme")
      
      # Add the image name as the title of the slide
      ppt <- ph_with(ppt, unique(sub("\\.[^.]*$", "", data_f$file_name)), location = ph_location_label(ph_label = "Title 15"))
      ppt <- ph_with(ppt, external_img(gbd$image_files), location = ph_location_label(ph_label = "Content Placeholder 2"))
      
      
      
      
    }
    
    
    
    
    
    # Save the PowerPoint file with the folder name
    ppt_file <- file.path(base_path, paste0(folder_name, ".pptx"))
    print(paste("Saving PowerPoint:", ppt_file))
    
    # Save the PowerPoint presentation
    print(ppt, target = ppt_file)
    
    Sys.sleep(10)
  }
}

# Specify the base path where your folders are located
base_path <- paste(getwd(),'/three_month_min',sep="")

base_path='//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Individual Work/DM Work_COP/2024-07-31 Section Analysis Ex/three_month_min'


# Run the main function to create PowerPoint decks
create_ppt_from_folders(base_path)


