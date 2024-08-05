library(sf)
library(tidyverse)
library(openxlsx)
library(buffeRs)
library(gridExtra)


production<-readRDS("prod.rds")




sticks<-read_sf('//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/Well_Sticks_Zone/BD_ALL_WELLS.shp')

prj_sec<-read.xlsx("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/Well_Sticks_Zone/codev_dev.xlsx")


sticks<-left_join(sticks,prj_sec%>%select(API10,Project,section_id,length,Spud.Date)%>%arrange(desc(length))%>%
                    distinct(API10,.keep_all=T)%>%select(-length))

sticks$Spud.Date=as.Date(sticks$Spud.Date,origin='1899-12-30')



sticks<-sticks%>%mutate(zone=ifelse(is.na(zone),Tank,zone))
sticks<-sticks%>%mutate(zone=ifelse(is.na(zone),"UNK",zone))

sticks<-sticks%>%mutate(path_group=ifelse(Tank=='BS3C','BS2',
                                          ifelse(Tank=='BS3S','WFMP A',
                                                 ifelse(Tank=='WFMP B','WFMP A',
                                                        ifelse(Tank=='DEAN','WFMP A',
                                                               ifelse(Tank=='JMILL','MSBY',
                                                                      ifelse(Tank=='LSBY','WFMP A',
                                                                             ifelse(Tank=='WFMP C','WFMP D',
                                                                                    ifelse(Tank=='LSBY','WFMP A',Tank)))))))))






c(sort(unique(sticks$zone)),"UNK")

zone_vec= c("AVLN","BS1","BS2","BS3C","BS3S","DEAN","JMILL",'LAVLN','LSBY','MAVLN','MSBY','UAVLN','WFMP A','WFMP A SD','WFMP A SH','WFMP B','WFMP C','WFMP C/D','WFMP D','UNK')
color_vec<-c('purple','dark green','yellow','orange','#CE0792','maroon','blue','dark green','brown','#D4BF01','red','cyan','#76FE0F','#0F7EFE','red','turquoise','yellow','orange','#07CECB','grey')

color_df<-data.frame(zone_vec,color_vec)




#sticks<-sticks%>%select(-color_vec)
sticks<-left_join(sticks,color_df%>%rename(zone=zone_vec))





# x=sticks%>%filter(grepl('DOMINATOR',WELLNAM))
# 
# 
# ggplot(x)+geom_sf(aes(color=zone))+
#   scale_color_manual(values=with(color_df, setNames(color_vec, zone_vec)))
# 



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





sticks<-sticks%>%mutate(Spud.Date=ifelse(is.na(Spud.Date),COMPLET,Spud.Date))
sticks$Spud.Date<-lubridate::as_date(sticks$Spud.Date)

sticks$year<-year(sticks$Spud.Date)  

#y='PIONEER'


company_fctn<-function(y){

sticks_prep<-sticks%>%filter(grepl(y,OPERATO))

sticks_prep<-sticks_prep%>%group_by(section_id)%>%mutate(section_count=max(row_number()))%>%
  filter(section_count>1)


image_output_fctn<-function(i){

ex<-sticks_prep%>%filter(section_id==i)


prod_filt<-production%>%filter(API10 %in% c(ex$API10))

max_length=max(ex$length)
max_length=max_length[1]

ex<-ex%>%mutate(row_id=row_number())%>%
  group_by(row_id)%>%
  mutate(geometry=st_extend_line(sf::st_geometry(geometry), max_length-length))%>%ungroup()

ex<-ex%>%filter(Spud.Date>"2010-12-31" & length>3500)

single_stick<-ex%>%filter(Spud.Date==max(Spud.Date))%>%distinct(API10,.keep_all=T)



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
        panel.grid.major.y = element_line(color = "darkgrey",linetype = 2))+
guides(fill = guide_legend(order=1,nrow=2,override.aes = list(size =4,shape=21)))+
  guides(shape = guide_legend(order=2,nrow=2,override.aes = list(size = 4,fill='black')))
  
  

prod_filt<-left_join(prod_filt,zones_backfill%>%mutate(zone2=zone)%>%select(zone,zone2))

prod_filt<-prod_filt%>%group_by(zone2,API10)%>%mutate(count=max(row_number()))%>%
  filter(count>1)


p2=if(nrow(prod_filt)==0){
  ggplot()
}else{
p2=ggplot(prod_filt)+geom_line(aes(Months_Online,Cum_Oil_FF,color=zone2,group=API10),size=1)+
  scale_color_manual(values=with(color_df, setNames(color_vec, zone_vec)))+
  ylab('Oil Per Thousand Foot')+xlab("")+theme_minimal()+facet_wrap(~zone2)+
  theme(legend.position='none')+scale_y_continuous(labels=scales::comma)
}

p3=if(nrow(prod_filt)==0){
  ggplot()
}else{
p3=ggplot(prod_filt)+geom_line(aes(Months_Online,Cum_Gas_FF,color=zone2,group=API10),size=1)+
  scale_color_manual(values=with(color_df, setNames(color_vec, zone_vec)))+
  ylab('Gas Per Thousand Foot')+xlab("")+theme_minimal()+facet_wrap(~zone2)+
  theme(legend.position='none')+scale_y_continuous(labels=scales::comma)
}
#library(gridExtra)

name_df=ex%>%mutate(WELLNAM2=ifelse(grepl('FEDERAL',WELLNAM),'FED',WELLNAM),titles=stringr::word(WELLNAM2,start=1,2))
name_df=name_df%>%select(titles)%>%distinct(titles)%>%filter(!is.na(titles))

name_df=name_df[1,]
company=unique(ex$OPERATO)
company=company[1]

p4=grid.arrange(arrangeGrob(p1,ncol=1),                             # First row with one plot spaning over 2 columns
             arrangeGrob(p2, p3, nrow = 2), # Second row with 2 plots in 2 different columns
             ncol = 2,widths=c(4,3),top=paste(company,name_df[1,],sep=":"))


dir <- paste(getwd(),company,sep="/") 
if (!dir.exists(dir)) dir.create(dir)

ex$year=as.integer(as.character(ex$year))
max_date=ex%>%filter(year==max(as.numeric(year)))%>%distinct(year)
max_date=max_date$year

ggsave(plot=p4,width=13.3,height=7.4,paste(dir,'/',max_date,"_",company,"_",name_df[1,],".png",sep=""))

Sys.sleep(1.5)
}


image_output_fctn_possibly<-purrr::possibly(image_output_fctn)

section_to_apply<-st_drop_geometry(sticks_prep)%>%group_by(section_id)%>%arrange(desc(Spud.Date))%>%
  distinct(section_id)

section_to_apply=unique(section_to_apply$section_id)

lapply(section_to_apply,image_output_fctn_possibly)
  
}

company_fctn_possibly<-purrr::possibly(company_fctn)

operator_vec=st_drop_geometry(sticks)%>%group_by(OPERATO)%>%mutate(count=max(row_number()))%>%arrange(desc(count))%>%
  filter(!grepl('COG|CONOCOPHILLIPS',OPERATO))%>%
  distinct(OPERATO)

operator_vec=operator_vec$OPERATO



lapply(operator_vec,company_fctn_possibly)
            