source("Functions/inventory_functions.R")
#source("Midland_Inventory/00_Setup.R")


Combined_Acreage<-read_sf("Op_Non_Unknown_Acreage/Combined_Acreage/Combined_Acreage.shp")


#Combined_Acreage<-Combined_Acreage%>%filter(grepl('DEVON|DIAMONDBACK|MARATHON',Company,ignore.case=T))


Combined_Acreage<-Combined_Acreage%>%mutate(S_SNAME=paste(Status,"_",SNAME,sep=""))%>%group_by(Company,Status,SNAME,S_SNAME)%>%
  summarise(geometry=st_union(geometry))





sections<-read_sf("Op_Non_Unknown_Acreage/sections_grid/sections_rowId.shp")
sections<-sections[-35164,]
#sections_comp<-st_intersection(st_transform(sections,st_crs(Combined_Acreage)),st_buffer(Combined_Acreage,dist=0))

#sections_filt<-left_join(sections,st_drop_geometry(sections_comp)%>%select(row_id,Company))


sf_use_s2(FALSE)

library(igraph)

#  tier_outlines<- tier_outlineOriginal 
#  tier_outlines<-tier_outlines%>%filter(!grepl("Basin|T2|T3",TC_Area))
#  tier_outlines<-tier_outlines%>%mutate(row_id=row_number())
#  
#  tier_outlines<-st_buffer(tier_outlines,dist=0)
#  
#  tier_outlines<-tier_outlines%>%group_by(LZ)%>%summarize(geometry = st_union(geometry))
#  
#  tier_outlines_comb <- st_transform(tier_outlines, "+init=epsg:32039")
# 
# source("Delaware_Inventory/00_Setup.R")
#  tier_outlines<- tier_outlineOriginalFINAL
#  tier_outlines<-tier_outlines%>%filter(!grepl("Basin|T2|T3",TC_Area))
#  tier_outlines<-tier_outlines%>%mutate(row_id=row_number())
#  
#  tier_outlines<-st_buffer(tier_outlines,dist=0)
#  
# tier_outlines<-tier_outlines%>%group_by(LZ)%>%summarize(geometry = st_union(geometry))
# tier_outlines <- st_transform(tier_outlines, "+init=epsg:32039")
#  
# tier_outlines<-rbind(tier_outlines_comb,tier_outlines)
 

 
 
#Basin Merge extended line
all_shp <- rbindlist(mapply(
  c,
  (
    list.files(
      path = "Final/Final/",
      pattern = "*.shp",
      full.names = TRUE
    ) %>%
      lapply(
        read_sf
      )
  ),
  (
    list.files(
      path = "Final/Final/",
      pattern = "*.shp",
      full.names = TRUE
    ) %>%
      basename() %>%
      as.list()
  ),
  SIMPLIFY = FALSE
),
fill = T)

#plotly::ggplotly(ggplot(st_as_sf(all_shp))+geom_sf())

#unique(all_shp$reservoir)



#Combine Lines and extend and resplit----

all_shp<-all_shp%>%mutate(Company=gsub("NonOperated.*","",V1),
                          Company=gsub("Unknown.*","",Company),
                          Company=gsub("Operated.*","",Company),
                          Company=str_trim(Company))

# all_shp<- all_shp%>%filter(!grepl('MINERALS|MINERAL',Company))
# 
# filt_top_40<- all_shp%>%mutate(Company=gsub("NonOperated.*","",V1),
#                                   Company=gsub("Unknown.*","",Company),
#                                   Company=gsub("Operated.*","",Company),
#                                   Company=str_trim(Company))%>%
#   group_by(Company)%>%summarise(Count=max(row_number()))
# 
# filt_top_40<-filt_top_40%>%arrange(desc(Count))
# filt_top_40<-filt_top_40[1:40,]

# all_shp<-all_shp%>%
#   filter(Company %in% c(filt_top_40$Company))



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


###Recombine and split----

vec_filter<- all_shp%>%mutate(FilterId=paste(V1,reservoir))%>%distinct(FilterId)
vec_filter<-vec_filter$FilterId


starting_tbl<-data.frame()



ORIGINAL<-all_shp

#all_shp<-ORIGINAL

#filterid=vec_filter[1]

#remove(filterid)
#combined_acreage=acreage

final_combine_split_fctn<-function(filterid){
  
  Company_Add<-filterid
  
  all_shp_filter<-ORIGINAL%>%mutate(FilterId=paste(V1,reservoir))%>%filter(FilterId==filterid)
  
    
  x=st_as_sf(all_shp_filter)
  y=x
  y=y%>%filter(length>100)
  
  if(nrow(y)<2){
    print('skip')
  }else{
  
  first_point<- st_line_sample(y,sample=0)
  first_point<-st_cast(first_point,'POINT')
  first_point<-st_as_sf(first_point)
  first_point<-sfc_as_cols(first_point)
  first_point<-first_point[,-1]
  first_point<-first_point%>%rename(sf_long=x...2,sf_lat=y)
  first_point<-first_point%>%mutate(row_id_dir=row_number())
  second_point<- st_line_sample(y,sample=1)
  second_point<-st_cast(second_point,'POINT')
  second_point<-st_as_sf(second_point)
  second_point<-sfc_as_cols(second_point)
  second_point<-second_point[,-1]
  second_point<-second_point%>%rename(bh_long=x...2,bh_lat=y)
  second_point<-second_point%>%mutate(row_id_dir=row_number())
  full_points<-left_join(first_point,second_point)
  full_points<-full_points%>%mutate(long_length=abs(sf_long-bh_long),lat_length=abs(sf_lat-bh_lat))
  full_points<-full_points%>%mutate(direction=ifelse(lat_length>long_length,'E/W','N/S'),row_id_dir=row_number())  
  
  
  if(nrow(full_points)<1 | unique(max(y$length))<500){
    print("skip again")
  }else{
  
  y <- y%>%mutate(row_id_dir=row_number())%>%
    left_join(.,full_points%>%select(row_id_dir,direction))%>%
    group_by(row_id,direction)%>%filter(length>500)%>%
    mutate(geometry=st_extend_line(sf::st_geometry(geometry), 300))%>%ungroup()%>%
    mutate(row_id_coord=row_number())
    
  
  
  coord_polys <-
    st_transform(y,st_crs("+proj=longlat +datum=WGS84")) %>% group_by(row_id) %>% st_coordinates(geometry) %>% as_tibble() %>%
    group_by(L1) %>% mutate(RowID = row_number()) %>%
    pivot_wider(names_from = RowID, values_from = X:Y)
  
  
  # Add relative bearing----
  coord_polys <- coord_polys %>% mutate(Bearing_Rel =
                                          ifelse(
                                            bearingRhumb(c(X_1, Y_1), c(X_2, Y_2)) > 180,
                                            bearingRhumb(c(X_1, Y_1), c(X_2, Y_2)) -
                                              180,
                                            bearingRhumb(c(X_1, Y_1), c(X_2, Y_2))
                                          ))
  
  round.choose <- function(x, roundTo, dir = 1) {
    if (dir == 1) {
      ##ROUND UP
      x + (roundTo - x %% roundTo)
    } else {
      if (dir == 0) {
        ##ROUND DOWN
        x - (x %% roundTo)
      }
    }
  }
  
  coord_polys$Bearing_Rel <-
    round.choose(coord_polys$Bearing_Rel, 10)
  

  y=left_join(y,coord_polys%>%select(L1,Bearing_Rel),by=c("row_id_coord"="L1"))
  
  y=y%>%mutate(Bearing_Rel=ifelse(Bearing_Rel==180,10,ifelse(Bearing_Rel==100,90,Bearing_Rel)))
  

join_vec<- y%>%distinct(Bearing_Rel,direction)%>%rename(dir=direction,bear=Bearing_Rel)

  ###########test join way function
  touch_df<- data.frame()  
  


  
  touch_df_fctn<- function(dir,bear){
    df_filter<-y%>%filter(dir==direction, Bearing_Rel==bear)
  
  (my_idx_touches <- st_intersects(st_buffer(df_filter,dist=100,endCapStyle = 'FLAT')))
  (my_igraph <- graph_from_adj_list(my_idx_touches))
  (my_components <- components(my_igraph)$membership)
  
  sf_ml3 <- df_filter %>% 
    group_by(section = as.character({{my_components}})) %>% 
    summarise()%>%
    mutate(row_id=row_number())
  
  pt2 <- st_cast(sf_ml3, "MULTIPOINT")%>%group_by(section)%>%mutate(id=row_number())%>%
    st_cast('POINT')
  pt2 <-sfc_as_cols(pt2)%>%select(-geometry)
  
  pt2<-pt2%>%group_by(row_id)%>%filter(y==max(y)|y==min(y))
  
  pt2 <-st_as_sf(pt2,coords=c("x","y")) %>% group_by(row_id) %>% summarize(m = mean(row_number())) %>% st_cast("LINESTRING")
  pt_touch<-pt2
  
  touch_df<<- rbind(touch_df,pt_touch)
  
  }
  
pmap(join_vec,touch_df_fctn)  



touch_df <- touch_df%>%mutate(row_id=row_number())%>%group_by(row_id)%>%mutate(geometry=st_extend_line(sf::st_geometry(geometry), -300))%>%ungroup()%>%
  mutate(row_id_coord=row_number())



####################################################################Old way  
  
  # ###only east west wells
  # if(nrow(east_west)==0 & nrow(north_south)>0){
  #   (my_idx_touches <- st_intersects(st_buffer(north_south,dist=100,endCapStyle = 'FLAT')))
  #   (my_igraph <- graph_from_adj_list(my_idx_touches))
  #   (my_components <- components(my_igraph)$membership)
  #   
  #   sf_ml3 <- north_south %>% 
  #     group_by(section = as.character({{my_components}})) %>% 
  #     summarise()%>%
  #     mutate(row_id=row_number())
  #   
  #   pt2 <- st_cast(sf_ml3, "MULTIPOINT")%>%group_by(section)%>%mutate(id=row_number())%>%
  #     st_cast('POINT')
  #   pt2 <-sfc_as_cols(pt2)%>%select(-geometry)
  #   
  #   pt2<-pt2%>%group_by(row_id)%>%filter(y==max(y)|y==min(y))
  #   
  #   pt2 <-st_as_sf(pt2,coords=c("x","y")) %>% group_by(row_id) %>% summarize(m = mean(row_number())) %>% st_cast("LINESTRING")
  #   pt<-pt2
  #     
  # }else if(nrow(east_west)>0 & nrow(north_south)==0){
  # (my_idx_touches <- st_intersects(st_buffer(east_west,dist=100,endCapStyle = 'FLAT')))
  # (my_igraph <- graph_from_adj_list(my_idx_touches))
  # (my_components <- components(my_igraph)$membership)
  # 
  # sf_ml3 <- east_west %>% 
  #   group_by(section = as.character({{my_components}})) %>% 
  #   summarise()%>%
  #   mutate(row_id=row_number())
  # 
  # pt2 <- st_cast(sf_ml3, "MULTIPOINT")%>%group_by(section)%>%mutate(id=row_number())%>%
  #   st_cast('POINT')
  # pt2 <-sfc_as_cols(pt2)%>%select(-geometry)
  # 
  # pt2<-pt2%>%group_by(row_id)%>%filter(y==max(y)|y==min(y))
  # 
  # pt2 <-st_as_sf(pt2,coords=c("x","y")) %>% group_by(row_id) %>% summarize(m = mean(row_number())) %>% st_cast("LINESTRING")
  # pt<-pt2
  # }else{
  # 
  #   (my_idx_touches <- st_intersects(st_buffer(north_south,dist=100,endCapStyle = 'FLAT')))
  #   (my_igraph <- graph_from_adj_list(my_idx_touches))
  #   (my_components <- components(my_igraph)$membership)
  #   
  #   sf_ml3 <- north_south %>% 
  #     group_by(section = as.character({{my_components}})) %>% 
  #     summarise()%>%
  #     mutate(row_id=row_number())
  #   
  #   pt2 <- st_cast(sf_ml3, "MULTIPOINT")%>%group_by(section)%>%mutate(id=row_number())%>%
  #     st_cast('POINT')
  #   pt2 <-sfc_as_cols(pt2)%>%select(-geometry)
  #   
  #   pt2<-pt2%>%group_by(row_id)%>%filter(y==max(y)|y==min(y))
  #   
  #   pt2 <-st_as_sf(pt2,coords=c("x","y")) %>% group_by(row_id) %>% summarize(m = mean(row_number())) %>% st_cast("LINESTRING")
  #   pt<-pt2
  #   
  #   (my_idx_touches <- st_intersects(st_buffer(east_west,dist=100,endCapStyle = 'FLAT')))
  #   (my_igraph <- graph_from_adj_list(my_idx_touches))
  #   (my_components <- components(my_igraph)$membership)
  #   
  #   sf_ml3 <- east_west %>% 
  #     group_by(section = as.character({{my_components}})) %>% 
  #     summarise()%>%
  #     mutate(row_id=row_number())
  #   
  #   pt2 <- st_cast(sf_ml3, "MULTIPOINT")%>%group_by(section)%>%mutate(id=row_number())%>%
  #     st_cast('POINT')
  #   pt2 <-sfc_as_cols(pt2)%>%select(-geometry)
  #   
  #   pt2<-pt2%>%group_by(row_id)%>%filter(y==max(y)|y==min(y))
  #   
  #   pt2 <-st_as_sf(pt2,coords=c("x","y")) %>% group_by(row_id) %>% summarize(m = mean(row_number())) %>% st_cast("LINESTRING")
  # 
  #   pt<-rbind(pt,pt2)
  # }
  # 
  #   
  # 
mround <-
  function(number, multiple)
    multiple * round(number / multiple)


pt=st_segments(touch_df)

print("move on")

pt<-st_as_sf(pt)%>%mutate(length=as.numeric(st_length(.)),id=row_number())


pt$Lat_TC = mround(pt$length / 5400, .25)

#split into 15000 length
a<-pt%>%filter((Lat_TC>4.9 & Lat_TC<6.5)|Lat_TC>7.9)



#split into 2 pieces Update this and next code to 3.25----
b<-pt%>%filter((Lat_TC>3 & Lat_TC<4.9))

#keep as is
c<-pt%>%filter(Lat_TC<3.25)

#split into 3 pieces
d<-pt%>%filter(Lat_TC>6.5 & Lat_TC%%3!=0 & Lat_TC<8)



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
  a_split<-split_lines(a,max_length=17500,id="id")
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
  combined_lines_split<-st_as_sf(combined_lines_split)
  
  
  final_lines<-combined_lines_split%>%mutate(row_id=row_number())
  
  vec=unique(final_lines$row_id)
  
  all_shp_small=data.frame()
  
  all_shp<-final_lines
  
  shorten_line=function(i){
    all_shp2=all_shp%>%filter(row_id==i)  
    all_shp2=st_as_sf(all_shp2)
    
    if(all_shp2$length<5000){
      all_shp2  
    }else{
      
      sf::st_geometry(all_shp2) <- st_extend_shorten_line(sf::st_geometry(all_shp2), -175)
    }
    
    all_shp2<-all_shp2%>%mutate(length=as.numeric(st_length(geometry)))
    
    print(i)
    
    all_shp_small<-rbind(all_shp_small,all_shp2)
    
  }
  
  final=lapply(vec,shorten_line)
  
  final=bind_rows(final)
  
  
  combined<- final%>%filter(length>4500)
  
  # combined<- combined%>%mutate(Status=ifelse(grepl("NonOperated",filterid),"NonOperated",
  #                                           ifelse(grepl("Unknown",filterid),"Unknown","Operated")))
  
  #combined<-combined%>%mutate(Company=sub("\\NonOperated.*", "", filterid))
  #combined<-combined%>%mutate(Company=sub("\\Operated*", "", filterid))
  #combined<-combined%>%mutate(Company=sub("\\Unknown.*", "", filterid))
  
  if(nrow(combined)==0){
    print("next zone")
  }else{
  
  combined<-combined%>%mutate(reservoir=sub(".*\\s", "", filterid))
  
  combined<-combined%>%mutate(reservoir=ifelse(reservoir=='A','WFMP A',ifelse(reservoir=='B','WFMP B',
                                                                              ifelse(reservoir=='C','WFMP C',
                                                                                     ifelse(reservoir=='SD','WFMP A SD',
                                                                                            ifelse(reservoir=='SH','WFMP A SH',
                                                                                                   ifelse(reservoir=='D','WFMP D',
                                                                              reservoir)))))))
  #tier_boundry<-tier_outlines%>%filter(LZ==unique(combined$reservoir))
  
  combined<-combined%>%mutate(row_id=row_number())
  
  #combined_tier1<-st_intersection(combined,tier_boundry)
  #combined_tier1<-st_drop_geometry(combined_tier1)
  #combined<-combined%>%mutate(Tier=ifelse(row_id %in% c(combined_tier1$row_id),"Tier1","Tier_3"))
  
  combined$Status=filterid
  
  combined$Company=Company_Add
  starting_tbl<<-rbind(starting_tbl,combined)
  
  
  
  print(filterid)
  }
  print(filterid)
  }
}
}


sfc_as_cols <- function(x, geometry, names = c("x", "y")) {
  if (missing(geometry)) {
    geometry <- sf::st_geometry(x)
  } else {
    geometry <- rlang::eval_tidy(enquo(geometry), x)
  }
  stopifnot(inherits(x, "sf") &&
              inherits(geometry, "sfc_POINT"))
  ret <- sf::st_coordinates(geometry)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[, !names(x) %in% names]
  ret <- setNames(ret, names)
  dplyr::bind_cols(x, ret)
}



lapply(vec_filter,final_combine_split_fctn)



#starting_tbl<-starting_tbl%>%mutate(Company="Endeavor")
#starting_tbl%>%filter(reservoir=='WFMP D')%>%distinct(Tier)

starting_tbl2<-starting_tbl%>%mutate(Status2=ifelse(grepl("NonOperated",Status),"NonOperated",
                                                   ifelse(grepl("Unknown",Status),"Unknown",
                                                          ifelse(grepl("([+])",Status),"Operated+",
                                                          "Operated"))))


starting_tbl2<-starting_tbl2%>%mutate(Company=gsub("NonOperated.*","",Company),
                                      Company=gsub("Unknown.*","",Company),
                                      Company=gsub("Operated.*","",Company),
                                      Company=str_trim(Company))

write_sf(starting_tbl2,"Final/Combined/combined_inventory_4.21.2024.shp")

#glimpse(starting_tbl2)
###COP ADJUST

######UNHASH ON NEXT BIG UPDATE OF COP!!!!!! 
# y=starting_tbl2%>%filter(!(grepl("CFILIP_BRNT",Company) & reservoir != 'BRNT'))%>%
#   mutate(Status2=ifelse(Company=='CFILIP_BRNT' & Status2=='Operated',"Unknown",
#                         ifelse(Company=='CFILIP_BRNT' & Status2=='Unknown',"Operated",
#                                Status2)))%>%
#   filter( !(grepl("CONOCOPHILLIPS",Company) & reservoir == 'BRNT'))%>%
#   mutate(Company=ifelse(Company=='CFILIP_BRNT','CONOCOPHILLIPS',Company))
# 
# write_sf(y,"Final/Combined/Combined_Company_Inventoryv2.shp")



#write_sf(starting_tbl2,"Final/Combined/Combined_Company_Inventory.shp")


#fwrite(st_drop_geometry(starting_tbl2),"Final/Combined/Combined_Inventory_mew_8.14v3.csv")

st_drop_geometry(starting_tbl2)%>%group_by(reservoir)%>%
  summarise(count=max(row_number()),length=sum(length))

  # 
# update<- read_sf("Final/Combined/Combined_Inventory_7.12v2.shp")
# 
# original<- read_sf("Final/Combined/Combined_Inventory_7.12.shp")
# 
# 
# 
# original<-original%>%rename(orig_row_id=row_id,orig_reservoir=reservoir)%>%select(orig_row_id,orig_reservoir)
# original<-st_buffer(original,dist=50,endCapStyle = 'FLAT')
# 
# update<-update%>%mutate(row_id=row_number())
# update_int<-st_intersection(update,original)
# 
# update_int<-update_int%>%filter(reservoir==orig_reservoir)%>%distinct(row_id,.keep_all=T)
# 
# 
# 
# st_drop_geometry(update_int)%>%group_by(reservoir)%>%
#   summarise(count=max(row_number()))
# 
# fwrite(st_drop_geometry(update_int),'update.csv')

#x=read_sf("Op_Non_Unknown_Acreage/Combined_Acreage/Combined_Acreage.shp")

# write_sf(combined_acreage,'civitas.shp')
# 
# plotly::ggplotly(ggplot(Combined_Acreage%>%filter(grepl('CONOCOPHILLIPS',Company))%>%
#                           summarize(geometry = st_union(geometry)          
#                         ))+geom_sf(fill='grey')+geom_sf(data=starting_tbl2,color='red')+theme_minimal())
