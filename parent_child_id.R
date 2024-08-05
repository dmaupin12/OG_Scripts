## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(fig.align = 'center',echo=T,warning=F,message=F,asis=F)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(sf)
library(rgdal)
library(tidyverse)
library(openxlsx)
library(sp)
library(proj4)
library(tidyr)
library(lubridate)
library(data.table)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
wells<- data.table::fread("master_survey_update.csv")
#wells<- wells%>%filter(grepl("ARICK-HOOPER UNIT",WELLNAMENUMBER))
#wells<- wells%>%filter(WELLNAMENUMBER=="33")

wells_survey_header_keep<-wells

wells<- wells%>% group_by(UWI)%>% mutate(Count=row_number())

wells$COMPLETION_DATE<- as.Date(wells$COMPLETION_DATE, format =  "%m/%d/%Y %H:%M:%S")

wells%>%as_tibble()%>%
  glimpse()

left = function (string,char){ substr(string,1,char) }



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
sfc_as_cols <- function(x, geometry, names = c("x","y")) {
  if (missing(geometry)) {
    geometry <- sf::st_geometry(x)
  } else {
    geometry <- rlang::eval_tidy(enquo(geometry), x)
  }
  stopifnot(inherits(x,"sf") && inherits(geometry,"sfc_POINT"))
  ret <- sf::st_coordinates(geometry)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
wells <- wells %>% rename(longitude = STATION_X, latitude = STATION_Y)

wells <-
  wells %>% arrange(UWI, Count) %>% filter(!is.na(longitude) |
                                                 is.na(latitude)) %>%
  group_by(UWI) %>%
  mutate(WellCount = dense_rank(row_number())) %>% filter(max(WellCount) >
                                                            1)

wells <- wells %>% filter(!is.na(longitude))

#wells<- wells%>% group_by(UWI)%>% mutate(MeanTVD= mean(TVD))

projcrs <- "+init=epsg:32039"

surface_wells <-
  wells %>% filter(WellCount == 1) %>% ungroup() %>% select(UWI, longitude, latitude)
surface_wells <-
  surface_wells %>% filter(!is.na(longitude), !is.na(latitude))

surface_wells <- st_as_sf(surface_wells,
                          coords = c("longitude", "latitude"),
                          crs = projcrs)

#st_crs(surface_wells) <- "+proj=longlat +datum=WGS84"
surface_wells <- st_transform(surface_wells, "+init=epsg:32039")

surface_wells <- sfc_as_cols(surface_wells)
surface_wells <- st_drop_geometry(surface_wells)
colnames(surface_wells)[2] <- 'surf_longitude'
colnames(surface_wells)[3] <- 'surf_latitude'


bottom_wells <-
  wells %>% filter(WellCount == 2) %>% ungroup() %>% select(UWI, longitude, latitude)
bottom_wells <-
  bottom_wells %>% filter(!is.na(longitude), !is.na(latitude))
bottom_wells <- st_as_sf(bottom_wells,
                         coords = c("longitude", "latitude"),
                         crs = projcrs)

#st_crs(bottom_wells) <- "+proj=longlat +datum=WGS84"
bottom_wells <- st_transform(bottom_wells, "+init=epsg:32039")

bottom_wells <- sfc_as_cols(bottom_wells)
bottom_wells <- st_drop_geometry(bottom_wells)
colnames(bottom_wells)[2] <- 'bh_longitude'
colnames(bottom_wells)[3] <- 'bh_latitude'



wells <- left_join(wells, surface_wells)
wells <- left_join(wells, bottom_wells)

wells <-
  wells %>% mutate(WellborePosition = ifelse(
    abs(surf_longitude - bh_longitude) > abs(surf_latitude - bh_latitude),
    "East/West",
    "North/South"
  ))

wells <-
  wells %>% mutate(Direction = ifelse(
    WellborePosition == 'North/South',
    ifelse(
      surf_latitude > bh_latitude,
      'SouthTrajectory',
      'NorthTrajectory'
    ),
    ifelse(
      WellborePosition == 'East/West',
      ifelse(
        surf_longitude > bh_longitude,
        'WestTrajectory',
        'EastTrajectory'
      ),
      NA
    )
  ))


######Make all well trajectories north to south and west to east
wells <- wells %>% group_by(UWI) %>%
  arrange(ifelse(grepl("North|West", Direction), desc(WellCount), WellCount)) %>%
  arrange(UWI) %>% mutate(WellCount = row_number())

wells <- wells %>% filter(!is.na(longitude), !is.na(latitude))
lines <- st_as_sf(wells,
                  coords = c("longitude", "latitude"),
                  crs = projcrs)



lines <- lines %>%
  group_by(UWI) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")




###rename geometry
coordinates(bottom_wells) <- ~ bh_longitude + bh_latitude
bottom_wells <- st_as_sf(bottom_wells)
colnames(bottom_wells)[2] = 'bottom_geometry'
st_geometry(bottom_wells) = 'bottom_geometry'


coordinates(surface_wells) <- ~ surf_longitude + surf_latitude
surface_wells <- st_as_sf(surface_wells)
colnames(surface_wells)[2] = 'surface_geometry'
st_geometry(surface_wells) = 'surface_geometry'



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
lines <- lines %>%ungroup()%>% mutate(length_Est = st_length(lines))

lines$length_Est <- as.numeric(lines$length_Est)
lines$UWI10 <- left(lines$UWI, 10)
lines <-
  lines %>% group_by(UWI) %>% filter(length_Est == max(length_Est)) %>% ungroup()
lines <- lines %>% select(-UWI10, -length_Est)


well_header <- wells[, c(1, 3:5, 20, 22, 24:28)]

well_header <-
  wells %>% select(UWI,
                   OPERATOR_SHORT,
                   WELLNAMENUMBER,
                   WELLNUMBER,COUNTY,
                   meanTVD,
                   surf_longitude:Direction)



well_header <-
  well_header %>% ungroup() %>% distinct(UWI, .keep_all = T)


well_header <- inner_join(well_header, st_drop_geometry(lines))


lines <- inner_join(lines, well_header)

st_crs(lines) <- "+init=epsg:32039"
lines <- st_transform(lines, "+proj=longlat +datum=WGS84")


lines <- lines %>% mutate(length_Est = st_length(lines))
lines$length_Est <- as.numeric(lines$length_Est)

lines$UWI10 <- left(lines$UWI, 10)
lines <-
  lines %>% group_by(UWI) %>% filter(length_Est == max(length_Est)) %>% ungroup()

lines <- lines %>% select(-UWI10, -length_Est)

gbd <- lines

gbd$UWI10 <- left(gbd$UWI, 10)


#write_sf(gbd, 'S:\\Maupin\\CI Project\\permian_lines_gbd.shp')
gbd <- st_transform(gbd, "+init=epsg:32039")

gbd <- gbd %>% mutate(length = st_length(gbd))

gbd <-
  left_join(
    gbd,
    wells %>% select(UWI, COMPLETION_DATE) %>% mutate(year = lubridate::year(COMPLETION_DATE)) %>%
      distinct(UWI, .keep_all = T)
  )

#write_sf(gbd, 'Survey_Sticks/permian_lines.shp')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
lines$Spacing <- 1500*2
lines$SpacingGeo <- lines$Spacing / 1.984127


#st_crs(lines) <- "+proj=longlat +datum=WGS84"

lines <- lines %>% ungroup()

lines.shp <- st_transform(lines, "+init=epsg:32039")

lines.shp <- st_transform(lines.shp, "+init=epsg:32039")
lines_bearing_Orig <- lines.shp


lines.shp <- lines.shp %>% mutate(Valid_Lines = as.numeric(st_length(geometry)))
lines.shp <- lines.shp %>% filter(Valid_Lines > 0)
lines.shp <- lines.shp %>% select(-Valid_Lines)
lines.shp <- as(lines.shp, "Spatial")

hz_buff <-
  rgeos::gBuffer(
    lines.shp,
    width = lines.shp$SpacingGeo,
    capStyle = "FLAT",
    byid = T
  )

hz_buff <- st_as_sf(hz_buff)

hz_buff$UWI10 <- left(hz_buff$UWI, 10)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
midpoint<- maptools::SpatialLinesMidPoints(lines.shp)
midpoint<- st_as_sf(midpoint)

midpoint<- midpoint[,1]

colnames(midpoint)[2]= 'midpoint_geometry'
st_geometry(midpoint)= 'midpoint_geometry'


midpoint_tbl<- left_join(midpoint,wells%>%distinct(UWI,.keep_all = T))

midpoint_tbl<-st_as_sf(midpoint_tbl)
midpoint_spdf<- sfc_as_cols(midpoint_tbl)%>%select(x,y)

midpoint_spdf<-st_drop_geometry(midpoint_spdf)

# clstx<-leaderCluster::leaderCluster(midpoint_spdf,1.609344,max_iter = 15, distance = 'haversine')
# 
# id<-as.data.frame(clstx$cluster_id)
# id<-id%>%mutate(RowNum=row_number())
# id$`clstx$cluster_id`<- as.character(id$`clstx$cluster_id`)
midpoint_tbl<- wells%>%ungroup()%>%mutate(RowNum= row_number())


#write_sf(midpoint_tbl,"midpoint_tbl.shp")



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
hz_buff2 <- hz_buff

hz_buff2 <- hz_buff2 %>% mutate(Area = st_area(hz_buff2))


well_intersect <- st_intersection(hz_buff2, hz_buff2)



well_intersect <-
  well_intersect %>% rename(PointInPolyUWI = UWI.1)

well_intersect <-
  well_intersect %>% filter(UWI != PointInPolyUWI)

well_intersect <- well_intersect %>%
  mutate(IntersectArea = st_area(.) %>% as.numeric())

well_intersect$Area <- as.numeric(well_intersect$Area)

well_intersect <-
  well_intersect %>% mutate(PercIntersect = IntersectArea / Area)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
well_intersect<- well_intersect%>%mutate(PercIntersect= IntersectArea/Area)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
well_intersect$TVDDiff <-
  abs(well_intersect$meanTVD - well_intersect$meanTVD.1)

#well_intersect_DEPTH<-well_intersect%>% group_by(UWI)%>% filter(TVDDiff<1000)

well_intersect_DEPTH <- st_drop_geometry(well_intersect)

lines <- st_transform(lines, "+init=epsg:32039")


well_intersect_DEPTH <- left_join(well_intersect_DEPTH, lines[, 1])
well_intersect_DEPTH <-
  well_intersect_DEPTH %>% filter(UWI != PointInPolyUWI)
well_intersect_DEPTH <- unique(well_intersect_DEPTH)

join_lines <- lines[, 1]
dist_output <-
  left_join(
    well_intersect_DEPTH %>% as.data.frame(),
    join_lines %>% as.data.frame(),
    by = c('PointInPolyUWI' = 'UWI')
  )

dist_output <- dist_output %>% mutate(Line_ID = row_number())

dist_outputf <-
  dist_output %>% ungroup() %>% mutate(Dist = st_distance(geometry.x, geometry.y, by_element = T))

dist_outputf <-
  dist_outputf %>% group_by(UWI, PointInPolyUWI) %>% distinct(Dist, .keep_all = T)

dist_outputf <-
  dist_outputf %>% filter(WellborePosition == WellborePosition.1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
###add midpoint distance to offset line
dist_outputf <-
  left_join(
    dist_outputf %>% as.data.frame(),
    midpoint %>% as.data.frame(),
    by = c('PointInPolyUWI' = 'UWI')
  )


dist_outputf <-
  dist_outputf %>% ungroup() %>% mutate(Dist_Midpoint = st_distance(midpoint_geometry, geometry.x, by_element = T))

dist_outputfinal <-
  dist_outputf %>% group_by(UWI, PointInPolyUWI) %>% distinct(Dist, .keep_all = T)
dist_outputfinal <-
  dist_outputfinal %>% filter(!(
    paste(WELLNAMENUMBER, WELLNUMBER) == paste(WELLNAMENUMBER.1, WELLNUMBER.1)
  ))


join_lines <-
  join_lines %>% ungroup() %>% mutate(length_Est = st_length(join_lines, by_element =
                                                               T))
join_lines <- st_drop_geometry(join_lines)
dist_outputfinal <- left_join(dist_outputfinal, join_lines)
dist_outputfinal$length_Est <- as.numeric(dist_outputfinal$length_Est)
#Add Offset Length Estimate

dist_outputfinal <-
  left_join(dist_outputfinal,
            join_lines,
            by = c("PointInPolyUWI" = "UWI"))

dist_outputfinal <-
  dist_outputfinal %>% rename(length_est = length_Est.x, length_est_offset =
                                length_Est.y)

dist_outputfinal$length_est_offset <-
  as.numeric(dist_outputfinal$length_est_offset)


dist_outputfinal$UWI10 <- left(dist_outputfinal$UWI, 10)
dist_outputfinal$PointInPolyUWI10 <-
  left(dist_outputfinal$PointInPolyUWI, 10)
dist_outputfinal <-
  dist_outputfinal %>% ungroup() %>% mutate(RowID = row_number())


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
dist_outputfinal <-
  dist_outputfinal %>% group_by(UWI) %>% filter(length_est == max(length_est)) %>%
  ungroup()

dist_outputfinalxlsx <- dist_outputfinal%>%select(!c(geometry.x,geometry.y,midpoint_geometry))


st_crs(midpoint) <- "+init=epsg:32039"
midpoint <- st_transform(midpoint, "+proj=longlat +datum=WGS84")
midpointcols <- sfc_as_cols(midpoint)
midpointcols <- st_drop_geometry(midpointcols)

dist_outputfinalxlsx <- left_join(dist_outputfinalxlsx, midpointcols)
dist_outputfinalxlsx <-
  left_join(dist_outputfinalxlsx,
            midpointcols,
            by = c("PointInPolyUWI" = "UWI"))

dist_outputfinalxlsx <-
  dist_outputfinalxlsx %>% mutate(Offset_Direction_Wellbore = ifelse(
    WellborePosition == "North/South",
    ifelse(x.x > x.y, "East", "West"),
    ifelse(
      WellborePosition == "East/West",
      ifelse(y.x > y.y, "North", "South"),
      "na"
    )
  ))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------

tst <- dist_outputfinalxlsx

tst <-
  left_join(tst,
            tst%>%select(UWI,PointInPolyUWI,Dist,Dist_Midpoint),
            by = c("UWI" = "PointInPolyUWI", "PointInPolyUWI" = "UWI"))


tst <-
  tst %>% mutate(
    Dist.x = ifelse(Dist.y < Dist.x, Dist.y, Dist.x),
    Dist_Midpoint.x = ifelse(
      Dist_Midpoint.y < Dist_Midpoint.x,
      Dist_Midpoint.y,
      Dist_Midpoint.x
    )
  )

tst <- tst %>% rename(Dist = Dist.x, Dist_Midpoint = Dist_Midpoint.x)

tst <- tst %>% select(-c(Dist.y, Dist_Midpoint.y))



tst <-
  tst %>% ungroup() %>% mutate(
    c.MasterDistance = ifelse(Dist_Midpoint > Dist * 1.7, Dist, Dist_Midpoint),
    c.hypotenuse = sqrt((abs(TVDDiff) ^ 2 + c.MasterDistance ^ 2))
  )

tst$UWI <- as.character(tst$UWI)
tst$PointInPolyUWI <- as.character(tst$PointInPolyUWI)

#write.xlsx(tst,'Master_Line_Spacing.xlsx')



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
lines_bearing_Orig <- lines_bearing_Orig[, 1]
lines_bearing <- lines_bearing_Orig



surf_point_arr_geo = st_line_sample(lines_bearing_Orig, sample = c(0))
surf_point_arr <- do.call(rbind, st_geometry(surf_point_arr_geo)) %>%
  as_tibble() %>% setNames(c("surface_long_bearing", "surface_lat_bearing"))

surf_point_arr <- surf_point_arr %>% mutate(RowId = row_number())

surf_point_arr <- st_as_sf(
  surf_point_arr,
  coords = c("surface_long_bearing", "surface_lat_bearing"),
  crs = "+init=epsg:32039"
)

st_crs(surf_point_arr) <- "+init=epsg:32039"
surf_point_arr <- st_transform(surf_point_arr, "+init=epsg:32039")

surf_point_arr <-
  surf_point_arr <- do.call(rbind, st_geometry(surf_point_arr)) %>%
  as_tibble() %>% setNames(c("surface_long_bearing", "surface_lat_bearing"))

surf_point_arr <- surf_point_arr %>% mutate(RowId = row_number())


bottom_point_arr_geo = st_line_sample(lines_bearing_Orig, sample = c(1))
bottom_point_arr <-
  do.call(rbind, st_geometry(bottom_point_arr_geo)) %>%
  as_tibble() %>% setNames(c("bh_long_bearing", "bh_lat_bearing"))
bottom_point_arr <- bottom_point_arr %>% mutate(RowId = row_number())

bottom_point_arr <-
  bottom_point_arr <-
  st_as_sf(
    bottom_point_arr,
    coords = c("bh_long_bearing", "bh_lat_bearing"),
    crs = "+init=epsg:32039"
  )

st_crs(bottom_point_arr) <- "+init=epsg:32039"
bottom_point_arr <- st_transform(bottom_point_arr, "+init=epsg:32039")

bottom_point_arr <-
  bottom_point_arr <- do.call(rbind, st_geometry(bottom_point_arr)) %>%
  as_tibble() %>% setNames(c("bh_long_bearing", "bh_lat_bearing"))

bottom_point_arr <- bottom_point_arr %>% mutate(RowId = row_number())



lines_bearing <- lines_bearing[, 1] %>% mutate(RowId = row_number())


wells_bearing <- left_join(lines_bearing, surf_point_arr)

wells_bearing <- left_join(wells_bearing, bottom_point_arr)


wells_bearing$UWI <- as.character(wells_bearing$UWI)



bearing_df <-
  tst %>% select(UWI, PointInPolyUWI) %>% left_join(wells_bearing) %>%
  distinct()
bearing_df <-
  bearing_df %>% left_join(wells_bearing, by = c("PointInPolyUWI" = "UWI")) %>%
  distinct()


bearing_df <- bearing_df %>% left_join(tst%>%select(UWI, WellborePosition.1,length_est)) %>% distinct()

rm(list = setdiff(
  ls(),
  c(
    "bearing_df",
    "tst",
    "lines_bearing_Orig",
    "midpoint",
    "sfc_as_cols",
    "dist_outputfinalxlsx",
    "wells_survey_header_keep",
    "gbd"
  )
))

bearing_df <-
  bearing_df %>% group_by(row_number()) %>% mutate(
    PercOverlap = ifelse(
      raster::pointDistance(
        c(surface_long_bearing.y, surface_lat_bearing.y),
        c(bh_long_bearing.x, bh_lat_bearing.x),
        lonlat = F
      ) >
        raster::pointDistance(
          c(surface_long_bearing.x, surface_lat_bearing.x),
          c(bh_long_bearing.y, bh_lat_bearing.y),
          lonlat = F
        ),
      raster::pointDistance(
        c(surface_long_bearing.x, surface_lat_bearing.x),
        c(bh_long_bearing.y, bh_lat_bearing.y),
        lonlat = F
      ),
      raster::pointDistance(
        c(surface_long_bearing.y, surface_lat_bearing.y),
        c(bh_long_bearing.x, bh_lat_bearing.x),
        lonlat = F
      )
    ) / length_est
  )


bearing_df <-
  bearing_df %>% mutate(PercOverlap = ifelse(PercOverlap > 1, 1, PercOverlap))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
bearing_df <-
  bearing_df %>% ungroup() %>% group_by(row_number()) %>% mutate(
    Dist_Hyp_Angle = ifelse(
      raster::pointDistance(
        c(surface_long_bearing.y, surface_lat_bearing.y),
        c(bh_long_bearing.x, bh_lat_bearing.x),
        lonlat = F
      ) >
        raster::pointDistance(
          c(surface_long_bearing.x, surface_lat_bearing.x),
          c(bh_long_bearing.y, bh_lat_bearing.y),
          lonlat = F
        ),
      raster::pointDistance(
        c(surface_long_bearing.x, surface_lat_bearing.x),
        c(bh_long_bearing.y, bh_lat_bearing.y),
        lonlat = F
      ),
      raster::pointDistance(
        c(surface_long_bearing.y, surface_lat_bearing.y),
        c(bh_long_bearing.x, bh_lat_bearing.x),
        lonlat = F
      )
    )
  )


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
lines_bearing_Orig <- lines_bearing_Orig[, 1]
lines_bearing <- lines_bearing_Orig



surf_point_arr_geo = st_line_sample(lines_bearing_Orig, sample = c(0))
surf_point_arr <- do.call(rbind, st_geometry(surf_point_arr_geo)) %>%
  as_tibble() %>% setNames(c("surface_long_bearing", "surface_lat_bearing"))

surf_point_arr <- surf_point_arr %>% mutate(RowId = row_number())

surf_point_arr <- st_as_sf(
  surf_point_arr,
  coords = c("surface_long_bearing", "surface_lat_bearing"),
  crs = "+init=epsg:32039"
)

st_crs(surf_point_arr) <- "+init=epsg:32039"
surf_point_arr <-
  st_transform(surf_point_arr, "+proj=longlat +datum=WGS84")

surf_point_arr <-
  surf_point_arr <- do.call(rbind, st_geometry(surf_point_arr)) %>%
  as_tibble() %>% setNames(c("surface_long_bearing", "surface_lat_bearing"))

surf_point_arr <- surf_point_arr %>% mutate(RowId = row_number())


bottom_point_arr_geo = st_line_sample(lines_bearing_Orig, sample = c(1))
bottom_point_arr <-
  do.call(rbind, st_geometry(bottom_point_arr_geo)) %>%
  as_tibble() %>% setNames(c("bh_long_bearing", "bh_lat_bearing"))
bottom_point_arr <- bottom_point_arr %>% mutate(RowId = row_number())

bottom_point_arr <-
  bottom_point_arr <-
  st_as_sf(
    bottom_point_arr,
    coords = c("bh_long_bearing", "bh_lat_bearing"),
    crs = "+init=epsg:32039"
  )

st_crs(bottom_point_arr) <- "+init=epsg:32039"
bottom_point_arr <-
  st_transform(bottom_point_arr, "+proj=longlat +datum=WGS84")

bottom_point_arr <-
  bottom_point_arr <- do.call(rbind, st_geometry(bottom_point_arr)) %>%
  as_tibble() %>% setNames(c("bh_long_bearing", "bh_lat_bearing"))

bottom_point_arr <- bottom_point_arr %>% mutate(RowId = row_number())



lines_bearing <- lines_bearing[, 1] %>% mutate(RowId = row_number())


wells_bearing <- left_join(lines_bearing, surf_point_arr)

wells_bearing <- left_join(wells_bearing, bottom_point_arr)


wells_bearing$UWI <- as.character(wells_bearing$UWI)

wells_bearing <- st_drop_geometry(wells_bearing)
wells_bearing <- wells_bearing %>% select(-RowId)

wells_bearing <- wells_bearing%>%rename(surface_long_bearing_wgs=surface_long_bearing,surface_lat_bearing_wgs=surface_lat_bearing ,bh_long_bearing_wgs=bh_long_bearing,bh_lat_bearing_wgs=bh_lat_bearing)


bearing_df <- bearing_df %>% left_join(wells_bearing) %>% distinct()
bearing_df <-
  bearing_df %>% left_join(wells_bearing, by = c("PointInPolyUWI" = "UWI")) %>%
  distinct()


bearing_df <-
  bearing_df %>% ungroup() %>% group_by(row_number()) %>% mutate(
    BearingRhumb = ifelse(
      raster::pointDistance(
        c(surface_long_bearing.y, surface_lat_bearing.y),
        c(bh_long_bearing.x, bh_lat_bearing.x),
        lonlat = F
      ) >
        raster::pointDistance(
          c(surface_long_bearing.x, surface_lat_bearing.x),
          c(bh_long_bearing.y, bh_lat_bearing.y),
          lonlat = F
        ),
      geosphere::bearingRhumb(
        c(surface_long_bearing_wgs.x, surface_lat_bearing_wgs.x),
        c(bh_long_bearing_wgs.y, bh_lat_bearing_wgs.y)
      ),
      geosphere::bearingRhumb(
        c(surface_long_bearing_wgs.y, surface_lat_bearing_wgs.y),
        c(bh_long_bearing_wgs.x, bh_lat_bearing_wgs.x)
      )
    )
  )


bearing_df <- bearing_df %>% ungroup() %>% group_by(row_number()) %>%
  mutate(BearingLine = geosphere::bearingRhumb(
    c(surface_long_bearing_wgs.x, surface_lat_bearing_wgs.x),
    c(bh_long_bearing_wgs.x, bh_lat_bearing_wgs.x)
  ))

bearing_df <-
  bearing_df %>% ungroup() %>% mutate(Angle = 180 - abs(((
    BearingLine - BearingRhumb
  ) + 360) - 360))


bearing_df <-
  bearing_df %>% ungroup() %>% mutate(Angle = 180 - abs(((
    BearingLine - BearingRhumb
  ) + 360) - 360))


bearing_df <-
  bearing_df %>% ungroup() %>% mutate(Angle = 90 - abs(((
    BearingLine - BearingRhumb
  ) + 360) - 360))

bearing_df <-
  bearing_df %>% ungroup() %>% mutate(TriHeightOpp = (sin(Angle * (pi / 180)) *
                                                        Dist_Hyp_Angle) / length_est)


bearing_df$PercOverlap <- bearing_df$TriHeightOpp


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
midpoint_off<- midpoint
st_crs(midpoint_off) <- "+proj=longlat +datum=WGS84"
midpoint_off<-st_transform(midpoint_off,"+init=epsg:32039")
midpoint_off$UWI= as.character(midpoint_off$UWI)
midpoint_off<- sfc_as_cols(midpoint_off,names=c("md_long","md_lat"))

midpoint_off<- st_drop_geometry(midpoint_off)

tst<-tst%>%left_join(midpoint_off,by=c("PointInPolyUWI"="UWI"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------

tst<-tst%>%left_join(bearing_df%>%select(
UWI, PointInPolyUWI,PercOverlap))%>%distinct()


tst<-tst%>%mutate(PercOverlap=ifelse(PercOverlap>1,1,PercOverlap))


tst<-tst%>%distinct(UWI,PointInPolyUWI,.keep_all = T)

tst<- tst%>%filter(PercOverlap>.2, PercIntersect>.05)

#tst<-tst%>%select(-midpoint_geometry)


#data.table::fwrite(tst,'PRADIP.csv')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
zone<- fread("zone.csv")
zone<- zone%>%mutate(Reservoir=
                        ifelse(LANDING_ZONE=="ATOKA","ATOKA",
 ifelse(LANDING_ZONE=="AVLN LWR","AVLN",
 ifelse(LANDING_ZONE=="AVLN MID","AVLN",
 ifelse(LANDING_ZONE=="AVLN UPR","AVLN",
 ifelse(LANDING_ZONE=="BLCN","BLCN",
 ifelse(LANDING_ZONE=="BRNT","BRNT",
 ifelse(LANDING_ZONE=="BYCN","BYCN",
 ifelse(LANDING_ZONE=="CBP","CBP",
 ifelse(LANDING_ZONE=="CHKY SD","CHKY",
 ifelse(LANDING_ZONE=="CLFK","CLFK",
 ifelse(LANDING_ZONE=="CYCN","CYCN",
 ifelse(LANDING_ZONE=="DEAN","DEAN",
 ifelse(LANDING_ZONE=="DEM","DEM",
 ifelse(LANDING_ZONE=="DLWR","DLWR",
 ifelse(LANDING_ZONE=="DO_NOT_CODE","DO",
 ifelse(LANDING_ZONE=="DVNN","DVNN",
 ifelse(LANDING_ZONE=="EBGR","EBGR",
 ifelse(LANDING_ZONE=="FBSG SD","FBSG",
 ifelse(LANDING_ZONE=="FBSG SH","FBSG",
 ifelse(LANDING_ZONE=="GRWS","GRWS",
 ifelse(LANDING_ZONE=="JMILL","JMILL",
 ifelse(LANDING_ZONE=="LSBY","LSBY",
 ifelse(LANDING_ZONE=="MSBY","MSBY",
 ifelse(LANDING_ZONE=="n.a.","n",
 ifelse(LANDING_ZONE=="OUT_OF_DB_BASIN","OUT",
 ifelse(LANDING_ZONE=="PENN SH","PENN",
 ifelse(LANDING_ZONE=="PRESENT DAY","PRESENT",
 ifelse(LANDING_ZONE=="QUEEN","QUEEN",
 ifelse(LANDING_ZONE=="SBSG LWR SH","SBSG",
 ifelse(LANDING_ZONE=="SBSG SD LWR","SBSG",
 ifelse(LANDING_ZONE=="SBSG SD UPR","SBSG",
 ifelse(LANDING_ZONE=="SHELF","SHELF",
 ifelse(LANDING_ZONE=="STRN","STRN",
 ifelse(LANDING_ZONE=="TBSG UPR SD","TBSG",
 ifelse(LANDING_ZONE=="TBSG V SD","TBSG",
 ifelse(LANDING_ZONE=="TBSG W SD","TBSG",
 ifelse(LANDING_ZONE=="USBY","USBY",
 ifelse(LANDING_ZONE=="WDFD","WDFD",
 ifelse(LANDING_ZONE=="WFMP","WFMP",
 ifelse(LANDING_ZONE=="WFMP A","WFMP A",
 ifelse(LANDING_ZONE=="WFMP A SD","WFMP A",
 ifelse(LANDING_ZONE=="WFMP A SH LWR","WFMP A",
 ifelse(LANDING_ZONE=="WFMP A SH UPR","WFMP A",
 ifelse(LANDING_ZONE=="WFMP B","WFMP B",
 ifelse(LANDING_ZONE=="WFMP B LWR","WFMP B",
 ifelse(LANDING_ZONE=="WFMP B UPR","WFMP B",
 ifelse(LANDING_ZONE=="WFMP C","WFMP C",
 ifelse(LANDING_ZONE=="BS1S","FBSG",LANDING_ZONE)))))))))))))))))))))))))))))))))))))))))))))))))

 zone<- zone%>%mutate(Reservoir=ifelse(LANDING_ZONE=="BS2S","SBSG",      
 ifelse(LANDING_ZONE=="BS1S","FBSG",
 ifelse(LANDING_ZONE=="BS1SH","FBSG",       
 ifelse(LANDING_ZONE=="BS3C","BS3C",
 ifelse(LANDING_ZONE=="BS3S","TBSG",
 ifelse(LANDING_ZONE=="WFMP SD","WFMP A",
 ifelse(LANDING_ZONE=="JOMILL","JMILL",
 ifelse(LANDING_ZONE=="WFMP UNC","WFMP A",LANDING_ZONE)))))))))


 
left = function (string,char){ substr(string,1,char) }

gbd<-left_join(gbd%>%mutate(API10=left(UWI,10)),zone)



write_sf(gbd,"BD_ALL_WELLS.shp",append=F)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#zone<- zone%>%select(API10,Reservoir)
#zone$API10=as.character(zone$API10)

zone$UWI<-as.character(zone$UWI)
tst<- tst%>%left_join(zone,by=c("UWI"="UWI"))
tst<- tst%>%left_join(zone,by=c("PointInPolyUWI"="UWI"))


tst<- tst%>%rename(Zone=Reservoir.x,Zone.1=Reservoir.y)

tst<- rename(tst,c.Hypotenuse=c.hypotenuse)

tst$WELLNUMBER<- as.character(tst$WELLNUMBER)
tst$WELLNUMBER.1<- as.character(tst$WELLNUMBER.1)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
tst<- tst%>%mutate(Offset_Direction_Wellbore=ifelse(Direction %in% c("NorthTrajectory","WestTrajectory"),ifelse(((bh_longitude-surf_longitude)*(md_lat-surf_latitude))-
    ((bh_latitude-surf_latitude)*(md_long-surf_longitude))<0,"Right","Left"),ifelse(Direction %in% c("SouthTrajectory","EastTrajectory"),ifelse(((bh_longitude-surf_longitude)*(md_lat-surf_latitude))-
    ((bh_latitude-surf_latitude)*(md_long-surf_longitude))<0,"Left","Right"),'na')))

#writexl::write_xlsx(tst,'Spacing_Output/Master_Line_Spacing.xlsx')

#tst<- read.xlsx("Spacing_Output/Master_Line_Spacing.xlsx")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
basin_outline<- read_sf("Basin_outline/BASIN_OUTLINE.shp")
settings<- read.xlsx("spacing_settings_filter/spacing_settings_filter.xlsx")



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
tst <-
  tst %>% mutate(ZoneAlias = ifelse(grepl("AVLN", Zone), "AVLN", ifelse(
    grepl("FBSG", Zone), "FBSG",
    ifelse(grepl("SBSG", Zone), "SBSG", ifelse(grepl("WFMP A", Zone), "WFMP A", Zone))
  )))

tst <-
  tst %>% mutate(ZoneAlias.1 = ifelse(grepl("AVLN", Zone.1), "AVLN", ifelse(
    grepl("FBSG", Zone.1), "FBSG",
    ifelse(grepl("SBSG", Zone.1), "SBSG", ifelse(grepl("WFMP A", Zone.1), "WFMP A", Zone.1))
  )))

tst <- st_as_sf(tst, coords = c("surf_longitude", "surf_latitude"))

st_crs(tst) <- "+init=epsg:32039"

tst <- st_transform(tst, "+init=epsg:32039")

basin_outline <- st_as_sf(basin_outline)

st_crs(basin_outline) <- "+proj=longlat +datum=WGS84"
basin_outline <- st_transform(basin_outline, "+init=epsg:32039")
basin_outline <- basin_outline %>% rename(basin = Name)

tst <- st_intersection(tst, basin_outline)
tst <- tst %>% mutate(ZoneAlias = paste(basin, ZoneAlias, sep = ""))
tst <- tst %>% mutate(ZoneAlias.1 = paste(basin, ZoneAlias.1, sep = ""))

tst <-
  tst %>% group_by(UWI10, UWI10.1) %>% distinct(UWI10, UWI10.1, .keep_all =
                                                  T)

tst <- tst %>% ungroup()


tst <-
  left_join(tst, settings[, c(3, 5, 6)], by = c("ZoneAlias" = "basin_zone"))
tst <-
  left_join(tst, settings[, c(3, 5, 6)], by = c("ZoneAlias.1" = "basin_zone"))

tst <- tst %>% mutate(DeltOrder = abs(order.x - order.y))


tst <-
  tst %>% ungroup() %>% mutate(
    c.MasterDistance = ifelse(
      PercOverlap < .5,
      Dist,
      ifelse(Dist_Midpoint > Dist * 1.7, Dist, Dist_Midpoint)
    ),
    c.Hypotenuse = sqrt((abs(TVDDiff) ^ 2 + c.MasterDistance ^ 2))
  )




## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
wells<- wells_survey_header_keep%>%distinct(UWI,.keep_all = T)

join_df<-wells%>%select(UWI,SPUD_DATE,COMPLETION_DATE)

join_df$UWI=as.character(join_df$UWI)

spacing<- left_join(tst,join_df,by=c("UWI"="UWI"))

spacing<- spacing%>%left_join(join_df,by=c("PointInPolyUWI"="UWI"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
tst<- sfc_as_cols(tst)



tst<- st_drop_geometry(tst)%>%rename(surf_longitude=x,surf_latitude=y)

tst_final= tst

tst_final$DeltOrder<-as.numeric(tst_final$DeltOrder)

#writexl::write_xlsx(tst_final,'Spacing_Output/Master_Line_Spacing.xlsx')



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------

tst_final <-
  tst_final %>% mutate(ZoneAlias = ifelse(grepl("AVLN", Zone), "AVLN", ifelse(
    grepl("FBSG", Zone), "FBSG",
    ifelse(grepl("SBSG", Zone), "SBSG", ifelse(grepl("WFMP A", Zone), "WFMP A", Zone))
  )))%>%
  mutate(ZoneAlias.1 = ifelse(grepl("AVLN", Zone.1), "AVLN", ifelse(
    grepl("FBSG", Zone.1), "FBSG",
    ifelse(grepl("SBSG", Zone.1), "SBSG", ifelse(grepl("WFMP A", Zone.1), "WFMP A", Zone.1))
  )))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
well_header<- wells%>% distinct(UWI,.keep_all=T)

well_header$COMPLETION_DATE<- as.character(as.Date(well_header$COMPLETION_DATE, format =  "%m/%d/%Y %H:%M:%S"))
well_header$SPUD_DATE<- as.character(as.Date(well_header$SPUD_DATE, format =  "%m/%d/%Y %H:%M:%S"))



well_header<- well_header%>%mutate(COMPLETION_DATE2=ifelse(is.na(COMPLETION_DATE),SPUD_DATE,COMPLETION_DATE))

well_header$COMPLETION_DATE2<-as.Date(well_header$COMPLETION_DATE2)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
well_header<-well_header%>%filter(!is.na(STATION_X)|!is.na(STATION_Y))

well_header_sf<- st_as_sf(well_header,coords=c("STATION_X","STATION_Y"))

st_crs(well_header_sf) <- "+init=epsg:32039"
well_header_sf <- st_transform(well_header_sf, "+init=epsg:32039")

well_header_sf<-st_buffer(well_header_sf,dist=300)




sections<- read_sf("Sections/Sections_Grid.shp")

st_crs(sections) <- "+proj=longlat +datum=WGS84"
sections <- st_transform(sections, "+init=epsg:32039")
sections<-st_buffer(sections,dist=0)

cluster_df_int<-st_intersection(well_header_sf,sections[,1])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
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


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
cluster_df_int <-
  cluster_df_int %>% mutate(Clust_ID = paste(OPERATOR_SHORT, "_", OBJECTID_1, sep ="")) %>%
  group_by(Clust_ID) %>%
  filter(!is.na(COMPLETION_DATE2))  %>%
  mutate(PeriodClust =dense_rank(create_date_periods(COMPLETION_DATE2, time_period = 120)))%>%
  mutate(DateClust =create_date_periods(COMPLETION_DATE2, time_period = 120))

cluster_df_int <-cluster_df_int %>%ungroup()%>%mutate(Project= paste(Clust_ID,PeriodClust,sep="_"))

cluster_df_int<-cluster_df_int%>%ungroup()%>%mutate(length=as.numeric(st_length(cluster_df_int)))

cluster_df_int<-cluster_df_int%>%group_by(OBJECTID_1)%>%mutate(Sec_Count=max(row_number()))

cluster_df_int_filt <-cluster_df_int %>% ungroup() %>% group_by(UWI) %>%
  filter(length == max(length)) %>% filter(Sec_Count == max(Sec_Count)) %>% ungroup() %>%
  distinct(UWI, .keep_all =T) %>% group_by(UWI) %>% mutate(row_id = row_number()) %>%
  ungroup() %>% filter(row_id == 1)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
cluster_df_int_filt$UWI<-as.character(cluster_df_int_filt$UWI)
tst_final<- tst_final%>%left_join(cluster_df_int_filt %>% select(UWI,Project))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#writexl::write_xlsx(tst_final,'Spacing_Output/Master_Line_Spacing.xlsx')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
wells$UWI<-as.character(wells$UWI)

tst_final <-
  tst_final %>% left_join(
    wells %>% select(UWI, SPUD_DATE, COMPLETION_DATE) %>% distinct(UWI, .keep_all =
                                                                         T)
  )
tst_final <-
  tst_final %>% left_join(
    wells %>% select(UWI, SPUD_DATE, COMPLETION_DATE) %>% distinct(UWI, .keep_all =
                                                                         T),
    by = c("PointInPolyUWI"= "UWI")
  )




## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
tst_final$COMPLETION_DATE.x<-as.Date(tst_final$COMPLETION_DATE.x, format =  "%m/%d/%Y")
tst_final$SPUD_DATE.x<-as.Date(tst_final$SPUD_DATE.x, format =  "%m/%d/%Y")


tst_final$COMPLETION_DATE.y<-as.Date(tst_final$COMPLETION_DATE.y, format =  "%m/%d/%Y")
tst_final$SPUD_DATE.y<-as.Date(tst_final$SPUD_DATE.y, format =  "%m/%d/%Y")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------

tst_final<- tst_final %>% mutate(
  COMPLETION_DATE.x = (ifelse(is.na(COMPLETION_DATE.x), SPUD_DATE.x + 90, COMPLETION_DATE.x)),
  COMPLETION_DATE.y = ifelse(is.na(COMPLETION_DATE.y), SPUD_DATE.y +
                               90,COMPLETION_DATE.y))


tst_final$COMPLETION_DATE.x<-zoo::as.Date.numeric(tst_final$COMPLETION_DATE.x)
tst_final$COMPLETION_DATE.y<-zoo::as.Date.numeric(tst_final$COMPLETION_DATE.y)


tst_final$Date_Diff<-tst_final$COMPLETION_DATE.x-tst_final$COMPLETION_DATE.y


####OUTPUT START----
spacing<-tst_final
spacing<-spacing%>%mutate(c.Hypotenuse=ifelse(is.na(c.Hypotenuse),c.MasterDistance,c.Hypotenuse))

#X=spacing%>%filter(UWI==1947)
# spacingOO<-spacing
# 
# spacing=X
# spacing=spacingOO

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
spacing<- spacing%>%rename(Horizontal_Dist=c.MasterDistance)
spacing<- spacing%>%rename(Hypotenuse=c.Hypotenuse)

spacing <-
  spacing %>% mutate(Parent_Child = ifelse(
    COMPLETION_DATE.x - COMPLETION_DATE.y > 360 & Horizontal_Dist <2000,
    "Child",
    ifelse(
      COMPLETION_DATE.x - COMPLETION_DATE.y < 361 &
        COMPLETION_DATE.x -
        COMPLETION_DATE.y > -361  & Horizontal_Dist <2000,
      "Development",
      ifelse(COMPLETION_DATE.x -
               COMPLETION_DATE.y < -360, "Parent", NA)
    )
  ))




## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
spacing<- spacing%>%filter(PercOverlap>.25,Horizontal_Dist<2500)

spacing<- spacing%>%filter((DeltOrder<2|is.na(DeltOrder)))
spacing<- spacing%>%filter(!(is.na(DeltOrder) & TVDDiff>1000))



spacingff<-spacing%>%select(UWI,PointInPolyUWI,TVDDiff,Offset_Direction_Wellbore,
    Parent_Child,Zone,Zone.1,Hypotenuse,Horizontal_Dist,COMPLETION_DATE.x,COMPLETION_DATE.y,OPERATOR_SHORT.1)

sz_spc<-spacingff%>%filter(Zone==Zone.1)%>%group_by(UWI,Offset_Direction_Wellbore)%>%
  filter(Hypotenuse==min(Hypotenuse))

adf_spc<-spacingff%>%filter(Zone!=Zone.1)%>%group_by(UWI,Offset_Direction_Wellbore)%>%
  filter(Hypotenuse==min(Hypotenuse))

comb<-rbind(sz_spc,adf_spc)

comb<-comb%>%filter(Parent_Child=='Child')%>%group_by(UWI)%>%
  summarise(Count=max(dense_rank(Offset_Direction_Wellbore)))%>%
  mutate(Offset_Infill=ifelse(Count==1,'Offset','Infill'))
  

write.xlsx(comb,'offset_infill.xlsx')


#X=spacing%>%filter(UWI=='3977')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
nearest_well_L150tvd<-spacing

nearest_well_L150tvd_2<-nearest_well_L150tvd%>%filter(DeltOrder<2)%>%
	group_by(UWI,Offset_Direction_Wellbore)%>% 
	filter(Hypotenuse==min(Hypotenuse))%>%
	distinct(Offset_Direction_Wellbore,.keep_all=TRUE)%>%
	filter(Hypotenuse>100)%>%ungroup()%>%
	group_by(UWI)%>%
	mutate(ChildCheck=any(Parent_Child=='Child'),
	DevCheck=any(Parent_Child=='Development'))

nearest_well_L150tvd_2<- nearest_well_L150tvd_2%>%arrange(Hypotenuse)


#####Three Month

nearest_well_L150tvd_2Max90Day<-nearest_well_L150tvd%>%filter(DeltOrder<2 & Date_Diff > -91)%>%
	group_by(UWI,Offset_Direction_Wellbore)%>% 
	filter(Hypotenuse==min(Hypotenuse))%>%
	distinct(Offset_Direction_Wellbore,.keep_all=TRUE)%>%
	filter(Hypotenuse>100)%>%ungroup()%>%
	group_by(UWI)%>%
	mutate(ChildCheck=any(Parent_Child=='Child'),
	DevCheck=any(Parent_Child=='Development'))

nearest_well_L150tvd_2Max90Day<- nearest_well_L150tvd_2Max90Day%>%arrange(Hypotenuse)


#####Six Month

nearest_well_L150tvd_2Max180Day<-nearest_well_L150tvd%>%filter(DeltOrder<2 & Date_Diff > -181)%>%
	group_by(UWI,Offset_Direction_Wellbore)%>% 
	filter(Hypotenuse==min(Hypotenuse))%>%
	distinct(Offset_Direction_Wellbore,.keep_all=TRUE)%>%
	filter(Hypotenuse>100)%>%ungroup()%>%
	group_by(UWI)%>%
	mutate(ChildCheck=any(Parent_Child=='Child'),
	DevCheck=any(Parent_Child=='Development'))

nearest_well_L150tvd_2Max180Day<- nearest_well_L150tvd_2Max180Day%>%arrange(Hypotenuse)


#####Nine Month

nearest_well_L150tvd_2Max270Day<-nearest_well_L150tvd%>%filter(DeltOrder<2 & Date_Diff > -271)%>%
	group_by(UWI,Offset_Direction_Wellbore)%>% 
	filter(Hypotenuse==min(Hypotenuse))%>%
	distinct(Offset_Direction_Wellbore,.keep_all=TRUE)%>%
	filter(Hypotenuse>100)%>%ungroup()%>%
	group_by(UWI)%>%
	mutate(ChildCheck=any(Parent_Child=='Child'),
	DevCheck=any(Parent_Child=='Development'))

nearest_well_L150tvd_2Max270Day<- nearest_well_L150tvd_2Max270Day%>%arrange(Hypotenuse)




## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
nearest_well<- spacing%>%filter(PercOverlap>.25,Horizontal_Dist<2001)%>%arrange(Hypotenuse)

nearest_well<- nearest_well%>%filter((DeltOrder<2|is.na(DeltOrder)))
nearest_well<- nearest_well%>%filter(!(is.na(DeltOrder) & TVDDiff>500))

nearest_well<-nearest_well%>%group_by(UWI,Offset_Direction_Wellbore)%>% 
	filter(Hypotenuse==min(Hypotenuse))%>%
	distinct(Offset_Direction_Wellbore,.keep_all=TRUE)%>%
	filter(Hypotenuse>100)%>%ungroup()%>%
	group_by(UWI)%>%
	mutate(ChildCheck=any(Parent_Child=='Child'),
	DevCheck=any(Parent_Child=='Development'))


#####Three Month

nearest_well_Max90Day<- spacing%>%filter(PercIntersect>.099 & Date_Diff > -91)


nearest_well_Max90Day<-nearest_well_Max90Day%>%group_by(UWI,Offset_Direction_Wellbore)%>% 
	filter(Hypotenuse==min(Hypotenuse))%>%
	distinct(Offset_Direction_Wellbore,.keep_all=TRUE)%>%
	filter(Hypotenuse>100)%>%ungroup()%>%
	group_by(UWI)%>%
	mutate(ChildCheck=any(Parent_Child=='Child'),
	DevCheck=any(Parent_Child=='Development'))

#####Six Month

nearest_well_Max180Day<- spacing%>%filter(PercIntersect>.099  & Date_Diff > -181)


nearest_well_Max180Day<-nearest_well_Max180Day%>%group_by(UWI,Offset_Direction_Wellbore)%>% 
	filter(Hypotenuse==min(Hypotenuse))%>%
	distinct(Offset_Direction_Wellbore,.keep_all=TRUE)%>%
	filter(Hypotenuse>100)%>%ungroup()%>%
	group_by(UWI)%>%
	mutate(ChildCheck=any(Parent_Child=='Child'),
	DevCheck=any(Parent_Child=='Development'))


#####Nine Month

nearest_well_Max270Day<- spacing%>%filter(PercIntersect>.099  & Date_Diff > -271)


nearest_well_Max270Day<-nearest_well_Max270Day%>%group_by(UWI,Offset_Direction_Wellbore)%>% 
	filter(Hypotenuse==min(Hypotenuse))%>%
	distinct(Offset_Direction_Wellbore,.keep_all=TRUE)%>%
	filter(Hypotenuse>100)%>%ungroup()%>%
	group_by(UWI)%>%
	mutate(ChildCheck=any(Parent_Child=='Child'),
	DevCheck=any(Parent_Child=='Development'))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
combined_90day<- full_join(nearest_well_L150tvd_2Max90Day,nearest_well_Max90Day)

combined_90day<- combined_90day%>%mutate(Parent_Child=ifelse(is.na(Parent_Child),
	"Blank",Parent_Child))
combined_90day<- combined_90day%>% group_by(UWI)%>%mutate(ChildCheck=any(Parent_Child=='Child'),
		DevCheck=any(Parent_Child=='Development')) 
	
combined_90day<- combined_90day%>%group_by(UWI,Offset_Direction_Wellbore)%>% 
	filter(Hypotenuse==min(Hypotenuse))%>%ungroup()%>%arrange(Hypotenuse)
combined_90day<- combined_90day%>%ungroup()%>%group_by(UWI,Offset_Direction_Wellbore)%>%
distinct(PointInPolyUWI,.keep_all=TRUE)

combined_90day<- combined_90day%>%ungroup()%>%group_by(UWI)%>%
	mutate(Infill_Offset= ifelse(sum(grepl("Child",Parent_Child))>1,"Infill",
	"Offset"))
	

#####Six Month

combined_180day<- full_join(nearest_well_L150tvd_2Max180Day,nearest_well_Max180Day)

combined_180day<- combined_180day%>%mutate(Parent_Child=ifelse(is.na(Parent_Child),
	"Blank",Parent_Child))
combined_180day<- combined_180day%>% group_by(UWI)%>%mutate(ChildCheck=any(Parent_Child=='Child'),
		DevCheck=any(Parent_Child=='Development')) 
	
combined_180day<- combined_180day%>%group_by(UWI,Offset_Direction_Wellbore)%>% 
	filter(Hypotenuse==min(Hypotenuse))%>%ungroup()%>%arrange(Hypotenuse)
combined_180day<- combined_180day%>%ungroup()%>%group_by(UWI,Offset_Direction_Wellbore)%>%
distinct(PointInPolyUWI,.keep_all=TRUE)

combined_180day<- combined_180day%>%ungroup()%>%group_by(UWI)%>%
	mutate(Infill_Offset= ifelse(sum(grepl("Child",Parent_Child))>1,"Infill",
	"Offset"))

#####Nine Month

combined_270day<- full_join(nearest_well_L150tvd_2Max270Day,nearest_well_Max270Day)

combined_270day<- combined_270day%>%mutate(Parent_Child=ifelse(is.na(Parent_Child),
	"Blank",Parent_Child))
combined_270day<- combined_270day%>% group_by(UWI)%>%mutate(ChildCheck=any(Parent_Child=='Child'),
		DevCheck=any(Parent_Child=='Development')) 
	
combined_270day<- combined_270day%>%group_by(UWI,Offset_Direction_Wellbore)%>% 
	filter(Hypotenuse==min(Hypotenuse))%>%ungroup()%>%arrange(Hypotenuse)
combined_270day<- combined_270day%>%ungroup()%>%group_by(UWI,Offset_Direction_Wellbore)%>%
distinct(PointInPolyUWI,.keep_all=TRUE)




## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
same_zone<- spacing%>%filter(PercOverlap>.25, Zone==Zone.1|TVDDiff<150, Hypotenuse>300,Horizontal_Dist<2001)


same_zone_EQUAL<-same_zone%>%filter(DeltOrder<1)%>%
  group_by(UWI,Offset_Direction_Wellbore)%>%
  filter(Hypotenuse==min(Hypotenuse))%>%
  distinct(Offset_Direction_Wellbore,.keep_all=TRUE)%>%
  filter(Hypotenuse>100)%>%ungroup()%>%
  group_by(UWI)%>%
  mutate(ChildCheck=any(Parent_Child=='Child'),
         DevCheck=any(Parent_Child=='Development'))


same_zone_EQUAL<- same_zone_EQUAL%>%arrange(Hypotenuse)
# 



same_zone_L500tvd<-same_zone%>%filter(DeltOrder<2)%>%
	group_by(UWI,Offset_Direction_Wellbore)%>% 
	filter(Hypotenuse==min(Hypotenuse))%>%
	distinct(Offset_Direction_Wellbore,.keep_all=TRUE)%>%
	filter(Hypotenuse>100)%>%ungroup()%>%
	group_by(UWI)%>%
	mutate(ChildCheck=any(Parent_Child=='Child'),
	DevCheck=any(Parent_Child=='Development'))
	

same_zone_L500tvd<- same_zone_L500tvd%>%arrange(Hypotenuse)


#####Three Month

same_zone_L500tvd_90Day<-same_zone%>%filter(DeltOrder<2 & Date_Diff > -91)%>%
	group_by(UWI,Offset_Direction_Wellbore)%>% 
	filter(Hypotenuse==min(Hypotenuse))%>%
	distinct(Offset_Direction_Wellbore,.keep_all=TRUE)%>%
	filter(Hypotenuse>100)%>%ungroup()%>%
	group_by(UWI)%>%
	mutate(ChildCheck=any(Parent_Child=='Child'),
	DevCheck=any(Parent_Child=='Development'))

same_zone_L500tvd_90Day<- same_zone_L500tvd_90Day%>%arrange(Hypotenuse)


#####Six Month

same_zone_L500tvd_180Day<-same_zone%>%filter(DeltOrder<2 & Date_Diff > -181)%>%
	group_by(UWI,Offset_Direction_Wellbore)%>% 
	filter(Hypotenuse==min(Hypotenuse))%>%
	distinct(Offset_Direction_Wellbore,.keep_all=TRUE)%>%
	filter(Hypotenuse>100)%>%ungroup()%>%
	group_by(UWI)%>%
	mutate(ChildCheck=any(Parent_Child=='Child'),
	DevCheck=any(Parent_Child=='Development'))

same_zone_L500tvd_180Day<- same_zone_L500tvd_180Day%>%arrange(Hypotenuse)


#####Nine Month

same_zone_L500tvd_270Day<-same_zone%>%filter(DeltOrder<2 & Date_Diff > -271)%>%
	group_by(UWI,Offset_Direction_Wellbore)%>% 
	filter(Hypotenuse==min(Hypotenuse))%>%
	distinct(Offset_Direction_Wellbore,.keep_all=TRUE)%>%
	filter(Hypotenuse>100)%>%ungroup()%>%
	group_by(UWI)%>%
	mutate(ChildCheck=any(Parent_Child=='Child'),
	DevCheck=any(Parent_Child=='Development'))

same_zone_L500tvd_270Day<- same_zone_L500tvd_270Day%>%arrange(Hypotenuse)






## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
prep_final_tbl<- full_join(nearest_well,nearest_well_L150tvd_2)%>%mutate(origin='closest')
prep_final_tbl<- full_join(prep_final_tbl,same_zone_EQUAL%>%mutate(origin='same_zone'))

prep_final_tbl<- prep_final_tbl%>%mutate(Parent_Child=ifelse(is.na(Parent_Child),
	"Blank",Parent_Child))

prep_final_tbl<- prep_final_tbl%>% group_by(UWI)%>%mutate(ChildCheck=any(Parent_Child=='Child'),
		DevCheck=any(Parent_Child=='Development')) 

#X=prep_final_tbl%>%filter(UWI=='1947')	

prep_final_tbl<- prep_final_tbl%>%group_by(UWI,Offset_Direction_Wellbore)%>% 
	filter(Hypotenuse==min(Hypotenuse)|origin=='same_zone')%>%ungroup()%>%arrange(Hypotenuse)
prep_final_tbl<- prep_final_tbl%>%ungroup()%>%group_by(UWI,Offset_Direction_Wellbore,origin)%>%
distinct(PointInPolyUWI,.keep_all=TRUE)



prep_final_tbl<- prep_final_tbl %>% filter(Hypotenuse>100)%>%
	group_by(UWI)%>%
	mutate(ChildCheck=any(Parent_Child=='Child'),
	DevCheck=any(Parent_Child=='Development'),
	ParentCheck=any(Parent_Child=='Parent'))%>%
	group_by(UWI10,Offset_Direction_Wellbore)%>%
	filter(Hypotenuse==min(Hypotenuse)|origin=='same_zone')%>%
	distinct(Offset_Direction_Wellbore,origin,.keep_all=TRUE)

#X=prep_final_tbl%>%filter(UWI=='1947')	


prep_final_tbl <- prep_final_tbl %>%
  left_join(
    combined_90day %>%
      select(
        OPERATOR_SHORT,
        WELLNAMENUMBER,
        WELLNUMBER,
        OPERATOR_SHORT.1,
        WELLNAMENUMBER.1,
        WELLNUMBER.1,
        PointInPolyUWI10,
        Horizontal_Dist,
        Hypotenuse,
        TVDDiff,
        Parent_Child,
        Offset_Direction_Wellbore,
        UWI,
        Date_Diff,
        COMPLETION_DATE.x,
        COMPLETION_DATE.y
      ) %>%
      rename(
        OPERATOR_SHORT90Day=OPERATOR_SHORT,
             WELLNAMENUMBER90Day=WELLNAMENUMBER,
             WELLNUMBER90Day=WELLNUMBER,
             OPERATOR_SHORT.190Day=OPERATOR_SHORT.1,
        WELLNAMENUMBER.190Day=WELLNAMENUMBER.1,
        WELLNUMBER.190Day=WELLNUMBER.1,
        PointInPolyUWI1090Day = PointInPolyUWI10,
        Horizontal_Dist90Day = Horizontal_Dist,
        Hypotenuse90Day = Hypotenuse,
        TVDDiff90Day = TVDDiff,
        Parent_Child90Day = Parent_Child,
        Date_Diff90Day=Date_Diff,
        COMPLETION_DATE.x90Day=COMPLETION_DATE.x,
        COMPLETION_DATE.y90Day=COMPLETION_DATE.y
      ),
    by = c(
      "UWI" = "UWI",
      "Offset_Direction_Wellbore" = "Offset_Direction_Wellbore"
    )
  )


prep_final_tbl <- prep_final_tbl %>%
  left_join(
    combined_180day %>%
      select(
        OPERATOR_SHORT,
        WELLNAMENUMBER,
        WELLNUMBER,
        OPERATOR_SHORT.1,
        WELLNAMENUMBER.1,
        WELLNUMBER.1,
        PointInPolyUWI10,
        Horizontal_Dist,
        Hypotenuse,
        TVDDiff,
        Parent_Child,
        Offset_Direction_Wellbore,
        UWI,
        Date_Diff,
        COMPLETION_DATE.x,
        COMPLETION_DATE.y
      ) %>%
      rename(
        OPERATOR_SHORT180Day=OPERATOR_SHORT,
             WELLNAMENUMBER180Day=WELLNAMENUMBER,
             WELLNUMBER180Day=WELLNUMBER,
             OPERATOR_SHORT.1180Day=OPERATOR_SHORT.1,
        WELLNAMENUMBER.1180Day=WELLNAMENUMBER.1,
        WELLNUMBER.1180Day=WELLNUMBER.1,
        PointInPolyUWI10180Day = PointInPolyUWI10,
        Horizontal_Dist180Day = Horizontal_Dist,
        Hypotenuse180Day = Hypotenuse,
        TVDDiff180Day = TVDDiff,
        Parent_Child180Day = Parent_Child,
        Date_Diff180Day=Date_Diff,
        COMPLETION_DATE.x180Day=COMPLETION_DATE.x,
        COMPLETION_DATE.y180Day=COMPLETION_DATE.y
        
      ),
    by = c(
      "UWI" = "UWI",
      "Offset_Direction_Wellbore" = "Offset_Direction_Wellbore"
    )
  )

prep_final_tbl <- prep_final_tbl %>%
  left_join(
    combined_270day %>%
      select(
        OPERATOR_SHORT,
        WELLNAMENUMBER,
        WELLNUMBER,
        OPERATOR_SHORT.1,
        WELLNAMENUMBER.1,
        WELLNUMBER.1,
        PointInPolyUWI10,
        Horizontal_Dist,
        Hypotenuse,
        TVDDiff,
        Parent_Child,
        Offset_Direction_Wellbore,
        UWI,
        Date_Diff,
        COMPLETION_DATE.x,
        COMPLETION_DATE.y
      ) %>%
      rename(OPERATOR_SHORT270Day=OPERATOR_SHORT,
             WELLNAMENUMBER270Day=WELLNAMENUMBER,
             WELLNUMBER270Day=WELLNUMBER,
             OPERATOR_SHORT.1270Day=OPERATOR_SHORT.1,
        WELLNAMENUMBER.1270Day=WELLNAMENUMBER.1,
        WELLNUMBER.1270Day=WELLNUMBER.1,
        PointInPolyUWI10270Day = PointInPolyUWI10,
        Horizontal_Dist270Day = Horizontal_Dist,
        Hypotenuse270Day = Hypotenuse,
        TVDDiff270Day = TVDDiff,
        Parent_Child270Day = Parent_Child,
        Date_Diff270Day=Date_Diff,
        COMPLETION_DATE.x270Day=COMPLETION_DATE.x,
        COMPLETION_DATE.y270Day=COMPLETION_DATE.y
      ),
    by = c(
      "UWI" = "UWI",
      "Offset_Direction_Wellbore" = "Offset_Direction_Wellbore"
    )
  )






## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
prep_final_tbl <- prep_final_tbl %>%ungroup()%>%
  mutate(Parent_Child_Update = ifelse(
    ChildCheck == TRUE,
    "Child",
    ifelse(
      DevCheck == TRUE,
      "Development",
      ifelse(DevCheck == FALSE & ChildCheck == FALSE, "Parent", "Missing")
    )
  ))


prep_final_tbl <- prep_final_tbl %>%ungroup()%>%
    mutate(  Parent_Child_Update=ifelse(is.na(Parent_Child_Update),"Missing",  Parent_Child_Update),
             Parent_Child90Day=ifelse(is.na(Parent_Child90Day),"Missing",  Parent_Child90Day),
             Parent_Child180Day=ifelse(is.na(Parent_Child180Day),"Missing",  Parent_Child180Day),
             Parent_Child270Day=ifelse(is.na(Parent_Child270Day),"Missing",  Parent_Child270Day),
    Parent_Child_Comb = ifelse(
      Parent_Child_Update == "Child" |
        Parent_Child90Day == "Child" |
        Parent_Child180Day == "Child" |
        Parent_Child270Day == "Child",
      "Child",
      ifelse(
        Parent_Child_Update == "Development" |
          Parent_Child90Day == "Development" |
          Parent_Child180Day == "Development" |
          Parent_Child270Day == "Development",
        "Development",
        ifelse(
                Parent_Child_Update == "Parent" |
                  Parent_Child90Day == "Parent" |
                  Parent_Child180Day == "Parent" |
                  Parent_Child270Day == "Parent",
                "Parent",
                "Missing"
                
              )
            )
          )
        )
      

prep_final_tbl <- prep_final_tbl %>% group_by(UWI) %>%
  mutate(Parent_Child_Comb = ifelse(
    any(Parent_Child_Comb ==
          "Child"),
    "Child",
    ifelse(
      any(Parent_Child_Comb == "Development"),
      "Development",
      Parent_Child_Comb
    )
  ))


prep_final_tbl<-prep_final_tbl%>%group_by(UWI)%>%mutate(Count=max(row_number()))%>%
  mutate(Infill_Offset=ifelse(Parent_Child_Comb!="Parent" & Count>1,"Infill",
                              ifelse(Parent_Child_Comb!="Parent" & Count<2,"Offset","")))



# aprep_final_tbl = prep_final_tbl %>% select(
#   UWI,
#   WELLNAMENUMBER,
#   Parent_Child_Comb,
#   Parent_Child_Update,
#   Parent_Child270Day,
#   Parent_Child90Day,
#   Parent_Child180Day,
#   Infill_Offset
# )







## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
#UWI10
final_table <- prep_final_tbl %>% select(
  UWI,
  OPERATOR_SHORT,
  WELLNAMENUMBER,
  WELLNUMBER,
  Hypotenuse,
  Parent_Child_Comb,
  Infill_Offset,
  meanTVD,
  TVDDiff,
  Horizontal_Dist,
  Date_Diff,
  PointInPolyUWI,
  PointInPolyUWI10,
  PointInPolyUWI1090Day,
  Hypotenuse90Day,
  TVDDiff90Day,
  Horizontal_Dist90Day,
  Date_Diff90Day,
  PointInPolyUWI10180Day,
  Hypotenuse180Day,
  TVDDiff180Day,
  Date_Diff180Day,
  Horizontal_Dist180Day,
  PointInPolyUWI10270Day,
  Hypotenuse270Day,
  TVDDiff270Day,
  Date_Diff270Day,
  Zone,
  Zone.1,
  COMPLETION_DATE.x,
  COMPLETION_DATE.y,
  COMPLETION_DATE.y90Day,
  COMPLETION_DATE.y180Day,
  COMPLETION_DATE.y270Day,
  Project
)


final_table$COMPLETION_DATE.x <-
  zoo::as.Date(final_table$COMPLETION_DATE.x)
final_table$COMPLETION_DATE.y <-
  zoo::as.Date(final_table$COMPLETION_DATE.y)
final_table$COMPLETION_DATE.y90Day <-
  zoo::as.Date(final_table$COMPLETION_DATE.y90Day)
final_table$COMPLETION_DATE.y180Day <-
  zoo::as.Date(final_table$COMPLETION_DATE.y180Day)
final_table$COMPLETION_DATE.y270Day <-
  zoo::as.Date(final_table$COMPLETION_DATE.y270Day)

left = function (string, char) {
  substr(string, 1, char)
}

final_table$UWI10 <- left(final_table$UWI, 10)
final_table$UWI14 = paste0(final_table$UWI, "00", sep = "")

final_table$OffsetUWI14 = paste0(final_table$PointInPolyUWI, "00", sep =
                                   "")
final_table$OffsetUWI1490Day = paste0(final_table$PointInPolyUWI1090Day, "0000", sep =
                                        "")
final_table$OffsetUWI14180Day = paste0(final_table$PointInPolyUWI10180Day, "0000", sep =
                                         "")
final_table$OffsetUWI14270Day = paste0(final_table$PointInPolyUWI10270Day, "0000", sep =
                                         "")

final_table <- final_table %>% rename(
  UWI10 = UWI10,
  Well_ID = UWI,
  Operator_Short = OPERATOR_SHORT,
  WELLNAMENUMBER = WELLNAMENUMBER,
  WELLNUMBER = WELLNUMBER,
  Hypotenuse = Hypotenuse,
  Parent_Child_Combined = Parent_Child_Comb,
  Infill_Offset = Infill_Offset,
  meanTVD = meanTVD,
  Vertical_Delta = TVDDiff,
  HorizontalDistance = Horizontal_Dist,
  DateDiff = Date_Diff,
  Offset_Well_ID = PointInPolyUWI,
  Offset_UWI10 = PointInPolyUWI10,
  Offset_UWI10_90Day = PointInPolyUWI1090Day,
  Hypotenuse_90Day = Hypotenuse90Day,
  Vertical_Delta_90Day = TVDDiff90Day,
  HorizontalDistance_90Day = Horizontal_Dist90Day,
  DateDiff_90Day = Date_Diff90Day,
  Offset_UWI10_180Day = PointInPolyUWI10180Day,
  Hypotenuse_180Day = Hypotenuse180Day,
  Vertical_Delta_180Day = TVDDiff180Day,
  DateDiff_180Day = Date_Diff180Day,
  HorizontalDistance_180Day = Horizontal_Dist180Day,
  Offset_UWI10_270Day = PointInPolyUWI10270Day,
  Hypotenuse_270Day = Hypotenuse270Day,
  Vertical_Delta_270Day = TVDDiff270Day,
  DateDiff_270Day = Date_Diff270Day,
  Zone = Zone,
  Offset_Zone = Zone.1,
  UWI14 = UWI14,
  Offset_UWI14 = OffsetUWI14,
  Offset_UWI14_90Day = OffsetUWI1490Day,
  Offset_UWI14_180Day = OffsetUWI14180Day,
  Offset_UWI14_270Day = OffsetUWI14270Day,
  Completion_Date = COMPLETION_DATE.x,
  Offset_Completion_Date = COMPLETION_DATE.y,
  Offset_Completion_Date_90Day = COMPLETION_DATE.y90Day,
  Offset_Completion_Date_180Day = COMPLETION_DATE.y180Day,
  Offset_Completion_Date_270Day = COMPLETION_DATE.y270Day
)


final_table <- final_table %>% select(
  UWI10,
  Well_ID,
  Operator_Short,
  WELLNAMENUMBER,
  WELLNUMBER,
  Hypotenuse,
  Parent_Child_Combined,
  Infill_Offset,
  meanTVD,
  Vertical_Delta,
  HorizontalDistance,
  DateDiff,
  Offset_Well_ID,
  Offset_UWI10,
  Offset_UWI10_90Day,
  Hypotenuse_90Day,
  Vertical_Delta_90Day,
  HorizontalDistance_90Day,
  DateDiff_90Day,
  Offset_UWI10_180Day,
  Hypotenuse_180Day,
  Vertical_Delta_180Day,
  DateDiff_180Day,
  HorizontalDistance_180Day,
  Offset_UWI10_270Day,
  Hypotenuse_270Day,
  Vertical_Delta_270Day,
  DateDiff_270Day,
  Zone,
  Offset_Zone,
  UWI14,
  Offset_UWI14,
  Offset_UWI14_90Day,
  Offset_UWI14_180Day,
  Offset_UWI14_270Day,
  Completion_Date,
  Offset_Completion_Date,
  Offset_Completion_Date_90Day,
  Offset_Completion_Date_180Day,
  Offset_Completion_Date_270Day,
  Project
)


final_table <-final_table %>%
   mutate(across(contains("UWI"), ~ifelse(.=="NA0000", NA, .)))

final_table$V1<-1

final_table<-final_table %>%
  relocate(V1)


final_table<-final_table%>%select(-Project)%>%left_join(st_drop_geometry(cluster_df_int_filt) %>% select(UWI,Project),by=c("Well_ID"="UWI"))

data.table::fwrite(final_table,"Final_Tables/Spacing_Output.csv")



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
same_zone_L500tvd_90Day_final <- same_zone_L500tvd_90Day %>%
  rename(
    Hypotenuse_90Day = Hypotenuse,
    Vert_Dist_90Day = TVDDiff,
    Hz_Dist_90Day = Horizontal_Dist,
    Offset_Zone = Zone.1,
    Completion_Date = COMPLETION_DATE.x,
    Offset_Completion_Date_90Day = COMPLETION_DATE.y,
    Offset_Well.Name_90Day = WELLNAMENUMBER.1,
    Offset_Well.Number_90Day = WELLNUMBER.1,
    Offset_UWI10_90Day = PointInPolyUWI10,
    Operator_Short=OPERATOR_SHORT,
    WELLNAMENUMBER=WELLNAMENUMBER,
    WELLNUMBER=WELLNUMBER
  ) %>%
  mutate(
    UWI14 = paste0(UWI, "00", sep = ""),
    Offset_UWI14_90Day = paste0(UWI, "00", sep = "")
  )%>%
select(
  UWI10,
  UWI,
  Operator_Short,
  WELLNAMENUMBER,
  WELLNUMBER,
  Hypotenuse_90Day,
  Vert_Dist_90Day,
  Hz_Dist_90Day,
  Zone,
  Offset_Zone,
  Completion_Date,
  Offset_Completion_Date_90Day,
  Offset_Well.Name_90Day,
  Offset_Well.Number_90Day,
  Offset_UWI10_90Day,
  UWI14,
  Offset_UWI14_90Day,
  Project
)



same_zone_L500tvd_90Day_final <-same_zone_L500tvd_90Day_final %>%
   mutate(across(contains("UWI"), ~ifelse(.=="NA0000", NA, .)))



same_zone_L500tvd_90Day_final$V1<-1

same_zone_L500tvd_90Day_final<-same_zone_L500tvd_90Day_final %>%
  relocate(V1)



data.table::fwrite(same_zone_L500tvd_90Day_final,"Final_Tables/Same_Zone_Spacing_Output.csv")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------
unfiltered_final_table <-
  tst_final %>% mutate(Parent_Child = ifelse(
    COMPLETION_DATE.x - COMPLETION_DATE.y > 360,
    "Child",
    ifelse(
      COMPLETION_DATE.x - COMPLETION_DATE.y < 361 &
        COMPLETION_DATE.x -
        COMPLETION_DATE.y > -361,
      "Development",
      ifelse(COMPLETION_DATE.x -
               COMPLETION_DATE.y < -360, "Parent", NA)
    )
  ))

unfiltered_final_table <- unfiltered_final_table %>% select(-PercIntersect,-Zone,-Zone.1) %>%
  rename(
    API12 = UWI,
    Operator_Short = OPERATOR_SHORT,
    WELLNAMENUMBER = WELLNAMENUMBER,
    WELLNUMBER = WELLNUMBER,
    meanTVD = meanTVD,
    surf_longitude = surf_longitude,
    surf_latitude = surf_latitude,
    bh_longitude = bh_longitude,
    bh_latitude = bh_latitude,
    WellborePosition = WellborePosition,
    Direction = Direction,
    Spacing = Spacing,
    SpacingGeo = SpacingGeo,
    UWI10 = UWI10,
    Area = Area,
    Offset_Well_ID = PointInPolyUWI,
    Offset_Operator_Short = OPERATOR_SHORT.1,
    Offset_WELLNAMENUMBER = WELLNAMENUMBER.1,
    Offset_WELLNUMBER = WELLNUMBER.1,
    Offset_meanTVD = meanTVD.1,
    Offset_surf_longitude = surf_longitude.1,
    Offset_surf_latitude = surf_latitude.1,
    Offset_bh_longitude = bh_longitude.1,
    Offset_bh_latitude = bh_latitude.1,
    Offset_WellborePosition = WellborePosition.1,
    Offset_Direction = Direction.1,
    Offset_Spacing = Spacing.1,
    Offset_SpacingGeo = SpacingGeo.1,
    Offset_UWI10 = UWI10.1,
    Offset_Area = Area.1,
    IntersectArea = IntersectArea,
    PercIntersect = PercOverlap,
    TVDDiff = TVDDiff,
    Line_ID = Line_ID,
    Dist = Dist,
    Dist_Midpoint = Dist_Midpoint,
    length_est = length_est,
    Offset_length_est = length_est_offset,
    RowID = RowID,
    x_x = x.x,
    y_x = y.x,
    x_y = x.y,
    y_y = y.y,
    Offset_Direction_Wellbore = Offset_Direction_Wellbore,
    HorizontalDistance = c.MasterDistance,
    Hypotenuse = c.Hypotenuse,
    Zone = ZoneAlias,
    Offset_Zone = ZoneAlias.1,
    Completion_Date = COMPLETION_DATE.x,
    Offset_Completion_Date = COMPLETION_DATE.y,
    Parent_Child = Parent_Child
  ) %>% mutate(
    UWI14 = paste0(API12, "00", sep = ""),
    Offset_UWI14 = paste0(Offset_Well_ID, "00", sep = "")
  ) %>%
      select(
        API12,
        Operator_Short,
        WELLNAMENUMBER,
        WELLNUMBER,
        meanTVD,
        surf_longitude,
        surf_latitude,
        bh_longitude,
        bh_latitude,
        WellborePosition,
        Direction,
        Spacing,
        SpacingGeo,
        UWI10,
        Area,
        Offset_Well_ID,
        Offset_Operator_Short,
        Offset_WELLNAMENUMBER,
        Offset_WELLNUMBER,
        Offset_meanTVD,
        Offset_surf_longitude,
        Offset_surf_latitude,
        Offset_bh_longitude,
        Offset_bh_latitude,
        Offset_WellborePosition,
        Offset_Direction,
        Offset_Spacing,
        Offset_SpacingGeo,
        Offset_UWI10,
        Offset_Area,
        IntersectArea,
        PercIntersect,
        TVDDiff,
        Line_ID,
        Dist,
        Dist_Midpoint,
        length_est,
        Offset_length_est,
        Offset_UWI10,
        RowID,
        x_x,
        y_x,
        x_y,
        y_y,
        Offset_Direction_Wellbore,
        HorizontalDistance,
        Hypotenuse,
        Zone,
        Offset_Zone,
        Completion_Date,
        Offset_Completion_Date,
        Parent_Child,
        UWI14,
        Offset_UWI14,
        Project
      )
  



#unfiltered_final_table[is.na(unfiltered_final_table)] <- " "


unfiltered_final_table$V1<-1

unfiltered_final_table<-unfiltered_final_table %>%
  relocate(V1)




data.table::fwrite(unfiltered_final_table,"Final_Tables/Unfiltered_Spacing_Output.csv",na="")









# gbd$UWI<-as.character(gbd$UWI)
# 
# child_lines<-left_join(gbd,final_table%>%arrange(desc(Offset_Well_ID))%>%distinct(Well_ID,.keep_all=T)%>%select(Well_ID,Parent_Child_Combined,Offset_Zone,HorizontalDistance,Hypotenuse),by=c('UWI'='Well_ID'))%>%
#   filter(grepl('COP_EER',OPERATOR_SHORT))
# 
# write.xlsx(st_drop_geometry(child_lines),'eer_child_id.xlsx')
# 
# 
# write_sf(child_lines,"EER_Child_Id.shp")
