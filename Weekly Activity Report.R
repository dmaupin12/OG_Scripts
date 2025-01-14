library(odbc)
#library(DBI)
library(tidyverse)
library(data.table)
library(sf)
library(tidyr)
library(nngeo)
library(buffeRs)
library(basemaps)
library(gt)
library(glue)
library(openxlsx)
library(readxl)
library(png)
library(officer)
library(magrittr)




# List all files in the folder
completions_map <- list.files('completions/maps/', full.names = TRUE)
completions_tbl <- list.files('completions/tables/', full.names = TRUE)
permits_map <- list.files('permits/maps/', full.names = TRUE)
permits_tbl <- list.files('permits/tables/', full.names = TRUE)


# Delete all files
file.remove(completions_map)
file.remove(completions_tbl)
file.remove(permits_map)
file.remove(permits_tbl)

setwd("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/Weekly Activity Report/")

sf::sf_use_s2(FALSE)

cop<-read_sf("//Conoco.net/ho_shared/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/BD_ANALYST/SHAPE_DROP/COP_ACREAGE_NEW!/COP_Acreage.shp")

mro<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Permian Inventory CoS Join/Op_Non_Unknown_Acreage/output/MARATHON.shp")

mro <- st_transform(mro,  crs = st_crs(3857))
cop <- st_transform(cop,  crs = st_crs(3857))

cop<-rbind(cop%>%mutate(a='a')%>%select(a),mro%>%mutate(a='a')%>%select(a))

potash<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/BD_ANALYST/F_AcrGIS/BD_Base_Map/Shapes/Potash_Areas/Potash_Areas.shp")
potash <- st_transform(potash,  crs = st_crs(3857))



time_filter<-as.Date(now())-16

current_date<-as.Date(now())-0


####completions filter

# nm_completions<-fread("completions/WellCompletionReport.xls.csv")
# 
# 
# 
# # Identify the indices of "GAS WELL COMPLETIONS" and "OIL WELL COMPLETIONS"
# gas_index <- which(nm_completions$V1 == "GAS WELL COMPLETIONS")
# oil_index <- which(nm_completions$V1 == "OIL WELL COMPLETIONS")
# 
# # Function to extract rows after a header
# extract_section <- function(start_index, data) {
#   # Rows between start index and next empty line
#   end_index <- which(data$V1[start_index:nrow(data)] == "")[1] + start_index - 1
#   data[(start_index + 1):(end_index - 1), ]
# }
# 
# # Extract relevant rows
# gas_well_completions <- extract_section(gas_index, nm_completions)
# oil_well_completions <- extract_section(oil_index, nm_completions)
# 
# # Combine the results
# result <- rbind(gas_well_completions, oil_well_completions)
# 
# nm_completions<-janitor::row_to_names(result,1)
# 
# nm_completions<-nm_completions%>%filter(API!='API')%>%select(API)
# 
# 
# tx_completions<-read.xlsx("completions/tx_completions.xlsx")
# 
# text_to_find <- "Status"
# 
# # Locate the row index where the text is found
# header_row <- which(apply(tx_completions, 1, function(row) any(row == text_to_find)))
# 
# # Check if a matching row was found
# if (length(header_row) > 0) {
#   # Subset the tx_completions from the header row onward
#   cleaned_tx_completions <- tx_completions[header_row:nrow(tx_completions), ]
#   
#   # Set the row as the header
#   colnames(cleaned_tx_completions) <- cleaned_tx_completions[1, ]
#   
#   # Remove the header row from the tx_completions
#   cleaned_tx_completions <- cleaned_tx_completions[-1, ]
#   
#   # Reset row names
#   rownames(cleaned_tx_completions) <- NULL
# } else {
#   stop("Text not found in any row.")
# }
# 
# cleaned_tx_completions<-cleaned_tx_completions%>%select(`API No.`)%>%rename(API=`API No.`)%>%
#   mutate(API=paste('42-',API,sep=""))
# 
# completions_filter<-rbind(nm_completions,cleaned_tx_completions)



myconn_out <- dbConnect(odbc::odbc(), "DRM_snowlflake") #connection to output database


ENV_RIGS<- dbGetQuery(myconn_out,"SELECT * FROM ENT_PRD.ENT.ENV_FOUNDATIONS_RIGS")

ENV_PERMITS <- dbGetQuery(
  myconn_out,
  glue("
    SELECT *
    FROM ENT_PRD.ENT.ENV_FOUNDATIONS_WELLS
    WHERE PERMITAPPROVEDDATE BETWEEN TO_DATE('{current_date - 8}', 'YYYY-MM-DD') AND TO_DATE('{current_date}', 'YYYY-MM-DD')
")
)



##ENV_COMPLETIONS$PERMITSUBMITTEDDATE



#original keep in case enverus add submit date
ENV_COMPLETIONS <- dbGetQuery(
  myconn_out,
  glue("
    SELECT *
    FROM ENT_PRD.ENT.ENV_FOUNDATIONS_WELLS
    WHERE ENVCOMPINSERTEDDATE BETWEEN TO_DATE('{current_date - 8}', 'YYYY-MM-DD') AND TO_DATE('{current_date}', 'YYYY-MM-DD')
")
)



ENV_DUCS <- dbGetQuery(
  myconn_out,
  glue("
    SELECT *
    FROM ENT_PRD.ENT.ENV_FOUNDATIONS_WELLS
    WHERE ENVWELLSTATUS = 'DUC'
")
)


ENV_DUCS <-ENV_DUCS %>%filter(TRAJECTORY=='HORIZONTAL')


# ENV_PERMITSS <- dbGetQuery(myconn_out, "
#     SELECT *
#     FROM ENT_PRD.ENT.ENV_FOUNDATIONS_WELLS
#     WHERE API_UWI = '30-015-46717'
# ")



#ENV_ACTIVITY_RIGS<-dbGetQuery(myconn_out,"SELECT * FROM ENT_PRD.ENT.ENV_ACTIVITY_RIGSTHROUGHTIME")

#ENV_ACTIVITY_RIGS$UPDATEDDATE<-as.Date(ENV_ACTIVITY_RIGS$UPDATEDDATE)


#glimpse(ENV_ACTIVITY_RIGS_filter)

# ENV_ACTIVITY_RIGS_filter<-st_drop_geometry(ENV_ACTIVITY_RIGS)%>%filter(UPDATEDDATE > as.Date(now())-8)%>%relocate(RIGID,RIGNAME_NUMBER,ENVBASIN,ENTRYDATE,UPDATEDDATE)%>%select(API_UWI,RIGID,RIGNAME_NUMBER,ENVBASIN,ENTRYDATE,UPDATEDDATE)
# ENV_ACTIVITY_RIGS_filter<-st_drop_geometry(ENV_ACTIVITY_RIGS_filter)%>%group_by(ENTRYDATE)%>%
#   distinct(RIGID,RIGNAME_NUMBER,ENVBASIN)
# 
# ENV_ACTIVITY_RIGS_filter<-ENV_ACTIVITY_RIGS_filter%>%filter(ENVBASIN %in% c("MIDLAND","DELAWARE","PERMIAN OTHER"))
# 
# ENV_ACTIVITY_RIGS_filter<-ENV_ACTIVITY_RIGS_filter%>%group_by(ENVBASIN,ENTRYDATE)%>%
#   summarise(Count=max(row_number()))


ENV_ACTIVITY_FRAC_CREW<-dbGetQuery(myconn_out,"SELECT * FROM ENT_PRD.ENT.ENV_ACTIVITY_FRACCREWSTHROUGHTIME")


#ENV_PERMITSoirg<- dbGetQuery(myconn_out,"SELECT * FROM ENT_PRD.ENT.ENV_FOUNDATIONS_PERMITS")

# query <- sprintf(
#   "SELECT * FROM ENT_PRD.ENT.ENV_FOUNDATIONS_WELLS WHERE API_UWI IN (%s)", 
#   api_filter
# )

# Execute the query
# ENV_COMPLETIONS <- dbGetQuery(myconn_out, query)
ENV_COMPLETIONS <-ENV_COMPLETIONS %>%group_by(API_UWI)%>%arrange(desc(OILTESTRATE_BBLPERDAY))%>%distinct(API_UWI,.keep_all=T)%>%
  ungroup()%>%filter(ENVWELLSTATUS %in% c("PRODUCING","COMPLETED","INACTIVE COMPLETED","INACTIVE PRODUCER"))%>%
  filter(COMPLETIONDATE>current_date-500)




basin_lines<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/BD_ANALYST/F_AcrGIS/BD_Base_Map/Shapes/Basin_Outlines/Basin_Outlines.shp")



# env_permits_filter<-ENV_PERMITS%>%
#   filter((APPROVEDDATE>time_filter) & PERMITTYPE=='NEW DRILL' & PERMITSTATUS %in% c("ACTIVE","PENDING") &
#            ENVBASIN %in% c("DELAWARE","MIDLAND","PERMIAN OTHER",NA) & TRAJECTORY %in% c('HORIZONTAL','UNDETERMINED'))%>%
#   filter(!is.na(GEOMPERMITTED_LINE))%>%
#   filter(ENVWELLSTATUS %in% c("PERMITTED","PERMIT PENDING APPROVAL","UNREPORTED"))


env_permits_filter<-ENV_PERMITS%>%
  filter(ENVBASIN %in% c("DELAWARE","MIDLAND","PERMIAN OTHER",NA) & TRAJECTORY %in% c('HORIZONTAL','UNDETERMINED'))%>%
  filter(!is.na(LATERALLINE))%>%
  filter(ENVWELLSTATUS %in% c("PERMITTED","PERMIT PENDING APPROVAL","UNREPORTED"))





env_permits_filter<-env_permits_filter%>%filter(LONGITUDE> -104.9 & LONGITUDE< -101 & LATITUDE >30 & LATITUDE < 34)

#permits
line_permits<-env_permits_filter
point_permits<-env_permits_filter

point_permits$geometry <- st_as_sfc(point_permits$GEOMSHL_POINT, crs = 4326)
point_permits <- st_as_sf(point_permits)
point_permits <- st_transform(point_permits,  crs = st_crs(3857))

line_permits$geometry <- st_as_sfc(line_permits$LATERALLINE, crs = 4326)
line_permits <- st_as_sf(line_permits)
line_permits <- st_transform(line_permits,  crs = st_crs(3857))

line_permits_int_cop<-st_intersection(line_permits,st_transform(cop,st_crs(line_permits)))

line_permits_int_cop<-st_transform(line_permits_int_cop,"+init=epsg:32039")
line_permits_int_cop<-line_permits_int_cop%>%mutate(length_int=as.numeric(st_length(geometry)))%>%
  filter(length_int>400)

line_permits<-line_permits%>%mutate(COP_CHECK=ifelse(API_UWI %in% c(line_permits_int_cop$API_UWI),'YES','NO'))
line_permits<-line_permits%>%mutate(COP_CHECK=ifelse(grepl('CONOCOPHILLIPS|MARATHON',ENVOPERATOR),'YES',COP_CHECK))


line_permits<-st_transform(line_permits,"+init=epsg:32039")
line_permits<-line_permits%>%mutate(LENGTH=as.numeric(st_length(geometry)))
line_permits <- st_transform(line_permits,  crs = st_crs(3857))



#completions
line_completions<-ENV_COMPLETIONS%>%filter(!is.na(LATERALLINE))
point_completions<-ENV_COMPLETIONS%>%filter(!is.na(LATERALLINE))


line_duc<-ENV_DUCS%>%filter(!is.na(LATERALLINE))
point_duc<-ENV_DUCS%>%filter(!is.na(LATERALLINE))

point_duc$geometry <- st_as_sfc(point_duc$GEOMSHL_POINT, crs = 4326)
point_duc <- st_as_sf(point_duc)
point_duc <- st_transform(point_duc,  crs = st_crs(3857))

line_duc$geometry <- st_as_sfc(line_duc$LATERALLINE, crs = 4326)
line_duc <- st_as_sf(line_duc)
line_duc <- st_transform(line_duc,  crs = st_crs(3857))

line_duc_int_cop<-st_intersection(line_duc,st_transform(cop,st_crs(line_duc)))

line_duc_int_cop<-st_transform(line_duc_int_cop,"+init=epsg:32039")
line_duc_int_cop<-line_duc_int_cop%>%mutate(length_int=as.numeric(st_length(geometry)))%>%
  filter(length_int>400)

line_duc<-line_duc%>%mutate(COP_CHECK=ifelse(API_UWI %in% c(line_duc_int_cop$API_UWI),'YES','NO'))
line_duc<-line_duc%>%mutate(COP_CHECK=ifelse(grepl('CONOCOPHILLIPS|MARATHON',ENVOPERATOR),'YES',COP_CHECK))


line_duc<-st_transform(line_duc,"+init=epsg:32039")
line_duc<-line_duc%>%mutate(LENGTH=as.numeric(st_length(geometry)))
line_duc <- st_transform(line_duc,  crs = st_crs(3857))

point_completions$geometry <- st_as_sfc(point_completions$GEOMSHL_POINT, crs = 4326)
point_completions <- st_as_sf(point_completions)
point_completions <- st_transform(point_completions,  crs = st_crs(3857))

line_completions$geometry <- st_as_sfc(line_completions$LATERALLINE, crs = 4326)
line_completions <- st_as_sf(line_completions)
line_completions <- st_transform(line_completions,  crs = st_crs(3857))

line_completions_int_cop<-st_intersection(line_completions,st_transform(cop,st_crs(line_completions)))

line_completions_int_cop<-st_transform(line_completions_int_cop,"+init=epsg:32039")
line_completions_int_cop<-line_completions_int_cop%>%mutate(length_int=as.numeric(st_length(geometry)))%>%
  filter(length_int>400)

line_completions<-line_completions%>%mutate(COP_CHECK=ifelse(API_UWI %in% c(line_completions_int_cop$API_UWI),'YES','NO'))
line_completions<-line_completions%>%mutate(COP_CHECK=ifelse(grepl('CONOCOPHILLIPS|MARATHON',ENVOPERATOR),'YES',COP_CHECK))

line_completions<-st_transform(line_completions,"+init=epsg:32039")
line_completions<-line_completions%>%mutate(LENGTH=as.numeric(st_length(geometry)))
line_completions <- st_transform(line_completions,  crs = st_crs(3857))



state_county <- tigris::counties(state = c("Texas","New Mexico")) %>%
  st_as_sf()

state_county_clip=state_county

state_county_clip<-st_transform(state_county_clip,st_crs(3857))

NDB<- state_county_clip%>%filter(NAME %in% c("Eddy","Lea"))
SDB<-state_county_clip%>%filter(NAME %in% c("Winkler","Loving","Ward","Pecos","Reeves","Culberson"))



asset_areas<-read_sf("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/BD_ANALYST/SHAPE_DROP/Permian Asset Team Regions/Asset_Team_Div.shp")


state_county_clip_int<-st_intersection(state_county_clip,st_transform(asset_areas%>%select(ABBREVIATI),st_crs(state_county_clip)))
state_county_clip_int<-state_county_clip_int%>%mutate(area=as.numeric(st_area(geometry)))%>%
  group_by(NAME)%>%filter(area==max(area))

state_county_clip<-left_join(state_county_clip,st_drop_geometry(state_county_clip_int)%>%select(ABBREVIATI,NAME))

state_county_clip<-state_county_clip%>%mutate(ABBREVIATI=ifelse(NAME %in% c(NDB$NAME),'NDB',
                                                                ifelse(NAME %in% c(SDB$NAME),'SDB',ABBREVIATI)))

state_county_clip<-state_county_clip%>%mutate(ABBREVIATI=ifelse(ABBREVIATI=='DBE','MBN',ABBREVIATI))

state_county_clip<-state_county_clip%>%filter(!is.na(ABBREVIATI),ABBREVIATI !='DBW')


permits_lines_int<-st_intersection(line_permits,st_transform(state_county_clip%>%select(ABBREVIATI),st_crs(line_permits)))
permits_lines_int<-permits_lines_int%>%mutate(length_int=as.numeric(st_length(geometry)))%>%
  group_by(API_UWI)%>%filter(length_int==max(length_int))%>%ungroup()%>%
  distinct(API_UWI,.keep_all=T)%>%st_drop_geometry()

completions_lines_int<-st_intersection(line_completions,st_transform(state_county_clip%>%select(ABBREVIATI),st_crs(line_completions)))
completions_lines_int<-completions_lines_int%>%mutate(length_int=as.numeric(st_length(geometry)))%>%
  group_by(API_UWI)%>%filter(length_int==max(length_int))%>%ungroup()%>%
  distinct(API_UWI,.keep_all=T)%>%st_drop_geometry()


duc_lines_int<-st_intersection(line_duc,st_transform(state_county_clip%>%select(ABBREVIATI),st_crs(line_duc)))
duc_lines_int<-duc_lines_int%>%mutate(length_int=as.numeric(st_length(geometry)))%>%
  group_by(API_UWI)%>%filter(length_int==max(length_int))%>%ungroup()%>%
  distinct(API_UWI,.keep_all=T)%>%st_drop_geometry()





PERMITS_LINES<-left_join(line_permits,permits_lines_int%>%select(API_UWI,ABBREVIATI))
PERMITS_LINES$ENVOPERATOR <- sapply(strsplit(PERMITS_LINES$ENVOPERATOR, " "), `[`, 1)
PERMITS_POINTS<-left_join(point_permits,permits_lines_int%>%select(API_UWI,ABBREVIATI))
PERMITS_POINTS$ENVOPERATOR <- sapply(strsplit(PERMITS_POINTS$ENVOPERATOR, " "), `[`, 1)

COMPLETIONS_LINES<-left_join(line_completions,completions_lines_int%>%select(API_UWI,ABBREVIATI))
COMPLETIONS_LINES<-COMPLETIONS_LINES%>%filter(!is.na(ABBREVIATI))
COMPLETIONS_LINES$ENVOPERATOR <- sapply(strsplit(COMPLETIONS_LINES$ENVOPERATOR, " "), `[`, 1)
COMPLETIONS_POINTS<-left_join(point_completions,completions_lines_int%>%select(API_UWI,ABBREVIATI))%>%filter(!is.na(ABBREVIATI))
COMPLETIONS_POINTS$ENVOPERATOR <- sapply(strsplit(COMPLETIONS_POINTS$ENVOPERATOR, " "), `[`, 1)



DUC_LINES<-left_join(line_duc,duc_lines_int%>%select(API_UWI,ABBREVIATI))
DUC_LINES<-DUC_LINES%>%filter(!is.na(ABBREVIATI))
DUC_LINES$ENVOPERATOR <- sapply(strsplit(DUC_LINES$ENVOPERATOR, " "), `[`, 1)
DUC_LINES<-DUC_LINES%>%distinct(API_UWI,.keep_all=T)%>%
  filter(SPUDTORIGRELEASE_DAYS>5 & SPUDTORIGRELEASE_DAYS<101 )

DUC_POINTS<-left_join(point_duc,duc_lines_int%>%select(API_UWI,ABBREVIATI))%>%filter(!is.na(ABBREVIATI))
DUC_POINTS$ENVOPERATOR <- sapply(strsplit(DUC_POINTS$ENVOPERATOR, " "), `[`, 1)
DUC_POINTS<-DUC_POINTS%>%distinct(API_UWI,.keep_all=T)
DUC_POINTS<-DUC_POINTS%>%distinct(API_UWI,.keep_all=T)%>%
  filter(SPUDTORIGRELEASE_DAYS>5 & SPUDTORIGRELEASE_DAYS<101 )





ENV_RIGS <- st_as_sf(ENV_RIGS,coords=c('RIGLONGITUDEWGS84','RIGLATITUDEWGS84'))
st_crs(ENV_RIGS) <- "+proj=longlat +datum=WGS84"
ENV_RIGS <- st_transform(ENV_RIGS,  crs = st_crs(3857))

ENV_RIGS$ENVOPERATOR <- sapply(strsplit(ENV_RIGS$ENVOPERATOR, " "), `[`, 1)

ENV_RIGS<-st_intersection(ENV_RIGS,st_transform(asset_areas,st_crs(ENV_RIGS)))

ENV_RIGS<-ENV_RIGS%>%
  distinct(RIGNAME_NUMBER,.keep_all=T)%>%
  distinct(API_UWI,.keep_all=T)%>%
  group_by(ENVOPERATOR)%>%mutate(max_rigs=max(row_number()))%>%
  ungroup()%>%mutate(rank=dense_rank(desc(max_rigs)))%>%relocate(rank,max_rigs)


SDB_bbox<-structure(c(xmin = -11619920.7839622, ymin = 3622020.24449297, 
                          xmax = -11440977.5000896, ymax = 3797963.52836558), class = "bbox", crs = structure(list(
                            input = "+init=epsg:3857", wkt = "PROJCRS[\"WGS 84 / Pseudo-Mercator\",\n    BASEGEOGCRS[\"WGS 84\",\n        ENSEMBLE[\"World Geodetic System 1984 ensemble\",\n            MEMBER[\"World Geodetic System 1984 (Transit)\"],\n            MEMBER[\"World Geodetic System 1984 (G730)\"],\n            MEMBER[\"World Geodetic System 1984 (G873)\"],\n            MEMBER[\"World Geodetic System 1984 (G1150)\"],\n            MEMBER[\"World Geodetic System 1984 (G1674)\"],\n            MEMBER[\"World Geodetic System 1984 (G1762)\"],\n            MEMBER[\"World Geodetic System 1984 (G2139)\"],\n            MEMBER[\"World Geodetic System 1984 (G2296)\"],\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]],\n            ENSEMBLEACCURACY[2.0]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"Popular Visualisation Pseudo-Mercator\",\n        METHOD[\"Popular Visualisation Pseudo Mercator\",\n            ID[\"EPSG\",1024]],\n        PARAMETER[\"Latitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]],\n        ID[\"EPSG\",3856]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n    USAGE[\n        SCOPE[\"unknown\"],\n        AREA[\"World between 85.06°S and 85.06°N.\"],\n        BBOX[-85.06,-180,85.06,180]]]"), class = "crs"))


NDB_bbox<-structure(c(xmin = -11635392.2139202, ymin = 3769687.56636695, 
                      xmax = -11495392.2139202, ymax = 3909687.56636695), class = "bbox", crs = structure(list(
                        input = "+init=epsg:3857", wkt = "PROJCRS[\"WGS 84 / Pseudo-Mercator\",\n    BASEGEOGCRS[\"WGS 84\",\n        ENSEMBLE[\"World Geodetic System 1984 ensemble\",\n            MEMBER[\"World Geodetic System 1984 (Transit)\"],\n            MEMBER[\"World Geodetic System 1984 (G730)\"],\n            MEMBER[\"World Geodetic System 1984 (G873)\"],\n            MEMBER[\"World Geodetic System 1984 (G1150)\"],\n            MEMBER[\"World Geodetic System 1984 (G1674)\"],\n            MEMBER[\"World Geodetic System 1984 (G1762)\"],\n            MEMBER[\"World Geodetic System 1984 (G2139)\"],\n            MEMBER[\"World Geodetic System 1984 (G2296)\"],\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]],\n            ENSEMBLEACCURACY[2.0]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"Popular Visualisation Pseudo-Mercator\",\n        METHOD[\"Popular Visualisation Pseudo Mercator\",\n            ID[\"EPSG\",1024]],\n        PARAMETER[\"Latitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]],\n        ID[\"EPSG\",3856]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n    USAGE[\n        SCOPE[\"unknown\"],\n        AREA[\"World between 85.06°S and 85.06°N.\"],\n        BBOX[-85.06,-180,85.06,180]]]"), class = "crs"))



MBN_bbox<-structure(c(xmin = -11459626.7170713, ymin = 3723342.64327339, 
                      xmax = -11242626.7170713, ymax = 3939942.64327339), class = "bbox", crs = structure(list(
                        input = "+init=epsg:3857", wkt = "PROJCRS[\"WGS 84 / Pseudo-Mercator\",\n    BASEGEOGCRS[\"WGS 84\",\n        ENSEMBLE[\"World Geodetic System 1984 ensemble\",\n            MEMBER[\"World Geodetic System 1984 (Transit)\"],\n            MEMBER[\"World Geodetic System 1984 (G730)\"],\n            MEMBER[\"World Geodetic System 1984 (G873)\"],\n            MEMBER[\"World Geodetic System 1984 (G1150)\"],\n            MEMBER[\"World Geodetic System 1984 (G1674)\"],\n            MEMBER[\"World Geodetic System 1984 (G1762)\"],\n            MEMBER[\"World Geodetic System 1984 (G2139)\"],\n            MEMBER[\"World Geodetic System 1984 (G2296)\"],\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]],\n            ENSEMBLEACCURACY[2.0]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"Popular Visualisation Pseudo-Mercator\",\n        METHOD[\"Popular Visualisation Pseudo Mercator\",\n            ID[\"EPSG\",1024]],\n        PARAMETER[\"Latitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]],\n        ID[\"EPSG\",3856]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n    USAGE[\n        SCOPE[\"unknown\"],\n        AREA[\"World between 85.06°S and 85.06°N.\"],\n        BBOX[-85.06,-180,85.06,180]]]"), class = "crs"))

MBS_bbox<-structure(c(xmin = -11425626.7170713, ymin = 3635193.51851527, 
                          xmax = -11265626.7170713, ymax = 3795193.51851527), class = "bbox", crs = structure(list(
                            input = "+init=epsg:3857", wkt = "PROJCRS[\"WGS 84 / Pseudo-Mercator\",\n    BASEGEOGCRS[\"WGS 84\",\n        ENSEMBLE[\"World Geodetic System 1984 ensemble\",\n            MEMBER[\"World Geodetic System 1984 (Transit)\"],\n            MEMBER[\"World Geodetic System 1984 (G730)\"],\n            MEMBER[\"World Geodetic System 1984 (G873)\"],\n            MEMBER[\"World Geodetic System 1984 (G1150)\"],\n            MEMBER[\"World Geodetic System 1984 (G1674)\"],\n            MEMBER[\"World Geodetic System 1984 (G1762)\"],\n            MEMBER[\"World Geodetic System 1984 (G2139)\"],\n            MEMBER[\"World Geodetic System 1984 (G2296)\"],\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]],\n            ENSEMBLEACCURACY[2.0]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"Popular Visualisation Pseudo-Mercator\",\n        METHOD[\"Popular Visualisation Pseudo Mercator\",\n            ID[\"EPSG\",1024]],\n        PARAMETER[\"Latitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]],\n        ID[\"EPSG\",3856]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n    USAGE[\n        SCOPE[\"unknown\"],\n        AREA[\"World between 85.06°S and 85.06°N.\"],\n        BBOX[-85.06,-180,85.06,180]]]"), class = "crs"))




RIGS_bbox<-structure(c(xmin = -11629072.7231269, ymin = 3610685.40660676, 
                       xmax = -11279072.7231269, ymax = 3910685.40660676), class = "bbox", crs = structure(list(
                         input = "+init=epsg:3857", wkt = "PROJCRS[\"WGS 84 / Pseudo-Mercator\",\n    BASEGEOGCRS[\"WGS 84\",\n        ENSEMBLE[\"World Geodetic System 1984 ensemble\",\n            MEMBER[\"World Geodetic System 1984 (Transit)\"],\n            MEMBER[\"World Geodetic System 1984 (G730)\"],\n            MEMBER[\"World Geodetic System 1984 (G873)\"],\n            MEMBER[\"World Geodetic System 1984 (G1150)\"],\n            MEMBER[\"World Geodetic System 1984 (G1674)\"],\n            MEMBER[\"World Geodetic System 1984 (G1762)\"],\n            MEMBER[\"World Geodetic System 1984 (G2139)\"],\n            MEMBER[\"World Geodetic System 1984 (G2296)\"],\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]],\n            ENSEMBLEACCURACY[2.0]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"Popular Visualisation Pseudo-Mercator\",\n        METHOD[\"Popular Visualisation Pseudo Mercator\",\n            ID[\"EPSG\",1024]],\n        PARAMETER[\"Latitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]],\n        ID[\"EPSG\",3856]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n    USAGE[\n        SCOPE[\"unknown\"],\n        AREA[\"World between 85.06°S and 85.06°N.\"],\n        BBOX[-85.06,-180,85.06,180]]]"), class = "crs"))
  
  
# LAT=31.9799985067331
# LONG=-102.893685926014
# ASDF=1213
# 
# X=data.frame(LONG,LAT,ASDF)
# 
# X=st_as_sf(X,coords=c("LONG","LAT"))
# st_crs(X) <- "+proj=longlat +datum=WGS84"
# X <- st_transform(X, st_crs(PERMITS_LINES))


# bbox <- st_as_sfc(st_bbox(X))
# bbox_centroid<-st_centroid(bbox)
# bbox_centroid<-st_as_sf(bbox_centroid)
# bbox<-st_segments(bbox)
# bbox<-st_as_sf(bbox)
# max_dist=as.numeric(max(st_distance(bbox_centroid,bbox)))
# #
#  square=buffer_rectangle(bbox_centroid,x_length = 350000,y_length=300000)
#  st_crs(square) <- "+init=epsg:3857"
#
 # bbox<-st_as_sf(square)
 # bbox<-st_bbox(bbox)

##COMPLETION ZONE ALIAS

# Create the data frame manually
zone_alias<- data.frame(
  ENVInterval = c(
    "1ST BONE SPRING", "2ND BONE SPRING", "2ND BONE SPRING SAND", 
    "3RD BONE SPRING", "3RD BONE SPRING SAND", "ABOVE UPPER AVALON", 
    "CENTRAL BASIN PLATFORM ALL", "DRINKARD", "LOWER AVALON", 
    "LOWER BLINEBRY", "LOWER PADDOCK", "LOWER PENNSYLVANIAN AND MISSISSIPPIAN", 
    "MIDDLE AVALON", "NM SHELF ALL", "UPPER AVALON", "UPPER BLINEBRY", 
    "UPPER PADDOCK", "WOLFCAMP A LOWER", "WOLFCAMP A UPPER", 
    "WOLFCAMP B LOWER", "WOLFCAMP B UPPER", "WOLFCAMP C", 
    "WOLFCAMP D", "WOLFCAMP XY", "YESO ALL"
  ),
  ZONE = c(
    "BS1", "BS2", "BS2", "BS3S", "BS3S", "UAVLN", 
    "CBP", "BS2", "LAVLN", "BLBRY", "PDCK", "PENN", 
    "MAVLN", "SHELF", "UAVLN", "BLBRY", "PDCK", 
    "WFMP A", "WFMP A", "WFMP B", "WFMP B", "WFMP C", 
    "WFMP D", "WFMP A", "YESO"
  ),
  stringsAsFactors = FALSE
)

COMPLETIONS_LINES<-left_join(COMPLETIONS_LINES,zone_alias,by=c("ENVINTERVAL"='ENVInterval'))


generate_random_hex_colors <- function(x) {
  replicate(x, paste0("#", paste0(sample(c(0:9, letters[1:6]), 6, replace = TRUE), collapse = "")))
}


color_add<-dput(structure(list(ENVOPERATOR = c("EXXON", "OCCIDENTAL", "DIAMONDBACK", 
                                               "MEWBOURNE", "EOG", "CONOCOPHILLIPS", "PERMIAN", "DEVON", "CHEVRON", 
                                               "APA", "CONTINENTAL", "COTERRA", "MATADOR", "DOUBLE", "OVINTIV", 
                                               "BLACKBEARD","TAP"), colors = c("blue", "lightblue", "#f7199b", "orange", 
                                                                         "black", "yellow", "turquoise", "red", "darkgreen", "gold", "darkblue", 
                                                                         "maroon", "purple", "brown", "#510A0A", "pink","darkorange")), class = "data.frame", row.names = c(NA, 
                                                                                                                                                             -17L)))



COMPLETIONS_POINTS<-left_join(COMPLETIONS_POINTS,color_add)

COMPLETIONS_POINTS<-COMPLETIONS_POINTS

PERMITS_POINTS<-left_join(PERMITS_POINTS,color_add)
PERMITS_POINTS<-PERMITS_POINTS

other_op_color<- COMPLETIONS_POINTS%>%select(ENVOPERATOR,colors)%>%filter(colors=='grey'| is.na(colors))%>%
  distinct(ENVOPERATOR,.keep_all=T)

other_op_permits<- PERMITS_POINTS%>%select(ENVOPERATOR,colors)%>%filter(colors=='grey'| is.na(colors))%>%
  distinct(ENVOPERATOR,.keep_all=T)


other_op_color<-rbind(other_op_color,other_op_permits)
other_op_color<-other_op_color%>%distinct(ENVOPERATOR,.keep_all=T)

hex_colors<-generate_random_hex_colors(nrow(other_op_color))

other_op_color<-cbind(other_op_color%>%select(-colors),colors=hex_colors)%>%rename(color.1=colors)



PERMITS_POINTS<-left_join(PERMITS_POINTS,st_drop_geometry(other_op_color))%>%
  mutate(colors=ifelse(is.na(colors),color.1,colors))

COMPLETIONS_POINTS<-left_join(COMPLETIONS_POINTS,st_drop_geometry(other_op_color))%>%
  mutate(colors=ifelse(is.na(colors),color.1,colors))


#Read in Historicals
hist_permits<-read.csv('historical_data/hist_permits.csv')
hist_completions<-read.csv('historical_data/hist_completions.csv')


PERMITS_POINTS<-PERMITS_POINTS%>%filter(!(API_UWI_UNFORMATTED %in% c(hist_permits$API_UWI_UNFORMATTED)))
PERMITS_LINES<-PERMITS_LINES%>%filter(!(API_UWI_UNFORMATTED %in% c(hist_permits$API_UWI_UNFORMATTED)))

COMPLETIONS_POINTS<-COMPLETIONS_POINTS%>%filter(!(API_UWI_UNFORMATTED %in% c(hist_completions$API_UWI_UNFORMATTED)))
COMPLETIONS_LINES<-COMPLETIONS_LINES%>%filter(!(API_UWI_UNFORMATTED %in% c(hist_completions$API_UWI_UNFORMATTED)))


write.csv(st_drop_geometry(PERMITS_POINTS),'historical_data/hist_permits.csv')
write.csv(st_drop_geometry(COMPLETIONS_POINTS),'historical_data/hist_completions.csv')


#i='SDB'

####PERMITS OUTPUT----

permit_map_fctn<-function(i){
bounding_box<- if(i=='NDB'){
  NDB_bbox
}else if(i=='SDB'){
    SDB_bbox
  }else if(i=='MBN'){
      MBN_bbox
  }else if(i=='MBS'){
    MBS_bbox
  }
      


basin_legend=if(i=='NDB'){
  c("left", "top")
}else if(i=='SDB'){
  c("left", 'bottom')
}else if(i %in% c('MBN','MBS')){
  c('right','top')
}


position_legend=if(i=='NDB'){
  c(.01,.99)
}else if(i=='SDB'){
  c(.01, .01)
}else if(i %in% c('MBN','MBS')){
  c(.99,.99)
}



ggplot()+
  basemap_gglayer(bounding_box, map_service = "carto", map_type = "light") +
  scale_fill_identity()+
  geom_sf(data=cop,fill='lightgrey',color=NA) + 
  geom_sf(data=state_county_clip,fill=NA)+
  geom_sf(data=potash,fill=NA,color='maroon')+
  geom_sf(data=PERMITS_POINTS%>%filter(ABBREVIATI==i),aes(color=ENVOPERATOR),size=3.5)+
  scale_color_manual(
    name = "Operator", # Legend title
    values = setNames(PERMITS_POINTS$colors, PERMITS_POINTS$ENVOPERATOR) # Map ENVOPERATOR to colors
  )+
  geom_sf(data=PERMITS_POINTS%>%filter(ABBREVIATI==i, grepl("CONOCOPHILLIPS|MARATHON",ENVOPERATOR)),size=1.5)+
  geom_sf(data=PERMITS_LINES%>%filter(ABBREVIATI==i))+
  geom_sf(data=PERMITS_LINES %>%filter(COP_CHECK=='YES' & ABBREVIATI==i),color='red',size=2)+
  geom_sf(data=basin_lines,color='blue',linewidth=.5)+
  #geom_sf(data=asset_areas,fill=NA,color='blue',linewidth=.75)+
  theme_void()+
     theme( plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"), plot.background = element_rect(
      colour = "black",
      size = 1,
      fill='white'
    ),
    legend.position = position_legend,
    legend.justification = basin_legend,
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.box.background = element_rect(color="black", size=1),
    legend.box.margin = margin(6, 6, 6, 6),
    legend.title=element_blank())+
    coord_sf(
      xlim = c(bounding_box[1], bounding_box[3]),
      ylim = c(bounding_box[2], bounding_box[4]),
      expand = T,
      lims_method = 'box'
    )+
  guides(color = guide_legend(ncol = 1))+
  geom_sf_text(data = state_county, aes(label = NAME), size = 3)


  ggsave(plot=last_plot(),paste('//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/Weekly Activity Report/permits/maps/',i,'_MAP.png',sep=""),
         width=7.28,height=7.28)
  

  permit_tables <- st_drop_geometry(PERMITS_LINES%>%filter(ABBREVIATI==i)) %>%
    arrange(ENVOPERATOR,WELLNAME)%>%
    select(WELLNAME, ENVOPERATOR, API_UWI, TVD_FT,FORMATION,LENGTH, PERMITAPPROVEDDATE, ENVWELLTYPE,COP_CHECK,ABSTRACT,TOWNSHIP,SECTION,RANGE,COUNTY) %>%
    mutate(TOWNSHIP=ifelse(is.na(TOWNSHIP),"",TOWNSHIP),
           SECTION=ifelse(is.na(SECTION),"",SECTION),
           RANGE=ifelse(is.na(RANGE),"",RANGE),
           ABSTRACT=ifelse(is.na(ABSTRACT),"",ABSTRACT))%>%
    mutate(row_id = row_number()) %>%
    group_by(group = ceiling(row_id / 28)) %>%
    rename(OPERATOR=ENVOPERATOR,DEPTH=TVD_FT,TYPE=ENVWELLTYPE,
           TWN=TOWNSHIP,SEC=SECTION,RNG=RANGE,SRVY=ABSTRACT)%>%
    group_split()
  
    # Function to apply gt styling and save each table as PNG
  save_table_as_png <- function(tbl, index) {
    gt_table <- tbl %>%
      select(, -row_id) %>%  # Remove COP_CHECK and helper columns
      gt() %>%
      gtExtras::gt_theme_espn() %>%
      tab_options(
        column_labels.font.weight = 'bold',
        column_labels.font.size = 9,
        table.font.size = 8,
        table.width = 900,
        data_row.padding = px(1)
      ) %>%
      tab_style(
        style = list(cell_text(weight = "bold")),
        locations = cells_body(rows = COP_CHECK == 'NO')
      )%>%
      tab_style(
        style = list(cell_text(weight = "bold", color = 'maroon')),
        locations = cells_body(rows = COP_CHECK == 'YES')
      ) %>%
      cols_hide(columns = vars(COP_CHECK,group))%>%
      cols_align(align='center',columns=3:12)%>%
      fmt_number(columns = c(4,6), decimals = 0, sep_mark = ",", dec_mark = ",")  
    
    # Save the table as a PNG file with unique names
    gtsave(gt_table, filename = paste0("permits/tables/",i,"_", index, ".png",sep=""))
  
    
    
    }
  
  # Apply the function to each table chunk, saving each as a PNG file
  walk2(permit_tables, seq_along(permit_tables), save_table_as_png)
  
  
}



lapply(c('NDB','SDB','MBN','MBS'),permit_map_fctn)



#####COMPLETIONS OUTPUT----
#i='MBS'
completions_map_fctn<-function(i){
  bounding_box<- if(i=='NDB'){
    NDB_bbox
  }else if(i=='SDB'){
    SDB_bbox
  }else if(i=='MBN'){
    MBN_bbox
  }else if(i=='MBS'){
    MBS_bbox
  }
  
  
  basin_legend=if(i=='NDB'){
    c("left", "top")
  }else if(i=='SDB'){
    c("left", 'bottom')
  }else if(i %in% c('MBN','MBS')){
    c('right','top')
  }
  
  
  position_legend=if(i=='NDB'){
    c(.01,.99)
  }else if(i=='SDB'){
    c(.01, .01)
  }else if(i %in% c('MBN','MBS')){
    c(.99,.99)
  }
  
  
  
  ggplot()+
    basemap_gglayer( bounding_box, map_service = "carto", map_type = "light") +
    scale_fill_identity()+
    geom_sf(data=cop,fill='lightgrey',color=NA) + 
    geom_sf(data=state_county_clip,fill=NA)+
    geom_sf(data=potash,fill=NA,color='maroon')+
    geom_sf(data=COMPLETIONS_POINTS%>%filter(ABBREVIATI==i),aes(color=ENVOPERATOR),size=3.5)+
    scale_color_manual(
      name = "Operator", # Legend title
      values = setNames(COMPLETIONS_POINTS$colors, COMPLETIONS_POINTS$ENVOPERATOR) # Map ENVOPERATOR to colors
    )+
    geom_sf(data=COMPLETIONS_LINES%>%filter(ABBREVIATI==i, grepl("CONOCOPHILLIPS|MARATHON",ENVOPERATOR)),size=1.5)+
    geom_sf(data=COMPLETIONS_LINES%>%filter(ABBREVIATI==i))+
    geom_sf(data=COMPLETIONS_LINES %>%filter(COP_CHECK=='YES' & ABBREVIATI==i),color='red',size=2)+
    geom_sf(data=basin_lines,color='blue',linewidth=.5)+
    #geom_sf(data=asset_areas,fill=NA,color='blue',linewidth=.75)+
    theme_void()+
    theme( plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"), plot.background = element_rect(
      colour = "black",
      size = 1,
      fill='white'
    ),
    legend.position = position_legend,
    legend.justification = basin_legend,
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.box.background = element_rect(color="black", size=1),
    legend.box.margin = margin(6, 6, 6, 6),
    legend.title=element_blank())+
    coord_sf(
      xlim = c(bounding_box[1], bounding_box[3]),
      ylim = c(bounding_box[2], bounding_box[4]),
      expand = T,
      lims_method = 'box'
    )+
    guides(color = guide_legend(ncol = 1))+
    geom_sf_text(data = state_county, aes(label = NAME), size = 3)
  
  
  ggsave(plot=last_plot(),paste('//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/Weekly Activity Report/completions/maps/',i,'_MAP.png',sep=""),
         width=7.28,height=7.28)
  
  
  completions_tables <- st_drop_geometry(COMPLETIONS_LINES) %>%ungroup()%>%
    arrange(ENVOPERATOR,WELLNAME)%>%
    filter(ABBREVIATI==i)%>%
    select(API_UWI,WELLNAME,WELLNUMBER,ENVOPERATOR,ZONE,COMPLETIONDATE,OILTESTRATE_BBLPERDAY,GASTESTRATE_MCFPERDAY,LENGTH,CUMOIL_BBLPER1000FT,CUMPROD_BOEPER1000FT,SECTION,
           TOWNSHIP,RANGE,BLOCK,TVD_FT,COP_CHECK,COUNTY)%>%
    mutate(TOWNSHIP=ifelse(is.na(TOWNSHIP),"",TOWNSHIP),
           SECTION=ifelse(is.na(SECTION),"",SECTION),
           RANGE=ifelse(is.na(RANGE),"",RANGE),
           BLOCK=ifelse(is.na(BLOCK),"",BLOCK))%>%
    mutate(STR=paste(SECTION,TOWNSHIP,RANGE,BLOCK,sep="-"),
           WELLNAME=paste(WELLNAME,WELLNUMBER))%>%
    ungroup()%>%
    mutate(row_id = row_number()) %>%
    group_by(group = ceiling(row_id / 28)) %>%
    rename(OPERATOR=ENVOPERATOR,TVD=TVD_FT,COMPDTE=COMPLETIONDATE,OILPERDAY=OILTESTRATE_BBLPERDAY,GASPERDAY=GASTESTRATE_MCFPERDAY,CUMOILKFT=CUMOIL_BBLPER1000FT,CUMBOEKFT=CUMPROD_BOEPER1000FT)%>%
    select(API_UWI,WELLNAME,OPERATOR,TVD,ZONE,LENGTH,COMPDTE,OILPERDAY,GASPERDAY,CUMOILKFT,CUMBOEKFT,
             row_id,COP_CHECK,group,COUNTY,STR)%>%
    group_split()
  
  # Function to apply gt styling and save each table as PNG
  save_comp_table_as_png <- function(tbl, index) {
    gt_table <- tbl %>%
      select(, -row_id) %>%  # Remove COP_CHECK and helper columns
      gt() %>%
      gtExtras::gt_theme_espn() %>%
      tab_options(
        column_labels.font.weight = 'bold',
        column_labels.font.size = 9,
        table.font.size = 8,
        table.width = 900,
        data_row.padding = px(1)
      ) %>%
      tab_style(
        style = list(cell_text(weight = "bold")),
        locations = cells_body(rows = COP_CHECK == 'NO')
      )%>%
      tab_style(
        style = list(cell_text(weight = "bold", color = 'maroon')),
        locations = cells_body(rows = COP_CHECK == 'YES')
      ) %>%
      cols_hide(columns = vars(COP_CHECK,group))%>%
      cols_align(align='center',columns=3:14)%>%
      fmt_number(columns = c(1:13), decimals = 0, sep_mark = ",", dec_mark = ",")  
    
    # Save the table as a PNG file with unique names
    gtsave(gt_table, filename = paste0("completions/tables/",i,"_", index, ".png",sep=""))
    
    
    
  }
  
  # Apply the function to each table chunk, saving each as a PNG file
  walk2(completions_tables, seq_along(completions_tables), save_comp_table_as_png)
  
  
}



lapply(c('NDB','SDB','MBN','MBS'),completions_map_fctn)








#####powerpoint
# Define file paths for maps and tables

dbn_map_perm <- file.path('permits/maps', "NDB_MAP.png")
dbn_tables_perm <- list.files('permits/tables', pattern = "NDB_\\d+\\.png", full.names = TRUE)

dbn_map <- file.path('completions/maps', "NDB_MAP.png")
dbn_tables <- list.files('completions/tables', pattern = "NDB_\\d+\\.png", full.names = TRUE)

dbs_map_perm <- file.path('permits/maps', "SDB_MAP.png")
dbs_tables_perm <- list.files('permits/tables', pattern = "SDB_\\d+\\.png", full.names = TRUE)

dbs_map <- file.path('completions/maps', "SDB_MAP.png")
dbs_tables <- list.files('completions/tables', pattern = "SDB_\\d+\\.png", full.names = TRUE)


mbs_map_perm <- file.path('permits/maps', "MBS_MAP.png")
mbs_tables_perm <- list.files('permits/tables', pattern = "MBS_\\d+\\.png", full.names = TRUE)

mbs_map <- file.path("completions/maps", "MBS_MAP.png")
mbs_tables <- list.files("completions/tables", pattern = "MBS_\\d+\\.png", full.names = TRUE)

mbn_map_perm <- file.path('permits/maps', "MBN_MAP.png")
mbn_tables_perm <- list.files('permits/tables', pattern = "MBN_\\d+\\.png", full.names = TRUE)


mbn_map <- file.path('completions/maps', "MBN_MAP.png")
mbn_tables <- list.files('completions/tables', pattern = "MBN_\\d+\\.png", full.names = TRUE)


# Function to add Rig Slides
add_rig_slide <- function(ppt, title, image_path) {
  
  ppt <-  add_slide(ppt, layout = "2_Custom Layout", master = "Office Theme")
  ppt <-  ph_with(ppt, title, location = ph_location_label(ph_label = "rig_title") )
  ppt <-  ph_with(ppt, external_img("rigs/RIG_MAP.png"), location = ph_location_label(ph_label = "rig_map") )
  ppt <-  ph_with(ppt, external_img('rigs/RIG_chart.png'), location = ph_location_label(ph_label = "rig_chart") )
  return(ppt)
}




# Function to add a slide with a map (custom map theme)
add_map_slide <- function(ppt, title, image_path) {
  
  ppt <-  add_slide(ppt, layout = "Custom Layout", master = "Office Theme")
  ppt <-  ph_with(ppt, title, location = ph_location_label(ph_label = "Title 499") )
  ppt <-  ph_with(ppt, external_img(image_path), location = ph_location_label(ph_label = "Content Placeholder 499") )
  return(ppt)
}

# Function to add a slide with a table (custom table theme)

add_table_slide <- function(ppt, title, image_path) {
  
  ppt <-  add_slide(ppt, layout = "1_Custom Layout", master = "Office Theme")
  ppt <-  ph_with(ppt, title, location = ph_location_label(ph_label = "Title 500") )
  
  ###ratio
  info<-attr(readPNG(image_path, info=T), 'info')
  dims<-info$dim/info$dpi
  width_dim=info$dim[1]
  height_dim=info$dim[2]
  ratio=height_dim/width_dim
  #.5788732
  # ppt <- ph_with(ppt, value=external_img(image_path),width=20,height=15,
  #                 location = ph_location(left= .1,top=.90),use_loc_size=F)
  # 
  ppt <- ph_with(ppt, value=external_img(image_path,width=13.33,height=13.33*ratio),
                  location = ph_location(left= 0,top=.90),use_loc_size=F)
  return(ppt)
}


####RIGS----
generate_random_hex_colors <- function(x) {
  replicate(x, paste0("#", paste0(sample(c(0:9, letters[1:6]), 6, replace = TRUE), collapse = "")))
}


color_add<-dput(structure(list(ENVOPERATOR = c("EXXON", "OCCIDENTAL", "DIAMONDBACK", 
                                             "MEWBOURNE", "EOG", "CONOCOPHILLIPS", "PERMIAN", "DEVON", "CHEVRON", 
                                             "APA", "CONTINENTAL", "COTERRA", "MATADOR", "DOUBLE", "OVINTIV", 
                                             "BLACKBEARD","TAP"), colors = c("blue", "lightblue", "#f7199b", "orange", 
                                                                       "black", "yellow", "turquoise", "red", "darkgreen", "gold", "darkblue", 
                                                                       "maroon", "purple", "brown", "#510A0A", "pink",'darkorange')), class = "data.frame", row.names = c(NA, 
                                                                                                                                                           -17L)))
ENV_RIGS<-left_join(ENV_RIGS,color_add)


#ENV_RIGS_SUM<-
  



sf_data<-ENV_RIGS%>%mutate(colors=ifelse(ENVOPERATOR %in% c(color_add$ENVOPERATOR),colors,'grey'))%>%
  mutate(ENVOPERATOR=ifelse(colors=='grey','OTHER',ENVOPERATOR))



ggplot(data = sf_data) +
  basemap_gglayer(RIGS_bbox, map_service = "carto", map_type = "light") +
  scale_fill_identity()+
  geom_sf(data=cop,fill='lightgrey',color=NA,alpha=.5) + 
  geom_sf(data=state_county_clip,fill=NA)+
  geom_sf(data=potash,fill=NA,color='maroon')+
  geom_sf(aes(color = ENVOPERATOR), size = 4) + # Use ENVOPERATOR for the legend
  scale_color_manual(
    name = "Operator", # Legend title
    values = setNames(sf_data$colors, sf_data$ENVOPERATOR) # Map ENVOPERATOR to colors
  ) +
  # geom_sf(data=ENV_RIGS%>%filter(rank>15),color='grey',size=3.5)+
  # geom_sf(data=ENV_RIGS%>%filter(rank<16),aes(color=ENVOPERATOR),size=3.5)+
  geom_sf(data=sf_data%>%filter(grepl('CONOCOPHILLIPS',ENVOPERATOR)),color='black',size=1)+
  geom_sf(data=basin_lines,color='blue',linewidth=.5)+
  theme_void()+
  theme( plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"), plot.background = element_rect(
    colour = "black",
    size = 1,
    fill='white'
  ),
  legend.position = 'none',
  legend.title=element_blank())+
  coord_sf(
    xlim = c(RIGS_bbox[1], RIGS_bbox[3]),
    ylim = c(RIGS_bbox[2], RIGS_bbox[4]),
    expand = T,
    lims_method = 'box'
  )+
  geom_sf_text(data = state_county, aes(label = NAME), size = 3)

  

ggsave(plot=last_plot(),'//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/Weekly Activity Report/rigs/RIG_MAP.png',
       width=6.5,height=5.59)


ggplot(st_drop_geometry(sf_data) %>% filter(ENVOPERATOR != 'OTHER') %>% distinct(ENVOPERATOR, .keep_all = TRUE)) +
  geom_bar(aes(fct_reorder(ENVOPERATOR, max_rigs), max_rigs, fill = ENVOPERATOR, label = max_rigs), stat = 'identity') + 
  scale_fill_manual(
    name = "Operator", # Legend title
    values = setNames(sf_data$colors, sf_data$ENVOPERATOR) # Map ENVOPERATOR to colors
  ) +
  theme_minimal() +
  theme(legend.position = 'none') +
  geom_text(aes(fct_reorder(ENVOPERATOR, max_rigs), max_rigs, label = max_rigs), color = "white",
            position = position_dodge2(width = 0.5),
            show.legend = FALSE, hjust = 1.2, size = 4, fontface = 'bold') +
  geom_text(data = st_drop_geometry(sf_data) %>% filter(ENVOPERATOR == 'CONOCOPHILLIPS')%>%distinct(max_rigs,.keep_all = T),
            aes(fct_reorder(ENVOPERATOR, max_rigs), max_rigs, label = max_rigs), color = "black",
            position = position_dodge2(width = 0.5),
            show.legend = FALSE, hjust = 1.2, size = 4, fontface = 'bold') +
  coord_flip() +
  xlab("") +
  ylab("")

ggsave(plot=last_plot(),'//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/__SOURCE FILES/Weekly Activity Report/rigs/RIG_chart.png',
       width=6.3,height=5.59 )



######POWERPOINT OUTPUT----

# Create a new PowerPoint object
ppt<- read_pptx("PPT_TEMPLATE_DO_NOT_DELETE/TEMPLATE.pptx")


##Rig Slide
ppt <- add_rig_slide(ppt, "Rigs", 'asdf')



###COMPLETIONS----
# Add DBN map slide
ppt <- add_map_slide(ppt, "NDB Completions", dbn_map)

# Add DBN table slides
for (i in seq_along(dbn_tables)) {
  ppt <- add_table_slide(ppt, paste("NDB Completions"), dbn_tables[i])
}

# Add DBS map slide
ppt <- add_map_slide(ppt, "SDB Completions", dbs_map)

# Add DBS table slides
for (i in seq_along(dbs_tables)) {
  ppt <- add_table_slide(ppt, paste("SDB Completions"), dbs_tables[i])
}

# Add MBN map slide
ppt <- add_map_slide(ppt, "MBN Completions", mbn_map)

# Add MBN table slides
for (i in seq_along(mbn_tables)) {
  ppt <- add_table_slide(ppt, paste("MBN Completions"), mbn_tables[i])
}


# Add MBS map slide
ppt <- add_map_slide(ppt, "MBS Completions", mbs_map)

# Add MBS table slides
for (i in seq_along(mbs_tables)) {
  ppt <- add_table_slide(ppt, paste("MBS Completions"), mbs_tables[i])
}


###PERMITS----
# Add DBN map slide
ppt <- add_map_slide(ppt, "NDB Permits", dbn_map_perm)

# Add DBN table slides
for (i in seq_along(dbn_tables_perm)) {
  ppt <- add_table_slide(ppt, paste("NDB Permits"), dbn_tables_perm[i])
}

# Add DBS map slide
ppt <- add_map_slide(ppt, "SDB Permits", dbs_map_perm)

# Add DBS table slides
for (i in seq_along(dbs_tables_perm)) {
  ppt <- add_table_slide(ppt, paste("SDB Permits"), dbs_tables_perm[i])
}

# Add MBN map slide
ppt <- add_map_slide(ppt, "MBN Permits", mbn_map_perm)

# Add MBN table slides
for (i in seq_along(mbn_tables_perm)) {
  ppt <- add_table_slide(ppt, paste("MBN Permits"), mbn_tables_perm[i])
}


# Add MBS map slide
ppt <- add_map_slide(ppt, "MBS Permits", mbs_map_perm)

# Add MBS table slides
for (i in seq_along(mbs_tables_perm)) {
  ppt <- add_table_slide(ppt, paste("MBS Permits"), mbs_tables_perm[i])
}



# Save the PowerPoint
output_file <- file.path(getwd(), "Weekly_Report.pptx")
print(ppt, target = output_file)

cat("PowerPoint presentation saved as", output_file)

x=st_drop_geometry(COMPLETIONS_POINTS)%>%group_by(ENVOPERATOR)%>%
  summarise(count=max(row_number()))
