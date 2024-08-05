### Connect to Vincent's SQL server
library(odbc)
library(tidyverse)
library(data.table)
library(readxl)

setwd("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Individual Work/DM Work_COP/Spacing")




### Open connection
myconn_out <- dbConnect(odbc::odbc(), "RCDB")


#########################################################################################################################################
#########################################################################################################################################

# hz_LandingZone = dbGetQuery(myconn_out,"SELECT 
#                         L48_PERMIAN_RC_DEV.dbo.LANDING_ZONE.UWI AS UWI,
#                         L48_PERMIAN_RC_DEV.dbo.LANDING_ZONE.LANDING_ZONE AS LANDING_ZONE,
#                          L48_PERMIAN_RC_DEV.dbo.LANDING_ZONE.BUSINESS_UNIT AS BUSINESS_UNIT
#                         FROM L48_PERMIAN_RC_DEV.dbo.LANDING_ZONE")


#########################################################################################################################################
#########################################################################################################################################

# ### Landing Zone table Prep:
# hz_DB_LandingZone1 = dbGetQuery(myconn_out,"SELECT 
#                         L48_PERMIAN_RC_DEV.dbo.TRINITY_LANDING_ZONE.UWI AS UWI,
#                         L48_PERMIAN_RC_DEV.dbo.TRINITY_LANDING_ZONE.LANDING_ZONE AS LANDING_ZONE,
#                         L48_PERMIAN_RC_DEV.dbo.TRINITY_LANDING_ZONE.IS_FORCE_CODED AS FRC_CODE,
#                         L48_PERMIAN_RC_DEV.dbo.TRINITY_LANDING_ZONE.REMARK AS REMARK,
#                         L48_PERMIAN_RC_DEV.dbo.TRINITY_LANDING_ZONE.ROW_CHANGED_DATE AS DATE
#                          
#                         FROM L48_PERMIAN_RC_DEV.dbo.TRINITY_LANDING_ZONE")



#########################################################################################################################################
#########################################################################################################################################

### Well Survey Station table Prep:

surveys<- dbGetQuery(myconn_out,"SELECT *
                         
                        FROM RCDB.dbo.PETREL_WELL_DIR_SRVY_STATION__LATERAL_wENV
                                    
                        ")

surveys <- surveys %>%
  mutate(API10 = substring(UWI, first=1, last=10))


### Filter out single-pt (& limited-pt) wells
surveys<- surveys %>%
  group_by(UWI) %>%
  mutate(COUNT = max(row_number())) %>%
  mutate(SinglePt = ifelse(COUNT == 1, "YES", "NO")) %>%
  filter(SinglePt == "NO") %>%
  mutate(LimitedPt = ifelse(COUNT <= 5, "YES", "NO")) %>%   # OPTIONAL
  filter(LimitedPt == "NO") %>%
  select(-c(COUNT, SinglePt, LimitedPt))







### Read In Midland Basin Landing Zone
# hz_MB_LandingZone <- read_excel("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@GGRE_MB/_SPOTFIRE/DataFiles/LandingZones/MBBU_LZ_UPDATE.xlsx", sheet = "Sheet1")
# 
# 
# ### Add Tank column to MB landing zone table
# colNames <- data.table(colnames(hz_MB_LandingZone))
# unique_LZs <- data.frame(unique(hz_MB_LandingZone$'Bulk Name'))

# unique(hz_LandingZone$LANDING_ZONE)
# 
# hz_LandingZone <- hz_LandingZone %>%
#   mutate(Tank = case_when(`LANDING_ZONE` == "CLFK" ~ "Clearfork", 
#                           `LANDING_ZONE` == "USBY" ~ "Upper Spraberry", 
#                           `LANDING_ZONE` == "MSBY" ~ "Middle Spraberry", 
#                           `LANDING_ZONE` == "JMILL" ~ "Jo Mill", 
#                           `LANDING_ZONE` == "LSBY" ~ "Lower Spraberry", 
#                           `LANDING_ZONE` == "DEAN" ~ "Dean", 
#                           `LANDING_ZONE` == "WFMP" ~ "Wolfcamp A", 
#                           `LANDING_ZONE` == "WFMP A" ~ "Wolfcamp A", 
#                           `LANDING_ZONE` == "WFMP B" ~ "Wolfcamp B", 
#                           `LANDING_ZONE` == "WFMP C" ~ "Wolfcamp C",
#                           `LANDING_ZONE` == "WFMP D" ~ "Wolfcamp D",
#                           `LANDING_ZONE` == "STRN" ~ "Strawn",                           
#                           `LANDING_ZONE` == "ATOKA" ~ "Atoka", 
#                           `LANDING_ZONE` == "BRNT" ~ "Barnett", 
#                           `LANDING_ZONE` == "WDFD" ~ "Woodford", 
#                           `LANDING_ZONE` == "DVNN" ~ "Devonian", 
#                           TRUE ~ ""))
# 
# # hz_MB_LandingZone <- hz_MB_LandingZone %>%
# #   mutate(API10 = substring(UWI, first=1, last=10)) %>%
# #   select(c(API10, LANDING_ZONE, Tank))
# 
# ### Work with Well Surveys
# 
# hz_LandingZone<-hz_LandingZone%>%arrange(LANDING_ZONE)
# 
# 
# hz_LandingZone <- hz_LandingZone %>%
#   mutate(Tank = ifelse(grepl('ATOK',LANDING_ZONE),'ATOKA',
#          ifelse(grepl('AVLN',LANDING_ZONE),'AVLN',
#          ifelse(grepl('BRNT',LANDING_ZONE),'BRNT',
#          ifelse(grepl('BS1',LANDING_ZONE),'BS1',
#          ifelse(grepl('BS2',LANDING_ZONE),'BS2',
#          ifelse(grepl('BS3C',LANDING_ZONE),'BS3C',
#          ifelse(grepl('BS3S',LANDING_ZONE),'BS3S',
#          ifelse(grepl('JOMILL',LANDING_ZONE),'JMILL',
#          ifelse(grepl('LSBY',LANDING_ZONE),'LSBY',
#          ifelse(grepl('MSBY',LANDING_ZONE),'MSBY',
#          ifelse(grepl('USBY',LANDING_ZONE),'MSBY',
#          ifelse(grepl('WFMP A',LANDING_ZONE),'WFMP A',
#          ifelse(grepl('WFMP B',LANDING_ZONE),'WFMP B',
#          ifelse(grepl('WFMP C',LANDING_ZONE),'WFMP C',
#          ifelse(grepl('WFMP D',LANDING_ZONE),'WFMP D',
#          ifelse(grepl('WFMP SD',LANDING_ZONE),'WFMP A',
#          ifelse(grepl('WFMP UNC',LANDING_ZONE),'WFMP A',
#          ifelse(grepl('WFMP WEDGE',LANDING_ZONE),'WFMP A',LANDING_ZONE)))))))))))))))))))

# 
# hz_LandingZone <- hz_LandingZone %>%
#    mutate(API10 = substring(UWI, first=1, last=10)) %>%
#    select(c(API10, LANDING_ZONE))




### LOG OFF ###
dbDisconnect(myconn_out)


# zone<- hz_LandingZone
# 
# 
# setwd("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Individual Work/DM Work_COP/Spacing")
# 
# fwrite(zone,'zone.csv')

#### Filter to Lateral Only in surveys



surveys<- surveys%>% group_by(UWI)%>%arrange(MD)%>%mutate(Count= dense_rank(row_number()))
surveys<-surveys%>%mutate(AZIMUTH=abs(ifelse(Az > 180, Az, Az - 360)))
surveys<- surveys%>% group_by(UWI)%>% mutate(meanTVD= mean(TVD))%>%mutate(Std= sd(AZIMUTH))
surveys<- surveys%>% group_by(UWI)%>%mutate(sd_filter= abs(AZIMUTH-mean(AZIMUTH)) < Std)
surveys<- surveys%>% group_by(UWI)%>%filter(!(Std>2 & sd_filter=='FALSE'))


surveys$STATION_LAT <- as.numeric(surveys$Lat)
surveys$STATION_LONG <- as.numeric(surveys$Long)

surveys<- surveys%>% group_by(UWI)%>% filter(Count==min(Count)|Count== max(Count))


####well header table

WELL_HEADER<- dbGetQuery(myconn_out,"SELECT *
                         
                        FROM RCDB.dbo.WELL_HEADER
                                    
                        ")


surveys<-left_join(surveys,WELL_HEADER%>%rename(UWI=API_12)%>%select(UWI,HOLE_DIRECTION)%>%distinct(UWI,.keep_all=T))

surveys<-surveys%>%filter(HOLE_DIRECTION=='HORIZONTAL')





data.table::fwrite(surveys%>% select(-Count,-c(Std:sd_filter,API10)),'Prepared_Surveys/master_survey_update2.csv')




