library(odbc)
library(DBI)
library(tidyverse)
library(data.table)

### Open connection
myconn_out <- dbConnect(odbc::odbc(), "RCDB") #connection to output database



spacing<-data.table::fread("//conoco.net/HO_SHARED/MaxWell_L48_MC/MAX_General/PERMIAN/@BUSINESS_DEVELOPMENT/TECHNICAL/Individual Work/DM Work_COP/Spacing/Final_Tables/Unfiltered_Spacing_Output.csv")

spacingf<-spacing%>%group_by(API12)%>%
  arrange(desc(Hypotenuse))%>%
  distinct(Offset_UWI10,.keep_all=T)%>%ungroup()%>%
  arrange(Hypotenuse)

left = function (string,char) {
  substr(string,1,char)
}

spacingf$API10=left(spacingf$API12,10)

spacingf<-spacingf%>%relocate(API10)%>%select(-V1)


spacingf<-spacingf%>%filter(API10 != Offset_UWI10)


spacingfO=spacingf

glimpse(spacingfO)


####masterline spacing column name change



dbWriteTable(myconn_out, paste('WellSpacingMasterlineUnqOffsetAPI',sep=""),spacingf, append=F,overwrite=T,  row.names=FALSE)




#dbRemoveTable(myconn_out, "WellSpacingMaasterlineUnqOffsetAPI")

