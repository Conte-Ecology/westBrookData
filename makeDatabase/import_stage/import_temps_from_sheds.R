agencies <- tbl(conSheds, "agencies") %>%
  select(agency_id=id, agency_name=name)
locations <- tbl(conSheds, "locations") %>%
  select(location_id=id, location_name=name, location_description=description, latitude, longitude, agency_id)
series <- tbl(conSheds, "series") %>%
  select(series_id=id, variable_id, location_id, start_datetime, end_datetime, value_count)
temps <- tbl(conSheds, "values") %>%
  select(series_id, datetime, value, flagged) %>%
  left_join(series, by="series_id") %>%
  left_join(locations, by="location_id") %>%
  left_join(agencies, by="agency_id") %>%
  filter(agency_name=="USGS_Conte") %>%
  select(location=location_name,datetime,temperature=value) %>%
  collect()

riverSection<-data.frame(location=c("Jimmy Nolan Brook sec 15",
                                    "Jimmy Nolan Brook",
                                    "O'Bear sec 1",
                                    "O'Bear Brook sec 15",
                                    "Stanley Brook sec 11",
                                    "West Brook B100",
                                    "Mitchell Brook sec 1",
                                    "West Brook 6",
                                    "West Brook 45",
                                    "West Brook 30",
                                    "West Brook smolt trap"),
                         section=c("15","1","1","15","11",
                                   "B100","1","6","45","30","smoltTrap"),
                         river=c("jimmy","jimmy","obear","obear",
                                 "stanley","west brook","mitchell",
                                 "west brook","west brook",
                                 "west brook","west brook"),
                         stringsAsFactors=F)

temps<-left_join(temps,riverSection,by='location')

#add in the early records from the depth logger
earlyPath<-file.path(original_data_dir,"earlyWestBrookEnv.csv")
early<-suppressWarnings(fread(earlyPath))
setnames(early,c("date/time","final depth"),c("dateTime","finalDepth"))
early<-early[,list(location="West Brook sec ??? Depth logger",
                   river="west brook",
                   section=NA,
                   datetime=as.POSIXct(dateTime*24*60*60,origin=as.POSIXct("1899-12-30 00:00:00")),
                   temperature=temp)]
temps<-bind_rows(temps,early)

dbDropTable("raw_temps")
dbWriteTable(con,"raw_temps",data.frame(temps),row.names=F)  

