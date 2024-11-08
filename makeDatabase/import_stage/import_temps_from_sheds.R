agencies <- tbl(conSheds, "agencies") %>%
  select(agency_id=id, agency_name=name)
locations <- tbl(conSheds, "locations") %>%
  select(location_id=id, location_name=name, location_description=description, latitude, longitude, agency_id)
series <- suppressWarnings(tbl(conSheds, "series") %>%
  select(series_id=id, variable_id, location_id, start_datetime, end_datetime, value_count))
temps <- tbl(conSheds, "values") %>%
  select(series_id, datetime, value) %>%
  left_join(series, by="series_id") %>%
  left_join(locations, by="location_id") %>%
  left_join(agencies, by="agency_id") %>%
  filter(agency_name=="USGS_Conte") %>%
  select(location=location_name,datetime,temperature=value) %>%
  collect(n=Inf)

riverSection<-data.frame(location=c("Jimmy Nolan Brook sec 15",
                                    "Jimmy Nolan Brook (Onset)",
                                    "O'Bear sec 1 (Onset)",
                                    "O'Bear Brook sec 15 (Onset)",
                                    "Stanley Brook sec 11",
                                    "West Brook B100 (Solinst)",
                                    "Mitchell Brook sec 1 (Onset)",
                                    "West Brook 6 (Onset)",
                                    "West Brook 45",
                                    "West Brook 30",
                                    "West Brook smolt trap",
                                    "Jimmy Brook Sec 2 (Solinst)",
                                    "Mitchell Brook Sec 1 (Solinst)",
                                    "O'Bear Brook Sec 1 (Solinst)"),
                         section=c("15","1","1","15","11",
                                   "B100","1","6","45","30","smoltTrap",
                                   "2","1","1"),
                         river=c("wb jimmy","wb jimmy","wb obear","wb obear",
                                 "stanley","wb west brook","wb mitchell",
                                 "west brook","west brook",
                                 "west brook","west brook",
                                 "wb jimmy","wb mitchell","wb obear"),
                         stringsAsFactors=F)

temps<-left_join(temps,riverSection,by='location')

badDates<-list(mitchell=seq(as.Date("2013-04-01"),as.Date("2013-08-01"),by='days'))

temps<-temps %>% filter(river!="wb mitchell"|! as.Date(datetime) %in% badDates$mitchell)

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

