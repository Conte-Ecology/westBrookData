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

getRiver<-function(location){
  location<-location[1]
  num<-grep(substr(location,1,6),c("Jimmy ","Mitche","O'Bear","West B"))
  if(length(num)==0){return(as.character(NA))}
  river<-c("jimmy","mitchell","obear","west brook")[num]
  return(river)
}

temps %>%
  group_by(location) %>%
  mutate(river=getRiver(location)) %>%
  ungroup()
           
dbDropTable("raw_temps")
dbWriteTable(con,"raw_temps",data.frame(temps))  

