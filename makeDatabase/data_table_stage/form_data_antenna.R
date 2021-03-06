source_data <- dbGetQuery(con, "SELECT * FROM tags_antenna;") %>%
               filter(is.na(alive_or_dead)|alive_or_dead!="dead")
source_data2<- dbGetQuery(con,"SELECT * FROM tags_antenna_2011_2015")
source_data3<- dbGetQuery(con,"SELECT * FROM tags_allflex_to_2011")

allTags<-tbl(conDplyr,"data_by_tag") %>%
  select(tag) %>%
  collect() %>%
  data.table()

column_code_portable <- list(
  tag = function(tag) {
    return(tag)
  },
  detection_date = function(earliest_detection_date_time) {
    detection_date <- parse_date_time(x=earliest_detection_date_time, orders=date.format)
    detection_date[detection_date > now()] <- 
      detection_date[detection_date > now()] - years(100)
    return(detection_date)
  },
  drainage = function(drainage,river){
    drainage[river %in% c("west brook","wb jimmy","wb mitchell","wb obear")]<-
      "west"
    return(drainage)
  },
  river = function(river) return(river),
  area = function(area) return(area),
  section = function(section) return(section),
  survey = function() return("portableAntenna"),
  sample_name = function(sample_name) return(sample_name),
  reader_id = function(reader_id) return(reader_id),
  sample_type = function(sample_type) return(sample_type),
  alive_or_dead = function(alive_or_dead) return(alive_or_dead),
  instance = function(antenna_instance) return(as.numeric(antenna_instance)),
  pass = function(pass) return(as.numeric(pass)),
  quarter = function(quarter) return(as.numeric(quarter)),
  left_or_right = function(left_or_right) return(left_or_right),
  habitat = function(habitat) return(habitat),
  cover = function(cover) return(cover),
  justification = function(justification) return(justification),
  comment = function(comment) return(comment)
)

portableData<-source_data[grepl("able",source_data$sample_type)|
                            grepl("wand",source_data$sample_type),]

portableData <- pipeline_data_transformation(
  data=portableData, pipeline=column_code_portable)

portableData<-portableData[portableData$tag %in% allTags$tag,]

dbWriteTable(con, 'data_portable_antenna', portableData, row.names=FALSE,
             overwrite=TRUE, append=FALSE)

column_code_stationary <- list(
  tag = function(tag) {
    return(tag)
  },
  detection_date = function(earliest_detection_date_time) {
    require(lubridate)
    detection_date <- parse_date_time(x=earliest_detection_date_time, orders=date.format)
    detection_date[detection_date > now()] <- 
      detection_date[detection_date > now()] - years(100)
    return(detection_date)
  },
  drainage = function(drainage,river){
    drainage[river %in% c("west brook","wb jimmy","wb mitchell","wb obear")]<-
      "west"
    return(drainage)
  },
  river = function(river) return(river),
  river_meter = function(distance_upriver_m,river){
    return(as.numeric(distance_upriver_m))},
  survey = function() return("stationaryAntenna"),
  reader_type = function(sample_type) return(sample_type),
  reader_id = function(reader_id) return(reader_id),
  departure = function(departure) return(as.POSIXct(as.numeric(departure),
                                                    origin=as.POSIXct("1899-12-30"))),
  comment = function(comment) return(comment)
)

column_code_new_stationary <- list(
  tag = function(tag) {
    return(tag)
  },
  detection_date = function(detection_date) {
    return(detection_date)
  },
  drainage = function(drainage,river){
    return(drainage)
  },
  river = function(river) return(river),
  river_meter = function(river_meter) return(as.numeric(river_meter)),
  survey = function() return("stationaryAntenna"),
  reader_type = function(reader_type) return(reader_type),
  reader_id = function(location) return(location),
  departure = function() (return(as.POSIXct(NA))),
  comment = function() return(as.character(NA))
)

filter15Min<-function(time){
  if(!all(difftime(time[2:length(time)],time[1:(length(time)-1)],
                   units="mins")>=15)&length(time)>1){
    for(t in 1:(length(time)-1)){
      if(is.na(time[t])){next}
      timeDiff<-difftime(time[(t+1):length(time)],time[t],units="mins")
      time[which(timeDiff<15)+t]<-NA
    }
  }
  return(time)
}

getDeparture<-function(time,goodTimes){
  if(any(is.na(goodTimes))){
    departure<-rep(as.POSIXct(NA),length(time))
    nonNa<-which(!is.na(goodTimes))
    depTimes<-na.omit(time[c(nonNa[2:length(nonNa)]-1,length(time))])
    departure[nonNa]<-depTimes
    
    # diffNa<-diff(nas)
    # departure[nas[which(diffNa>1)]]<-
    #   time[nas[which(diffNa>1)]]
    # departure[length(time)]<-time[length(time)]
  } else {
    departure<-time
  }
  
  return(departure)
}

stationaryData<-source_data[grep("stationary",source_data$sample_type),]

stationaryData <- pipeline_data_transformation(
  data=stationaryData, pipeline=column_code_stationary) %>%
  data.table() %>%
  setkey(tag,detection_date) %>%
  .[,goodTimes:=filter15Min(detection_date),by=tag] %>%
  .[!is.na(goodTimes)] %>%
  .[,goodTimes:=NULL]

newStationaryData<-pipeline_data_transformation(
  data=source_data2,pipeline=column_code_new_stationary)
newStationaryData<-data.table(newStationaryData) %>%
  setkey(tag,detection_date) %>%
  .[,goodTimes:=filter15Min(detection_date),by=tag] %>%
  .[,departure:=getDeparture(detection_date,goodTimes),by=tag] %>%
  .[!is.na(goodTimes)] %>%
  .[,goodTimes:=NULL]

oldAllflexData<-pipeline_data_transformation(
  data=source_data3,pipeline=column_code_new_stationary)
oldAllflexData<-data.table(oldAllflexData) %>%
  setkey(tag,detection_date) %>%
  .[,goodTimes:=filter15Min(detection_date),by=tag] %>%
  .[,departure:=getDeparture(detection_date,goodTimes),by=tag] %>%
  .[!is.na(goodTimes)] %>%
  .[,goodTimes:=NULL]

stationaryData<-rbind(stationaryData,newStationaryData,oldAllflexData) %>%
  data.table() %>%
  setkey(river,river_meter)

antennaLocations<-fread(paste0(processed_data_dir,"/antenna_locations.csv")) %>%
  setnames("old_river_meter","river_meter") %>%
  setkey(river,river_meter)

stationaryData<-antennaLocations[stationaryData] %>%
  .[is.na(true_river_meter),true_river_meter:=river_meter] %>%
  .[,river_meter:=true_river_meter] %>%
  .[,":="(true_river_meter=NULL)]

stationaryData<-stationaryData[tag %in% allTags$tag]

dups<-duplicated(stationaryData[,,.(tag,detection_date,river,river_meter)])
stationaryData<-stationaryData[!dups]

dbWriteTable(con, 'data_stationary_antenna', data.frame(stationaryData), row.names=FALSE,
             overwrite=TRUE, append=FALSE)

