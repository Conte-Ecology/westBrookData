antennaLocations<-tbl(conDplyr,"antenna_deployment") %>%
  collect(n=Inf) %>%
  data.table() %>%
  .[,.(river,river_meter)] %>%
  unique()

roundMeter<-function(river_meter){
  river_meter<-as.numeric(river_meter)
  out<-NA
  for(i in 1:length(river_meter)){
    belowThresh<-min(abs(river_meter[i]-antennaLocations$river_meter))<=1
    w<-which.min(abs(river_meter[i]-antennaLocations$river_meter))
    out[i]<-ifelse(belowThresh,antennaLocations$river_meter[w],NA)
  }
  return(out)
}

column_code <- list(
  drainage = function() return("west"),
  river = function(river,river_meter){
    rivers<-c("wb jimmy"="jimmy",
              "wb mitchell"="mitchell",
              "wb obear"="obear",
              "west brook"="west brook")
    
    river_meter<-roundMeter(river_meter)
    
    rivers<-names(rivers)[match(river,rivers)]
    
    rows<-match(as.numeric(river_meter[is.na(rivers)]),
                antennaLocations$river_meter)
    rivers[is.na(rivers)]<-antennaLocations[rows,river]
    
    return(rivers)},
  river_meter = function(river_meter){
    river_meter<-roundMeter(river_meter)
    return(river_meter)},
  datetime = function(date,time,reader_time,adjusted_time) {
    time[is.na(time)]<-adjusted_time[is.na(time)] #grabbing other times if it wasn't recorded
    time[is.na(time)]<-reader_time[is.na(time)]
    time[is.na(time)]<-0.5 #assigning noon to NA times for now if no time is available
    return(as.POSIXct(as.numeric(date)*3600*24+as.numeric(time)*3600*24,origin=as.POSIXct("1899-12-30")))
  },
  section = function(section) return(section),
  id = function(id) return(id),
  on_off = function(on_off) return(as.numeric(on_off)),
  origin = function(origin) return(origin),
  sheet_number = function(sheet_number) return(sheet_number),
  reader_id = function(reader_id) return(reader_id),
  buffer_arrival = function(buffer_arrival) return(as.numeric(buffer_arrival)),
  buffer_departure = function(buffer_departure) return(as.numeric(buffer_departure)),
  reader_time = function(date, reader_time){
    return(as.POSIXct(as.numeric(date)*3600*24+as.numeric(reader_time)*3600*24,
                      origin=as.POSIXct("1899-12-30")))
  },
  adjusted_time = function(date, adjusted_time,reader_time){
    return(as.POSIXct(as.numeric(date)*3600*24+as.numeric(adjusted_time)*3600*24,
                      origin=as.POSIXct("1899-12-30")))
  },
  stake_height = function(stake_height) return(as.numeric(stake_height)),
  actions = function(actions) return(actions),
  rms_on_arrival = function(rms){
    rmsSplit<-tstrsplit(rms,"/")
    return(as.numeric(rmsSplit[[1]]))
  },
  rms_on_departure = function(rms){
    rmsSplit<-strsplit(rms,"/")
    return(as.numeric(rmsSplit[[2]]))
  },
  #phase = function(ph) return(as.numeric(ph)), #nothing in here, so leaving it out
  signal = function(signal) return(as.numeric(signal)),
  current_on_arrival = function(current){
    curSplit<-tstrsplit(current,"/")
    return(as.numeric(curSplit[[1]]))
  },
  current_on_departure = function(current){
    curSplit<-tstrsplit(current,"/")
    return(as.numeric(curSplit[[2]]))
  },
  initials = function(initials) return(initials),
  comments_1 = function(comments_1) return(comments_1),
  comments_2 = function(comments_2) return(comments_2),
  field_book = function(field_book) return(field_book)
)

log <- pipeline_data_transformation(
  data=dbGetQuery(con, "SELECT * FROM raw_antenna_log;"),
  pipeline=column_code) %>%
  data.table()

#river meters are backwards for these antennas in the log files
log[river=="wb jimmy"&river_meter %in% c(5020,5024.1)&datetime<as.POSIXct("2007-01-01"),river_meter:=5020]
log[river=="wb jimmy"&river_meter %in% c(5020,5024.1)&datetime>as.POSIXct("2007-01-01"),river_meter:=5024.1]

#save lists of ones that need a closer look
signalInComments<-log[grepl("signal",comments_1)|grepl("signal",comments_2)] %>%
  .[,reason:="signal in comments"]
mediumSignal<-log[signal>20&signal<75,] %>%
  .[,reason:="medium signal"]
rmsLow<-log[rms_on_arrival<1&rms_on_arrival!=0] %>%
  .[,reason:="low rms"]
onOffNa<-log[is.na(on_off)] %>%
  .[,reason:="on_off is NA"]

weirdos<-rbind(signalInComments,mediumSignal,rmsLow,onOffNa)
write.csv(weirdos,file.path(processed_data_dir,"logLinesToCheck.csv"))


#look for indicators of non-working antennas and adjust on_off accordingly
log[grepl("rnl",comments_1)|grepl("rnl",comments_2),on_off:=0]
log[signal>=75,on_off:=0]
log[rms_on_arrival==0,on_off:=0]
log[grepl("doa",actions)|grepl("doa",comments_1)|
      grepl("doa",comments_2),
    on_off:=0]
log[grepl("tf",actions)|grepl("rnl",actions)|grepl("doa",actions),on_off:=0]

dbWriteTable(con, 'data_antenna_log', data.frame(log), row.names=FALSE,
             overwrite=TRUE, append=FALSE)
