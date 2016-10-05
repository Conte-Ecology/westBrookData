deployed<-tbl(conDplyr,"antenna_deployment") %>%
  select(river,river_meter,deployed,removed) %>%
  filter(!is.na(deployed)) %>%
  collect(n=Inf) %>%
  data.table() %>%
  .[,":="(deployed=as.Date(deployed),
          removed=as.Date(removed),
          date=as.Date("2009-01-01"))] %>%
  setkey(date)

allDates<-data.table(date=seq.Date(min(deployed$deployed),
                                     max(deployed$removed),
                                   "day"),
                                 key="date")

deployed<-deployed[,.SD[allDates],by=.(river,river_meter,deployed,removed)]
deployed<-deployed[date>=deployed&date<=removed,.(river,river_meter,date)] %>%
          .[,deployed:=TRUE]

log<-tbl(conDplyr,"data_antenna_log") %>%
  select(river,river_meter,datetime,on_off) %>%
  collect(n=Inf) %>%
  data.table() %>%
  .[,date:=as.Date(datetime)] %>%
  .[,datetime:=NULL]

status<-full_join(deployed,log,by=c("river","river_meter","date")) %>%
  data.table()

detection<-tbl(conDplyr,"data_stationary_antenna") %>%
  filter(drainage=="west") %>%
  select(river,river_meter,detection_date,tag) %>%
  collect(n=Inf) %>%
  data.table() %>%
  .[,detection_date:=as.Date(detection_date)] %>%
  .[,.(detections=.N,
       unique_detections=length(unique(tag))),
    by=.(river,river_meter,detection_date)] %>%
  setnames("detection_date","date")

status<-full_join(detection,status,by=c("river","river_meter","date")) %>%
  data.table() %>%
  .[is.na(detections),detections:=0] %>%
  setkey(river,river_meter,date)

status<-status[!duplicated(status)]

status<-status[,dayDiff:=c(NA,diff(date)),by=river_meter]

status[,conflict:=length(unique(on_off))>1,by=.(river,river_meter,date)]

status[detections>0&is.na(deployed),.(minDate=min(date),maxDate=max(date)),by=.(river,river_meter)]

fillOnOff<-function(onOff,detections){
  known<-which(!is.na(onOff))
  unknown<-which(is.na(onOff))
  
  if(all(is.na(onOff))){return(onOff)}
  
  for(i in unknown){
    if(!is.na(onOff[i])) {next} 
    if(i>max(known)) {
      onOff[i:length(onOff)]<-3 #set it to on after the last check
      next} 
    nextKnown<-known[min(which(i<known))]
    if(onOff[nextKnown]==0){
      if(all(detections[i:(nextKnown-1)]==0)){
        onOff[i:(nextKnown-1)]<-0
      } else {
        onOff[i]<-3
      }
    } else {
      onOff[i:(nextKnown-1)]<-onOff[nextKnown]
    }
  }
  return(onOff)
}

status[,chunk:=cumsum(!is.na(dayDiff)&dayDiff>1),by=river_meter]
status[,on_off2:=fillOnOff(on_off,detections),by=.(river_meter,chunk)]

dbWriteTable(con, 'data_antenna_status', data.frame(status), row.names=FALSE,
             overwrite=TRUE, append=FALSE)
