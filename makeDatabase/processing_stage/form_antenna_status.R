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
  select(river,river_meter,detection_date) %>%
  collect(n=Inf) %>%
  data.table() %>%
  .[,detection_date:=as.Date(detection_date)] %>%
  .[,.(detections=.N),by=.(river,river_meter,detection_date)] %>%
  setnames("detection_date","date")

status<-full_join(detection,status,by=c("river","river_meter","date")) %>%
  data.table() %>%
  #.[!duplicated(status)] %>%
  setkey(river,river_meter,date)

#status<-status[,dayDiff:=c(NA,diff(date)),by=river_meter]

status[,conflict:=length(unique(on_off))>1,by=.(river,river_meter,date)]

status[detections>0&is.na(deployed),.(minDate=min(date),maxDate=max(date)),by=.(river,river_meter)]

fillOnOff<-function(onOff){
  known<-which(!is.na(onOff))
  unknown<-which(is.na(onOff))

  for(i in unknown){
    onOff[i]<-onOff[known[min(which(i<known))]]
  }
  return(onOff)
}
