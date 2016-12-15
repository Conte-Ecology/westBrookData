antennaCheck<-function(river=NULL,riverMeter=NULL,timeWindow=90){
  require(data.table)
  require(getWBData)

    
  reconnect()
  
  ant<-tbl(conDplyr,"data_stationary_antenna") %>%
    collect(n=Inf) %>%
    data.table() %>%
    .[,date:=as.Date(detection_date)] %>%
    .[,.(N=.N,nInd=length(unique(tag))),by=.(river,river_meter,date)] %>%
    setkey(date) %>%
    .[,.SD[data.table(date=seq.Date(min(date),max(date),"day"),key="date")],.(river,river_meter)] %>%
    setkey(river,river_meter,date) %>%
    .[is.na(N),N:=0] %>%
    .[is.na(nInd),nInd:=0]
  
  
  onOff<-fread("/home/projects/westbrook/dataOut/processedData/antennaStatus.csv") %>%
    .[,date:=as.Date(date)]
  antennas<-names(onOff)[grep("_",names(onOff))]
  
  onOffOut<-tbl(conDplyr,"data_antenna_status") %>%
    collect(n=Inf) %>%
    data.table()
  
  cachedOut<-onOffOut
  
  antCompleted<-names(onOffOut)[!grepl("date",names(onOffOut))]
  # antennas<-antennas[!antennas %in% antCompleted] #THIS DOESN'T WORK!!!!
  
  for(a in antennas){
    whichAntenna<-a
    whichRiver<-strsplit(whichAntenna,"_")[[1]][1]
    whichRiverMeter<-strsplit(whichAntenna,"_")[[1]][2]
    
    present<-tstrsplit(onOff[[whichAntenna]],"_")[[1]]
    on<-tstrsplit(onOff[[whichAntenna]],"_")[[2]]
    toDo<-which(present==TRUE&on==FALSE)
    filled<-NULL
    
    dates<-range(onOff$date[present==TRUE])
    if(is.na(dates[1])){next}
    plot(N~date,data=ant[river==whichRiver&
                           river_meter==whichRiverMeter],#&
                           #date>=dates[1]&date<=dates[2]],
         type='l',main=paste(whichRiver,whichRiverMeter))
    # par(new=T)
    points(nInd~date,data=ant[river==whichRiver&
                              river_meter==whichRiverMeter],#&
                              #date>=dates[1]&date<=dates[2]],
         type='l',lty=2)
    # axis(3,pretty(ant[river==whichRiver&
    #                     river_meter==whichRiverMeter&
    #                     date>=dates[1]&date<=dates[2],nInd]))
    cat ("Displaying all data from this antenna\nPress [enter] to start on/off determination")
    line <- readline()
    
    for(t in toDo){
      if(t %in% filled){
        next
      }
      dates<-seq.Date(onOff$date[t]-round(timeWindow/2),
                      onOff$date[t]+round(timeWindow/2),
                      "day")
      plot(N~date,data=ant[river==whichRiver&
                             river_meter==whichRiverMeter&
                             date %in% dates],
           type='l',main=paste(whichRiver,whichRiverMeter,year(onOff$date[t])))
      points(nInd~date,data=ant[river==whichRiver&
                             river_meter==whichRiverMeter&
                             date %in% dates],
           type='l',lty=2)
      # axis(3,pretty(ant[river==whichRiver&
      #                     river_meter==whichRiverMeter&
      #                     date %in% dates,nInd]))
      points(0~onOff$date[t],pch=19,col='red')
      
      action<-readline("Enter action ('help' for options): ")
      if(action=="help"){
        cat("options are:\n 1 = on\n 0 = off\n fill0 = off until the next detection
            \n fill1 = on until the next detection\n skip = move on without doing anything
            \n Q = save and quit\n nextAntenna = save and move on to the next antenna")
        action<-readline("Enter action ('help' for options): ")
      }
      if(!action %in% c("0","1","skip","fill0","fill1","Q","nextAntenna")){
        cat("invalid entry\n")
        action<-readline("Enter action ('help' for options): ")
      }
      if(action %in% c("0","1")){
        on[t]<-as.numeric(action)
        next
      }
      if(action=="skip"){next}
      if(grepl("fill",action)){
        lastFill<-min(min(which(diff(toDo[which(toDo==t):length(toDo)])>1))+which(toDo==t)-1,
                      length(toDo))
        filled<-toDo[which(toDo==t):lastFill]
        on[filled]<-as.numeric(substr(action,5,5))
        next
      }
      if(action %in% c("Q","nextAntenna")){
        break
      }
    }
    onOffOut[[whichAntenna]]<-as.logical(on)
    
    dbWriteTable(con,'data_antenna_status',data.frame(onOffOut), row.names=FALSE,
                 overwrite=TRUE, append=FALSE)
    if(action=="Q"){break}
  }
  
}