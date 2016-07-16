#'Adds known state to data.frame created with createCmrData()
#'@param cmrData A data.frame created using createCmrData()
#'@param knownDead Logical indicating whether to set known state based on tags known to be dead
#'@return A data.frame including \code{$knownZ} which is the known state (0=not born,1=alive,2=dead,NA=unknown)
#'@export
addKnownZ<-function(cmrData,knownDead=T,useAntenna=F){
  reconnect()
  
  if(any(c("west brook","wb jimmy","wb mitchell","wb obear") %in%
         cmrData$river)) {drainage="west"} else {
           drainage="stanley"
         }
  
  getKnown<-function(x){
    firstObs<-min(which(x==1))
    lastObs<-max(which(x==1))
    known<-rep(0,length(x))
    known[firstObs:lastObs]<-1
    if(lastObs!=length(known)){
    known[(lastObs+1):length(known)]<-NA
    }
    return(known)
  }
  
  cmrData<-cmrData %>%
             group_by(tag) %>%
               mutate(knownZ=getKnown(enc)) %>%
                 ungroup()
  
  
  if(useAntenna){
    lastAnt<-tbl(conDplyr,"data_by_tag") %>%
             select(tag,last_antenna_detection) %>%
             filter(!is.na(last_antenna_detection)) %>%
             collect(n=Inf)
    
    fillUntilLastAntenna<-function(tag,detectionDates,z){
      if(sum(tag[1]==lastAnt$tag)==0) return(z)
      
      last1<-max(detectionDates[which(z==1)])
      lastAntDet<-lastAnt[lastAnt$tag==tag[1],][["last_antenna_detection"]]
      lastAntDet<-max(detectionDates[which(detectionDates<lastAntDet)])
    
      if(last1>lastAntDet) return(z)
      
      z[detectionDates>last1 & detectionDates<=lastAntDet]<-1
      return(z)
    }
    
    cmrData<-cmrData %>%
      group_by(tag) %>%
      mutate(knownZ=fillUntilLastAntenna(tag,detectionDate,knownZ)) %>%
      ungroup()
  }
  
  if(knownDead){
    dead<-dbGetQuery(con,paste("SELECT tag,date_known_dead",
                               "FROM data_by_tag",
                               "WHERE date_known_dead IS NOT NULL")
    )
    
    samples<-dbGetQuery(con,paste0("SELECT sample_number,start_date ",
                                  "FROM data_seasonal_sampling ",
                                  "WHERE seasonal='TRUE' ",
                                  "AND drainage='",drainage,"'")) %>%
      group_by(sample_number) %>%
        transmute(start_date=min(start_date)) %>%
          ungroup() %>%
            unique() %>%
              arrange(start_date)
    
    dead<-suppressWarnings(dead %>% 
          group_by(tag) %>% 
          transmute(date_known_dead=as.POSIXct(date_known_dead)) %>%
          mutate(firstSampleDead=
               dplyr::filter(samples,as.Date(start_date)>as.Date(date_known_dead)) %>% 
               #select(sample_number) %>% 
               .[['sample_number']] %>%
               min(.)
            ) %>%
          ungroup())

    cmrData<-dead %>% 
               select(tag,firstSampleDead) %>% 
                 right_join(cmrData,by="tag") %>%
                   mutate(isDead=sampleNumber>=firstSampleDead&!is.na(firstSampleDead))
    cmrData[cmrData$isDead==T,"knownZ"]<-2
    
    cmrData<-select(cmrData,-isDead,-firstSampleDead)
  }
  return(cmrData)
}

