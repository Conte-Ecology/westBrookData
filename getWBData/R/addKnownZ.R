#'Adds known state to data.frame created with createCmrData()
#'@param cmrData A data.frame created using createCmrData()
#'@param knownDead Logical indicating whether to set known state based on tags known to be dead
#'@return A data.frame including \code{$knownZ} which is the known state (0=not born,1=alive,2=dead,NA=unknown)
#'@export
addKnownZ<-function(cmrData,knownDead=T){
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
  if(knownDead){
    dead<-dbGetQuery(con,paste("SELECT tag,date_known_dead",
                               "FROM data_by_tag",
                               "WHERE date_known_dead IS NOT NULL")
    )
    
    samples<-dbGetQuery(con,paste("SELECT sample_number,start_date",
                                  "FROM data_seasonal_sampling",
                                  "WHERE seasonal='TRUE'")) %>%
      group_by(sample_number) %>%
        transmute(start_date=min(start_date)) %>%
          ungroup() %>%
            distinct() %>%
              arrange(start_date)
    
    dead<-dead %>% group_by(tag) %>% transmute(date_known_dead=as.POSIXct(date_known_dead)) %>%
            mutate(firstSampleDead=
               filter(samples,start_date>date_known_dead) %>% select(sample_number) %>% min()
            )
    
    cmrData<-dead %>% 
               select(tag,firstSampleDead) %>% 
                 right_join(cmrData,by="tag") %>%
                   mutate(isDead=sampleNumber>=firstSampleDead&!is.na(firstSampleDead)) %>%
                     mutate(knownZ=knownZ+isDead) %>%
                       select(-isDead,-firstSampleDead)
  }
}

