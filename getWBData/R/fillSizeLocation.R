#'Fills in data for location and length when a fish was not observed using some simple assumptions
#'For location, a fish is assumed to remain in the same location until it is observed elsewhere. For length, a linear interpolation is used to fill lengths between observations and the average by ageInSamples/river is used after the last observation.
#'@param data A data.frame created with createCoreData() and createCmrData()
#'@return Data for location and size even when fish are not observed.
#'@export

fillSizeLocation<-function(data){
  
  fillLocation<-function(location){
    known<-which(!is.na(location))
    unknown<-which(is.na(location))
    nKnown<-length(unique(location[known]))
    if(nKnown==1){location[unknown]<-location[known[1]]}else{
      for(i in unknown){
        location[i]<-location[known[max(which(i>known))]]
      }
    }
    return(location)
  }
  
  lengthByAge<-data %>%
               group_by(ageInSamples) %>%
               summarize(meanLength=mean(observedLength,na.rm=T))
  
  fillLength<-function(length,ageInSamples){
    firstObs<-suppressWarnings(min(which(!is.na(length))))
    lastObs<-suppressWarnings(max(which(!is.na(length))))
    if(firstObs!=Inf){
      if(length(firstObs:lastObs)>1){
        length[firstObs:lastObs]<-approx(length[firstObs:lastObs],n=length(firstObs:lastObs))$y
      }
    }
    if(any(is.na(length))){
      fillWithMean<-which(is.na(length))
      length[fillWithMean]<-lengthByAge$meanLength[match(ageInSamples[fillWithMean],
                                                      lengthByAge$ageInSamples)]
    }
    return(length)
  }
  
  
  
  data<-data %>%
  group_by(tag) %>%
  mutate(river=fillLocation(river)) %>%
  mutate(section=fillLocation(section)) %>%
  mutate(observedLength=fillLength(observedLength,ageInSamples)) %>%
  ungroup()
  
  return(data)
}