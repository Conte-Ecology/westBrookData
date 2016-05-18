#'Fills in data for location and length when a fish was not observed using some simple assumptions
#'For location, a fish is assumed to remain in the same location until it is observed elsewhere. For length, a linear interpolation is used to fill lengths between observations and the average by ageInSamples/river is used after the last observation.
#'@param data A data.frame created with createCoreData() and createCmrData()
#'@return Data for location and size even when fish are not observed.
#'@export

fillSizeLocation<-function(data,size=T,location=T){
  #fills river or section with the last one observed
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

  #means to fill after last observations
  lengthByAge<-data %>%
               filter(!is.na(river)) %>%
               group_by(ageInSamples,river,species) %>%
               summarize(meanLengthRiver=mean(observedLength,na.rm=T))
  overallLengthByAge<-data %>%
                      group_by(ageInSamples,species) %>%
                      summarize(meanLength=mean(observedLength,na.rm=T))
  oldFishLength<-data %>%
                 group_by(species) %>%
                 filter(ageInSamples>=17,!is.na(observedLength)) %>%
                 summarize(oldLength=mean(observedLength))

  #interpolates lengths between observations
  fillLength<-function(length){
    firstObs<-suppressWarnings(min(which(!is.na(length))))
    lastObs<-suppressWarnings(max(which(!is.na(length))))
    if(firstObs!=Inf){
      if(length(firstObs:lastObs)>1){
        length[firstObs:lastObs]<-approx(length[firstObs:lastObs],n=length(firstObs:lastObs))$y
      }
    }
    return(length)
  }

  #fill river, section, and interpolated lengths

  if(location){
  data<-data %>%
  group_by(tag) %>%
  mutate(river=fillLocation(river)) %>%
  mutate(section=fillLocation(section)) %>%
  ungroup()
  }

  if(size){
  data<-data %>%
  group_by(tag) %>%
  mutate(observedLength=fillLength(observedLength)) %>%
  ungroup() %>%
  left_join(lengthByAge,by=c('ageInSamples','river','species')) %>%
  left_join(overallLengthByAge,by=c('ageInSamples','species')) %>%
  left_join(oldFishLength,by='species')

  #fill non-interpolatable lengths with means preferentially by river, overall mean, or average of old fish
  data[is.na(data$observedLength),"observedLength"]<-
    data[is.na(data$observedLength),"meanLengthRiver"]

  data[is.na(data$observedLength),"observedLength"]<-
    data[is.na(data$observedLength),"meanLength"]

  data[is.na(data$observedLength)&data$ageInSamples>=15,"observedLength"]<-
    data[is.na(data$observedLength)&data$ageInSamples>=15,"oldLength"]

  data<-data %>% select(-meanLengthRiver,-meanLength,-oldLength)
  }

#tried briefly to correct for fish getting smaller after they got the mean, but gave up
#   makeLengthMonotonic<-function(length,enc,tag){
#     print(tag[1])
#     lastObs<-max(which(enc==1))
#     deltaLength<-diff(length)
#     badFills<-which(deltaLength<0)+1
#     badFills<-badFills[badFills>lastObs]
#     while(length(na.omit(badFills))>0){
#       for(b in badFills){
#         length[b]<-length[lastObs]
#       }
#       deltaLength<-diff(length)
#       badFills<-intersect(which(deltaLength<0),which(enc==0))
#     }
#     return(length)
#   }
#
#   data<-data %>%
#         group_by(tag) %>%
#         mutate(length=makeLengthMonotonic(observedLength,enc,tag)) %>%
#         ungroup()

  return(data)
}
