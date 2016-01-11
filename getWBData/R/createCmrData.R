#'Create data strucuture for capture-mark-recapture analyses
#'Creates encounter histories for each individual and indices for reference within jags
#'
#'@param coreData A data.frame created with \code{createCoreData()}
#'@param startDate A Date or POSIXct time to pad samples back to
#'@param endDate A A Date or POSIXct time to pad samples forward to
#'@param maxAgeInSamples An age in samples (age/4) after which encounter histories are censored
#'@return A data.frame containing encounter histories (\code{$enc}) with all samples 
#'  within the range defined. \code{$sampleIndex} and \code{$tagIndex} are also created
#'  for refence within jags objects.
#'
#'
#'@export

createCmrData<-function(coreData,minCohort=1900,
                        dateStart=as.POSIXct("1900-01-01"),
                        dateEnd=as.POSIXct("2100-01-01"),
                        maxAgeInSamples=20){
  reconnect()
  
  #get the sample data
  sampleQuery<-paste("SELECT sample_number,season,end_date,year",
                     "FROM data_seasonal_sampling",
                     "WHERE seasonal='TRUE'")
  samples<-RPostgreSQL::dbGetQuery(con,sampleQuery) %>% 
                  arrange(sample_number)
  names(samples)<-camelCase(names(samples))
  
  #subset data to the samples of interest
  samplesToInclude<-filter(samples,endDate>dateStart,endDate<dateEnd)[["sampleNumber"]]
  coreData<-coreData %>% filter(sampleNumber %in% samplesToInclude)
  
  ###pad with samples where individual was unobserved
  tagProperties<-c('dateKnownDead','lastAntennaDetection','cohort',
                  'species','firstCaptureSample','lastCaptureSample')
  tagProperties<-tagProperties[tagProperties %in% names(coreData)]

  allSamples<-min(samplesToInclude):max(samplesToInclude)
  allSampleTags<-data.frame(tag=rep(unique(coreData$tag),each=length(allSamples)),
                            sampleNumber=rep(allSamples,length(unique(coreData$tag))),
                            stringsAsFactors=F)
  
  for(t in tagProperties){
    allSampleTags[[t]]<-rep(
                        coreData %>% group_by(tag) %>% summarize_(paste0("unique(",t,")")) %>%
                          ungroup() %>% .[[paste0("unique(",t,")")]],
                        each=length(allSamples))
                        
  }
  coreData<-right_join(coreData,allSampleTags)
  #create cmr related columns
  coreData<-coreData %>% 
              mutate(enc=as.numeric(!is.na(detectionDate))) %>% # create encounter history
                mutate(sampleIndex=sampleNumber-min(sampleNumber)+1) %>% 
                  mutate(tagIndex=as.numeric(as.factor(tag)))
  
  #calculate ageInSamples
  coreData<-samples %>%
              filter(season==2) %>%
                select(year,sampleNumber) %>%
                  rename(sampleBorn=sampleNumber) %>%
                    distinct() %>%
                      right_join(coreData,by=c("year"="cohort")) %>%
                        rename(cohort=year) %>%
                          mutate(ageInSamples=sampleNumber-sampleBorn) %>%
                            select(-sampleBorn)
  #censor individuals when they get too old
  coreData<-coreData %>%
              filter(ageInSamples<maxAgeInSamples)
  
  return(coreData)
}
