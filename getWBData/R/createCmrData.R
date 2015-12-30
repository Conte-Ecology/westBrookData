#'Create data strucuture for capture-mark-recapture analyses
#'Creates encounter histories for each individual and indices for reference within jags
#'
#'@param coreData A data.frame created with \code{createCoreData()}
#'@param startDate A Date or POSIXct time to pad samples back to
#'@param endDate A A Date or POSIXct time to pad samples forward to
#'@param maxAgeInSamples An age in samples (age/4) after which encounter histories are censored
#'@returns A data.frame containing encounter histories (\code{$enc}) with all samples 
#'  within the range defined. \code{$sampleIndex} and \code{$tagIndex} are also created
#'  for refence within jags objects.
#'
#'

createCmrData<-function(coreData,dateStart,dateEnd=as.POSIXct("2005-01-01"),
                        maxAgeInSamples=100){
  reconnect()
  
  dateStart<-as.POSIXct(dateStart)
  sampleQuery<-paste("SELECT sample_name,sample_number,season,end_date,year",
                     "FROM data_seasonal_sampling",
                     "WHERE seasonal='TRUE'")
  samples<-RPostgreSQL::dbGetQuery(con,sampleQuery) %>% 
                  arrange(sample_number)
  names(samples)<-camelCase(names(samples))
  
  samplesToInclude<-filter(samples,endDate>dateStart,endDate<dateEnd)[["sampleNumber"]]
  coreData<-coreData %>% filter(sampleNumber %in% samplesToInclude)
  
  firstSampleNum<-min(coreData$sampleNumber)
  lastSampleNum<-max(coreData$sampleNumber)
  
  #I couldn't figure out quickly how to do this in dplyr, so it's in data.table for now
  #.SD refers to the subsetted data.table (i.e., each tag's data.table)
  #J creates a data.table to use as a join on the data.table's key (i.e., sampleNumber here)
  #pad with samples where individual was unobserved
  tagProperties<-c('tag','dateKnownDead','lastAntennaDetection',
                  'species','firstCaptureSample','lastCaptureSample')
  tagProperties<-tagProperties[tagProperties %in% names(coreData)]
  
  coreData<-data.table(coreData)
  setkey(coreData,sampleNumber)
  coreData<-coreData[,.SD[J(firstSampleNum:lastSampleNum)],by=as.list(mget(tagProperties))]
  coreData<-tbl_df(data.frame(coreData))
  
  coreData<-coreData %>% 
              mutate(enc=as.numeric(!is.na(detectionDate))) %>% # create encounter history
                mutate(sampleIndex=sampleNumber-min(sampleNumber)+1) %>% 
                  mutate(tagIndex=as.numeric(as.factor(tag)))
  
  samples %>%
    filter(season==2) %>%
      select(year,sampleNumber) %>%
        rename(sampleBorn=sampleNumber) %>%
          right_join(coreData,by=c("year"="cohort")) %>%
            transmute(ageInSamples=sampleNumber-sampleBorn)

  
  
  return(coreData)
}