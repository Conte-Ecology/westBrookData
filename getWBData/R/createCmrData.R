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

createCmrData<-function(coreData,
                        dateStart=as.POSIXct("1900-01-01"),
                        dateEnd=as.POSIXct("2100-01-01"),
                        maxAgeInSamples=20,
                        censorDead=F,
                        censorEmigrated=T,
                        modelType="CJS",
                        inside=T){
  reconnect()
  
  #get the sample data
  sampleQuery<-paste("SELECT distinct sample_number,sample_name,season,end_date,year",
                     "FROM data_seasonal_sampling",
                     "WHERE seasonal='TRUE'")
  samples<-dbGetQuery(con,sampleQuery) %>% 
             arrange(sample_number)
  names(samples)<-camelCase(names(samples))
  
  #subset data to the samples of interest
  samplesToInclude<-dplyr::filter(samples,endDate>dateStart,endDate<dateEnd)[["sampleNumber"]]
  coreData<-coreData %>% dplyr::filter(sampleNumber %in% samplesToInclude)
  
  #Subset to fish inside the core study area
  if(inside) coreData<-coreData %>% dplyr::filter(area %in% c('inside','trib'))
  
  ###pad with samples where individual was unobserved
  tagProperties<-c('dateKnownDead','lastAntennaDetection','cohort',
                  'species','firstCaptureSample','lastCaptureSample','sex')
  tagProperties<-tagProperties[tagProperties %in% names(coreData)]

  allSamples<-as.numeric(min(samplesToInclude):max(samplesToInclude))
  allTags<-coreData %>% select(one_of(c("tag",tagProperties))) %>% distinct()
  allSampleTags<-data.frame(tag=rep(allTags$tag,each=length(allSamples)),
                            sampleNumber=rep(allSamples,length(unique(coreData$tag))),
                            stringsAsFactors=F) %>%
                 mutate(sampleName=samples$sampleName[match(sampleNumber,samples$sampleNumber)])
  for(t in tagProperties){
    allSampleTags[[t]]<-rep(allTags[[t]],each=length(allSamples))
                        
  }
  coreData<-suppressMessages(right_join(coreData,allSampleTags))
  #create cmr related columns
  coreData<-coreData %>% 
              mutate(enc=as.numeric(!is.na(detectionDate))) %>% # create encounter history
                mutate(sampleIndex=sampleNumber-min(sampleNumber)+1) %>% 
                  mutate(tagIndex=as.numeric(as.factor(tag)))
  
  #calculate ageInSamples
  coreData<-samples %>%
              dplyr::filter(season==2) %>%
                select(year,sampleNumber) %>%
                  rename(sampleBorn=sampleNumber) %>%
                    distinct() %>%
                      right_join(coreData,by=c("year"="cohort")) %>%
                        rename(cohort=year) %>%
                          mutate(ageInSamples=sampleNumber-sampleBorn) %>%
                            select(-sampleBorn)
  #censor individuals when they get too old
  coreData<-coreData %>%
              dplyr::filter(ageInSamples<=maxAgeInSamples)
  
  
  if(modelType=="CJS"){ #remove occasions prior to the first capture
    coreData<-coreData %>%
      group_by(tag) %>%
      filter(sum(enc)>0) %>% #removes fish that were not observed (i.e. too old on first capture)
      mutate(firstObs=sampleNumber[min(which(enc==1))]) %>%
      dplyr::filter(sampleNumber>=firstObs) %>%
      select(-firstObs) %>%
      ungroup()
  }
  
  #censor occasions where an individual is dead or emigrated if these options are chosen
  columns<-"tag"
  notNull<-NULL
  if(censorDead){columns<-c(columns,"date_known_dead")
  notNull<-c(notNull,"date_known_dead IS NOT NULL")
  }
  if(censorEmigrated){columns<-c(columns,"date_emigrated")
  notNull<-c(notNull,"date_emigrated IS NOT NULL")
  }
  if(censorEmigrated|censorDead){ #censor emigrated and/or dead
    query<-paste("SELECT",
                 paste(columns,collapse=","),
                 "FROM data_by_tag",
                 "WHERE",
                 paste(notNull,collapse=" OR "))
    
    toCensor<-dbGetQuery(con,query)
    dateNames<-(names(toCensor)[2:ncol(toCensor)])
    expr<-paste0("as.POSIXct(min(",paste(dateNames,collapse=","),",na.rm=T))")
    toCensor<-toCensor %>% group_by(tag) %>% transmute_(censorDate=expr) %>% ungroup()
    
    samples<-dbGetQuery(con,"SELECT sample_number,start_date FROM data_seasonal_sampling WHERE sample_number is not NULL")
    firstCensoredSample<-function(date){
      sample<-samples %>% 
        dplyr::filter(start_date>date) %>% 
        dplyr::filter(start_date==min(start_date)) %>% 
        .[["sample_number"]]
      return(sample[1])
    }
    toCensor<-toCensor %>% 
      group_by(tag) %>% 
      mutate(firstCensoredSample=firstCensoredSample(censorDate)) %>%
      ungroup()
    
    coreData<-toCensor %>%
      select(tag,firstCensoredSample) %>%
      right_join(coreData,by="tag") %>%
      dplyr::filter(sampleNumber<firstCensoredSample|is.na(firstCensoredSample)) %>%
      select(-firstCensoredSample)
  }#end emigrated or dead section

  
  return(coreData)
}
