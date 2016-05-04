#'Add environmental data to core westbrook data
#'@return Environmental data added to a coreData
#'@param coreData a data.frame created using createCoreData
#'@param sampleFlow Logical indicating whether to add the discharge on detectionDate to the output
#'@param fun A quoted name of a function used to characterize the environmental data between samples (e.g., mean, median, min, max). Custom functions can be used but must have two arguments: x and na.rm.
#'@details View this function as a template to be altered to get different env variables or different summaries. / This function add columns of mean daily temperature and mean daily flow to coreData. For each individual, it returns the mean env values between capture dates or between median capture dates of all individuals of a fish was not captured on an occasion.
#' The internal function getIntervalMean returns means for the observed river or for all rivers if an individual was unobserved.
#' Could alter getIntervalMean to return data for the mean daily min or max env data or for data from the last observed river, instead of all rivers.
#' 
#'@export

addEnvironmental <-function( coreData, sampleFlow=F , funName="mean"){
  func<-get(funName)
  # get temperature data from database
  envData<-tbl(conDplyr,"data_daily_temperature") %>%
            collect() %>%
            full_join(tbl(conDplyr,"data_flow_extension") %>% collect(),
                      by=c("river","date")) %>%
            select(-source) %>%
            dplyr::filter(date <= max(coreData$detectionDate),
                          date >= min(coreData$detectionDate)) %>%
            data.frame()
  
  ###########################################################################
  # set up table of intervals based on date for each capture interval by fish
  coreData <- coreData %>%
                group_by( tag ) %>%
                mutate( lagDetectionDate = lead( detectionDate ) ) %>%
                ungroup()
  
  # function to get mean environmental data for each row of coreData
  getIntervalMean <- function( start,end,r,e ,fun=func){
    d <- envData$date

    if( e == "Temperature" ) {
      envCol <- "daily_mean_temp"
      if ( is.na( r ) )  meanTemp <- fun( envData[ d >= start & d <= end , envCol ], na.rm = T ) #use data from all rivers
      if ( !is.na( r ) ) meanTemp <- fun( envData[ d >= start & d <= end & envData$river == r, envCol ], na.rm = T ) 
    }
    
    if( e == "Flow" ) {
      envCol <- "qPredicted"
      meanTemp <- fun( envData[ d >= start & d <= end , envCol ], na.rm = T )
    }
    
    return( meanTemp )
  }
  
  # get unique start and end dates from coreData and calc env means for the intervals
  coreDataUniqueDates <- coreData %>%
                           select( river, detectionDate, lagDetectionDate ) %>%
                           distinct() %>%
                           group_by( river, detectionDate, lagDetectionDate ) %>% #just a loop, probably a better way to do this.
                           mutate( meanTemperature = getIntervalMean( detectionDate, lagDetectionDate, river, "Temperature" ),
                                   meanFlow =        getIntervalMean( detectionDate, lagDetectionDate, river, "Flow" )) %>%
                           ungroup()
  
  coreData <- left_join( coreData,coreDataUniqueDates,
                         by=c("detectionDate","river","lagDetectionDate"))
  
#   i=495
#   getIntervalMean( coreData$detectionDate[i],coreData$lagDetectionDate[i],coreData$river[i],"Temperature" )
#   
#    for(i in 1:20){
#       print(  c(i,coreDataUniqueDates$detectionDate[i],coreDataUniqueDates$lagDetectionDate[i],coreDataUniqueDates$river[i],
#                 getIntervalMean( coreDataUniqueDates$detectionDate[i],coreDataUniqueDates$lagDetectionDate[i],coreDataUniqueDates$river[i], "Temperature" )) )
#     }
  if(sampleFlow){
  coreData <- envData %>%
              filter(!is.na(qPredicted)) %>%
              select(date,qPredicted) %>%
              rename(flowForP=qPredicted) %>%
              distinct() %>%
              right_join (coreData,by=c("date"="detectionDate")) %>%
              rename(detectionDate=date)
  }
  
  names(coreData)[which(names(coreData)=="meanTemperature")]<-paste0(funName,"Temperature")
  names(coreData)[which(names(coreData)=="meanFlow")]<-paste0(funName,"Flow")
  
  return( coreData )  
    
}