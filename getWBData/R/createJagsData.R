#'Create data needed for JAGS runs
#'@return List of evalRows and firstObsRows
#'@param coreData a data.frame created using createCoreData
#'@param modelType either "CJS" or "JS"
#'@details \strong{Options of columns to add}
#'
#'@export

createJagsData <-function(coreData, modelType = 'CJS'){

  # function to add dummy rows and columns for zRiv=1
  addRowColMeans <- function(m){
    m <- cbind( rowMeans(m),m )
    m <- rbind( colMeans(m),m )
    return ( m )
  }
  # function to add dummy columns for zRiv=1
  addColMeans <- function(m){
    m <- cbind( rowMeans(m),m )
    return ( m )
  }

  ##################################################################################
  # check data - will delete when function is finalized
  # need to get rid of repeatRows - they mess up evalRows etc.

  repeatRows <- coreData %>% group_by( tag,sampleNumber ) %>% dplyr::filter( n() > 1 )
  if(nrow(repeatRows)>0){
    repeatRows<<-repeatRows
    warning("Multiple observations of at least one individual in a sample
            (print repeatRows to view cases)")
    }
  #
  ##################################################################################

if(modelType != 'CJS' & modelType != 'JS' ) warning('ModelType needs to be CJS or JS')

  #### CJS #####
  # evalRows
if(modelType == 'CJS'){

  evalRows <- coreData %>%
    mutate( evalRows = row_number() ) %>%
    group_by(tag) %>%
    dplyr::filter( sampleNumber != min(sampleNumber) ) %>%
    ungroup() %>%
    select( evalRows )

  nEvalRows <- nrow( evalRows )
}

  #### JS #####
if(modelType == 'JS'){

  evalRows <- coreData %>%
    mutate( evalRows = row_number() ) %>%
    group_by(tag) %>%
    dplyr::filter( sampleNumber != max(sampleNumber) ) %>%
    ungroup() %>%
    select( evalRows )

  nEvalRows <- nrow( evalRows )
}

# firstObsRows
  firstObsRows <- coreData %>%
    mutate( firstObsRows = row_number() ) %>%
    group_by(tag) %>%
    dplyr::filter( sampleNumber == min(sampleNumber) ) %>%
    ungroup() %>%
    select( firstObsRows )

  nFirstObsRows <- nrow( firstObsRows )

  # lastObsRows
  lastObsRows <- coreData %>%
    mutate( lastObsRows = row_number() ) %>%
    group_by(tag) %>%
    dplyr::filter( sampleNumber == max(sampleNumber) ) %>%
    ungroup() %>%
    select( lastObsRows )

  nLastObsRows <- nrow( lastObsRows )

#################################################################
  coreData$year <-as.numeric(format(coreData$detectionDate, format="%Y"))
  coreData$riverOrdered <- as.numeric( factor(coreData$river,levels=c('west brook','wb jimmy','wb mitchell','wb obear'), ordered=T) )

  # means for standardizing
  #####################################################################
  stdBySeasonRiverYear <- coreData %>%
                            group_by(river,season,year) %>%
                            summarize(
                                 lengthMn=mean(observedLength, na.rm=TRUE),

                                 tempMn = mean(meanTemperature, na.rm=TRUE),
                        #         tempMnP=mean(temperatureForP, na.rm=TRUE),
                                 flowMn=mean(meanFlow, na.rm=TRUE)
                        #         dischMnP=mean(dischargeForP,na.rm=T)
                                 )
  stdBySeasonRiverYear<-stdBySeasonRiverYear[!is.na(stdBySeasonRiverYear$river),]
  #
  stdBySeasonRiver <- stdBySeasonRiverYear %>%
                        group_by(river,season) %>%
                        summarize(
                             lengthMean=mean(lengthMn, na.rm=TRUE),
                             lengthSd=sd(lengthMn, na.rm=TRUE),
                             lengthLo = quantile(lengthMn,c(0.025), na.rm=TRUE),
                             lengthHi = quantile(lengthMn,c(0.975), na.rm=TRUE),
                              tempMean=mean(tempMn, na.rm=TRUE),
#                              tempMeanP=mean(tempMnP, na.rm=TRUE),
                              tempSd=sd(tempMn, na.rm=TRUE),
#                              tempSdP=sd(tempMnP, na.rm=TRUE),
                              tempLo = quantile(tempMn,c(0.025), na.rm=TRUE),
                              tempHi = quantile(tempMn,c(0.975), na.rm=TRUE),
                              flowMean=mean(flowMn, na.rm=TRUE),
                              flowSd=sd(flowMn, na.rm=TRUE),
#                              dischMeanP=mean(dischMnP,na.rm=T),
#                              dischSdP=sd(dischMnP,na.rm=T),
                              flowLo = quantile(flowMn,c(0.025), na.rm=TRUE),
                              flowHi = quantile(flowMn,c(0.975), na.rm=TRUE)
                               )
  ############# To get rid of NA Rivers
  stdBySeasonRiver<-stdBySeasonRiver[!is.na(stdBySeasonRiver$river),]



#################################################################

  d <- within(
    data = list(),
    expr = {

      encDATA = as.numeric( coreData$enc )
      z = coreData$knownZ
      riverDATA = coreData$riverOrdered
      nRivers = coreData %>% filter(!is.na(river)) %>%
                select(river) %>% unique() %>% nrow()
      lengthDATA = coreData$observedLength
      #availableDATA = dMData$available01 no longer need - taken care of with censor()
      ind = coreData$tagIndex

      # For standardizing length
      lengthMean = addColMeans( matrix(stdBySeasonRiver$lengthMean,
                                       nrow=length(unique(coreData$season)),
                                       ncol=nRivers))
      lengthSd =   addColMeans( matrix(stdBySeasonRiver$lengthSd,
                                       nrow=length(unique(coreData$season)),
                                       ncol=nRivers ))

      # environmental covariates pertaining to intervals.  These are
      # covariates of growth and survival

      # For standardizing env predictors of growth and surv
      tempMean = addColMeans( matrix(stdBySeasonRiver$tempMean,nrow=length(unique(coreData$season)),ncol=nRivers ))
      tempSd =   addColMeans( matrix(stdBySeasonRiver$tempSd,nrow=length(unique(coreData$season)),ncol=nRivers ))
      flowMean = addColMeans( matrix(stdBySeasonRiver$flowMean,nrow=length(unique(coreData$season)),ncol=nRivers ))
      flowSd =   addColMeans( matrix(stdBySeasonRiver$flowSd,nrow=length(unique(coreData$season)),ncol=nRivers ))

      # Now doing standardization in the bugs code to make sure that unobserved fish get observed std env data
      tempDATA = as.numeric(coreData$meanTemperature)
      flowDATA = as.numeric(coreData$meanFlow)

      flowMeanDATA = flowMean
      flowSDDATA =   flowSd
      tempMeanDATA = tempMean
      tempSDDATA =   tempSd

      #emPermDATA = dMData$emPerm

      intervalDays = as.numeric( ( coreData$lagDetectionDate - coreData$detectionDate ) / 86400 )

      # indexing of the input and state vectors
      year = coreData$year - min(as.numeric(coreData$year)) + 1
      nYears = max(coreData$year) - min(as.numeric(coreData$year)) + 1
      season = coreData$season
      nAllRows = nrow(coreData)


      nFirstObsRows = nFirstObsRows
      firstObsRows = firstObsRows$firstObsRows

      nOcc = length(unique(coreData$sampleNumber))
      occ = coreData$sampleNumber-min(coreData$sampleNumber)-1

      nEvalRows = nEvalRows
      evalRows = evalRows$evalRows

      lastObsRows = lastObsRows$lastObsRows
      nLastObsRows = nLastObsRows

      nOut = nEvalRows # evalRows to output for each trace

      # mean intervaldays by season and river for interval boundaries [ s,r ]
      dIntDays <- data.frame(enc=encDATA, int=as.numeric(intervalDays), river=riverDATA, season=season)
      dIntDaysMean <- dIntDays %>%
                      dplyr::filter( enc == 1 ) %>%
                      group_by( season,river ) %>%
                      summarize( int = mean( int, na.rm=TRUE ) ) %>%
                      ungroup() %>%
                      as.matrix()

      intervalMeans <- addColMeans( matrix(dIntDaysMean[,'int'],nrow=length(unique(coreData$season)),ncol=nRivers, byrow=T) )
      rm(dIntDays)

      proportionSampled <- coreData$proportionSampled
      ####################################
      # will need to update this depending on model structure
      #
      # create a data frame of max lengths for YOYs from Matt with some visual fixes,

      # including fall,winter 0+ fish in YOY category
      #  cutoffYOYDATA <- cutoffYOYDATA

      # including fall,winter 0+ and spring 1+ fish in YOY category
      #cutoffYOYDATA <- cutoffYOYInclSpring1DATA
      #
      ########################################


    }
  )

  return(d)
}
