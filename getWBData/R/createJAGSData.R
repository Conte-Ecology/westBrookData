#'Create data needed for JAGS runs
#'@return List of evalRows and firstObsRows
#'@param coreData a data.frame created using createCoreData
#'@param 
#'@details \strong{Options of columns to add}
#' 
#'@export

createJAGSData <-function(coreData, modelType = 'CJS'){
 
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
  
#  nrow(coreData)
#  length(unique(coreData$tag))
  
  
#  coreData %>% group_by(tag) %>% summarize( n=n() ) %>% 
    #filter(n==1)
#    ggplot( aes(n)) + geom_histogram()
  
#  repeatRows <- coreData %>% group_by( tag,sampleNumber ) %>% filter( n() > 1 )
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

  
#################################################################
  coreData$year <- as.factor(format(coreData$detectionDate, format="%Y"))
  coreData$riverOrdered <- as.numeric( factor(coreData$river,levels=c('west brook','wb jimmy','wb mitchell','wb obear'), ordered=T) )
  
  # means for standardizing 
  #####################################################################  
  stdBySeasonRiverYear <- coreData %>%
                            group_by(river,seasonNumber,year) %>% 
                            summarize(
                                 lengthMn=mean(observedLength, na.rm=TRUE)
                                 
                        #         tempMn=mean(fullMeanT, na.rm=TRUE),
                        #         tempMnP=mean(temperatureForP, na.rm=TRUE), 
                        #         flowMn=mean(fullMeanD, na.rm=TRUE),
                        #         dischMnP=mean(dischargeForP,na.rm=T) 
                                 )
  stdBySeasonRiverYear<-stdBySeasonRiverYear[!is.na(stdBySeasonRiverYear$river),]
  #
  stdBySeasonRiver <- stdBySeasonRiverYear %>%
                        group_by(river,seasonNumber) %>% 
                        summarize(
                             lengthMean=mean(lengthMn, na.rm=TRUE),                       
                             lengthSd=sd(lengthMn, na.rm=TRUE),
                             lengthLo = quantile(lengthMn,c(0.025), na.rm=TRUE),
                             lengthHi = quantile(lengthMn,c(0.975), na.rm=TRUE)
#                              tempMean=mean(tempMn, na.rm=TRUE),
#                              tempMeanP=mean(tempMnP, na.rm=TRUE),
#                              tempSd=sd(tempMn, na.rm=TRUE),
#                              tempSdP=sd(tempMnP, na.rm=TRUE),
#                              tempLo = quantile(tempMn,c(0.025), na.rm=TRUE),
#                              tempHi = quantile(tempMn,c(0.975), na.rm=TRUE),
#                              flowMean=mean(flowMn, na.rm=TRUE), 
#                              flowSd=sd(flowMn, na.rm=TRUE),
#                              dischMeanP=mean(dischMnP,na.rm=T),
#                              dischSdP=sd(dischMnP,na.rm=T),
#                              flowLo = quantile(flowMn,c(0.025), na.rm=TRUE),
#                              flowHi = quantile(flowMn,c(0.975), na.rm=TRUE) 
                               )
  ############# To get rid of NA Rivers
  stdBySeasonRiver<-stdBySeasonRiver[!is.na(stdBySeasonRiver$river),]
  
  
  
#################################################################
  
  d <- within(
    data = list(),
    expr = {
      
      encDATA = as.numeric( coreData$enc ) 
      riverDATA = coreData$riverOrdered
      nRivers = length( unique( coreData$river ) ) - 1 #-1 for NA
      lengthDATA = coreData$observedLength
      #availableDATA = dMData$available01 no longer need - taken care of with censor()
      ind = coreData$tagIndex
      
      # For standardizing length
      lengthMean = addColMeans( matrix(stdBySeasonRiver$lengthMean,nrow=length(unique(coreData$seasonNumber))-1,ncol=length(unique(as.numeric(coreData$riverOrdered)-0))-1) )
      lengthSd =   addColMeans( matrix(stdBySeasonRiver$lengthSd,  nrow=length(unique(coreData$seasonNumber))-1,ncol=length(unique(as.numeric(coreData$riverOrdered)-0))-1) )
## to here     
      lengthMean0 = stdByRiver$lengthMean0
      lengthSd0 = stdByRiver$lengthSd0
      # environmental covariates pertaining to intervals.  These are
      # covariates of growth and survival
      
      # For standardizing env predictors of growth and surv
      tempMean = addColMeans( matrix(stdBySeasonRiver$tempMean,nrow=length(unique(dMData$season)),ncol=length(unique(as.numeric(dMData$riverN)-0))-1) )
      tempSd =   addColMeans( matrix(stdBySeasonRiver$tempSd,nrow=length(unique(dMData$season)),ncol=length(unique(as.numeric(dMData$riverN)-0))-1) )  
      flowMean = addColMeans( matrix(stdBySeasonRiver$flowMean,nrow=length(unique(dMData$season)),ncol=length(unique(as.numeric(dMData$riverN)-0))-1) )
      flowSd =   addColMeans( matrix(stdBySeasonRiver$flowSd,nrow=length(unique(dMData$season)),ncol=length(unique(as.numeric(dMData$riverN)-0))-1) )  
      
      ## Predictors of phi for correcting N1 where countForN ==0
      tempForN = tempForN
      flowForN = flowForN
      
      # not used anymore, see below
      #  tempDATA = ( as.numeric(dMData$fullMeanT) - stdBySeason$tempMean[ as.numeric(dMData$season)] ) / stdBySeason$tempSd[ as.numeric(dMData$season) ]
      #  flowDATA = ( as.numeric(dMData$fullMeanD) - stdBySeason$flowMean[ as.numeric(dMData$season)] ) / stdBySeason$flowSd[ as.numeric(dMData$season) ]  
      
      
      # Now doing standardization in the bugs code to make sure that unobserved fish get observed std env data
      tempDATA = as.numeric(dMData$fullMeanT)
      flowDATA = as.numeric(dMData$fullMeanD)
      
      flowMeanDATA = flowMean 
      flowSDDATA =   flowSd
      tempMeanDATA = tempMean 
      tempSDDATA =   tempSd
      
      # emPermNA, used to censor likelihood for permanent emigrants
      # 1 on line before last observation with subsequent bottom of the study site antenna hit. 0's before and after if em, NAs otherwise
      # trying emPerm without the NAs 
      emPermDATA = dMData$emPerm 
      
      intervalDays = as.numeric(dMData$fullMeanIntLen )
      
      # Environmental covariates for p 
      flowP = as.numeric(dMData$dischargeForP)
      temperatureP = as.numeric(dMData$temperatureForP)
      #For standardizing env predictors of p
      flowMeanP = addRowColMeans( matrix(stdBySeasonRiver$dischMeanP,nrow=length(unique(dMData$season)),ncol=length(unique(as.numeric(dMData$riverN)-0))-1) )
      flowSdP = addRowColMeans( matrix(stdBySeasonRiver$dischSdP,nrow=length(unique(dMData$season)),ncol=length(unique(as.numeric(dMData$riverN)-0))-1) )
      tempMeanP = addRowColMeans( matrix(stdBySeasonRiver$tempMeanP,nrow=length(unique(dMData$season)),ncol=length(unique(as.numeric(dMData$riverN)-0))-1) )
      tempSdP = addRowColMeans( matrix(stdBySeasonRiver$tempSdP,nrow=length(unique(dMData$season)),ncol=length(unique(as.numeric(dMData$riverN)-0))-1) )
      
      # , growthSd = sd(((dMData$lagLength - dMData$length)/(as.numeric(dMData$intervalLength)))*365/4, na.rm=TRUE)
      ######## NEVER!!!! #########  gr = (dMData$lagLength - dMData$length)/(as.numeric(dMData$intervalLength))
      # indexing of the input and state vectors
      year = dMData$year-min(dMData$year) + 1
      nYears = max(dMData$year)-min(dMData$year)+1
      season = as.numeric(as.character(dMData$season)) 
      nAllRows = length(dMData[,1])
      nFirstObsRows = evalList$nFirstObsRows
      firstObsRows = evalList$firstObsRows
      nOcc = length(unique(dMData$sampleNum))
      occ = dMData$sampleNum-min(dMData$sampleNum)-1
      nEvalRows = evalList$nEvalRows # rows that will matter if we start using JS, and 
      evalRows = evalList$evalRows   # that matter now for the growth model
      lastPossibleRows = subset( 1:nrow(dMData),dMData$lastAIS==dMData$ageInSamples ) # need to put this in makedMData
      nLastPossibleRows = evalList$nFirstObsRows
      
      lastObsRows = evalList$lastObsRows
      nLastObsRows = evalList$nLastObsRows
      
      lastRows = lastPossibleRows
      nLastRows = nLastPossibleRows
      
      nOut = evalList$nEvalRows # evalRows to output for each trace
      
      #create variables that hold information on counts - data held in statsForN (made in makeDMData.R - based on pheno2Long, so has all cohorts. need to throw away years before dMData's first cohort)
      minYear <- min(dMData$year)
      firstYearIndex <- minYear-statsForN$minYear + 1
      # countForN has dummy river 1 in it
      countForN <- statsForN$countForN[,firstYearIndex:dim(statsForN$countForN)[2],]
      
      meanForN <- statsForN$meanForN
      sdForN <- statsForN$sdForN
      
      #  dMDataF <- dMData[ dMData$first == dMData$sampleNum, ]
      #  nTagged1 <- table(dMDataF$season,dMDataF$year,dMDataF$riverOrdered)
      
      #Fill in random #s for zRiv=1
      #  nTagged <- abind(matrix(round(runif(4*nYears,10,50)), nrow=4,ncol=nYears),nTagged1)
      floodDurDATA <- floodDur
      droughtDurDATA <- droughtDur
      floodFreqDATA <- floodFreq
      
      # mean intervaldays by season and river for interval boundaries [ s,r ]
      dIntDays <- data.frame(enc=encDATA, int=intervalDays, river=riverDATA, season=season)
      dIntDaysMean <- ddply(dIntDays[dIntDays$enc==1,], .(season,river), colMeans)
      intervalMeans <- addColMeans( matrix(dIntDaysMean$int,nrow=length(unique(dMData$season)),ncol=length(unique(as.numeric(dMData$riverN)-0))-1, byrow=T) ) 
      rm(dIntDays)
      
      # propSamp - proportion of each season,river,year combo that got sampled (proportion of sctions sampled)
      # this doesn't work indexed by evalRows becasue there's no river value when fish aren't captured
      #propSampledDATA = dMData$propSampled
      
      # check data for propSampledDATA
      #tmp <- data.frame(cbind(dMData$year,dMData$season,dMData$river,dMData$enc,dMData$section))
      #names(tmp ) <- c('year','season','river','enc','section')
      #tmp2 <- tmp[tmp$enc==1 ,]  #& tmp$section %in% 1:47
      #f <- ftable(tmp2$year,tmp2$river,tmp2$season,tmp2$section)
      #cbind(1:nrow(f),rowSums(f))
      
      propSampledDATA =  array( 1, c(4,5,11) ) #season, river year
      
      propSampledDATA[ c(1,4),2:5,1 ] <- 0     #all spring and winter samples in 2002
      propSampledDATA[ 2,3:4,1 ] <- 0          #J and M summer samples in 2002
      #propSampledDATA[ 4,2:5,1 ] <- 0          #all winter samples in 2002
      propSampledDATA[ 4,2,2 ] <- 30/47        #WB winter sample in 2003
      propSampledDATA[ 4,2,3 ] <- 3/47         #WB winter sample in 2004
      propSampledDATA[ 4,2,4 ] <- 0            #WB winter sample in 2005
      propSampledDATA[ 4,2,6 ] <- 0            #WB winter sample in 2007 
      
      
      # zeroSectionsDATA - completely unsampled winter samples. not including samples before season 4,year 1 because we didn't want to rewrite the meanPhiS34 indexing [mostly noise ni these estimates anyway]
      
      zeroSectionsDATA <- array( 0, c(4,5,11) ) #season, river year
      
      zeroSectionsDATA[ 3:4,2:5,1 ] <- 1          #all winter samples in 2002
      zeroSectionsDATA[ 3:4,2,4 ] <- 1            #WB winter sample in 2005
      zeroSectionsDATA[ 3:4,2,6 ] <- 1            #WB winter sample in 2007
      
      
      
      ####################################
      # create a data frame of max lengths for YOYs from Matt with some visual fixes,
      
      # including fall,winter 0+ fish in YOY category
      #  cutoffYOYDATA <- cutoffYOYDATA 
      
      # including fall,winter 0+ and spring 1+ fish in YOY category
      cutoffYOYDATA <- cutoffYOYInclSpring1DATA
      #
      ########################################
      
      ### read in parameter values from previous run ############
      iterToUse <- 5000
      chainToUse <- 2
      
      grBetaInt <- out$grBetaInt[,,,,iterToUse,chainToUse]
      grBeta <- out$grBeta[ ,,,,iterToUse,chainToUse ]   
      
      grSigmaBeta <- out$grSigmaBeta[ ,,,,iterToUse,chainToUse ]  
      
      pBetaInt <- out$pBetaInt[ ,,,,iterToUse,chainToUse ] 
      pBeta <- out$pBeta[ ,,,,iterToUse,chainToUse ] 
      
      phiBetaInt <- out$phiBetaInt[ ,,,iterToUse,chainToUse ]
      phiBeta <- out$phiBeta[ ,,,,iterToUse,chainToUse ]
      
      psiBeta <- out$psiBeta[ ,,,iterToUse,chainToUse ]
      
    }
  )
  
  
  
  
  
  
  
  
  
  
   
  return(list(evalRows = evalRows$evalRows,
              nEvalRows = nEvalRows,
              firstObsRows = firstObsRows$firstObsRows,
              nFirstObsRows = nFirstObsRows,
              
))
}