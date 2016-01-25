
#'Add environmental data to core westbrook data
#'@return Environmental data added to a coreData
#'@param coreData a data.frame created using createCoreData
#'@param 
#'@details \strong{Options of columns to add}
#' 
#'@export

addEnvironmental <-function( coreData ){

  # get temperature data from database
  queryTemp <-"SELECT * FROM data_daily_temperature"
  envData <- RPostgreSQL::dbGetQuery(con,queryTemp)
  
  # get flow extension data from database
  queryFlow <-"SELECT * FROM data_flow_extension"
  flowData <- RPostgreSQL::dbGetQuery(con,queryFlow)
  
  #merge data
  envData <- left_join( envData, flowData )
  
  # set up table of intervals based on date for each capture interval by fish
  coreData <- coreData %>%
                group_by( tag ) %>%
                mutate( lagDetectionDate = lead( detectionDate ) ) 
  
  %>%
                select( tag, sampleNumber, detectionDate, lagDetectionDate )
  
  
  tmp <- coreData %>%
           group_by( tag,sampleNumber ) %>%
           mutate(
             indMeanTemp = 
               mean( envData$daily_mean_temp[ envData$river == river &
                                              envData$date >= detectionDate & 
                                              envData$date <= lagDetectionDate ], rm.na=T
                    )
                  )
            
       
           
                

  
}