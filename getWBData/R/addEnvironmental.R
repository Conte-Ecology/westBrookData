#'Add environmental data to core westbrook data
#'@return Environmental data added to a coreData
#'@param coreData a data.frame created using createCoreData
#'@param 
#'@details \strong{Options of columns to add}
#' 
#'@export

addEnvironmental <-function(coreData,tempData){

 
#######  
# fill in median dates for unsampled occasions - this will go in addSampleProperties  
medianDate <- 
    coreData %>%
      #dplyr::filter( !is.na( river )) %>%
      group_by( sampleNumber ) %>%
      summarize( medianDate = median( detectionDate, na.rm = T )  )

coreData1 <- 
  coreData %>% 
    left_join( .,medianDate  ) %>%
    mutate( date = medianDate ) 
coreData1$date[ !is.na(coreData1$detectionDate) ] <- coreData1$detectionDate  #couldn't get this to work easily in dplyr and ifelse does not return a date class          
              
#######

  
}