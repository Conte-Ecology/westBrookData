##############################################################################
# input block
  selectDrainage = 'west'                # west, NOT YET INCLUDED -> sawmill, stanley, shorey, catamaran
  selectSpecies = 'bkt'                  # bkt, bnt, ats, bnd?
  selectCohorts = 1997:2015              # cohort years
  subsetDMdataAgeInSamples = 15    # upper limit for ageInSamples
#####################################################################  

#'Select rows from the core database
#'
#'Creates a link to the West Brook database
#'@return A link to the database
selectRows <- function( drainage = selectDrainage,
                        species = selectSpecies,
                        cohorts = selectCohorts,
                        subsetDMdataAgeInSamples = subsetDMdataAgeInSamples){
  
  #                      drainage =       
  #                      species='bkt', 
  #                      cohorts=c(1996,2015),
  #                      studyYears = c(2000,2015),
  #                      modelType = 'js',
  #                      dbCredentials="~/wb_credentials.rds", 
  #                      processedDir="~/process-data/data_store/processed_data" 
  
  
  
 
  
  # exclude fish that were captured for the first time after the
  # following ageInSamples
  # set equal to subsetDMdataAgeInSamples - 1 to have no effect 
  maxAgeInSamplesFirstCapt <- subsetDMdataAgeInSamples - 1 #4  0
  
  
  # this could be specified in the function call, but for now it's just 
  # set up for west brook
  if (species == 'ats'  ) {
    subsetRiver <- tolower(c('WEST BROOK'))#,'WB JIMMY') 
    #,'WB MITCHELL',"WB OBEAR") 
    #riverSubset <- c("SHOREY BROOK") 
    # "SAWMILL RIVER","WB JIMMY","WB OBEAR", "WB MITCHELL", "CATAMARAN BROOK", 
    #riverSubset <- c("SAWMILL RIVER")
    subsetArea <- tolower(c('INSIDE', 'ABOVE', 'BELOW'))#, 'TRIB' ) 
  }
  
  if (species == c('bkt')) {
    subsetRiver <- tolower(c('WEST BROOK','WB JIMMY','WB MITCHELL',"WB OBEAR")) 
    subsetArea <- tolower(c('INSIDE', 'ABOVE', 'BELOW', 'TRIB','ABOVE ABOVE',
                            'aboveabove','BELOW BELOW' )) 
  }
  
  if (species == c('bnt')) {
    subsetRiver <- tolower(c('WEST BROOK','WB MITCHELL',"WB JIMMY")) 
    subsetArea <- tolower(c('INSIDE', 'ABOVE', 'BELOW', 'TRIB','ABOVE ABOVE',
                            'aboveabove','BELOW BELOW' )) 
  }
  
  
  
  
  
  
}