#'Add immutable tag properties to core westbrook data
#'@return Immutable tag properties added to a data.frame
#'@param coreData a data.frame created using createCoreData
#'@param columnsToAdd A character vector of columns to inlcude; can replace or add to baseColumns
#'@details \strong{Options of columns to add}
#' dateKnownDead, lastAntennaDetection, species, firstCaptureSample, lastCaptureSample, cohort, sex, familyId
#'@export

addTagProperties<-function(coreData,
  columnsToAdd=c("species","cohort")
  ){
  columnsToAdd<-fillUnderscore(columnsToAdd)
  possibleColumns<-RPostgreSQL::dbGetQuery(con,
                         paste0("SELECT column_name ",
                                "FROM information_schema.columns ",
                                "WHERE table_name = 'data_by_tag'")
                         )$column_name
  if(any(!columnsToAdd %in% possibleColumns)){
    badColumn<-columnsToAdd[!columnsToAdd %in% allColumns]
    stop(paste0(
            paste(badColumn,collpase=", "),
            " do(es) not exist in data_by_tag, see help for possible columns"
            )
         )
    }

  columnQuery<-paste(unique(c(columnsToAdd,"tag")),collapse=", ")
  query<-paste("SELECT",columnQuery,"FROM data_by_tag")
  tagProperties<-RPostgreSQL::dbGetQuery(con,query)
  
  if(coreData %>% filter(is.na(tag)) %>% nrow() >0){
    untagged<-coreData %>% filter(is.na(tag))
    coreData<-coreData %>% select(-cohort,-species)
  }
  
  coreData<-coreData %>%
            filter(!is.na(tag)) %>%
            left_join(tagProperties,by="tag")
  names(coreData)<-camelCase(names(coreData))
  
  if(exists("untagged",inherits=F)){
    coreData<-bind_rows(coreData,untagged)
  }
  return(coreData)
}