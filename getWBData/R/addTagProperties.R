#'Add immutable tag properties to core westbrook data
#'@return Immutable tag properties added to a data.frame
#'@param coreData a data.frame created using createCoreData
#'@param columnsToAdd A character vector of columns to inlcude; can replace or add to baseColumns
#'@details \strong{Options of columns to add}
#' dateKnownDead, lastAntennaDetection, species, firstCaptureSample, lastCaptureSample, cohort
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
  
  coreData<-dplyr::left_join(coreData,tagProperties,by="tag")
  
  names(coreData)<-camelCase(names(coreData))
  return(coreData)
}