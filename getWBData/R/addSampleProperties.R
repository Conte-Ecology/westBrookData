#'Add immutable sample properties to a data frame
#'@return A data.frame appended with sample properties
#'@param data A data.frame created with createCoreData(), which must include sampleNumber as a column 
#'@param basecolumns Logical: include default columns? year,season,medianDate
#'@param columnsToAdd A character vector of columns to inlcude; can replace or add to baseColumns
#'@export

addSampleProperties<-function(data,defaultColumns=T,columnsToAdd=NULL){
  reconnect()
  
  if(defaultColumns==T){
    chosenColumns<-c("year","season","median_date")
  }
  chosenColumns<-c(chosenColumns,columnsToAdd) %>% fillUnderscore()
  if(is.null(chosenColumns)) stop("Must choose at least one column to add")
  
  fillMedianDate<-"median_date" %in% chosenColumns
  chosenColumns<-c(chosenColumns[chosenColumns!="median_date"],
                   "sample_number","river") %>% unique()
  
  newData<-tbl(conDplyr,'data_seasonal_sampling') %>%
    select(one_of(chosenColumns)) %>%
    filter(!is.na(sample_number)) %>%
    collect()
  names(newData)<-camelCase(names(newData))

  data<-left_join(data,newData,by=c('sampleNumber','river'))
  
  if(fillMedianDate){
    newData<-tbl(conDplyr,'data_seasonal_sampling') %>%
      select(sample_number,median_date) %>%
      distinct() %>%
      collect()
    names(newData)<-camelCase(names(newData))
    
    data<-left_join(data,newData,by='sampleNumber')
    data[is.na(data$detectionDate),"detectionDate"]<-
      data[is.na(data$detectionDate),"medianDate"]
    data<-select(data,-medianDate)
  }
  return(data)
}