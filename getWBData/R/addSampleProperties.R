#'Add immutable sample properties to a data frame
#'@return A data.frame appended with sample properties
#'@param data A data.frame created with createCoreData(), which must include sampleNumber as a column 
#'@param basecolumns Logical: include default columns? proportionSampled,year,season
#'@param columnsToAdd A character vector of columns to inlcude; can replace or add to baseColumns
#'@export

addSampleProperties<-function(data,defaultColumns=T,columnsToAdd=NULL){
  data %>% mutate(naRiver=is.na(river))
  data[is.na(data$river)]<-"west brook"
  
  
  if(defaultColumns==T){
    chosenColumns<-c("proportion_sampled","year","season","median_date")
  }
  chosenColumns<-c(chosenColumns,columnsToAdd) %>% fillUnderscore()
  if(is.null(chosenColumns)) stop("Must choose at least one column to add")
  
  fillMedianDate<-"median_date" %in% chosenColumns
  chosenColumns<-c(chosenColumns,"sample_number","river") %>% unique()
  
  newData<-tbl(conDplyr,'data_seasonal_sampling') %>%
    select(one_of(chosenColumns)) %>%
    filter(!is.na(sample_number)) %>%
    collect()
  names(newData)<-camelCase(names(newData))

  data<-left_join(data,newData,by=c('sampleNumber','river'))
  
  if(fillMedianDate){
    data[is.na(data$detectionDate),"detectionDate"]<-
      data[is.na(data$detectionDate),"medianDate"]
    data[naRiver==T,river]<-NA
    data<-select(data,-medianDate,-naRiver)
  }
  return(data)
}