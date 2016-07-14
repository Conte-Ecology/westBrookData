#'Add immutable sample properties to a data frame
#'@return A data.frame appended with sample properties
#'@param data A data.frame created with createCoreData(), which must include sampleNumber as a column 
#'@param basecolumns Logical: include default columns? year,season,medianDate
#'@param columnsToAdd A character vector of columns to inlcude; can replace or add to baseColumns
#'@export

#could pull sample_name through, but need to deal with the multiple names per number before that
addSampleProperties<-function(data,defaultColumns=T,columnsToAdd=NULL){
  reconnect()
  whichDrainage<-"west"
  if(all(!unique(data$river) %in% c("west brook","wb jimmy","wb mitchell","wb obear"))){
    whichDrainage<-"stanley"
  }
  
  if(defaultColumns==T){
    chosenColumns<-c("year","season","median_date","proportion_sampled")
  }
  chosenColumns<-c(chosenColumns,columnsToAdd) %>% fillUnderscore()
  if(is.null(chosenColumns)) stop("Must choose at least one column to add")
  
  fillMedianDate<-"median_date" %in% chosenColumns
  proportionSampled<-"proportion_sampled" %in% chosenColumns
  chosenColumns<-c(chosenColumns[!chosenColumns %in% c("median_date","proportion_sampled")],
                   "sample_name","sample_number") %>% unique()
  
  newData<-tbl(conDplyr,'data_seasonal_sampling') %>%
    dplyr::filter(seasonal==TRUE&drainage==whichDrainage) %>%
    select(one_of(chosenColumns)) %>%
    distinct() %>%
    collect(n=Inf)
  names(newData)<-camelCase(names(newData))

  data<-left_join(data,newData,by=c('sampleName','sampleNumber'))
  
  if(fillMedianDate){
    newData<-tbl(conDplyr,'data_seasonal_sampling') %>%
      filter(drainage==whichDrainage) %>%
      select(sample_name,median_date) %>%
      distinct() %>%
      collect(n=Inf)
    names(newData)<-camelCase(names(newData))
    
    data<-left_join(data,newData,by='sampleName','sampleNumber')
    data[is.na(data$detectionDate),"detectionDate"]<-
      data[is.na(data$detectionDate),"medianDate"]
    data<-select(data,-medianDate)
  }
  if(proportionSampled){
    newData<-tbl(conDplyr,'data_seasonal_sampling') %>%
      dplyr::filter(seasonal==TRUE&drainage==whichDrainage) %>%
      select(sample_name,sample_number,river,proportion_sampled) %>%
      distinct() %>%
      collect(n=Inf)
    names(newData)<-camelCase(names(newData))
    
    data<-left_join(data,newData,by=c('sampleName','sampleNumber','river'))
  }
  
  return(data)
}