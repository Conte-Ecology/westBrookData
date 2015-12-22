#'Test function for extracting data
#'@return Not much...yet
#'@export
getFish<-function(species=c("bkt","bnt","ats"),
                  tagged=T,
                  sampleType=NA,
                  baseColumns=T,
                  columnsToAdd=NULL){
  reconnect() #make sure the link to the database still exists
  if(baseColumns){
    chosenColumns<-c("tag","detection_date")
  } else chosenColumnns<-NULL
  
  if(is.null(columnsToAdd)){
    chosenColumns <- c(chosenColumns,columnsToAdd)
  }
  
  chosenColumns<-unique(chosenColumns)
  
  query<-paste("SELECT",paste(chosenColumns,collapse=",")," FROM data_tagged_captures")
  data<-dbGetQuery(con,query)
  return(data)
}