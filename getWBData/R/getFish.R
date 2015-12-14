#'Test function for extracting data
#'@returns Not much...yet
#'@export
getFish<-function(tagged=T,sampleType=NA){
  reconnect() #make sure the link to the database still exists

  dbGetQuery(con,"SELECT * FROM")
}