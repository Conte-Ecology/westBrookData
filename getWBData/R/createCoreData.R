#'Download data from West Brook database
#'@returns a data.frame of fish data
#'@param sampleType A character vector of sampling types to be included in output, options are: electrofishing,stationaryAntenna,portableAntenna,trap
#'@param baseColumns Logical: include default columns? tag,detection_date
#'@param species A character vector of species to include, possible codes: bkt,bnt,ats,bnd
#'@param columnsToAdd A character vector of columns to inlcude; can replace or add to baseColumns
#'@param includeUntagged Logical: include untagged fish?
#'@export
createCoreData<-function(sampleType="electrofishing",
                  species=c("bkt","bnt","ats"),
                  baseColumns=T,
                  columnsToAdd=NULL,
                  includeUntagged=F){
  reconnect() #make sure the link to the database still exists
  #define the tables to grab from
  st<-list(electrofishing="data_tagged_captures",
           stationaryAntenna="data_stationary_antenna",
           portableAntenna="data_portable_antenna",
           trap="data_tagged_captures")
  
  tables<-st[sampleType]
  if(includeUntagged) tables<-c(tables,"untagged_captures")
  
  #define the columns to grab
  if(baseColumns){
    chosenColumns<-c("tag","detection_date")
  } else chosenColumnns<-NULL
  
  if(!is.null(columnsToAdd)){
    chosenColumns <- c(chosenColumns,columnsToAdd)
  }

  columnQuery<-paste(unique(chosenColumns),collapse=",")
  
  dataOut<-NULL
  for(t in tables){
    query<-paste("SELECT",columnQuery,
                "FROM",t)
    dataOut<-rbind(dataOut,RPostgreSQL::dbGetQuery(con,query))
  }
  return(dataOut)
}

