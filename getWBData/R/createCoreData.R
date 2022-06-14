#'Download data from West Brook database
#'@return a data.frame of fish data
#'@param sampleType A character vector of sampling types to be included in output, options are: electrofishing,stationaryAntenna,portableAntenna,trap,seine,snorkel
#'@param baseColumns Logical: include default columns? tag,detection_date
#'@param columnsToAdd A character vector of columns to inlcude; can replace or add to baseColumns
#'@param includeUntagged Logical: include untagged fish? This also adds cohort and species for these individuals
#'@details \strong{Column names for tables}
#' These are the options for columns to add depending on the sampleType chosen. 
#'  When multiple sampleTypes are selected, columns that only exist within one are given NA for the other sampleType(s)
#'@details \emph{captures:}
#'@details tag, fishNumber, species, cohort, sampleNumber, detectionDate, seasonNumber, river, area, section, observedLength, observedWeight, survey, sampleName
#'@details \emph{stationaryAntenna:}
#'@details tag, detectionDate, river, riverMeter, survey, readerId, departure, comment
#'@details \emph{portableAntenna:}
#'@details tag, detectionDate, river, area, section, survey, sampleName, readerId, sampleType, aliveOrDead, instance, pass, quarter, leftOrRight, habitat, cover, justification, comment
#'@details \emph{acoustic:}
#'@details tag, acousticTag, datetime, section, quarter

#'  
#'@export
createCoreData<-function(sampleType="electrofishing",
                  baseColumns=T,
                  columnsToAdd=NULL,
                  includeUntagged=F,
                  whichDrainage="west"){
  reconnect() #make sure the link to the database still exists

  if(any(!sampleType %in% c("trap","electrofishing","seine","acoustic",
                            "snorkel","stationaryAntenna","portableAntenna"))){
    invalidType<-sampleType[which(!sampleType %in% c("trap","electrofishing","seine",
                                                     "snorkel","stationaryAntenna",
                                                     "portableAntenna"))]
    stop(paste("Invalid sampleType:",invalidType))
  }
  tables<-NULL
  #define the survey values to grab from capture data depending on sampleType
  if(any(sampleType %in% c("trap","electrofishing","seine","snorkel"))){
    tables<-c(sampleType[!sampleType %in% c("trap","electrofishing","seine","snorkel")],"captures")
    captureTypes<-sampleType[sampleType %in% c("trap","electrofishing","seine","snorkel")]
    possibleCaptureTypes<-list(trap=c("box trap","screw trap","duda fyke","fyke net"),
                       electrofishing="shock",
                       seine=c("snorkel-seine-day","night seine"),
                       snorkel=c("snorkel-seine-day","snorkel-night"))
    captureTypes<-possibleCaptureTypes[captureTypes] %>% unlist()
    names(captureTypes)<-NULL
  }
  tables<-c(tables,sampleType[!sampleType %in% c("trap","electrofishing","seine","snorkel")]) %>%
          unique()
  
  st<-list(captures="data_tagged_captures",
           stationaryAntenna="data_stationary_antenna",
           portableAntenna="data_portable_antenna",
           acoustic="data_acoustic")
  
  tables<-st[tables]
  if(includeUntagged) tables<-c(tables,"data_untagged_captures")
  
  
  #define the columns to grab
  if(baseColumns){
    chosenColumns<-c("tag","detection_date","sample_name",
                     "sample_number","river","section","area",
                     "observed_length")
  } else chosenColumnns<-NULL
  
  if(!is.null(columnsToAdd)){
    chosenColumns <- fillUnderscore(c(chosenColumns,columnsToAdd))
  }
  
  if(is.null(chosenColumns)){stop("No columns chosen")}
  chosenColumns<-unique(c(chosenColumns,"survey"))
#create receptacle for data
  dataOut<-NULL
  
#add data from each table
  for(t in tables){
    #select only columns that exist in each table
    tableColumns<-DBI::dbGetQuery(con,
                  paste0("SELECT column_name ",
                        "FROM information_schema.columns ",
                        "WHERE table_name = '",t,"'")
                  )$column_name
    if(t=="data_untagged_captures"){
      chosenTableColumns<-match(c(chosenColumns,"species","cohort"),
                                tableColumns,nomatch=0) %>% .[.>0]
      } else chosenTableColumns<-match(chosenColumns,tableColumns,nomatch=0) %>% .[.>0]
    
    newData<-tbl(conDplyr,t) %>%
              filter(drainage==whichDrainage) %>%
              select(chosenTableColumns)
    if(t=="data_tagged_captures"){
      if(length(captureTypes)>1){
        newData<-newData %>% dplyr::filter(survey %in% captureTypes)
        } else {newData<-newData %>% dplyr::filter(survey==captureTypes)}
    }
    newData<-collect(newData,n=Inf)
    
    if(t=="data_acoustic"){
      newData$survey<-"acoustic"
    }
    
    dataOut<-bind_rows(dataOut,newData)
  }
  
  columnsNotIncluded<-chosenColumns[!chosenColumns %in% names(dataOut)]
  if(length(columnsNotIncluded)>0){
    warning(paste0("column(s) ",paste(columnsNotIncluded,collapse=", "),
                   " do(es) not exist in any sampleType selected"))
  }
  if(!"survey" %in% columnsToAdd & sampleType[1]!="acoustic"){
    dataOut<-select(dataOut,-survey)
  }
  names(dataOut)<-camelCase(names(dataOut))
  dataOut<-arrange(dataOut,tag,detectionDate)
  return(dataOut)
}

