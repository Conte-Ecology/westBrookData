#'Download data from West Brook database
#'@return a data.frame of fish data
#'@param sampleType A character vector of sampling types to be included in output, options are: electrofishing,stationaryAntenna,portableAntenna,trap
#'@param baseColumns Logical: include default columns? tag,detection_date
#'@param columnsToAdd A character vector of columns to inlcude; can replace or add to baseColumns
#'@param includeUntagged Logical: include untagged fish?
#'@details \strong{Column names for tables}
#' These are the options for columns to add depending on the sampleType chosen. 
#'  When multiple sampleTypes are selected, columns that only exist within one are given NA for the other sampleType(s)
#'@details \emph{electrofishing:}
#'@details tag, fish_number, species, cohort, sample_number, detection_date, season_number, river, area, section, observed_length, survey, sample_name
#'@details \emph{stationaryAntenna:}
#'@details tag, detection_date, river, area, section, survey, sample_name, reader_id, sample_type, alive_or_dead, instance, arrival, departure, comment
#'@details \emph{portableAntenna:}
#'@details tag, detection_date, river, area, section, survey, sample_name, reader_id, sample_type, alive_or_dead, instance, pass, quarter, left_or_right, habitat, cover, justification, comment
#'@details \emph{trap:}
#'@details tag, fish_number, species, cohort, sample_number, detection_date, season_number, river, area, section, observed_length, survey, sample_name
#'  
#'@export
createCoreData<-function(sampleType=NULL,
                  baseColumns=T,
                  columnsToAdd=NULL,
                  includeUntagged=F,
                  selectRows=list()){
  reconnect() #make sure the link to the database still exists
  #define the tables to grab from
  st<-list(electrofishing="data_tagged_captures",
           stationaryAntenna="data_stationary_antenna",
           portableAntenna="data_portable_antenna",
           trap="data_tagged_captures",
           dead="tags_dead")
  
  tables<-st[sampleType]
  if(includeUntagged) tables<-c(tables,"untagged_captures")
  
  #define the columns to grab
  if(baseColumns){
    chosenColumns<-c("data_by_tag.tag","detection_date")
  } else chosenColumnns<-NULL
  
  if(!is.null(columnsToAdd)){
    chosenColumns <- c(chosenColumns,columnsToAdd)
  }
  #define tags to grab
#   if(length(selectRows>0)){
#     rowQuery<-NULL
#     for(n in names(selectRowsOuput)){
#       paste(" = '",selectRows[[n]])
#     }
#     selectionColumns<-paste(c("tag",names(selectRowsOutput),collapse=", ")
#                           
#     tags<-dbGetQuery(con,paste0("SELECT tag FROM by_tag_data",
#                                 "WHERE " selectionColumns)
#   }
  
  #create receptacle for data
  dataOut<-NULL
  
#add data from each table
  for(t in tables){
    #select only columns that exist in each table
    tableColumns<-dbGetQuery(con,
                  paste0("SELECT column_name ",
                        "FROM information_schema.columns ",
                        "WHERE table_name = '",t,"'")
                  )$column_name
    chosenTableColumns<-chosenColumns[chosenColumns %in% tableColumns]
    columnQuery<-paste(unique(chosenTableColumns),collapse=",")
    query<-paste("SELECT",columnQuery,
                "FROM",t,
                "INNER JOIN data_by_tag",
                "ON ",paste(t,"tag",sep=".")," = data_by_tag.tag",
                "WHERE data_by_tag.species = 'bkt'")
    #add data from this table to the data receptacle 
    dataOut<-dplyr::bind_rows(dataOut,RPostgreSQL::dbGetQuery(con,query))
  }
  
  columnsNotIncluded<-chosenColumns[!chosenColumns %in% names(dataOut)]
  if(length(columnsNotIncluded)>0){
    warning(paste0("column(s) ",paste(columnsNotIncluded,collapse=", "),
                   " do not exist in any sampleType selected"))
    }
  
  return(dataOut)
}

