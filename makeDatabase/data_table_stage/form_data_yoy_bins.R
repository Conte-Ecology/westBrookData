column_code <- list(
  river=function(river) return(river),
  species=function(species) return(species),
  sample_name=function(sample) return(as.character(sample)),
  age=function(age) return(as.numeric(age)),
  cohort_min_length=function(cohort_min_length){
    return(as.numeric(cohort_min_length))
    },
  cohort_max_length=function(cohort_max_length){
    return(as.numeric(cohort_max_length))
  },
  cohort=function(cohort){return(as.numeric(cohort))}
)

source_data <- dbGetQuery(con, "SELECT * FROM yoy_bins;")
source_data <- data.table(pipeline_data_transformation(
  data=source_data, pipeline=column_code))
setkey(source_data,sample_name)

samples<-data.table(dbGetQuery(con,"SELECT * FROM data_seasonal_sampling"))
samples<-unique(samples[,list(sample_name,year)])
setkey(samples,sample_name)

source_data<-samples[source_data]
# source_data[,cohort:=year-age] 
source_data[,year:=NULL]

dbWriteTable(con, 'data_yoy_bins', data.frame(source_data), row.names=FALSE,
             overwrite=TRUE, append=FALSE)
