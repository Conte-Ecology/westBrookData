column_code <- list(
  river=function(river) return(river),
  species=function(species) return(species),
  sample_name=function(sample) return(as.numeric(sample)),
  age=function(age) return(as.numeric(age)),
  cohort_min_length=function(cohort_min_length){
    return(as.numeric(cohort_min_length))
    },
  cohort_max_length=function(cohort_max_length){
    return(as.numeric(cohort_max_length))
  }
)


source_data <- dbGetQuery(con, "SELECT * FROM yoy_bins;")
source_data <- data.table(pipeline_data_transformation(
  data=source_data, pipeline=column_code))

samples<-data.table(dbGetQuery(con,"SELECT year, sample_name FROM data_seasonal_sampling"))
samples[,sample_name:=as.numeric(sample_name)]
setkey(samples,sample_name)
setkey(source_data,sample_name)

yoy_bins<-samples[source_data]
       
dbWriteTable(con, 'data_yoy_bins', source_data, row.names=FALSE,
             overwrite=TRUE, append=FALSE)
