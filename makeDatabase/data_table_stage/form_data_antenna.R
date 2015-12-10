column_code <- list(
  tag = function(tag) {
	#	    return(paste(tag, species, sep='-'))
		return(tag)
  },
	detection_date = function(earliest_detection_date_time) {
		require(lubridate)
		detection_date <- parse_date_time(x=earliest_detection_date_time, orders='mdyhm')
		detection_date[detection_date > now()] <- 
			detection_date[detection_date > now()] - years(100)
		return(detection_date)
	},
	river = function(river) return(river),
	area = function(area) return(area),
	section = function(section) return(section),
	survey = function(survey) return(survey),
	sample_name = function(sample_name) return(sample_name),
	reader_id = function(reader_id) return(reader_id)
)


source_data <- dbGetQuery(con, "SELECT * FROM tags_antenna;")
source_data <- pipeline_data_transformation(
	data=source_data, pipeline=column_code)


dbWriteTable(con, 'data_antenna', source_data, row.names=FALSE,
						 overwrite=TRUE, append=FALSE)



