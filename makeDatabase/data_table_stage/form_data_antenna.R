source_data <- dbGetQuery(con, "SELECT * FROM tags_antenna;")

column_code_portable <- list(
  tag = function(tag) {
	#	    return(paste(tag, species, sep='-'))
		return(tag)
  },
	detection_date = function(earliest_detection_date_time) {
		require(lubridate)
		detection_date <- parse_date_time(x=earliest_detection_date_time, orders=date.format)
		detection_date[detection_date > now()] <- 
			detection_date[detection_date > now()] - years(100)
		return(detection_date)
	},
	river = function(river) return(river),
	area = function(area) return(area),
	section = function(section) return(section),
  survey = function() return("portableAntenna"),
	sample_name = function(sample_name) return(sample_name),
	reader_id = function(reader_id) return(reader_id),
  sample_type = function(sample_type) return(sample_type),
	alive_or_dead = function(alive_or_dead) return(alive_or_dead),
	instance = function(antenna_instance) return(antenna_instance),
  pass = function(pass) return(pass),
	quarter = function(quarter) return(quarter),
	left_or_right = function(left_or_right) return(left_or_right),
	habitat = function(habitat) return(habitat),
	cover = function(cover) return(cover),
	justification = function(justification) return(justification),
  comment = function(comment) return(comment)
)

portableData<-source_data[grep("able",source_data$sample_type),]

portableData <- pipeline_data_transformation(
	data=portableData, pipeline=column_code_portable)

dbWriteTable(con, 'data_portable_antenna', portableData, row.names=FALSE,
             overwrite=TRUE, append=FALSE)

column_code_stationary <- list(
  tag = function(tag) {
    #	    return(paste(tag, species, sep='-'))
    return(tag)
  },
  detection_date = function(earliest_detection_date_time) {
    require(lubridate)
    detection_date <- parse_date_time(x=earliest_detection_date_time, orders=date.format)
    detection_date[detection_date > now()] <- 
      detection_date[detection_date > now()] - years(100)
    return(detection_date)
  },
  river = function(river) return(river),
  area = function(area) return(area),
  section = function(section) return(section),
  survey = function() return("stationaryAntenna"),
  sample_name = function(sample_name) return(sample_name),
  reader_id = function(reader_id) return(reader_id),
  sample_type = function(sample_type) return(sample_type),
  alive_or_dead = function(alive_or_dead) return(alive_or_dead),
  instance = function(antenna_instance) return(antenna_instance),
  arrival = function(arrival) return(arrival),
  departure = function(departure) return(departure),
  comment = function(comment) return(comment)
)

stationaryData<-source_data[grep("stationary",source_data$sample_type),]

stationaryData <- pipeline_data_transformation(
  data=stationaryData, pipeline=column_code_stationary)

dbWriteTable(con, 'data_stationary_antenna', stationaryData, row.names=FALSE,
             overwrite=TRUE, append=FALSE)


