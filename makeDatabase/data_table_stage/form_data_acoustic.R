column_code <- list(
  tag = function(tag) {
		return(tag)
  },
  acoustic_tag = function(acoustic_tag) {
    return(acoustic_tag)
  },
	detection_date = function(datetime) {
		require(lubridate)
		detection_date <- parse_date_time(x=datetime, orders=date.format)
		detection_date[detection_date > now()] <- 
			detection_date[detection_date > now()] - years(100)
		return(detection_date)
	},
	section = function(section) return(section),
	quarter = function(quarter) return(as.numeric(quarter))
)

acoustic <- pipeline_data_transformation(
	data=dbGetQuery(con, "SELECT * FROM tags_acoustic;"),
	pipeline=column_code)

dbWriteTable(con, 'data_acoustic', acoustic, row.names=FALSE,
             overwrite=TRUE, append=FALSE)