columns <- list(
	who = c('tag'),
	when = c('earliest_detection_date_time'),
	where = c('river','area','section','distance_upriver_m','distance_upriver_from_confluence'),
	how = c('reader_id','sample_type','sample_name','survey','source')
)

select_stmt <- paste(
		"SELECT", paste(unlist(columns), collapse=', '), 
		"FROM", "tags_antenna"
	)

if (getOption('verbose',FALSE)) print(queries)

dbDropTable("tags_detected")
create_query <- paste0(
	"CREATE TABLE tags_detected AS ",
	"(", select_stmt, ");"
)
dbSendQuery(link$conn, create_query)


