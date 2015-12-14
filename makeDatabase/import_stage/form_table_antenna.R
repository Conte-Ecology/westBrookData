columns <- list(
	who = c('tag'),
	when = c('earliest_detection_date_time','arrival','departure'),
	where = c('river','area','section','quarter','left_or_right'),
	how = c('reader_id','sample_type','sample_name','survey','pass'),
	what = c('alive_or_dead','antenna_instance','comment','habitat','cover','justification')
)

select_stmt <- paste(
		"SELECT", paste(unlist(columns), collapse=', '), 
		"FROM", "raw_tags_antenna"
	)

if (getOption('verbose',FALSE)) print(queries)

dbDropTable("tags_antenna")
create_query <- paste0(
	"CREATE TABLE tags_antenna AS ",
	"(", select_stmt, ");"
)
dbSendQuery(con, create_query)


