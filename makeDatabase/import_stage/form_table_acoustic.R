columns <- list(
	who = c('tag','acoustic_tag'),
	when = c('datetime'),
	where = c('section','quarter'),
	how = c('receiver')
)

select_stmt <- paste(
		"SELECT", paste(unlist(columns), collapse=', '), 
		"FROM", "raw_stanley_acoustic_data"
	)

if (getOption('verbose',FALSE)) print(queries)

dbDropTable("tags_acoustic")
create_query <- paste0(
	"CREATE TABLE tags_acoustic AS ",
	"(", select_stmt, ");"
)
dbSendQuery(con, create_query)


