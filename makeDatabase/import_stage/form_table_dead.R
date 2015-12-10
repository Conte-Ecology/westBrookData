columns <- list(
	who = c('tag'),
	when = c('date_known_dead')
)

select_stmt <- paste(
		"SELECT", paste(unlist(columns), collapse=', '), 
		"FROM", "raw_tags_dead"
	)
dbDropTable("tags_dead")
create_query <- paste0(
	"CREATE TABLE tags_dead AS ",
	"(", select_stmt, ");"
)
dbSendQuery(con, create_query)