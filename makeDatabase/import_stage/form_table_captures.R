columns <- list(
	who = c('tag','fish_number'),
	when = c('date','sample_name'),
	where = c('river','area','section'),
	how = c('sample_type','survey'),
	stable_trait = c('species','cohort'),
	mutable_trait = c('measured_length','measured_weight')
)

captures_stub <- paste(
		"SELECT", paste(unlist(columns), collapse=', '), "FROM"
	)

electrofishing_samples <- c(
	'raw_tags_salmon_wb', 'raw_tags_trout_wb', 'raw_tags_tribs_wb')

queries <- list()
for ( nom in electrofishing_samples ) {
	queries[[nom]] <- paste(captures_stub, nom)
}

if (getOption('verbose',FALSE)) print(queries)

dbDropTable("raw_captures")
create_query <- paste0(
	"CREATE TABLE raw_captures AS ",
	"(", queries[[1]], ");"
)
dbSendQuery(con, create_query)

for (query in queries[2:length(queries)]) {
	insert_query <- paste0(
		"INSERT INTO raw_captures",
		"(", query, ");"
	)
	dbSendQuery(con, insert_query)
}

tags_captures<-data.table(dbGetQuery(con,"SELECT * FROM raw_captures WHERE tag IS NOT NULL"))
tags_captures[,sample_name:=as.character(as.numeric(sample_name))]

dbWriteTable(conn=con, name='tags_captures',value=data.frame(tags_captures),
             overwrite=TRUE, row.names=FALSE)
