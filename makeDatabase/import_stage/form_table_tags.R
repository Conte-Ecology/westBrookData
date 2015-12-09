columns <- list(
	who = c('tag')
)

captures_stub <- paste(
		"SELECT", paste(unlist(columns), collapse=', '), "FROM"
	)

queries <- list()
for ( nom in c('tags_captures','tags_dead','tags_antenna') ) {
	queries[[nom]] <- paste(captures_stub, nom, "WHERE tag IS NOT NULL")
}

if (getOption('verbose',FALSE)) print(queries)

tags <- data.frame(tag=NULL, species=NULL)
for (query in queries) {
	tags <- rbind(tags,dbGetQuery(link$conn, paste0(query,";")))
	tags <- unique(tags)
}
tags[['tag_number']] <- as.numeric(factor(x=tags[['tag']]))

dbDropTable("tags")
dbWriteTable(link$conn, 'tags', tags, row.names=FALSE)

# get_tags <- function(link) {
# 	tags <- dbGetQuery(link$conn, "SELECT * FROM tags ORDER BY tag_number;")
# 	tags_f <- factor(x=tags$tag_number, labels=tags$tag)
# 	return(tags_f)
# }
# 
# tag_to_tag_number <- function(tags, link) {
# 	tag_map <- get_tags(link)	
# 	tag_mapping <- sapply(tags, function(tag) which(tag_map == tag))
# 	tags <- as.numeric(tag_map[tag_mapping])
# 	return(tags)
# }

