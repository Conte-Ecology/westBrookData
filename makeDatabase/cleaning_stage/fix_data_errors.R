tag_history <- data.table(dbGetQuery(con, statement = "SELECT * FROM data_tagged_captures;"))

drop_these <- vector(mode='numeric', length=0)
could_not_find <- list()
for (e in errors) {
	match_these <- names(e)[names(e) != '#']
	matches <- sapply(
		X=match_these, 
		FUN=function(name,history,error) {
			sapply(history[[name]],identical,y=error[[name]])
		},
		history=tag_history, error=e
	)
	fix_this_row <- which(apply(matches,1,all))
	if (length(fix_this_row) == 0) {
		could_not_find <- c(could_not_find,e)	
	} else {
		actions <- names(e[['#']])
		if (length(actions) == 1 && actions == 'DROP' && e[['#']][['DROP']]) {
			drop_these <- c(drop_these,fix_this_row)
		} else {
			for (fix_this_column in actions) {
				cat("x: ", fix_this_row, ", y: ", fix_this_column, ", ")
				if (identical(tag_history[fix_this_row, fix_this_column], e[['#']][[fix_this_column]])) {
					cat(fix_this_column, ": (already) ", e[['#']][[fix_this_column]], "\n") 
				} else {
					tag_history[fix_this_row, fix_this_column] <- e[['#']][[fix_this_column]]
					cat(fix_this_column, ": ", e[['#']][[fix_this_column]], "\n")
				}
			}
		}
	}
}

if (length(drop_these) != 0) {
	tag_history <- tag_history[-drop_these,]
} else {
	cat("No records to drop.\n")
}

split_tag_history <- split(x=tag_history, f=tag_history[['tag']])

split_tag_history <- mclapply(
	X=split_tag_history,
	FUN=function(history) {
		originals <- !duplicated(history[['detection_date']])
		history <- history[originals,]
		return(history)
	},
	mc.cores=getOption("mc.cores",6)
)

tag_history <- do.call(what=rbind, args=split_tag_history)

dbDropTable(con,'data_tagged_captures')
dbWriteTable(con, 'data_tagged_captures', tag_history, 
						 row.names=FALSE, overwrite=TRUE, append=FALSE)


