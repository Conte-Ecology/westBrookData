tag_history <- data.table(dbGetQuery(con, statement = "SELECT * FROM data_tagged_captures;"))
setkey(tag_history,tag,fish_number,sample_name,section)

dropThese <- vector(mode='numeric', length=0)
couldNotFind <- list()

fixErrors<-function(e){
  fixThis<-e$fixThis
  e<-e[names(e)!="fixThis"]

  if(suppressWarnings(any(key(tag_history)!=names(e)))){
    suppressWarnings(key(tag_history)<-names(e))}
	
	fixThisRow<-tag_history[e,,which=T]
	
	if (is.na(fixThisRow)) {
		couldNotFind[[length(couldNotFind)+1]]<<-e	
	} else {
	
		actions <- names(fixThis)
		if (length(actions) == 1 && actions == 'DROP') {
		  cat("drop\n")
			dropThese <<- c(dropThese,fixThisRow)
		} else {
			for (fixThisColumn in actions) {
				if (identical(tag_history[fixThisRow, get(fixThisColumn)], fixThis[[fixThisColumn]])) {
					cat(fixThisColumn, ": (already) ", fixThis[[fixThisColumn]], "\n") 
				} else {
					set(tag_history,fixThisRow,which(fixThisColumn==names(tag_history)),
					    fixThis[[fixThisColumn]])
					cat(fixThisColumn, ": ", fixThis[[fixThisColumn]], "\n")
				}
			}
		}
	}
}

for(e in errors){
  fixErrors(e)
}

if (length(dropThese) != 0) {
	tag_history <- tag_history[-dropThese]
} else {
	cat("No records to drop.\n")
}

dbDropTable('data_tagged_captures')
dbWriteTable(con, 'data_tagged_captures', tag_history, 
						 row.names=FALSE, overwrite=TRUE, append=FALSE)