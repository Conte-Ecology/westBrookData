tag_history <- data.table(dbGetQuery(con, statement = "SELECT * FROM data_tagged_captures;"))
setkey(tag_history,tag,fish_number,sample_name,section)

dropThese <- vector(mode='numeric', length=0)
couldNotFind <- list()

fixErrors<-function(e,data){
  fixThis<-e$fixThis
  e<-e[names(e)!="fixThis"]

  if(suppressWarnings(any(key(data)!=names(e)))){
    suppressWarnings(key(data)<-names(e))}
	
	fixThisRow<-data[e,,which=T]
	
	suppressWarnings(if(is.na(fixThisRow)) {
		couldNotFind[[length(couldNotFind)+1]]<<-e	
	}) else {
	
		actions <- names(fixThis)
		if (length(actions) == 1 && actions == 'DROP') {
		  cat("drop\n")
			dropThese <<- c(dropThese,fixThisRow)
		} else {
			for (fixThisColumn in actions) {
				if (identical(data[fixThisRow, get(fixThisColumn)], fixThis[[fixThisColumn]])) {
					cat(fixThisColumn, ": (already) ", fixThis[[fixThisColumn]], "\n") 
				} else {
					set(data,fixThisRow,which(fixThisColumn==names(data)),
					    fixThis[[fixThisColumn]])
					cat(fixThisColumn, ": ", fixThis[[fixThisColumn]], "\n")
				}
			}
		}
	}
}

for(e in errors){
  fixErrors(e,tag_history)
}

portableAntenna <- data.table(dbGetQuery(con, statement = "SELECT * FROM data_portable_antenna;"))
stationaryAntenna <- data.table(dbGetQuery(con, statement = "SELECT * FROM data_stationary_antenna;"))
deadTags<- data.table(dbGetQuery(con, statement = "SELECT * FROM tags_dead;"))
setkey(portableAntenna,tag)
setkey(stationaryAntenna,tag)
setkey(deadTags,tag)

for(e in crossDrainageDuplicates){
  fixErrors(e,tag_history)
  fixErrors(e,portableAntenna)
  fixErrors(e,stationaryAntenna)
}

for(e in deadErrors){
  fixErrors(e,deadTags)
}

if (length(dropThese) != 0) {
	tag_history <- tag_history[-dropThese]
} else {
	cat("No records to drop.\n")
}

dbDropTable('data_tagged_captures')
dbWriteTable(con, 'data_tagged_captures', tag_history, 
						 row.names=FALSE, overwrite=TRUE, append=FALSE)

dbDropTable('data_portable_antenna')
dbWriteTable(con, 'data_portable_antenna', portableAntenna, 
             row.names=FALSE, overwrite=TRUE, append=FALSE)

dbDropTable('data_stationary_antenna')
dbWriteTable(con, 'data_stationary_antenna', stationaryAntenna, 
             row.names=FALSE, overwrite=TRUE, append=FALSE)

dbDropTable('tags_dead')
dbWriteTable(con, 'tags_dead', deadTags, 
             row.names=FALSE, overwrite=TRUE, append=FALSE)
