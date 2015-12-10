tag_history <- dbGetQuery(con, statement = "SELECT * FROM data_tag_history;")

split_tag_history <- split(x=tag_history, f=tag_history[['tag']], drop=FALSE)


## This function is spaghetti b/c little can be assumed about tag
## identity or whether length is measured.

split_tag <- mclapply(
	X=split_tag_history,
	FUN=function(tag_history) {
		tag_history <- tag_history[order(tag_history[['detection_date']]),]
		subtag <- 1
		tag_history[['subtag']] <- NA
		tag_history[['subtag']][1] <- subtag
		if (!is.na(tag_history[1,'species'])) {
			tag_species <- tag_history[1,'species']
		} else {
			some_species <- tag_history[,'species']
			some_species <- some_species[!is.na(some_species)]
			tag_species <- some_species[1]
			tag_history[1,'species'] <- tag_species
		}
		if (!is.na(tag_history[1,'observed_length'])) {
			tag_length <- tag_history[1,'observed_length']
			tag_which <- 1
		} else {
			some_lengths <- tag_history[,'observed_length']
			if (!all(is.na(some_lengths))) {
				tag_length <- min(some_lengths,na.rm=TRUE)
				tag_which <- which(tag_history[,'observed_length'] == tag_length)
			} else {
				tag_length <- NA
			}
		}
		if (nrow(tag_history) > 1) {
			for ( i in 2:nrow(tag_history)) {
				bump_subtag <- FALSE
				if (!is.na(tag_history[i,'species'])) {
					new_species <- tag_history[i,'species']
					if (new_species != tag_species) {
						tag_species <- new_species
						bump_subtag <- TRUE
					}
				} else {
					tag_history[i,'species'] <- tag_species
				}
				if (!is.na(tag_history[i,'observed_length'])) {
					new_length <- tag_history[i,'observed_length']
					interval <- days(round(tag_history[i,'detection_date'] -
																 tag_history[tag_which,'detection_date']))
					if (!is.na(tag_length) && ((new_length - tag_length) < -10) ) {
						tag_length <- new_length
						tag_which <- which(tag_history[,'observed_length'] == tag_length)
						bump_subtag <- TRUE
					}
					if (
						!is.na(tag_length) && 
						( (new_length - tag_length) >  60) &&
						(interval < days(round(365/4)))
					) {
						tag_length <- new_length
						tag_which <- which(tag_history[,'observed_length'] == tag_length)
						bump_subtag <- TRUE
					}
				} else {
					# DO NOTHING.
				}
				if (bump_subtag) {
					tag_history[i,'subtag'] <- tag_history[i-1,'subtag'] + 1
				} else {
					tag_history[i,'subtag'] <- tag_history[i-1,'subtag']
				}
				if (!is.na(tag_history[i,'species']))
					tag_species <- tag_history[i,'species']
				if (!is.na(tag_history[i,'observed_length'])) 
					tag_length <- tag_history[i,'observed_length']
			}
		} 
		return(tag_history)
	}
)

tag_history <- do.call(what=rbind, args=split_tag)

dbWriteTable(con, 'data_tag_history', tag_history, row.names=FALSE,
						 overwrite=TRUE, append=FALSE)

