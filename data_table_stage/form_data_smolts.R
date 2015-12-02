trap_recaptures <-  dbGetQuery(link$conn, 
	"SELECT * FROM data_trap_recaptures WHERE species = 'ats';")

smolts <- by(
	data=trap_recaptures,
	INDICES=trap_recaptures[,c('tag')],
	FUN=function(dat) {
		last_date <- max(dat[['detection_date']], na.rm=TRUE)
		dat <- dat[dat[['detection_date']] == last_date,,drop=FALSE]
		dat <- dat[1,c('tag','detection_date','observed_length','survey'),drop=FALSE]
		return(dat)
	}
)
smolts <- do.call(what=rbind, args=smolts)

dbWriteTable(link$conn,'data_smolts',smolts, overwrite=TRUE, append=FALSE, row.names=FALSE)


