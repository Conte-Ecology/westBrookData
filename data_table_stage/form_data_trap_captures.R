recaptures <- dbReadTable(link$conn,'data_recaptures')

trap_recaptures <- recaptures[
	recaptures[['survey']] %in% c('box trap','screw trap','duda fyke')
,]

trap_recaptures <- unique(trap_recaptures[
	order(trap_recaptures[['species']],
				trap_recaptures[['tag']],
				trap_recaptures[['detection_date']]),])

## Trap recaptures typically have no date/time but they are the last
## time a fish might be seen so we give them a final time of day:

hour(trap_recaptures[['detection_date']]) <- 23
minute(trap_recaptures[['detection_date']]) <- 59
second(trap_recaptures[['detection_date']]) <- 59

dbWriteTable(link$conn, 'data_trap_recaptures', trap_recaptures,
						 row.names=FALSE, overwrite=TRUE, append=FALSE)




