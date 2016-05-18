captures <- dbReadTable(con,'data_tagged_captures')

trap_captures <- captures[
	captures[['survey']] %in% c('box trap','screw trap','duda fyke','fyke net')
,]

trap_captures <- unique(trap_captures[
	order(trap_captures[['species']],
				trap_captures[['tag']],
				trap_captures[['detection_date']]),])

## Trap captures typically have no date/time but they are the last
## time a fish might be seen so we give them a final time of day:

hour(trap_captures[['detection_date']]) <- 23
minute(trap_captures[['detection_date']]) <- 59
second(trap_captures[['detection_date']]) <- 59

dbWriteTable(con, 'data_trap_captures', trap_captures,
						 row.names=FALSE, overwrite=TRUE, append=FALSE)




