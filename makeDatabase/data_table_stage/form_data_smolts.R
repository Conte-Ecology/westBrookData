trap_captures <-  data.table(dbGetQuery(con, 
	"SELECT * FROM data_trap_captures WHERE species = 'ats';"))

smolts<-trap_captures[,lastDate:=max(detection_date),by=tag]
smolts<-smolts[detection_date==lastDate,list(tag,detection_date,observed_length,survey)]

dbWriteTable(con,'data_smolts',smolts, overwrite=TRUE, append=FALSE, row.names=FALSE)