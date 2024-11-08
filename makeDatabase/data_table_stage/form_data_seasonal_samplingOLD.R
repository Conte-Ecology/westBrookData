captures <- dbReadTable(con,'data_captures')
sampling <- dbReadTable(con, 'data_seasonal_sampling')

seasonal_captures <- captures[ 
	captures[['sample_number']] %in%
	sampling[sampling[['seasonal']],'sample_number']
,]

seasonal_captures <- seasonal_captures[
	order(seasonal_captures[['species']],
				seasonal_captures[['tag']],
				seasonal_captures[['detection_date']]),]

pl <- ggplot(
	data=seasonal_captures, 
	aes(x=sample_number, y=as.numeric(as.factor(tag))  )
) + geom_raster()

dbWriteTable(con, 'data_seasonal_captures', seasonal_captures, row.names=FALSE,
						 overwrite=TRUE, append=FALSE)


