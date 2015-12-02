source_data <- readRDS(
	file=file.path(processed_data_dir,'section_table.rds')
)

dbWriteTable(link$conn, 'data_locations', source_data, row.names=FALSE,
						 overwrite=TRUE, append=FALSE)



