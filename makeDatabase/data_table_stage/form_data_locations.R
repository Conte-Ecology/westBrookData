source_data <- readRDS(
	file=file.path(original_data_dir,'section_table.rds')
)

dbWriteTable(con, 'data_locations', source_data, row.names=FALSE,
						 overwrite=TRUE, append=FALSE)



