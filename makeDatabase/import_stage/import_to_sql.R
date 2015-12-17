for (name in c(tag_data_names,"yoy_bins")) {
	data <- safe_read_csv(file=csv_files[name], instructions=standardize_files[name])
  
	for ( nom in names(data)) {
		data[[nom]] <- pipeline_string_transformation(string=data[[nom]], 
				pipeline=standard_string_transformations[
					c('drop_leading_whitespace','drop_trailing_whitespace','to_lower_case')]
		)
	} 

	write.csv(x=data, file=file.path(processed_data_dir,paste0(name,'.csv')), row.names=FALSE)
	dbWriteTable(conn=con, name=paste0("raw_",name), value=data,row.names=FALSE,
							 overwrite=TRUE, append=FALSE)

}





