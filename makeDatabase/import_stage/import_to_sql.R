files<-c(tag_data_names,"yoy_bins",stanley_data_names)
imports<-tbl(conDplyr,"import_dates") %>% collect() %>% data.frame

for(name in files){
  if(!importAll){
    if(imports[imports$file==name,"date"]>
       file.info(csv_files[name])$mtime){next}
  }
  
	data <- safe_read_csv(file=csv_files[name], instructions=standardize_files[name])

	for ( nom in names(data)) {
		data[[nom]] <- pipeline_string_transformation(string=data[[nom]],
				pipeline=standard_string_transformations[
					c('drop_leading_whitespace','drop_trailing_whitespace','to_lower_case')]
		)
	}
	if(name=="stanley_tags"){data$survey<-"shock"}
	if(name=="stanley_fyke_net"){data$survey<-"fyke net"}

	write.csv(x=data, file=file.path(processed_data_dir,paste0(name,'.csv')), row.names=FALSE)
	dbWriteTable(conn=con, name=paste0("raw_",name), value=data,row.names=FALSE,
							 overwrite=TRUE, append=FALSE)
	
	#update the import dates
	imports[imports$file==name,"date"]<-Sys.time()

}

#write the updated import dates
dbWriteTable(conn=con, "import_dates", value=imports,row.names=FALSE,
             overwrite=TRUE, append=FALSE)

