data <- safe_read_table(file=file.path(original_data_dir,"families.txt"),
                      instructions=file.path(standardizeFilesDir,"family_standardize.R"))

for ( nom in names(data)) {
  data[[nom]] <- pipeline_string_transformation(string=data[[nom]], 
                                                pipeline=standard_string_transformations[
                                                  c('drop_leading_whitespace','drop_trailing_whitespace','to_lower_case')]
  )
  if(nom=="vial_number") next 
  data[[nom]]<- gsub("[.]"," ",data[[nom]])
} 

write.csv(x=data, file=file.path(processed_data_dir,paste0("family",'.csv')), row.names=FALSE)
dbWriteTable(conn=con, name=paste0("raw_","family"), value=data,row.names=FALSE,
             overwrite=TRUE, append=FALSE)


