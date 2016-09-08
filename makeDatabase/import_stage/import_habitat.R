habitat<-read_excel(file.path(original_data_dir,"Habitat West Brook.xlsx"),
                    sheet=6,col_types=rep("text",9))

stanleyHabitat<-read_excel(file.path(original_data_dir,"stanleyWetWidths.xls"),
                           col_types=rep("text",10))

unknowns <- list(-9999, "-9999", "-9999\r\n-9999", "", -999, -99,"-9999.00","999.00",
                 -99999,"9999",9999, "np",-999.9,"-999.9",".9999","-999")

#######WEST BROOK
for(nom in names(habitat)){
habitat[[nom]] <- pipeline_string_transformation(string=habitat[[nom]],
                                              pipeline=standard_string_transformations[
                                                c('drop_leading_whitespace','drop_trailing_whitespace','to_lower_case')])
habitat[[nom]][habitat[[nom]] %in% unknowns]<-NA
}

nameMap<-c(source="Source",
           drainage="Drainage",
           river="River",
           sample_name="Sample Num",
           area="Area",
           section="Section",
           width_at_bottom = "Section Width",
           width = "Average Section Width",
           comment="Comment")

names(habitat)<-names(nameMap[match(names(habitat),nameMap)])
habitat<-habitat[habitat$section !="braid"&!is.na(habitat$section),]
dbWriteTable(con, 'raw_habitat_west_brook', habitat, row.names=FALSE,
             overwrite=TRUE, append=FALSE)

############STANLEY
for(nom in names(stanleyHabitat)){
  stanleyHabitat[[nom]] <- pipeline_string_transformation(string=stanleyHabitat[[nom]],
                                                       pipeline=standard_string_transformations[
                                                         c('drop_leading_whitespace','drop_trailing_whitespace','to_lower_case')])
  stanleyHabitat[[nom]][stanleyHabitat[[nom]] %in% unknowns]<-NA
}

stanleyHabitat<-stanleyHabitat[,-c(6,7)]

nameMap<-c(id="ID",
           river="Branch",
           sample_name="sample",
           section="Section",
           width="Width (m)",
           maximum_depth="Deepest Point (m)",
           distance_of_max_depth="Distance at Depth (m)",
           comment="comment")

names(stanleyHabitat)<-names(nameMap[match(names(stanleyHabitat),nameMap)])
stanleyHabitat$drainage="stanley"
dbWriteTable(con, 'raw_habitat_stanley', stanleyHabitat, row.names=FALSE,
             overwrite=TRUE, append=FALSE)


