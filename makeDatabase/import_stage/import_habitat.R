habitat<-read_excel(file.path(original_data_dir,"Habitat West Brook.xlsx"),
                    sheet=1,skip=4,col_types=rep("text",14))
habitat<-habitat[,-c(13,14)]

tribHabitat<-read_excel(file.path(original_data_dir,"Habitat West Brook.xlsx"),
                    sheet=2,col_types=rep("text",13))
tribHabitat<-tribHabitat[,-13]

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
           quarter="Location",
           habitat_type = "Hab Type",
           width = "Quart Width",
           cover_type = "Quart Cover Type",
           percent_cover = "Quart Perc Cover",
           comment="Comment")

names(habitat)<-names(nameMap[match(names(habitat),nameMap)])
habitat<-habitat[habitat$section !="braid"&!is.na(habitat$section),]

####TRIB HABITAT
for(nom in names(tribHabitat)){
  tribHabitat[[nom]] <- pipeline_string_transformation(string=tribHabitat[[nom]],
                                                   pipeline=standard_string_transformations[
                                                     c('drop_leading_whitespace','drop_trailing_whitespace','to_lower_case')])
  tribHabitat[[nom]][tribHabitat[[nom]] %in% unknowns]<-NA
}

nameMap<-c(source="Source",
           drainage="Drainage",
           river="River",
           sample_name="Sample Num",
           area="Area",
           section="Section",
           quarter="Quarter",
           habitat_type="Hab Type",
           width = "Quart Width",
           cover_type = "Quart Cover Type",
           percent_cover = "Quart Perc Cover",
           comment="Comment")

names(tribHabitat)<-names(nameMap[match(names(tribHabitat),nameMap)])

habitat<-bind_rows(habitat,tribHabitat)

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


