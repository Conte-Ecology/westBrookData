sites<-read_excel(file.path(original_data_dir,"sites.xlsx"),
                  col_types=rep("text",9))

unknowns <- list(-9999, "-9999", "-9999\r\n-9999", "", -999, -99,"-9999.00","999.00",
                 -99999,"9999",9999, "np",-999.9,"-999.9",".9999","-999")

#######WEST BROOK
for(nom in names(sites)){
  sites[[nom]] <- pipeline_string_transformation(string=sites[[nom]],
                                                   pipeline=standard_string_transformations[
                                                     c('drop_leading_whitespace','drop_trailing_whitespace','to_lower_case')])
  sites[[nom]][sites[[nom]] %in% unknowns]<-NA
}
sites<-sites[,-6]

nameMap<-c(drainage="Drainage",
           river="River",
           area="Area",
           section="Section",
           quarter="Quarter",
           quarter_length = "Quart Length",
           river_meter = "River KM",
           confluence_river_meter = "Confl River KM")

names(sites)<-names(nameMap[match(names(sites),nameMap)])

dbWriteTable(con, 'raw_sites', sites, row.names=FALSE,
             overwrite=TRUE, append=FALSE)
