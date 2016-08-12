family<-dbGetQuery(con,"SELECT * FROM family") %>% data.table()
family[,observed_length:=as.numeric(observed_length)]
multipleFamilies<-family[!is.na(tag),.N,by=tag] %>%
                  .[N>1]
if(nrow(multipleFamilies)>0){
  warning(paste("tag numbers",
              paste(multipleFamilies$tag,collapse=", "),
              "have multiple family assignments"))
}

dbWriteTable(con, 'data_family', family, row.names=FALSE,
             overwrite=TRUE, append=FALSE)

