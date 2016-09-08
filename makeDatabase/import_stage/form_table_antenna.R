columns <- list(
	who = c('tag'),
	when = c('earliest_detection_date_time','arrival','departure'),
	where = c('drainage','river','area','section','quarter','left_or_right','distance_upriver_m'),
	how = c('reader_id','sample_type','sample_name','survey','pass'),
	what = c('alive_or_dead','antenna_instance','comment','habitat','cover','justification')
)

select_stmt <- paste(
  "SELECT", paste(unlist(columns), collapse=', '), 
  "FROM", "raw_tags_antenna"
)

wb<-dbGetQuery(con,select_stmt)
stanley<-tbl(conDplyr,"raw_stanley_antenna") %>%
         filter(!is.na(tag)) %>%
         collect(n=Inf)

tagsAntenna<-bind_rows(wb,stanley)[,unlist(columns)] %>% data.frame()

dbWriteTable(con, 'tags_antenna', tagsAntenna, row.names=FALSE,
             overwrite=TRUE, append=FALSE)

