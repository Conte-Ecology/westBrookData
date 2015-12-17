columns <- list(
  who = c('fish_number'),
  when = c('date','sample_name'),
  where = c('river','area','section'),
  how = c('sample_type','survey'),
  stable_trait = c('species','cohort'),
  mutable_trait = c('measured_length','measured_weight')
)

untagged_captures<-data.table(dbGetQuery(con,"SELECT * FROM raw_captures WHERE tag IS NULL"))
untagged_captures[,sample_name:=as.character(as.numeric(sample_name))]

dbWriteTable(conn=con, name='untagged_captures',value=data.frame(untagged_captures),
             overwrite=TRUE, row.names=FALSE)