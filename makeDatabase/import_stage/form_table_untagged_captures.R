columns <- list(
  who = c('fish_number'),
  when = c('date','sample_name'),
  where = c('river','area','section'),
  how = c('sample_type','survey'),
  stable_trait = c('species','cohort'),
  mutable_trait = c('measured_length','measured_weight')
)

select_stmt<-paste(
  "SELECT", paste(unlist(columns), collapse=', '), "FROM ",
    "raw_captures WHERE tag IS NULL"
)

create_query <- paste0(
  "CREATE TABLE untagged_captures AS ",
  "(", select_stmt, ");"
)

dbDropTable('untagged_captures')

dbSendQuery(con, create_query)