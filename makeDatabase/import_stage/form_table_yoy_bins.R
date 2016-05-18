columns <- list(
  who = c('age','species'),
  when = c('sample'),
  where = c('drainage','river'),
  what = c('cohort_min_length','cohort_max_length','cohort')
)

select_stmt <- paste(
  "(SELECT", paste(unlist(columns), collapse=', '), 
  "FROM", "raw_yoy_bins)",
  "UNION",
  "(SELECT", paste(unlist(columns), collapse=', '), 
  "FROM", "raw_stanley_cohorts)"
)

if (getOption('verbose',FALSE)) print(queries)

dbDropTable("yoy_bins")
create_query <- paste0(
  "CREATE TABLE yoy_bins AS ",
  "(", select_stmt, ");"
)
dbSendQuery(con, create_query)