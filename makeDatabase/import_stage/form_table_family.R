columns <- list(
  who = c('fish_number','tag','vial_number'),
  when = c('sample_name'),
  where = c('river','area','section'),
  stable_trait = c('species','cohort','family_id'),
  mutable_trait = c('observed_length','sex')
)

query<-paste(
  "SELECT", paste(unlist(columns), collapse=', '), "FROM raw_family"
    )

family<-data.table(dbGetQuery(con,query))
family[,sample_name:=as.character(as.numeric(sample_name))]

dbWriteTable(conn=con, name='family',value=data.frame(family),
             overwrite=TRUE, row.names=FALSE)