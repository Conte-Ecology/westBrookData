columns <- list(
	who = c('tag','fish_number'),
	when = c('date','sample_name'),
	where = c('river','area','section'),
	how = c('sample_type','survey','comments'),
	stable_trait = c('species','cohort','sex'),
	mutable_trait = c('measured_length','measured_weight','maturity')
)

captures_stub <- paste(
		"SELECT", paste(unlist(columns), collapse=', '), "FROM"
	)

electrofishing_samples <- c(
	'raw_tags_salmon_wb', 'raw_tags_trout_wb', 'raw_tags_tribs_wb')

queries <- list()
for ( nom in electrofishing_samples ) {
	queries[[nom]] <- paste(captures_stub, nom)
}

if (getOption('verbose',FALSE)) print(queries)

dbDropTable("raw_captures")

rowsToExclude<-c("no bn","no bk","no fish","no trout","no ats","nob",
                 "no  ats","no  bkt","no p2","skunked","dry","no water")

rawCaptures<-NULL
for(nom in electrofishing_samples){
rawCaptures<-tbl(conDplyr,nom) %>%
             select(one_of(unlist(columns))) %>%
             collect() %>%
             filter(!grepl(paste(rowsToExclude,collapse="|"),comments)) %>%
             filter(species!="nobntp2") %>%
             filter(!is.na(tag)|
                      !is.na(measured_weight)|
                      !is.na(measured_length)|
                      (!is.na(comments) & !grepl(" log",comments))|
                      as.numeric(sample_name)<=16) %>%
             bind_rows(rawCaptures)
}

dbWriteTable(con,"raw_captures",data.frame(rawCaptures),
             row.names=F,overwrite=T)

tags_captures<-data.table(dbGetQuery(con,"SELECT * FROM raw_captures WHERE tag IS NOT NULL"))
tags_captures[,sample_name:=as.character(as.numeric(sample_name))]

dbWriteTable(conn=con, name='tags_captures',value=data.frame(tags_captures),
             overwrite=TRUE, row.names=FALSE)
