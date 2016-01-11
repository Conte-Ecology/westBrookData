keep<-c("tag","date_known_dead","sampling_mortality")

dead<-dbGetQuery(con,"SELECT * FROM tags_dead")

dead$sampling_mortality<-grepl("sampl",tolower(dead$justification))
dead<-dead[,keep]