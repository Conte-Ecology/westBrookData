##only daily means in SHEDS for now, should be pulling raw

shedsConnector<-function(){
  
  #obtain credentials interactively from the user
  cat("\nEnter postgres username")
  usr <- scan(what=character(),nmax=1,quiet=TRUE)
  cat("\nEnter postgres password (not stored in history, console will be cleared upon entry)")
  pass <- scan(what=character(),nmax=1,quiet=TRUE)
  
  credentials<-list(drv="PostgreSQL",
                    host="felek.cns.umass.edu",
                    user=usr,
                    password=pass,
                    dbname="sheds")
  assign('shedsCon',do.call(RPostgreSQL::dbConnect,credentials),env=parent.frame())
  cat("\014")
}

shedsConnector()

dbGetQuery(shedsCon,"SELECT * FROM information_schema.columns WHERE TABLE_NAME='dataset'")
bla<-data.table(dbGetQuery(shedsCon,"SELECT * FROM dataset"))

locations<-c("West Brook 6","West Brook 30","West Brook 45","West Brook B100",
"Mitchell Brook sec 1","Mitchell Brook below waterfall",
"O'Bear sec 1","O'Bear Brook sec 15",
"Jimmy Nolan Brook","Jimmy Nolan Brook sec 15")

