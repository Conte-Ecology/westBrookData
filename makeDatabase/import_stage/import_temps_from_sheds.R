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

dbGetQuery(shedsCon,SELECT * FROM information_schema.column WHERE TABLE_NAME='dataset')
