wbConnector<-function(){
  
  #obtain credentials interactively from the user
  cat("\nEnter postgres username")
  usr <- scan(what=character(),nmax=1,quiet=TRUE)
  cat("\nEnter postgres password (not stored in history, console will be cleared upon entry)")
  pass <- scan(what=character(),nmax=1,quiet=TRUE)
  
  credentials<-list(drv="PostgreSQL",
                    host="osensei.cns.umass.edu",
                    user=usr,
                    password=pass,
                    dbname="westbrook")
  con<<-do.call(RPostgreSQL::dbConnect,credentials)
  conDplyr<<-do.call(src_postgres,credentials)
  
  credentials$host="felek.cns.umass.edu"
  credentials$dbname="sheds"
  conSheds<<-do.call(src_postgres,credentials)
  
  cat("\014")
}

#'Reconnect to the West Brook database
#'
#'Tests the connection to the database and runs wbConnect() if the connection has not been established or has expired
#'@return A connection to the West Brook database
#'@export
reconnect<-function(){
  if(!exists("con")){wbConnector()} else {
    if(class(con)!="PostgreSQLConnection"){wbConnector()} else{
      if(class(try(RPostgreSQL::dbGetQuery(con,""),silent=T))=="try-error"){
        wbConnector()
      }
    }
  }
}


