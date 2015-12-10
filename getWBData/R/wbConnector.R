#'Connect to West Brook sql database
#'
#'Creates a link to the West Brook database
#'@return A link to the database
wbConnector<-function(){
  
  #obtain credentials interactively from the user
  cat("\nEnter postgres username")
  usr <- scan(what=character(),nmax=1,quiet=TRUE)
  cat("\nEnter postgres password (console will be cleared upon entry)")
  pass <- scan(what=character(),nmax=1,quiet=TRUE)
  
  credentials<-list(drv="PostgreSQL",
                    host="osensei.cns.umass.edu",
                    user=usr,
                    password=pass,
                    dbname="westbrook")
  con<<-do.call(dbConnect,credentials)
  cat("\014")
}

#'Reconnect to the West Brook database
#'
#'Tests the connection to the database and runs wbConnect() if the connection has not been established or has expired
#'@returns A connection to the West Brook database
#'@export
reconnect<-function(){
  if(!exists("con")){wbConnector()} else {
    if(class(con)!="PostgreSQLConnection"){wbConnector()} else{
      if(class(try(dbGetQuery(con,""),silent=T))=="try-error"){
        wbConnector()
      }
    }
  }
}


