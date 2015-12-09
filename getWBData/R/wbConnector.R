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
  
  credentials<-list(port=5432,
                    host="",
                    user=usr,
                    password=pass,
                    dbname="westbrook")
  credentialPath<-file.path("~/temp.rds")
  saveRDS(credentials,credentialPath)
  #create a database connection with the credentials
  link<-integrator::db_connector(credentialPath)
  file.remove(credentialPath)
  
  cat("\014") #clear console
  
  link<<-link
}

#'Reconnect to the West Brook database
#'
#'Tests the connection to the database and runs wbConnect() if the connection has not been established or has expired
reconnect<-function(){
  if(!exists("link")){wbConnector()} else {
    if(class(link)!="db_connector"){wbConnector()} else{
      if(class(try(dbGetQuery(link$conn,""),silent=T))=="try-error"){
        wbConnector()
      }
    }
  }
}


