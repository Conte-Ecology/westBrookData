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
  credentialPath<-file.path(westbrookDir,"temp.rds")
  saveRDS(credentials,credentialPath)
  #create a database connection with the credentials
  link<-db_connector(credentialPath)
  file.remove(credentialPath)
  cat("\014")
  return(link)
}