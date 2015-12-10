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
  link<<-list()#for now just maintaining compatibility with old structure
  link$conn<<-do.call(dbConnect,credentials)
  cat("\014")
}