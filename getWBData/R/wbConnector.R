#'Connect to West Brook sql database
#'
#'Creates two links to the West Brook database one for DBI and one for dplyr
#'@return \code{con} A link to the database for use with DBI functions
#'@return \code{conDplyr} A link to the database for use with dplyr functions
wbConnector<-function(){
  #!/usr/bin/r -vi  

  host <- Sys.getenv("WESTBROOK_HOST")
  if (nchar(host) == 0) {
    host<-readline("Enter postgres host: ")
    port<-readline("Enter postgres port: ")
    dbname<-readline("Enter postgres database name: ")
    user<-readline("Enter postgres user: ")
    pass<-readline("Enter postgres password: ")
  } else {
    port <- Sys.getenv("WESTBROOK_PORT")
    dbname <- Sys.getenv("WESTBROOK_DBNAME")
    user <- Sys.getenv("WESTBROOK_USER")
    password <- Sys.getenv("WESTBROOK_PASSWORD")
  }
  
  con <<- DBI::dbConnect(
    drv=RPostgres::Postgres(),
    host=host,
    port=port,
    user=user,
    password=password,
    dbname=dbname
  )
  conDplyr <<- con
  cat("\014")
}

#'Reconnect to the West Brook database
#'
#'Tests the connection to the database and runs wbConnect() if the connection has not been established or has expired
#'@return A connection to the West Brook database
#'@export
reconnect<-function(){
  if(!exists("con")){wbConnector()} else {
    if(class(con)!="PqConnection"){wbConnector()} else{
      if(class(try(DBI::dbGetQuery(con,""),silent=T))=="try-error"|
         any(suppressWarnings(class(try(tbl(conDplyr,"yoy_bins"),silent=T))=="try-error"))){
        wbConnector()
      }
    }
  }
}


