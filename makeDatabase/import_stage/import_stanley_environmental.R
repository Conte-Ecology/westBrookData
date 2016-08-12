stanleyEnv<-fread(file.path(original_data_dir,"stanley_environmental.txt")) %>%
                    setnames(c("Date/Time","Temperature","Salinity","Logger"),
                             c("datetime","temperature","salinity","logger")) %>%
                    .[,.(datetime,temperature,salinity,logger)] %>%
                    .[,section:=c(1,2,11)[match(logger,c("sec1CTD","sec 2","sec 11"))]] %>%
                    .[,datetime:=parse_date_time(datetime,orders=date.format)] %>%
                    .[section!=11] %>%
                    setkey(section,datetime)


stanleyDepth<-suppressWarnings(fread(file.path(original_data_dir,"stanleyDepthTemperature.csv"))) %>%
  setnames(c("date","time","depth","temperature","comment","datetime")) %>%
  .[,section:=11] %>%
  .[,datetime:=as.POSIXct(datetime)] %>%
  .[,.(section,datetime,temperature,depth)] %>%
  setkey(section,datetime)


  
stanleyEnv<-bind_rows(stanleyEnv,stanleyDepth) %>%
            data.frame()

dbWriteTable(con, 'stanley_environmental', stanleyEnv, row.names=FALSE,
             overwrite=TRUE, append=FALSE)