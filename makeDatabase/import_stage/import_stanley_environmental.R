stanleyEnv<-fread(file.path(original_data_dir,"stanley_environmental.txt")) %>%
                    setnames(c("Date/Time","Temperature","Salinity","Logger"),
                             c("datetime","temperature","salinity","logger")) %>%
                    .[,.(datetime,temperature,salinity,logger)] %>%
                    .[,section:=c(1,2,11)[match(logger,c("sec1CTD","sec 2","sec 11"))]] %>%
                    .[,datetime:=parse_date_time(datetime,orders=date.format)]


dbWriteTable(con, 'stanley_environmental', stanleyEnv, row.names=FALSE,
             overwrite=TRUE, append=FALSE)