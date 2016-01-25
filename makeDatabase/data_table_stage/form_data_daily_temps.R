dailyTemps<-tbl(conDplyr,'data_hourly_temperature') %>%
            filter(river %in% c("jimmy","mitchell","obear","west brook")) %>%
            collect() %>%
            mutate(date=as.Date(datetime)) %>%
            group_by(river,date) %>%
            summarize(mean_daily_temp=mean(temperature)) %>%
            ungroup()

dbDropTable("data_daily_temperature")
dbWriteTable(con,"data_daily_temperature",data.frame(dailyTemps),row.names=F)



