dailyTemps<-tbl(conDplyr,'data_hourly_temps') %>%
            filter(river %in% c("jimmy","mitchell","obear","west brook")) %>%
            collect() %>%
            mutate(date=as.Date(datetime)) %>%
            group_by(river,date) %>%
            summarize(mean_daily_temp=mean(temperature)) %>%
            ungroup()

dbDropTable("data_daily_temps")
dbWriteTable(con,"data_daily_temps",data.frame(dailyTemps),row.names=F)



