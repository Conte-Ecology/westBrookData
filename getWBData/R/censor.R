#'Remove rows for individuals that are known to have emigrated or died
#'
#'Currently incomplete, the samples to censor are identified, but not yet censored
#'@param cmrData A data.frame created using createCmrData
#'@param emigrated Logical indicated whether to censor individuals that emigrated permanently
#'@param dead Logical indicating whether to censore individuals that are known dead (see also createKnownState)
#'@returns A data.frame without the rows
#'
censor<-function(cmrData,emigrated=T,dead=F,beforeObs=T){
  reconnect()
  

  columns<-"tag"
  notNull<-NULL
  if(dead){columns<-c(columns,"date_known_dead")
           notNull<-c(notNull,"date_known_dead IS NOT NULL")
           }
  if(emigrated){columns<-c(columns,"date_emigrated")
                notNull<-c(notNull,"date_emigrated IS NOT NULL")
  }
  if(emigrated|dead){ #censor emigrated and/or dead
    query<-paste("SELECT",
                 paste(columns,collapse=","),
                 "FROM data_by_tag",
                 "WHERE",
                 paste(notNull,collapse=" OR "))
    
    toCensor<-dbGetQuery(con,query)
    dateNames<-(names(toCensor)[2:ncol(toCensor)])
    expr<-paste0("as.POSIXct(min(",paste(dateNames,collapse=","),",na.rm=T))")
    toCensor<-toCensor %>% group_by(tag) %>% transmute_(censorDate=expr)
    
    samples<-dbGetQuery(con,"SELECT sample_name,start_date FROM data_seasonal_sampling")
    firstCensoredSample<-function(date){
      samples %>% 
        filter(start_date>date) %>% 
          filter(start_date==min(start_date)) %>% 
            .[["sample_name"]]
    }
    toCensor<-toCensor %>% 
                group_by(tag) %>% 
                  mutate(firstCensoredSample=firstCensoredSample(censorDate))
    
    cmrData<-toCensor %>%
               select(tag,firstCensoredSample) %>%
                 right_join(cmrData,by="tag") %>%
                   filter(sampleNumber<firstCensoredSample|is.na(firstCensoredSample)) %>%
                     select(-firstCensoredSample)
  }#end emigrated or dead section
  
  if(beforeObs){ #remove occasions prior to the first capture
    cmrData<-cmrData %>%
               group_by(tag) %>%
                 mutate(firstObs=sampleNumber[min(which(enc==1))]) %>%
                   filter(sampleNumber>=firstObs) %>%
                     select(-firstObs)
  }
  
  return(cmrData)
}