library(getWBData)
coreData<-createCoreData() %>% 
            addTagProperties() %>%
              # dplyr::filter(species=="bkt") %>%
                createCmrData() %>%
                    addSampleProperties() %>%
                      addEnvironmental() %>%
                        addKnownZ()

jagsData <- createJagsData(coreData)


coreData<-createCoreData()
coreData<-addTagProperties(coreData)
coreData<-dplyr::filter(coreData,species %in% c("bkt","bnt","ats"))
coreData<-dplyr::filter(coreData,species %in% c("bkt"))
coreData<-createCmrData(coreData)
coreData<-addSampleProperties(coreData)
coreData<-addEnvironmental(coreData)
coreData<-addKnownZ(coreData)
jagsData<-createJagsData(coreData)
