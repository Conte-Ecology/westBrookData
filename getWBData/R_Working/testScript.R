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
cmrData<-createCmrData(coreData)
cmrData<-addSampleProperties(cmrData)
cmrData<-addEnvironmental(cmrData)
cmrData<-addKnownZ(cmrData)
jagsData<-createJagsData(cmrData)
