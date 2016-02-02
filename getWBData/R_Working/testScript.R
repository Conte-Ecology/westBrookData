library(getWBData)
coreData<-createCoreData(sampleType="electrofishing",
                         columnsToAdd=c("sampleNumber","river",
                                        'observedLength','observedWeight')) %>% 
            addTagProperties() %>%
              dplyr::filter(species=="bkt") %>%
                createCmrData(maxAgeInSamples=20) %>%
                    addSampleProperties() %>%
                      addEnvironmental() %>%
                        addKnownZ()

jagsData <- createJagsData(coreData)


coreData<-createCoreData(sampleType="electrofishing",
                         columnsToAdd=c("sampleNumber","river",
                                        'observedLength','observedWeight'))
coreData<-addTagProperties(coreData,columnsToAdd=c("sex","cohort","species"))
coreData<-dplyr::filter(coreData,species == "bkt")
coreData<-createCmrData(coreData,maxAgeInSamples=20)
# coreData<-censor(coreData)
coreData<-addSampleProperties(coreData)
coreData<-addEnvironmental(coreData)
coreData<-addKnownZ(coreData)
#jagsData<-createJAGSData(coreData)
