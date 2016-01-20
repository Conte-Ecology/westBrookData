library(getWBData)
coreData<-createCoreData(sampleType="electrofishing",
                         columnsToAdd=c("sampleNumber","river",
                                        'observedLength','observedWeight')) %>% 
            addTagProperties() %>%
              dplyr::filter(species=="bkt") %>%
                createCmrData(maxAgeInSamples=20) %>%
                  censor() %>%
                    #addSampleProperties() %>%
                      #addEnvironmental() %>%
                        addKnownZ()

JAGSData <- createJAGSData(coreData)


coreData<-createCoreData(sampleType="electrofishing",
                         columnsToAdd=c("sampleNumber","river",
                                        'observedLength','observedWeight'))
coreData<-addTagProperties(coreData)
coreData<-dplyr::filter(coreData,species == "bkt")
coreData<-createCmrData(coreData,maxAgeInSamples=20)
# coreData<-censor(coreData)
#addSampleProperties(coreData)
#addEnvironmental(coreData)
coreData<-addKnownZ(coreData)
#createJAGSData()
