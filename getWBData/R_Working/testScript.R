library(getWBData)
coreData<-createCoreData(sampleType="captures",columnsToAdd=c("sampleNumber","river")) %>% 
            addTagProperties() %>%
              filter(species=="bkt") %>%
                createCmrData(maxAgeInSamples=20) %>%
                  censor() %>%
                    #addSampleProperties() %>%
                      #addEnvironmental() %>%
                        addKnownZ() #%>%
                          #createJagsData()

coreData<-createCoreData(sampleType="captures",columnsToAdd=c("sampleNumber","river"))
coreData<-addTagProperties(coreData)
coreData<-filter(coreData,species=="bkt")
coreData<-createCmrData(coreData,maxAgeInSamples=20)
coreData<-censor(coreData)
#addSampleProperties(coreData)
#addEnvironmental(coreData)
addKnownZ()
#createJagsData()
