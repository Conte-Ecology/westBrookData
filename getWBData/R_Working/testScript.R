library(getWBData)
coreData<-createCoreData(sampleType="captures",columnsToAdd=c("sampleNumber","river")) %>%
# coreData<-coreData %>%
            addTagProperties() %>%
              filter(species=="bkt") %>%
                createCmrData(maxAgeInSamples=20) %>%
                  censor() %>%
                    addKnownZ()

coreData<-createCoreData(sampleType="captures",columnsToAdd=c("sampleNumber","river"))
coreData<-addTagProperties(coreData)
coreData<-filter(coreData,species=="bkt")
coreData<-createCmrData(coreData,maxAgeInSamples=20)
coreData<-censor(coreData)
coreData<-addKnownZ(coreData)
