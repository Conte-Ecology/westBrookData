library(getWBData)
coreData<-createCoreData() %>% 
          addTagProperties() %>%
          createCmrData() %>%
          addKnownZ() %>%
          fillSizeLocation() %>%
          addSampleProperties() %>%
          addEnvironmental(sampleFlow=T)
  
jagsData <- createJagsData(coreData)


coreData<-createCoreData(whichDrainage="stanley")
coreData<-addTagProperties(coreData)
coreData<-dplyr::filter(coreData,species %in% c("bkt","bnt","ats"))
coreData<-dplyr::filter(coreData,species %in% c("bkt"))
cmrData<-createCmrData(coreData)
cmrData<-addKnownZ(cmrData)
cmrData<-addSampleProperties(cmrData)
cmrData<-addEnvironmental(cmrData)
cmrData<-addKnownZ(cmrData)
jagsData<-createJagsData(cmrData)
