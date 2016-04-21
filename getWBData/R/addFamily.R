#'Add family information to a west brook data.frame
#'@return Family identity added to a data.frame
#'@param coreData a data.frame created using createCoreData
#'@param familyId Logical indicating whether to include the original family ID
#'@param familyIndex Logical indicating whether to include a 1:nFamilies index of families
#'@details Merges in family information for fish that have it using tag number. There is not currently a way to add family information for untagged fish, but family data for all fish can be acquired using tbl(conDplyr,data_family)
#'@export

addFamily<-function(coreData,familyId=F,familyIndex=T){
  family<-tbl(conDplyr,dataByTag) %>%
          select(tag,family_id) %>%
          setNames(camelCase(names(.))) %>%
          filter(!is.na(tag))
          collect()
  
  coreData<-left_join(coreData,family,by="tag")
  
  if(familyIndex){
    coreData[!is.na(coreData$familyId)]<-match(coreData[!is.na(coreData$familyId),"familyId"]
                                               ,unique(coreData[!is.na(coreData$familyId),"familyId"]))
  }
  if(!familyId){
    coreData<-select(coreData,-familyId)
  }
  
  return(coreData)
}