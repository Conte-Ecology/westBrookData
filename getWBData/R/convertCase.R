#'Convert camel-case string to underscore separated
#'
#'@param x Character (vector) to be converted to camel-case
#'@return A string (vector) with underscore separated words
#'@export

fillUnderscore<-Vectorize(function(x){
  whichCaps<-which(unlist(strsplit(x,""))!=unlist(strsplit(tolower(x),"")))
  if(1 %in% whichCaps){whichCaps<-whichCaps[-1]}
  xSplit<-substring(x,c(1,whichCaps),c(whichCaps-1,nchar(x)))
  filled<-paste(tolower(xSplit),collapse="_")
  return(filled)
})

#'Convert string of separated words to camel-case
#'
#'@param x Character(vector) to be converted to camel-case
#'@param sep Character separating words in the original string
#'@return Camel-case string(s)
#'@export

camelCase<-Vectorize(function(x,sep="_"){
  xSplit<-unlist(strsplit(x,sep))
  
  if(length(xSplit)==1){return(xSplit)}
  for(n in 2:length(xSplit)){
    xSplit[n]<-paste0(toupper(substr(xSplit[n],1,1)),
                      substr(xSplit[n],2,nchar(xSplit[n])))
  }
  camelled<-paste(xSplit,collapse="")
  return(camelled)
})