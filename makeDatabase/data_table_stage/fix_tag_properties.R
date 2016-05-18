tag_history <- data.table(dbGetQuery(con, statement = "SELECT * FROM data_tagged_captures;"))
tag_history<-tag_history[!duplicated(tag_history)]
setkey(tag_history,tag,detection_date)

#Create lists of tags with different types of issues
tagIssues<-list(multiSpecies=NULL,
                multiSex=NULL,
                weirdGrowth=NULL)

tagIssues$multiObsWithinSample<-
  tag_history[duplicated(tag_history[,list(tag,sample_name,section,detection_date)],by=NULL),
                                     tag]

#Drop records that are duplicate sample/tag combos
dropThese<-NULL
fixMultiObs<-function(row,printChanges=F){
  thisTag<-tag_history[row,tag]
  sample<-tag_history[row,sample_name]
  
  whichRows<-tag_history[tag==thisTag&sample_name==sample,,which=T]
  if(length(whichRows)==1) stop("only 1 row for tag/sample combination")
  if(length(unique(tag_history[whichRows,detection_date]))>1){
    keep<-whichRows[which.min(tag_history[whichRows,detection_date])]
    drop<-whichRows[whichRows!=keep]
  } else {
    if(length(unique(tag_history[whichRows,section]))>1){
      keep<-whichRows[which.min(tag_history[whichRows,section])]
      drop<-whichRows[whichRows!=keep]
    } else {
      keep<-whichRows[1]
      drop<-whichRows[-1]
    }
  }
  if(printChanges==T){print(tag_history[c(keep,drop),
                    list(action=c("keep",rep("drop",length(drop))),
                         tag,sample_number,detection_date,river,section,observed_length)])
  }
  dropThese<<-c(dropThese,drop)
}

for(r in which(duplicated(tag_history[,list(tag,sample_name)],by=NULL))){
  fixMultiObs(r)
}
if(!is.null(dropThese)){
tag_history<-tag_history[-unique(dropThese)]
}

#Make sure each tag has only one species
fixSpecies<-function(species,tag){
  nSpecies<-length(unique(na.omit(species)))
  if(nSpecies>1){
    tagIssues$multiSpecies<<-c(tagIssues$multiSpecies,tag[1])
    nBySpecies<-aggregate(species,list(species),length)
    species<-rep(nBySpecies[which.max(nBySpecies[,2]),1],length(species))
  }
  return(species)
}

tag_history[,species:=fixSpecies(species,tag),by=tag]

#Make sure each tag has only one sex
fixSex<-function(sex,tag){
  nSex<-length(unique(na.omit(sex)))
  if(nSex==0){return(rep(as.character(NA),length(sex)))}
  if(nSex>1){
    tagIssues$multiSex<<-c(tagIssues$multiSex,tag[1])
    nBySex<-aggregate(sex,list(sex),length)
    sex<-rep(nBySex[which.max(nBySex[,2]),1],length(sex))
  }
  return(sex)
}
tag_history[,sex:=fixSex(sex,tag),by=tag]

#Check whether length sequences are reasonable
checkLength<-function(length,date,tag){
  whichHaveLength<-which(!is.na(length))
  if(length(whichHaveLength)>1){
    growthRate<-diff(length[whichHaveLength])/max((as.numeric(diff(date[whichHaveLength]))),1)
    realisticGrowth<-growthRate>-0.1&growthRate<1
    if(any(!realisticGrowth)){
      tagIssues$weirdGrowth<<-c(tagIssues$weirdGrowth,tag[1])
      cat("\ntag",tag[1],":",length)
    }
  }
}

tag_history[,checkLength(observed_length,detection_date,tag),by=tag]

#check for outliers in length-weight regressions
lengthWeightComboCheck<-function(tag,length,weight,tolerance=0.999999){
  logLength<-log(length)
  logWeight<-log(weight)
  fit<-lm(logWeight~logLength)
  predicted<-predict(fit,data.frame(logLength=logLength),
                     interval="prediction",level=tolerance) %>%
    data.frame()
  outliers<-which(logWeight<predicted$lwr|logWeight>predicted$upr)
  lengthWeightComboTags<<-c(lengthWeightComboTags,tag[outliers])
}
lengthWeightComboTags<-NULL
tag_history[,lengthWeightComboCheck(tag,observed_length,observed_weight)]
tagIssues[['lengthWeightCombos']]<-lengthWeightComboTags

writeIssues<-function(tagIssues,issuesPath){
  multiObsWithinSample<-!is.null(tagIssues$multiObsWithinSample)
  multiSpecies<-!is.null(tagIssues$multiSpecies)
  weirdGrowth<-!is.null(tagIssues$weirdGrowth)
  lengthWeightCombos<-!is.null(tagIssues$lengthWeightComboTags)
  
  nIssues<-max(sapply(tagIssues,length))
  issues<-array(NA,dim=c(nIssues,4))
  dimnames(issues)<-list(NULL,c("multiObsWithinSample",
                                "multiSpecies",
                                "weirdGrowth",
                                "lengthWeightCombos")
                         )
  for(i in dimnames(issues)[[2]]){
    if(!is.null(tagIssues[[i]]) & length(tagIssues[[i]])>0){
      issues[,i]<-c(tagIssues[[i]],rep(as.character(NA),dim(issues)[1]-length(tagIssues[[i]])))
    }
  }
  
  if(any(multiObsWithinSample,multiSpecies,weirdGrowth)){
    warning(paste0("\nThe following issue types were identified for some tags:",
                ifelse(multiObsWithinSample,"\nmultiple observations within a sample",""),
                ifelse(multiSpecies,"\nmultiple species ids",""),
                ifelse(weirdGrowth,"\nunrealistic length sequence",""),
                "\n\nProblem tags were written to ",issuesPath),call.=F)
    write.csv(issues,issuesPath)
  }
}

writeIssues(tagIssues,file.path(dataOutDir,"tagIssues.csv"))


dbWriteTable(con, 'data_tagged_captures', tag_history, row.names=FALSE,
						 overwrite=TRUE, append=FALSE)

