tag_history <- data.table(dbGetQuery(con, statement = "SELECT * FROM data_captures;"))
tag_history<-tag_history[!duplicated(tag_history)]
setkey(tag_history,tag,detection_date)

#Create lists of tags with different types of issues
tagIssues<-list(multiSpecies=NULL,
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

tag_history<-tag_history[-unique(dropThese)]

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

#Check whether length sequences are reasonable
checkLength<-function(length,date,tag){
  whichHaveLength<-which(!is.na(length))
  if(length(whichHaveLength)>1){
    growthRate<-diff(length[whichHaveLength])/(as.numeric(diff(date[whichHaveLength])))
    realisticGrowth<-growthRate>-0.1&growthRate<1
    if(any(!realisticGrowth)){
      tagIssues$weirdGrowth<<-c(tagIssues$weirdGrowth,tag[1])
      cat("\ntag",tag[1],":",length)
    }
  }
}

tag_history[,checkLength(observed_length,detection_date,tag),by=tag]

dbWriteTable(con, 'data_captures', tag_history, row.names=FALSE,
						 overwrite=TRUE, append=FALSE)

