column_code <- list(
  drainage = function() return("west"),
  river = function(river){
    rivers<-c("wb jimmy"="jimmy",
              "wb mitchell"="mitchell",
              "wb obear"="obear",
              "west brook"="west brook")
    rivers<-names(rivers)[match(river,rivers)]
    rivers[is.na(rivers)]<-river[is.na(rivers)]
    return(rivers)},
  river_meter = function(river_meter) return(as.numeric(river_meter)),
  datetime = function(date,time) {
    time[is.na(time)]<-0.5 #assignin noon to NA times for now
    return(as.POSIXct(as.numeric(date)*3600*24+as.numeric(time)*3600*24,origin=as.POSIXct("1899-12-30")))
  },
  section = function(section) return(section),
  id = function(id) return(id),
  on_off = function(on_off) return(as.numeric(on_off)),
  origin = function(origin) return(origin),
  sheet_number = function(sheet_number) return(sheet_number),
  reader_id = function(reader_id) return(reader_id),
  buffer_arrival = function(buffer_arrival) return(as.numeric(buffer_arrival)),
  buffer_departure = function(buffer_departure) return(as.numeric(buffer_departure)),
  reader_time = function(date, reader_time){
    return(as.POSIXct(as.numeric(date)*3600*24+as.numeric(reader_time)*3600*24,
                      origin=as.POSIXct("1899-12-30")))
  },
  adjusted_time = function(date, adjusted_time){
    return(as.POSIXct(as.numeric(date)*3600*24+as.numeric(adjusted_time)*3600*24,
                      origin=as.POSIXct("1899-12-30")))
  },
  stake_height = function(stake_height) return(as.numeric(stake_height)),
  actions = function(actions) return(actions),
  rms = function(rms) return(as.numierc(rms)),
  phase = function(ph) return(as.numeric(ph)),
  signal = function(signal) return(as.numeric(signal)),
  current_on_arrival = function(current){
    curSplit<-unlist(strsplit(current,"/"))
    return(as.numeric(curSplit[1]))
  },
  current_on_departure = function(current){
    curSplit<-unlist(strsplit(current,"/"))
    cur<-ifelse(length(curSplit==2),curSplit[2],curSplit[1])
    return(as.numeric(cur))
  },
  initials = function(initials) return(initials),
  comments_1 = function(comments_1) return(comments_1),
  comments_2 = function(comments_2) return(comments_2),
  field_book = function(field_book) return(field_book)
)

log <- pipeline_data_transformation(
  data=dbGetQuery(con, "SELECT * FROM raw_antenna_log;"),
  pipeline=column_code)

dbWriteTable(con, 'data_antenna_log', log, row.names=FALSE,
             overwrite=TRUE, append=FALSE)