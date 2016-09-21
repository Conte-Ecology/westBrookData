sheets<-excel_sheets(file.path(original_data_dir,"antenna/logData.xlsx"))
sheets<-grep("FINAL",sheets)

nameMap<-c(date="Date",
           time="Time",
           river="River",
           river_meter="River Km",
           section="Section",
           id="ID",
           on_off="ON /OFF",
           origin="Origin",
           sheet_number="Sheet #",
           reader_id="ReaderID",
           buffer_arrival="Buffer Arrival",
           buffer_departure="Buffer Departure",
           reader_time="Reader Time",
           adjusted_time="Time Adj.",
           stake_height="Stake Height",
           actions="Actions",
           rms="RMS",
           ph="PH",
           signal="Signal",
           current="Current",
           initials="Initials",
           comments_1="Comments 1",
           comments_2="Comments 2",
           field_book="Field Book")

logs<-NULL
for(s in sheets){
  sheetDims<-readxl:::xlsx_dim(path=file.path(original_data_dir,"antenna/logData.xlsx"),sheet=s-1)
  log<-read_excel(file.path(original_data_dir,"antenna/logData.xlsx"),
                  sheet=1,col_types=rep("text",sheetDims[2]))
  log<-log[,1:24]
  logs<-bind_rows(log,logs)
}
rm(log)

names(logs)<-names(nameMap)[match(names(logs),nameMap)]
for(nom in names(logs)){
  logs[[nom]]<-pipeline_string_transformation(string=logs[[nom]],
                                 pipeline=standard_string_transformations[
                                   c('drop_leading_whitespace','drop_trailing_whitespace','to_lower_case')])
}



dbWriteTable(con, 'raw_antenna_log', logs, row.names=FALSE,
             overwrite=TRUE, append=FALSE)
