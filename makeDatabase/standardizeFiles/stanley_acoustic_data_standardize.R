column_map <- c(
  id = "ID",
  year = "Year",
  date = "Date",
  time = "Time",
  datetime = "Date/Time",
  receiver = "Receiver",
  acoustic_tag = "Acoustic Tag Number",
  tag = "PIT Tag Number",
  section = "Section",
  quarter = "Location"
)

unknowns <- list(-9999, "-9999", "-9999\r\n-9999", "", -999, -99,"-9999.00","999.00",
                 -99999,"9999",9999, "np",-999.9,"-999.9",".9999","-999")
