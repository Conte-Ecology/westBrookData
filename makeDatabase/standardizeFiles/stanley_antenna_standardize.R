column_map <- c(
  drainage = "DRAINAGE",
  old_river = "RIVER",
  sample_type = "SAMPLE TYPE",
  area = "AREA",
  sample_name = "SAMPLE NUM",
  tag = "TAG NUMBER",
  earliest_detection_date_time = "DATE/TIME",
  date = "Date",
  time = "Time",
  habitat = "HABITAT",
  section = "SECTION",
  quarter = "QUARTER",
  species = "SPECIES",
  comments = "COMMENTS",
  distance_upriver_m = "RKM",
  river = "Branch",
  alive_or_dead = "MinOfAlive/Dead",
  justification = "MinOfJustification",
  sample_complete = "SAMPLE COMPLETE")


value_map <- list(
	habitat = list(
		input = c(	"rf",			"rn",		"r",		"lrf"),
		output = c(	"riffle",	"run",	"run",	"riffle")
	),
	alive_or_dead = list(
		input = c(	"live",		"alive-stationary"),
		output = c(	"alive",	"alive")
	)	
)

unknowns <- list(-9999, "-9999", "-9999\r\n-9999", "", -999, -99,"-9999.00","999.00",
                 -99999,"9999",9999, "np",-999.9,"-999.9",".9999","-999")
