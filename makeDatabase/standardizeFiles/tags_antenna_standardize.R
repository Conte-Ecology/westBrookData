column_map <- c(
	tag = "Tag",
	species = "Species",
	cohort = "Year of Stocking",
	date_of_most_recent_measured_length = "Most Recent Size Date",
	most_recent_measured_length = "MaxOfLength",
	sample_type = "Sample Type",
	reader_id = "ReaderID",
	file_number = "FileNo",
	site_id = "SiteID",
	user_id = "UserID",
	earliest_detection_date = "MinOfDate",
	earliest_detection_time = "MinOfTime",
	earliest_detection_date_time = "MinOfDate/Time",
	section = "Section",
	quarter = "Quarter",
	distance_upriver_m = "River KM",
	distance_upriver_from_confluence = "Confl River KM",
	software_used = "Software",
	date_record_added = "Date Rec Added",
	drainage = "Drainage",
	river = "River",
	area = "Area",
	section_m = "SectionM",
	left_or_right = "L-R",
	habitat = "Fish Habitat",
	alive_or_dead = "Alive-Dead",
	alive_sample_based = "Alive Sample Based",
	comment = "Comment",
	day_or_night = "Day-Night",
	source = "Source",
	duration_of_detection = "MaxOfDuration(sec)",
	lag = "Lag(d)",
	instance = "Instance",
	antenna_instance = "AntInstance",
	antenna_threshold = "Threshold",
	survey = "Survey",
	arrival = "Arrival",
	departure = "Departure",
	pass = "Pass",
	sample_name = "Sample num",
	cover = "Fish Cover",
	justification = "Justification",
	recapture_indicator = "Recap"
)

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
