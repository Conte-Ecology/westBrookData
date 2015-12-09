column_map <- c(
	id = "ID", 
	source = "Source", 
	tag = "Tag Number", 
	date_known_dead = "DeadDate", 
	drainage = "Drainage", 
	river = "River", 
	area = "Area", 
	quarter = "Quarter", 
	bank_side = "Bank Side", 
	where_found = "Where found", 
	justification = "Justification", 
	down_flag = "DownFlag", 
	up_flag = "UpFlag", 
	down_distance = "DownDistance", 
	up_distance = "UpDistance", 
	easting = "Easting", 
	northing = "Northing", 
	comment = "Comment"
)

value_map <- list(
	area = list(
		input = c("aboveabove"),
		output = c("above above")
	),
	bank_side = list(
		input = c("l", "m", "r", "r/m"),
		output = c("left", "middle", "right", "middle")
	)
)

unknowns <- list(-9999, "-9999", "-9999\r\n-9999", "", -999, -99, -99999,
                 "9999",9999, "np",-999.9,"-999.9",".9999","-999")
