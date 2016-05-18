column_map <- c(
	id = "ID", 
	tag = "Tag Number", 
	alive_dead = "Alive/Dead",
	date_known_dead = "Dead Date",
	source = "Source", 
	justification = "Justification"
	)


unknowns <- list(-9999, "-9999", "-9999\r\n-9999", "", -999, -99, -99999,"-9999.00","999.00",
                 "9999",9999, "np",-999.9,"-999.9",".9999","-999")
