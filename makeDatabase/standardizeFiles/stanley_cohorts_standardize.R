column_map <- c(
  drainage = "Drainage",
  old_river = "River",
  river = "BRANCH",
	species = "Species",
  sample = "Sample",
	cohort = "YOS",
  cohort_min_length = "Min Length",
  cohort_max_length = "Max Length",
  age = "Age"
)

unknowns <- list(-9999, "-9999", "-9999\r\n-9999", "", -999, -99, -99999,"-9999.00","999.00",
                 "9999",9999, "np",-999.9,"-999.9",".9999","-999")
