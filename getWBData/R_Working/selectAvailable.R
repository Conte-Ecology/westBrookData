


# if include fish from cohorts < 1997, they will have ageInSamples that
#don't go back to 1. for now we are leaving out cohort < 1997
#when we want to include them, we'll need to augment back to ageInsamples 1
#by adding in negative smample numbers
subsetDMdataCohortMin <- min(cohorts) # >=
subsetDMdataCohortMax <- max(cohorts) # <=

 
