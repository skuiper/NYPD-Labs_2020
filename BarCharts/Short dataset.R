library(plyr)
library(dplyr)
library(tidyr)
nypd1 <- select(nypd_clean, "SUSPECT_ARRESTED_FLAG", "FRISKED_FLAG", "SEARCHED_FLAG", "PHYSICAL_FORCE_WEAPON_IMPACT_FLAG", "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG", 
                "PHYSICAL_FORCE_OC_SPRAY_USED_FLAG", "SUSPECT_SEX", "SUSPECT_RACE_DESCRIPTION", "YEAR2", "PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG", "PHYSICAL_FORCE_OTHER_FLAG",
                "CRIME_DESCRIPTION_GENERAL", "PHYSICAL_FORCE_CEW_FLAG", "PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG", "STOP_LOCATION_PRECINCT")
nypd1$Stopped <- rep(1, length(nypd1$SUSPECT_ARRESTED_FLAG))
nypd1$SUSPECT_SEX <- ifelse(nypd1$SUSPECT_SEX == "F", "Female", ifelse(nypd1$SUSPECT_SEX == "M", "Male", "Unknown"))
nypd1$SUSPECT_RACE_DESCRIPTION <- recode(nypd1$SUSPECT_RACE_DESCRIPTION, `White Hispanic` = "Hispanic", `Black Hispanic` = "Hispanic", `American Indian` = "Native")
names(nypd1) <- c("Arrested", "Frisked", "Searched", "Weapon", "HandCuff", "PepperSpray", "Gender", "Race", "Year", "Firearm", "Other", "CrimeType", "CEW", "Instruction", "pct", "Stopped")
nypd1$Gender[is.na(nypd1$Gender)] <- "Unknown"

nypd <- select(nypd1, "Arrested", "Frisked", "Searched", "Weapon", "HandCuff", "PepperSpray", "Firearm", "Other", "Stopped")
nypd1$Gender[is.na(nypd1$Gender)] <- "Unknown"
dat <- aggregate(nypd, by = list(Gender = nypd1$Gender, Race = nypd1$Race, Year = nypd1$Year, pct = nypd1$pct, CrimeType = nypd1$CrimeType), FUN=sum, na.rm = TRUE)

write.csv(dat, "nypd_short.csv")
