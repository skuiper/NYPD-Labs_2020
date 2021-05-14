library(dplyr)
library(readr)
library(tidyverse)
library(readxl)

#---------Begining of Data Before 2016------------#

#Load Yearly dataset before 2016

c('sex','race','year','pct','detailcm','arstmade','frisked','searched','pf_hcuff','pf_pepsp','pistol','riflshot','asltweap','machgun','pf_hands','pf_wall','pf_grnd','pf_baton','pf_other','timestop')


x2005 <- read_csv("NYPD_RawData/2005.csv",na = "empty")[ ,c('sex','race','year','pct','detailcm','arstmade','frisked','searched','pf_hcuff','pf_pepsp','pistol','riflshot','asltweap','machgun','pf_hands','pf_wall','pf_grnd','pf_baton','pf_other','sumissue','timestop','pf_drwep','pf_ptwep')
]
x2006 <- read_csv("NYPD_RawData/2006.csv",na = "empty")[ ,c('sex','race','year','pct','details_','arstmade','frisked','searched','pf_hcuff','pf_pepsp','pistol','riflshot','asltweap','machgun','pf_hands','pf_wall','pf_grnd','pf_baton','pf_other','sumissue','timestop','pf_drwep','pf_ptwep')
]
x2007 <- read_csv("NYPD_RawData/2007.csv",na = "empty")[ ,c('sex','race','year','pct','detailcm','arstmade','frisked','searched','pf_hcuff','pf_pepsp','pistol','riflshot','asltweap','machgun','pf_hands','pf_wall','pf_grnd','pf_baton','pf_other','sumissue','timestop','pf_drwep','pf_ptwep')
]
x2008 <- read_csv("NYPD_RawData/2008.csv",na = "empty")[ ,c('sex','race','year','pct','detailcm','arstmade','frisked','searched','pf_hcuff','pf_pepsp','pistol','riflshot','asltweap','machgun','pf_hands','pf_wall','pf_grnd','pf_baton','pf_other','sumissue','timestop','pf_drwep','pf_ptwep')
]
x2009 <- read_csv("NYPD_RawData/2009.csv",na = "empty")[ ,c('sex','race','year','pct','detailcm','arstmade','frisked','searched','pf_hcuff','pf_pepsp','pistol','riflshot','asltweap','machgun','pf_hands','pf_wall','pf_grnd','pf_baton','pf_other','sumissue','timestop','pf_drwep','pf_ptwep')
]
x2010 <- read_csv("NYPD_RawData/2010.csv",na = "empty")[ ,c('sex','race','year','pct','detailcm','arstmade','frisked','searched','pf_hcuff','pf_pepsp','pistol','riflshot','asltweap','machgun','pf_hands','pf_wall','pf_grnd','pf_baton','pf_other','sumissue','timestop','pf_drwep','pf_ptwep')
]
x2011 <- read_csv("NYPD_RawData/2011.csv",na = "empty")[ ,c('sex','race','year','pct','detailcm','arstmade','frisked','searched','pf_hcuff','pf_pepsp','pistol','riflshot','asltweap','machgun','pf_hands','pf_wall','pf_grnd','pf_baton','pf_other','sumissue','timestop','pf_drwep','pf_ptwep')
]
x2012 <- read_csv("NYPD_RawData/2012.csv",na = "empty")[ ,c('sex','race','year','pct','detailcm','arstmade','frisked','searched','pf_hcuff','pf_pepsp','pistol','riflshot','asltweap','machgun','pf_hands','pf_wall','pf_grnd','pf_baton','pf_other','sumissue','timestop','pf_drwep','pf_ptwep')
]
x2013 <- read_csv("NYPD_RawData/2013.csv",na = "empty")[ ,c('sex','race','year','pct','detailCM','arstmade','frisked','searched','pf_hcuff','pf_pepsp','pistol','riflshot','asltweap','machgun','pf_hands','pf_wall','pf_grnd','pf_baton','pf_other','sumissue','timestop','pf_drwep','pf_ptwep')
]
x2014 <- read_csv("NYPD_RawData/2014.csv",na = "empty")[ ,c('sex','race','year','pct','detailCM','arstmade','frisked','searched','pf_hcuff','pf_pepsp','pistol','riflshot','asltweap','machgun','pf_hands','pf_wall','pf_grnd','pf_baton','pf_other','sumissue','timestop','pf_drwep','pf_ptwep')
]
x2015 <- read_csv("NYPD_RawData/2015.csv",na = "empty")[ ,c('sex','race','year','pct','detailCM','arstmade','frisked','searched','pf_hcuff','pf_pepsp','pistol','riflshot','asltweap','machgun','pf_hands','pf_wall','pf_grnd','pf_baton','pf_other','sumissue','timestop','pf_drwep','pf_ptwep')
]
x2016 <- read_csv("NYPD_RawData/2016.csv",na = "empty")[ ,c('sex','race','year','pct','detailCM','arstmade','frisked','searched','pf_hcuff','pf_pepsp','pistol','riflshot','asltweap','machgun','pf_hands','pf_wall','pf_grnd','pf_baton','pf_other','sumissue','timestop','pf_drwep','pf_ptwep')
]

#Process dataset before 2016

x2007<-x2007[!(x2007$detailcm=='NA'),]

x2005$pct <- as.numeric(x2005$pct)
x2005$detailcm <- as.numeric(x2005$detailcm)
x2005$timestop <- as.numeric(x2005$timestop)
x2006$pct <- as.numeric(x2006$pct)
x2006$detailcm <- as.numeric(x2006$details_)
x2006$timestop <- as.numeric(x2006$timestop)
x2007$pct <- as.numeric(x2007$pct)
x2007$detailcm <- as.numeric(x2007$detailcm)
x2007$timestop <- as.numeric(x2007$timestop)

x2008$pct <- as.numeric(x2008$pct)
x2008$detailcm <- as.numeric(x2008$detailcm)
x2008$timestop <- as.numeric(x2008$timestop)

x2009$pct <- as.numeric(x2009$pct)
x2009$detailcm <- as.numeric(x2009$detailcm)
x2009$timestop <- as.numeric(x2009$timestop)

x2010$pct <- as.numeric(x2010$pct)
x2010$detailcm <- as.numeric(x2010$detailcm)
x2010$timestop <- as.numeric(x2010$timestop)

x2011$pct <- as.numeric(x2011$pct)
x2011$detailcm <- as.numeric(x2011$detailcm)
x2011$timestop <- as.numeric(x2011$timestop)

x2012$pct <- as.numeric(x2012$pct)
x2012$detailcm <- as.numeric(x2012$detailcm)
x2012$timestop <- as.numeric(x2012$timestop)

x2013$pct <- as.numeric(x2013$pct)
x2013$detailcm <- as.numeric(x2013$detailCM)
x2013$timestop <- as.numeric(x2013$timestop)

x2014$pct <- as.numeric(x2014$pct)
x2014$detailcm <- as.numeric(x2014$detailCM)
x2014$timestop <- as.numeric(x2014$timestop)

x2015$pct <- as.numeric(x2015$pct)
x2015$detailcm <- as.numeric(x2015$detailCM)
x2015$timestop <- as.numeric(x2015$timestop)

x2016$pct <- as.numeric(x2016$pct)
x2016$detailcm <- as.numeric(x2016$detailCM)
x2016$timestop <- as.numeric(x2016$timestop)


x2005<-select(x2005,sex,race,year,pct,detailcm,arstmade,frisked,searched,pf_hcuff,pf_pepsp,pistol,riflshot,asltweap,machgun,pf_hands,pf_wall,pf_grnd,pf_baton,pf_other,timestop,sumissue,pf_drwep,pf_ptwep)
x2006<-select(x2006,sex,race,year,pct,detailcm,arstmade,frisked,searched,pf_hcuff,pf_pepsp,pistol,riflshot,asltweap,machgun,pf_hands,pf_wall,pf_grnd,pf_baton,pf_other,timestop,sumissue,pf_drwep,pf_ptwep)
x2007<-select(x2007,sex,race,year,pct,detailcm,arstmade,frisked,searched,pf_hcuff,pf_pepsp,pistol,riflshot,asltweap,machgun,pf_hands,pf_wall,pf_grnd,pf_baton,pf_other,timestop,sumissue,pf_drwep,pf_ptwep)
x2008<-select(x2008,sex,race,year,pct,detailcm,arstmade,frisked,searched,pf_hcuff,pf_pepsp,pistol,riflshot,asltweap,machgun,pf_hands,pf_wall,pf_grnd,pf_baton,pf_other,timestop,sumissue,pf_drwep,pf_ptwep)
x2009<-select(x2009,sex,race,year,pct,detailcm,arstmade,frisked,searched,pf_hcuff,pf_pepsp,pistol,riflshot,asltweap,machgun,pf_hands,pf_wall,pf_grnd,pf_baton,pf_other,timestop,sumissue,pf_drwep,pf_ptwep)
x2010<-select(x2010,sex,race,year,pct,detailcm,arstmade,frisked,searched,pf_hcuff,pf_pepsp,pistol,riflshot,asltweap,machgun,pf_hands,pf_wall,pf_grnd,pf_baton,pf_other,timestop,sumissue,pf_drwep,pf_ptwep)
x2011<-select(x2011,sex,race,year,pct,detailcm,arstmade,frisked,searched,pf_hcuff,pf_pepsp,pistol,riflshot,asltweap,machgun,pf_hands,pf_wall,pf_grnd,pf_baton,pf_other,timestop,sumissue,pf_drwep,pf_ptwep)
x2012<-select(x2012,sex,race,year,pct,detailcm,arstmade,frisked,searched,pf_hcuff,pf_pepsp,pistol,riflshot,asltweap,machgun,pf_hands,pf_wall,pf_grnd,pf_baton,pf_other,timestop,sumissue,pf_drwep,pf_ptwep)
x2013<-select(x2013,sex,race,year,pct,detailcm,arstmade,frisked,searched,pf_hcuff,pf_pepsp,pistol,riflshot,asltweap,machgun,pf_hands,pf_wall,pf_grnd,pf_baton,pf_other,timestop,sumissue,pf_drwep,pf_ptwep)
x2014<-select(x2014,sex,race,year,pct,detailcm,arstmade,frisked,searched,pf_hcuff,pf_pepsp,pistol,riflshot,asltweap,machgun,pf_hands,pf_wall,pf_grnd,pf_baton,pf_other,timestop,sumissue,pf_drwep,pf_ptwep)
x2015<-select(x2015,sex,race,year,pct,detailcm,arstmade,frisked,searched,pf_hcuff,pf_pepsp,pistol,riflshot,asltweap,machgun,pf_hands,pf_wall,pf_grnd,pf_baton,pf_other,timestop,sumissue,pf_drwep,pf_ptwep)
x2016<-select(x2016,sex,race,year,pct,detailcm,arstmade,frisked,searched,pf_hcuff,pf_pepsp,pistol,riflshot,asltweap,machgun,pf_hands,pf_wall,pf_grnd,pf_baton,pf_other,timestop,sumissue,pf_drwep,pf_ptwep)

#Merge dataset before 2016

dat1 <- full_join(x2005, x2006)
dat1 <- full_join(dat1, x2007)
dat1 <- full_join(dat1, x2008)
dat1 <- full_join(dat1, x2009)
dat1 <- full_join(dat1, x2010)
dat1 <- full_join(dat1, x2011)
dat1 <- full_join(dat1, x2012)
dat1 <- full_join(dat1, x2013)
dat1 <- full_join(dat1, x2014)
dat1 <- full_join(dat1, x2015)
dat1 <- full_join(dat1, x2016)

#Check PCT
sum(is.na(dat1$pct))
sum(is_empty(dat1$pct))
barplot(prop.table(table(dat1$pct)))
dat1$pct[dat1$pct == 121] <- 122
dat1 <- dat1[!is.na(dat1$pct),]

#Check Sex
sum(is.na(dat1$sex))
sum(is_empty(dat1$sex))
barplot(prop.table(table(dat1$sex)))
dat1$Gender <- ifelse(dat1$sex == 'F' , "Female", ifelse(dat1$sex == 'M', "Male", "Unknown"))
barplot(prop.table(table(dat1$Gender)))


#Check Race
sum(is.na(dat1$race))
sum(is_empty(dat1$race))
barplot(prop.table(table(dat1$race)))
dat1 <- dat1[!is.na(dat1$race),]

dat1$race <- as.character(dat1$race)

dat1$race <- ifelse(dat1$race == "A" | dat1$race == "ASIAN / PACIFIC ISLANDER" |
                      dat1$race == "ASIAN/PAC.ISL", "Asian",
                    ifelse(dat1$race == "B" | dat1$race == "BLACK", "Black",
                           ifelse(dat1$race == "I" | dat1$race == "AMER IND" |
                                    dat1$race == "AMERICAN INDIAN/ALASKAN NATIVE", "American Indian",
                                  ifelse(dat1$race == "P" | dat1$race == "BLACK HISPANIC", "Black Hispanic",
                                         ifelse(dat1$race == "Q" | dat1$race == "WHITE HISPANIC", "White Hispanic",
                                                ifelse(dat1$race == "W" | dat1$race == "WHITE", "White", "Other"))))))

dat1$race <- ifelse(dat1$race == "Asian" | dat1$race == "Other" | dat1$race == "American Indian", "Other", dat1$race)
dat1$race <- ifelse(dat1$race == "Black Hispanic" | dat1$race == "White Hispanic", "Hispanic", dat1$race)

dat1$Race <-dat1$race



#Check Year
sum(is.na(dat1$year))
sum(is_empty(dat1$year))
barplot(prop.table(table(dat1$year)))
dat1 <- dat1[!is.na(dat1$year),]
dat1$Year <- dat1$year


#Check Detailcm (To be determined)
sum(is.na(dat1$detailcm))
sum(is_empty(dat1$detailcm))
barplot(prop.table(table(dat1$detailcm)))
dat1 <- dat1[!is.na(dat1$detailcm),]

conversion_table <- read_csv("NYPD_RawData/Crime Description_Final.csv",na = "empty")[ ,c('DetailCM','General')]
dat1 <- left_join(dat1, conversion_table , by = c("detailcm" = "DetailCM")) 
dat1$CrimeType <- dat1$General

#Check Arrest Made
sum(is.na(dat1$arstmade))
sum(is_empty(dat1$arstmade))
barplot(prop.table(table(dat1$arstmade)))
dat1 <- dat1[!is.na(dat1$arstmade),]
dat1$Arrested <- ifelse(dat1$arstmade == 'N' , 0, 1)

#Check Frisked
sum(is.na(dat1$frisked))
sum(is_empty(dat1$frisked))
barplot(prop.table(table(dat1$frisked)))
dat1 <- dat1[!is.na(dat1$frisked),]
dat1$Frisked <- ifelse(dat1$frisked == 'N' , 0, 1)


#Check Searched
sum(is.na(dat1$searched))
sum(is_empty(dat1$searched))
barplot(prop.table(table(dat1$searched)))
dat1 <- dat1[!is.na(dat1$searched),]
dat1$Searched <- ifelse(dat1$searched == 'N' , 0, 1)

#Check pf_hcuff
sum(is.na(dat1$pf_hcuff))
sum(is_empty(dat1$pf_hcuff))
barplot(prop.table(table(dat1$pf_hcuff)))
dat1 <- dat1[!is.na(dat1$pf_hcuff),]
dat1$HandCuff <- ifelse(dat1$pf_hcuff == 'Y' , 1, 0)


#Check pf_pepsp
sum(is.na(dat1$pf_pepsp))
sum(is_empty(dat1$pf_pepsp))
barplot(prop.table(table(dat1$pf_pepsp)))
dat1 <- dat1[!is.na(dat1$pf_pepsp),]
dat1$PepperSpray <- ifelse(dat1$pf_pepsp == 'Y' , 1, 0)

#Check sumissue
sum(is.na(dat1$sumissue))
sum(is_empty(dat1$sumissue))
barplot(prop.table(table(dat1$sumissue)))
dat1 <- dat1[!is.na(dat1$sumissue),]
dat1$Summons <- ifelse(dat1$sumissue == 'Y' , 1, 0)

#-------Firearm Features------------#
#Check pistol
sum(is.na(dat1$pistol))
sum(is_empty(dat1$pistol))
barplot(prop.table(table(dat1$pistol)))
dat1 <- dat1[!is.na(dat1$pistol),]
dat1$pistol <- ifelse(dat1$pistol == 'N' | dat1$pistol == 'NA', 'N', 'Y')

#Check riflshot
sum(is.na(dat1$riflshot))
sum(is_empty(dat1$riflshot))
barplot(prop.table(table(dat1$riflshot)))
dat1 <- dat1[!is.na(dat1$riflshot),]
dat1$riflshot <- ifelse(dat1$riflshot == 'N' | dat1$riflshot == 'NA', 'N', 'Y')

#Check asltweap
sum(is.na(dat1$asltweap))
sum(is_empty(dat1$asltweap))
barplot(prop.table(table(dat1$asltweap)))
dat1 <- dat1[!is.na(dat1$asltweap),]
dat1$asltweap <- ifelse(dat1$asltweap == 'N' | dat1$asltweap == 'NA', 'N', 'Y')

#Check machgun
sum(is.na(dat1$machgun))
sum(is_empty(dat1$machgun))
barplot(prop.table(table(dat1$machgun)))
dat1 <- dat1[!is.na(dat1$machgun),]
dat1$machgun <- ifelse(dat1$machgun == 'N' | dat1$machgun == 'NA', 'N', 'Y')

#Check pf_drwep
sum(is.na(dat1$pf_drwep))
sum(is_empty(dat1$pf_drwep))
barplot(prop.table(table(dat1$pf_drwep)))
dat1 <- dat1[!is.na(dat1$pf_drwep),]
dat1$pf_drwep <- ifelse(dat1$pf_drwep == 'N' | dat1$pf_drwep == 'NA', 'N', 'Y')

#Check pf_ptwep
sum(is.na(dat1$pf_ptwep))
sum(is_empty(dat1$pf_ptwep))
barplot(prop.table(table(dat1$pf_ptwep)))
dat1 <- dat1[!is.na(dat1$pf_ptwep),]
dat1$pf_ptwep <- ifelse(dat1$pf_ptwep == 'N' | dat1$pf_ptwep == 'NA', 'N', 'Y')

dat1$Firearm <- ifelse(dat1$pistol == 'Y' | dat1$riflshot == 'Y' | dat1$asltweap == 'Y' | dat1$machgun == 'Y'| dat1$pf_drwep == 'Y'| dat1$pf_ptwep == 'Y', 1, 0)

#--------Other Features--------#
#Check pf_hands
sum(is.na(dat1$pf_hands))
sum(is_empty(dat1$pf_hands))
barplot(prop.table(table(dat1$pf_hands)))
dat1 <- dat1[!is.na(dat1$pf_hands),]
dat1$pf_hands <- ifelse(dat1$pf_hands == 'N' | dat1$pf_hands == 'NA', 'N', 'Y')

#Check pf_wall
sum(is.na(dat1$pf_wall))
sum(is_empty(dat1$pf_wall))
barplot(prop.table(table(dat1$pf_wall)))
dat1 <- dat1[!is.na(dat1$pf_wall),]
dat1$pf_wall <- ifelse(dat1$pf_wall == 'N' | dat1$pf_wall == 'NA', 'N', 'Y')

#Check pf_grnd
sum(is.na(dat1$pf_grnd))
sum(is_empty(dat1$pf_grnd))
barplot(prop.table(table(dat1$pf_grnd)))
dat1 <- dat1[!is.na(dat1$pf_grnd),]
dat1$pf_grnd <- ifelse(dat1$pf_grnd == 'N' | dat1$pf_grnd == 'NA', 'N', 'Y')

#Check pf_baton
sum(is.na(dat1$pf_baton))
sum(is_empty(dat1$pf_baton))
barplot(prop.table(table(dat1$pf_baton)))
dat1 <- dat1[!is.na(dat1$pf_baton),]
dat1$pf_baton <- ifelse(dat1$pf_baton == 'N' | dat1$pf_baton == 'NA', 'N', 'Y')

#Check pf_other
sum(is.na(dat1$pf_other))
sum(is_empty(dat1$pf_other))
barplot(prop.table(table(dat1$pf_other)))
dat1 <- dat1[!is.na(dat1$pf_other),]
dat1$pf_other <- ifelse(dat1$pf_other == 'N' | dat1$pf_other == 'NA', 'N', 'Y')

dat1$Other <- ifelse(dat1$pf_hands == 'Y' | dat1$pf_wall == 'Y' | dat1$pf_grnd == 'Y' | dat1$pf_baton == 'Y' | dat1$pf_other == 'Y', 1, 0)

#--------CEW-----------#

dat1$CEW <- 0

#Check Time-stop
sum(is.na(dat1$timestop))
sum(is_empty(dat1$timestop))
barplot(prop.table(table(dat1$timestop)))
dat1$timestop[is.na(dat1$timestop)] <- 0
dat1$Stopped <- ifelse(dat1$timestop != 0 , 1, 0)


#---------End of Data Before 2016------------#

dat1 <- select(dat1, Gender, Race, Year, pct, CrimeType, Arrested, Frisked, Searched, HandCuff, PepperSpray, Firearm, Other, CEW, Stopped, Summons)




#---------Beginning of Data After 2016------------#

c('SUSPECT_SEX','SUSPECT_RACE_DESCRIPTION','YEAR2','STOP_LOCATION_PRECINCT','SUSPECTED_CRIME_DESCRIPTION','SUSPECT_ARRESTED_FLAG','FRISKED_FLAG','SEARCHED_FLAG','PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG','PHYSICAL_FORCE_OC_SPRAY_USED_FLAG','FIREARM_FLAG','PHYSICAL_FORCE_OTHER_FLAG','PHYSICAL_FORCE_CEW_FLAG','STOP_FRISK_DATE')



#Convert data

#Load Yearly dataset after 2016
x2017 <- read_csv("NYPD_RawData/2017.csv",na = "empty")[ ,c('SUSPECT_SEX','SUSPECT_RACE_DESCRIPTION','YEAR2','STOP_LOCATION_PRECINCT','SUSPECTED_CRIME_DESCRIPTION','SUSPECT_ARRESTED_FLAG','FRISKED_FLAG','SEARCHED_FLAG','PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG','PHYSICAL_FORCE_OC_SPRAY_USED_FLAG','FIREARM_FLAG','PHYSICAL_FORCE_OTHER_FLAG','PHYSICAL_FORCE_CEW_FLAG','STOP_FRISK_DATE','PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG','PHYSICAL_FORCE_RESTRAINT_USED_FLAG','PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG','PHYSICAL_FORCE_WEAPON_IMPACT_FLAG','SUMMONS_ISSUED_FLAG')]
x2018 <- read_csv("NYPD_RawData/2018.csv",na = "empty")[ ,c('SUSPECT_SEX','SUSPECT_RACE_DESCRIPTION','YEAR2','STOP_LOCATION_PRECINCT','SUSPECTED_CRIME_DESCRIPTION','SUSPECT_ARRESTED_FLAG','FRISKED_FLAG','SEARCHED_FLAG','PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG','PHYSICAL_FORCE_OC_SPRAY_USED_FLAG','FIREARM_FLAG','PHYSICAL_FORCE_OTHER_FLAG','PHYSICAL_FORCE_CEW_FLAG','STOP_FRISK_DATE','PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG','PHYSICAL_FORCE_RESTRAINT_USED_FLAG','PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG','PHYSICAL_FORCE_WEAPON_IMPACT_FLAG','SUMMONS_ISSUED_FLAG')]
x2019 <- read_csv("NYPD_RawData/2019.csv",na = "empty")[ ,c('SUSPECT_SEX','SUSPECT_RACE_DESCRIPTION','YEAR2','STOP_LOCATION_PRECINCT','SUSPECTED_CRIME_DESCRIPTION','SUSPECT_ARRESTED_FLAG','FRISKED_FLAG','SEARCHED_FLAG','PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG','PHYSICAL_FORCE_OC_SPRAY_USED_FLAG','FIREARM_FLAG','PHYSICAL_FORCE_OTHER_FLAG','PHYSICAL_FORCE_CEW_FLAG','STOP_FRISK_DATE','PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG','PHYSICAL_FORCE_RESTRAINT_USED_FLAG','PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG','PHYSICAL_FORCE_WEAPON_IMPACT_FLAG','SUMMONS_ISSUED_FLAG')]

x2017$STOP_LOCATION_PRECINCT <- ifelse(x2017$STOP_LOCATION_PRECINCT == "NA", "", x2017$STOP_LOCATION_PRECINCT)
x2017$STOP_FRISK_DATE <- as.numeric(x2017$STOP_FRISK_DATE)
x2018$STOP_FRISK_DATE <- as.numeric(x2018$STOP_FRISK_DATE)
x2019$STOP_FRISK_DATE <- as.numeric(x2019$STOP_FRISK_DATE)


x2017$STOP_LOCATION_PRECINCT <- as.numeric(x2017$STOP_LOCATION_PRECINCT)
x2018$STOP_LOCATION_PRECINCT <- as.numeric(x2018$STOP_LOCATION_PRECINCT)
x2019$STOP_LOCATION_PRECINCT <- as.numeric(x2019$STOP_LOCATION_PRECINCT)

dat2 <- full_join(x2017, x2018)
dat2 <- full_join(dat2, x2019)

#Check PCT
sum(is.na(dat2$STOP_LOCATION_PRECINCT))
sum(is_empty(dat2$STOP_LOCATION_PRECINCT))
barplot(prop.table(table(dat2$STOP_LOCATION_PRECINCT)))
dat2$STOP_LOCATION_PRECINCT[dat2$STOP_LOCATION_PRECINCT == 121] <- 122
dat2 <- dat2[!is.na(dat2$STOP_LOCATION_PRECINCT),]
dat2$pct <- dat2$STOP_LOCATION_PRECINCT

#Check Sex
sum(is.na(dat2$SUSPECT_SEX))
sum(is_empty(dat2$SUSPECT_SEX))
barplot(prop.table(table(dat2$SUSPECT_SEX)))
dat2$Gender <- ifelse(dat2$SUSPECT_SEX == 'FEMALE' , "Female", ifelse(dat2$SUSPECT_SEX == 'MALE', "Male", "Unknown"))
barplot(prop.table(table(dat1$Gender)))


#Check Race
sum(is.na(dat2$SUSPECT_RACE_DESCRIPTION))
sum(is_empty(dat2$SUSPECT_RACE_DESCRIPTION))
barplot(prop.table(table(dat2$SUSPECT_RACE_DESCRIPTION)))
dat2 <- dat2[!is.na(dat2$SUSPECT_RACE_DESCRIPTION),]

dat2$SUSPECT_RACE_DESCRIPTION <- as.character(dat2$SUSPECT_RACE_DESCRIPTION)

dat2$ASIAN <- ifelse(dat2$SUSPECT_RACE_DESCRIPTION == "ASIAN/PAC.ISL", 1,0)
dat2$BLACK <- ifelse(dat2$SUSPECT_RACE_DESCRIPTION == "BLACK", 1,0)
dat2$AMERICAN_INDIAN <- ifelse(dat2$SUSPECT_RACE_DESCRIPTION == "MALE", 1,0)
dat2$BLACK_HISPANIC <- ifelse(dat2$SUSPECT_RACE_DESCRIPTION == "BLACK HISPANIC", 1,0)
dat2$WHITE_HISPANIC <- ifelse(dat2$SUSPECT_RACE_DESCRIPTION == "WHITE HISPANIC", 1,0)
dat2$WHITE <- ifelse(dat2$SUSPECT_RACE_DESCRIPTION == "WHITE", 1,0)
dat2$OTHER <- ifelse(dat2$SUSPECT_RACE_DESCRIPTION == "(null)", 1,0)

dat2$Race <- ifelse(dat2$ASIAN == 1 | dat2$AMERICAN_INDIAN == 1 | dat2$OTHER==1, "Other",
                    ifelse(dat2$BLACK_HISPANIC == 1 | dat2$WHITE_HISPANIC, "Hispanic",
                           ifelse(dat2$WHITE == 1, "White","Black")))


#Check Year
sum(is.na(dat2$YEAR2))
sum(is_empty(dat2$YEAR2))
barplot(prop.table(table(dat2$YEAR2)))
dat2 <- dat2[!is.na(dat2$YEAR2),]
dat2$Year <- dat2$YEAR2

#Check Crime Description
sum(is.na(dat2$SUSPECTED_CRIME_DESCRIPTION))
sum(is_empty(dat2$SUSPECTED_CRIME_DESCRIPTION))
barplot(prop.table(table(dat2$SUSPECTED_CRIME_DESCRIPTION)))
dat2 <- dat2[!is.na(dat2$SUSPECTED_CRIME_DESCRIPTION),]

conversion_table <- read_csv("NYPD_RawData/Crime Description_Final.csv",na = "empty")[ ,c('Description','General')]
dat2 <- left_join(dat2, conversion_table , by = c("SUSPECTED_CRIME_DESCRIPTION" = "Description")) 
dat2$CrimeType <- dat2$General

sum(is.na(dat2$CrimeType))

#Check Arrest Made
sum(is.na(dat2$SUSPECT_ARRESTED_FLAG))
sum(is_empty(dat2$SUSPECT_ARRESTED_FLAG))
barplot(prop.table(table(dat2$SUSPECT_ARRESTED_FLAG)))
dat2 <- dat2[!is.na(dat2$SUSPECT_ARRESTED_FLAG),]
dat2$Arrested <- ifelse(dat2$SUSPECT_ARRESTED_FLAG == 'N' , 0, 1)

#Check Frisked
sum(is.na(dat2$FRISKED_FLAG))
sum(is_empty(dat2$FRISKED_FLAG))
barplot(prop.table(table(dat2$FRISKED_FLAG)))
dat2 <- dat2[!is.na(dat2$FRISKED_FLAG),]
dat2$Frisked <- ifelse(dat2$FRISKED_FLAG == 'N' , 0, 1)


#Check Searched
sum(is.na(dat2$SEARCHED_FLAG))
sum(is_empty(dat2$SEARCHED_FLAG))
barplot(prop.table(table(dat2$SEARCHED_FLAG)))
dat2 <- dat2[!is.na(dat2$SEARCHED_FLAG),]
dat2$Searched <- ifelse(dat2$SEARCHED_FLAG == 'N' , 0, 1)

#Check pf_hcuff
sum(is.na(dat2$PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG))
sum(is_empty(dat2$PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG))
barplot(prop.table(table(dat2$PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG)))
dat2 <- dat2[!is.na(dat2$PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG),]
dat2$HandCuff <- ifelse(dat2$PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG == 'Y' , 1, 0)


#Check pf_pepsp
sum(is.na(dat2$PHYSICAL_FORCE_OC_SPRAY_USED_FLAG))
sum(is_empty(dat2$PHYSICAL_FORCE_OC_SPRAY_USED_FLAG))
barplot(prop.table(table(dat2$PHYSICAL_FORCE_OC_SPRAY_USED_FLAG)))
dat2 <- dat2[!is.na(dat2$PHYSICAL_FORCE_OC_SPRAY_USED_FLAG),]
dat2$PepperSpray <- ifelse(dat2$PHYSICAL_FORCE_OC_SPRAY_USED_FLAG == 'Y' , 1, 0)

#--------------------Firearm Features--------------------#

#Check Firearm
sum(is.na(dat2$FIREARM_FLAG))
sum(is_empty(dat2$FIREARM_FLAG))
barplot(prop.table(table(dat2$FIREARM_FLAG)))
dat2 <- dat2[!is.na(dat2$FIREARM_FLAG),]


#Check Physical Draw Point Firearm Flag
sum(is.na(dat2$PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG))
sum(is_empty(dat2$PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG))
barplot(prop.table(table(dat2$PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG)))
dat2 <- dat2[!is.na(dat2$PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG),]


dat2$Firearm <- ifelse(dat2$FIREARM_FLAG == 'Y' | dat2$PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG == 'Y' , 1, 0)

#-------------------Other Features-----------------------#
#Check PHYSICAL_FORCE_OTHER_FLAG
sum(is.na(dat2$PHYSICAL_FORCE_OTHER_FLAG))
sum(is_empty(dat2$PHYSICAL_FORCE_OTHER_FLAG))
barplot(prop.table(table(dat2$PHYSICAL_FORCE_OTHER_FLAG)))
dat2 <- dat2[!is.na(dat2$PHYSICAL_FORCE_OTHER_FLAG),]

#Check PHYSICAL_FORCE_RESTRAINT_USED_FLAG
sum(is.na(dat2$PHYSICAL_FORCE_RESTRAINT_USED_FLAG))
sum(is_empty(dat2$PHYSICAL_FORCE_RESTRAINT_USED_FLAG))
barplot(prop.table(table(dat2$PHYSICAL_FORCE_RESTRAINT_USED_FLAG)))
dat2 <- dat2[!is.na(dat2$PHYSICAL_FORCE_RESTRAINT_USED_FLAG),]


#Check PHYSICAL_FORCE_WEAPON_IMPACT_FLAG 
sum(is.na(dat2$PHYSICAL_FORCE_WEAPON_IMPACT_FLAG))
sum(is_empty(dat2$PHYSICAL_FORCE_WEAPON_IMPACT_FLAG))
barplot(prop.table(table(dat2$PHYSICAL_FORCE_WEAPON_IMPACT_FLAG)))
dat2 <- dat2[!is.na(dat2$PHYSICAL_FORCE_WEAPON_IMPACT_FLAG),]


dat2$Other <- ifelse(dat2$PHYSICAL_FORCE_OTHER_FLAG == 'Y' | dat2$PHYSICAL_FORCE_RESTRAINT_USED_FLAG |  dat2$PHYSICAL_FORCE_WEAPON_IMPACT_FLAG, 1, 0)

#--------------------Verbal-----------------------#

#Check PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG
sum(is.na(dat2$PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG))
sum(is_empty(dat2$PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG))
barplot(prop.table(table(dat2$PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG)))
dat2 <- dat2[!is.na(dat2$PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG),]
dat2$Verbal <- ifelse(dat2$PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG == 'Y' , 1, 0)

#--------------------SUMMONS_ISSUED_FLAG-----------------------#

#Check PHYSICAL_FORCE_VERBAL_INSTRUCTION_FLAG
sum(is.na(dat2$SUMMONS_ISSUED_FLAG))
sum(is_empty(dat2$SUMMONS_ISSUED_FLAG))
barplot(prop.table(table(dat2$SUMMONS_ISSUED_FLAG)))
dat2 <- dat2[!is.na(dat2$SUMMONS_ISSUED_FLAG),]
dat2$Summons <- ifelse(dat2$SUMMONS_ISSUED_FLAG == 'Y' , 1, 0)

#--------------------CEW-----------------------#
#Check pf_crew

sum(is.na(dat2$PHYSICAL_FORCE_CEW_FLAG))
sum(is_empty(dat2$PHYSICAL_FORCE_CEW_FLAG))
barplot(prop.table(table(dat2$PHYSICAL_FORCE_CEW_FLAG)))
dat2 <- dat2[!is.na(dat2$PHYSICAL_FORCE_CEW_FLAG),]
dat2$CEW <- ifelse(dat2$PHYSICAL_FORCE_CEW_FLAG == 'Y' , 1, 0)


#Check Time-stop
sum(is.na(dat2$STOP_FRISK_DATE))
sum(is_empty(dat2$STOP_FRISK_DATE))
barplot(prop.table(table(dat2$STOP_FRISK_DATE)))
dat2$STOP_FRISK_DATE[is.na(dat2$STOP_FRISK_DATE)] <- 0
dat2$Stopped <- ifelse(dat2$STOP_FRISK_DATE != 0 , 1, 0)

#---------End of Data After 2016------------#

dat2 <- select(dat2, Gender, Race, Year, pct, CrimeType, Arrested, Frisked, Searched, HandCuff, PepperSpray, Firearm, Other, CEW, Stopped, Verbal, Summons)

#--------Merge Two Big Dataset--------------#

Final_data <- full_join(dat1,dat2)

#-------Aggregate Bar Chart Dataset---------#

Final_data_aggregated <- aggregate(select(Final_data,Arrested,Frisked,Searched,HandCuff,PepperSpray,Firearm, Other, CEW, Stopped, Verbal, Summons), by = list(Gender = Final_data$Gender, Race = Final_data$Race, Year = Final_data$Year, pct = Final_data$pct, CrimeType = Final_data$CrimeType), FUN=sum, na.rm = TRUE)
Final_data_aggregated <- Final_data_aggregated[order(Final_data_aggregated$Year, Final_data_aggregated$pct,Final_data_aggregated$CrimeType),]
write.csv(Final_data_aggregated, "nypd_arrestBar_Yusen.csv")