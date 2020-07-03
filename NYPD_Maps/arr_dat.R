arr <- read_csv("nypd_arrest.csv")
pop <- read_csv("arrestData.csv")

arr2 <- select(arr, Arrested)
arr <- aggregate(arr2, by = list(Race = arr$Race, pct = arr$pct, year = arr$Year), FUN = sum)
arr <- spread(arr, Race, Arrested)
arr[is.na(arr)] <- 0
arr <- subset(arr, pct != 208760)
arr <- mutate(arr, TotalA = White+Black+Asian+Hispanic+Other+Native)
arr <- select(arr, -Other)
names(arr) <- c("Precinct", "Year", "AsPacA", "BlackA", "HispA", "NativeA", "WhiteA", "TotalA")
pop <- select(pop, "Precinct", "Population", "Area", "White", "Black", "Native", "AsPac", "Hisp")

dat <- join(pop, arr, by = "Precinct")
write.csv(dat, "ArrestDat.csv")

dat2 <- subset(dat, Year == 2016)
write.csv(dat2, "ArrestDat16.csv")
