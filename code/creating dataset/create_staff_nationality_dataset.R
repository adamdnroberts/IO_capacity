library(plyr)
library(dplyr)
library(lubridate)

#use https://www.freeconvert.com/pdf-to-text to convert pdfs, then trim headings and footings

rawpath <- "~/EU_capacity/raw/Staff/"

nat11 <- read.csv(
  paste0(rawpath, "2011-Europa_SP2_BS_Nat_x_DG_201112.csv"),
  header = FALSE
)
nat11 <- rbind(nat11, c("date", rep("2011-12-01", length(nat11) - 1)))

nat12 <- read.csv(
  paste0(rawpath, "2012-Europa_SP2_BS_Nat_x_DG_201212.csv"),
  header = FALSE
)
nat12 <- rbind(nat12, c("date", rep("2012-12-01", length(nat12) - 1)))

nat13 <- read.csv(
  paste0(rawpath, "2013-Europa_SP2_BS_Nat_x_DG_201312.csv"),
  header = FALSE
)
nat13 <- rbind(nat13, c("date", rep("2013-12-01", length(nat13) - 1)))

nat14 <- read.csv(
  paste0(rawpath, "2014-Europa_SP2_BS_Nat_x_DG_201412.csv"),
  header = FALSE
)
nat14 <- rbind(nat14, c("date", rep("2014-12-01", length(nat14) - 1)))

nat15 <- read.csv(
  paste0(rawpath, "01012015 Statistical-Bulletin.csv"),
  header = FALSE
)
nat15 <- rbind(nat15, c("date", rep("2015-01-01", length(nat15) - 1)))

nat16 <- read.csv(
  paste0(rawpath, "Statistical-Bulleting 20160101.csv"),
  header = FALSE
)
nat16 <- rbind(nat16, c("date", rep("2016-01-01", length(nat16) - 1)))

nat17 <- read.csv(
  paste0(rawpath, "Statistical-Bulletin 20170101-18.csv"),
  header = FALSE
)
nat17 <- rbind(nat17, c("date", rep("2017-01-01", length(nat17) - 1)))

nat18_1 <- read.csv(
  paste0(rawpath, "01012018_NationalityAndDG.csv"),
  header = FALSE
)
nat18_1 <- rbind(nat18_1, c("date", rep("2018-01-01", length(nat18_1) - 1)))

nat18_10 <- read.csv(
  paste0(rawpath, "NationalityAndDG2018.csv"),
  header = FALSE
)
nat18_10 <- rbind(nat18_10, c("date", rep("2018-10-01", length(nat18_10) - 1)))

nat19_1 <- read.csv(
  paste0(rawpath, "20190101_NationalityAndDG.csv"),
  header = FALSE
)
nat19_1 <- rbind(nat19_1, c("date", rep("2019-01-01", length(nat19_1) - 1)))

nat19_4 <- read.csv(
  paste0(rawpath, "20190401_NationalityAndDG.csv"),
  header = FALSE
)
nat19_4 <- rbind(nat19_4, c("date", rep("2019-04-03", length(nat19_1) - 1)))

nat20 <- read.csv(
  paste0(rawpath, "01072020_NationalityAndDG.csv"),
  header = FALSE
)
nat20 <- rbind(nat20, c("date", rep("2020-07-01", length(nat20) - 1)))

nat21_1 <- read.csv(
  paste0(rawpath, "01012021_NationalityAndDG.csv"),
  header = FALSE
)
nat21_1 <- rbind(nat21_1, c("date", rep("2021-01-01", length(nat21_1) - 1)))

nat21_4 <- read.csv(
  paste0(rawpath, "01042021_NationalityAndDG.csv"),
  header = FALSE
)
nat21_4 <- rbind(nat21_4, c("date", rep("2021-04-01", length(nat21_4) - 1)))

nat21_7 <- read.csv(
  paste0(rawpath, "01072021_NationalityAndDG.csv"),
  header = FALSE
)
nat21_7 <- rbind(nat21_7, c("date", rep("2021-07-01", length(nat21_7) - 1)))

nat21_10 <- read.csv(
  paste0(rawpath, "01102021_NationalityAndDG.csv"),
  header = FALSE
)
nat21_10 <- rbind(nat21_10, c("date", rep("2021-10-01", length(nat21_10) - 1)))

nat22_1 <- read.csv(
  paste0(rawpath, "01012022_NationalityAndDG.csv"),
  header = FALSE
)
nat22_1 <- rbind(nat22_1, c("date", rep("2022-01-01", length(nat22_1) - 1)))

nat22_4 <- read.csv(
  paste0(rawpath, "01042022_NationalityAndDG.csv"),
  header = FALSE
)
nat22_4 <- rbind(nat22_4, c("date", rep("2022-04-01", length(nat22_4) - 1)))

nat22_7 <- read.csv(
  paste0(rawpath, "01072022_NationalityAndDG.csv"),
  header = FALSE
)
nat22_7 <- rbind(nat22_7, c("date", rep("2022-07-01", length(nat22_7) - 1)))

nat22_10 <- read.csv(
  paste0(rawpath, "01102022_NationalityAndDG.csv"),
  header = FALSE
)
nat22_10 <- rbind(nat22_10, c("date", rep("2022-10-01", length(nat22_10) - 1)))

nat23_1 <- read.csv(
  paste0(rawpath, "01012023_NationalityAndDG.csv"),
  header = FALSE
)
nat23_1 <- rbind(nat23_1, c("date", rep("2023-01-01", length(nat23_1) - 1)))

# data with 3 letter codes
c3 <- list(
  nat11,
  nat12,
  nat13,
  nat14,
  nat15,
  nat16,
  nat17,
  nat18_1,
  nat18_10,
  nat19_1,
  nat19_4,
  nat20
)

# data with 2 letter codes
c2 <- list(
  nat21_1,
  nat21_4,
  nat21_7,
  nat21_10,
  nat22_1,
  nat22_4,
  nat22_7,
  nat22_10,
  nat23_1
)

# loop to apply changes
c3_new <- lapply(c3, function(x) {
  x <- as.data.frame(t(x))
  colnames(x) <- x[1, ]
  c3 <- x[, 1]
  z <- lapply(x[, 3:ncol(x) - 1], as.numeric)
  date <- x[, ncol(x)]
  x <- cbind(as.data.frame(c3), as.data.frame(z), as.data.frame(date))
  x <- x[-1, ]
})

c2_new <- lapply(c2, function(x) {
  x <- as.data.frame(t(x))
  colnames(x) <- x[1, ]
  c2 <- x[, 1]
  z <- lapply(x[, 3:ncol(x) - 1], as.numeric)
  date <- x[, ncol(x)]
  x <- cbind(as.data.frame(c2), as.data.frame(z), as.data.frame(date))
  x <- x[-1, ]
})

# Combine the modified data frames, c3
staff_c3 <- do.call(rbind.fill, c3_new)
staff_c3$date <- as.Date(staff_c3$date)

iso_alpha2 <- read.csv("~/EU_capacity/raw/Staff/iso_alpha2.csv")
iso_alpha3 <- read.csv("~/EU_capacity/raw/Staff/iso_alpha3.csv")

staff_c3_names <- merge(
  x = staff_c3,
  y = iso_alpha3,
  by.x = "c3",
  by.y = "code",
  all.x = TRUE
)
#test[is.na(test$country),] #to check merge

c3_char <- subset(staff_c3_names, select = c("c3", "date", "country"))
c3_num <- select(staff_c3_names, -one_of(c("c3", "date", "country")))
c3_num$total <- rowSums(c3_num, na.rm = TRUE)

staff_c3_names <- cbind(c3_char, c3_num)
staff_c3_names <- subset(staff_c3_names, c3 != "Total")

#print(cbind(staff_c3_names$country, staff_c3_names$total))

# Combine the modified data frames
staff_c2 <- do.call(rbind.fill, c2_new)
staff_c2$date <- as.Date(staff_c2$date)
staff_c2$ECFIN <- as.numeric(staff_c2$ECFIN)

staff_c2_names <- merge(
  x = staff_c2,
  y = iso_alpha2,
  by.x = "c2",
  by.y = "code",
  all.x = TRUE
)
# test <- staff_c2_names[is.na(staff_c2_names$country),] #to check merge

c2_char <- subset(staff_c2_names, select = c("c2", "date", "country"))
c2_num <- select(
  staff_c2_names,
  -one_of(c("c2", "date", "country", "year", "cctld", "notes"))
)
c2_num$total <- rowSums(c2_num, na.rm = TRUE)

staff_c2_names <- cbind(c2_char, c2_num)

staff <- rbind.fill(staff_c3_names, staff_c2_names)
staff$country[is.na(staff$country)] <- "Other"

staff$year <- year(ymd(staff$date))
staff$month <- month(ymd(staff$date))
staff$spring <- 0
staff$spring[staff$month <= 6] <- 1

staff[is.na(staff)] <- 0

staff$X. <- NULL
staff$c.NA_real_..NA_real_..NA_real_..NA_real_..NA_real_..NA_real_.. <- NULL
staff$c2 <- NULL
staff$IDEA[staff$year <= 2013] <- staff$BEPA[staff$year <= 2013]
staff$BEPA <- NULL
staff$IDEA[staff$year <= 2019 & staff$year >= 2014] <- staff$EPSC[
  staff$year <= 2019 & staff$year >= 2014
]
staff$EPSC <- NULL
staff$NEAR[staff$year <= 2014] <- staff$ELARG[staff$year <= 2014]
staff$ELARG <- NULL
staff$SANTE[staff$year <= 2014] <- staff$SANCO[staff$year <= 2014]
staff$SANCO <- NULL
staff$Total <- NULL

staff <- staff %>% dplyr::rename(COLLEGE = COLLÈGE)

save(staff, file = "~/EU_capacity/data/Commission_nationalities.Rdata")
write.csv(
  staff,
  file = "~/EU_capacity/data/Commission_nationalities.csv",
  row.names = FALSE
)
