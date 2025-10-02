#options(scipen = 999)

library(dplyr)

s15_full <- read.csv(
  "~/ec_project/raw/Projections/Spring 2015/AMECO16.TXT",
  sep = ";"
)
s15_full$new2016 <- as.numeric(s15_full$X2016)

s15nt <- data.frame(
  'country' = s15_full$COUNTRY,
  'code' = s15_full$CODE,
  'title' = s15_full$TITLE,
  'subchapter' = s15_full$SUB.CHAPTER,
  'unit' = s15_full$UNIT,
  'date' = as.Date("2015-05-07"), #ymd
  'spring' = 1,
  'year' = 2015,
  'p0' = s15_full$X2015,
  'p1' = s15_full$new2016
)

a15_full <- read.csv(
  "~/ec_project/raw/Projections/Autumn 2015/AMECO16.TXT",
  sep = ";"
)
a15_full$new2017 <- as.numeric(a15_full$X2017)

a15nt <- data.frame(
  'country' = a15_full$COUNTRY,
  'code' = a15_full$CODE,
  'title' = a15_full$TITLE,
  'subchapter' = a15_full$SUB.CHAPTER,
  'unit' = a15_full$UNIT,
  'date' = as.Date("2015-11-05"), #ymd
  'spring' = 0,
  'year' = 2015,
  'p0' = a15_full$X2015,
  'p1' = a15_full$X2016,
  'p2' = a15_full$new2017
)

s16_full <- read.csv(
  "~/ec_project/raw/Projections/Spring 2016/AMECO16.TXT",
  sep = ";"
)
s16_full$new2017 <- as.numeric(s16_full$X2017)

s16nt <- data.frame(
  'country' = s16_full$COUNTRY,
  'code' = s16_full$CODE,
  'title' = s16_full$TITLE,
  'subchapter' = s16_full$SUB.CHAPTER,
  'unit' = s16_full$UNIT,
  'date' = as.Date("2016-05-03"), #ymd
  'spring' = 1,
  'year' = 2016,
  'p0' = s16_full$X2016,
  'p1' = s16_full$new2017
)

a16_full <- read.csv(
  "~/ec_project/raw/Projections/Autumn 2016/AMECO16.TXT",
  sep = ";"
)
a16_full$new2018 <- as.numeric(a16_full$X2018)

a16nt <- data.frame(
  'country' = a16_full$COUNTRY,
  'code' = a16_full$CODE,
  'title' = a16_full$TITLE,
  'subchapter' = a16_full$SUB.CHAPTER,
  'unit' = a16_full$UNIT,
  'date' = as.Date("2016-11-09"), #ymd
  'spring' = 0,
  'year' = 2016,
  'p0' = a16_full$X2016,
  'p1' = a16_full$X2017,
  'p2' = a16_full$new2018
)

s17_full <- read.csv(
  "~/ec_project/raw/Projections/Spring 2017/AMECO16.TXT",
  sep = ";"
)
s17_full$new2018 <- as.numeric(s17_full$X2018)

s17nt <- data.frame(
  'country' = s17_full$COUNTRY,
  'code' = s17_full$CODE,
  'title' = s17_full$TITLE,
  'subchapter' = s17_full$SUB.CHAPTER,
  'unit' = s17_full$UNIT,
  'date' = as.Date("2017-05-11"), #ymd
  'spring' = 1,
  'year' = 2017,
  'p0' = s17_full$X2017,
  'p1' = s17_full$new2018
)

a17_full <- read.csv(
  "~/ec_project/raw/Projections/Autumn 2017/AMECO16.TXT",
  sep = ";"
)
a17_full$new2019 <- as.numeric(a17_full$X2019)

a17nt <- data.frame(
  'country' = a17_full$COUNTRY,
  'code' = a17_full$CODE,
  'title' = a17_full$TITLE,
  'subchapter' = a17_full$SUB.CHAPTER,
  'unit' = a17_full$UNIT,
  'date' = as.Date("2017-11-09"), #ymd
  'spring' = 0,
  'year' = 2017,
  'p0' = a17_full$X2017,
  'p1' = a17_full$X2018,
  'p2' = a17_full$new2019
)

s18_full <- read.csv(
  "~/ec_project/raw/Projections/Spring 2018/AMECO16.TXT",
  sep = ";"
)
s18_full$new2019 <- as.numeric(s18_full$X2019)

s18nt <- data.frame(
  'country' = s18_full$COUNTRY,
  'code' = s18_full$CODE,
  'title' = s18_full$TITLE,
  'subchapter' = s18_full$SUB.CHAPTER,
  'unit' = s18_full$UNIT,
  'date' = as.Date("2018-05-03"), #ymd
  'spring' = 1,
  'year' = 2018,
  'p0' = s18_full$X2018,
  'p1' = s18_full$new2019
)

a18_full <- read.csv(
  "~/ec_project/raw/Projections/Autumn 2018/AMECO16.TXT",
  sep = ";"
)
a18_full$new2020 <- as.numeric(a18_full$X2020)

a18nt <- data.frame(
  'country' = a18_full$COUNTRY,
  'code' = a18_full$CODE,
  'title' = a18_full$TITLE,
  'subchapter' = a18_full$SUB.CHAPTER,
  'unit' = a18_full$UNIT,
  'date' = as.Date("2018-11-08"), #ymd
  'spring' = 0,
  'year' = 2018,
  'p0' = a18_full$X2018,
  'p1' = a18_full$X2019,
  'p2' = a18_full$new2020
)

s19_full <- read.csv(
  "~/ec_project/raw/Projections/Spring 2019/AMECO16.TXT",
  sep = ";"
)
s19_full$new2020 <- as.numeric(s19_full$X2020)

s19nt <- data.frame(
  'country' = s19_full$COUNTRY,
  'code' = s19_full$CODE,
  'title' = s19_full$TITLE,
  'subchapter' = s19_full$SUB.CHAPTER,
  'unit' = s19_full$UNIT,
  'date' = as.Date("2019-05-07"), #ymd
  'spring' = 1,
  'year' = 2019,
  'p0' = s19_full$X2019,
  'p1' = s19_full$new2020
)

a19_full <- read.csv(
  "~/ec_project/raw/Projections/Autumn 2019/AMECO16.TXT",
  sep = ";"
)
a19_full$new2021 <- as.numeric(a19_full$X2021)

a19nt <- data.frame(
  'country' = a19_full$COUNTRY,
  'code' = a19_full$CODE,
  'title' = a19_full$TITLE,
  'subchapter' = a19_full$SUB.CHAPTER,
  'unit' = a19_full$UNIT,
  'date' = as.Date("2019-11-07"), #ymd
  'spring' = 0,
  'year' = 2019,
  'p0' = a19_full$X2019,
  'p1' = a19_full$X2020,
  'p2' = a19_full$new2021
)

s20_full <- read.csv(
  "~/ec_project/raw/Projections/Spring 2020/AMECO16.TXT",
  sep = ";"
)
s20_full$new2021 <- as.numeric(s20_full$X2021)

s20nt <- data.frame(
  'country' = s20_full$COUNTRY,
  'code' = s20_full$CODE,
  'title' = s20_full$TITLE,
  'subchapter' = s20_full$SUB.CHAPTER,
  'unit' = s20_full$UNIT,
  'date' = as.Date("2020-05-06"), #ymd
  'spring' = 1,
  'year' = 2020,
  'p0' = s20_full$X2020,
  'p1' = s20_full$new2021
)

a20_full <- read.csv(
  "~/ec_project/raw/Projections/Autumn 2020/AMECO16.TXT",
  sep = ";"
)
a20_full$new2022 <- as.numeric(a20_full$X2022)

a20nt <- data.frame(
  'country' = a20_full$COUNTRY,
  'code' = a20_full$CODE,
  'title' = a20_full$TITLE,
  'subchapter' = a20_full$SUB.CHAPTER,
  'unit' = a20_full$UNIT,
  'date' = as.Date("2020-11-05"), #ymd
  'spring' = 0,
  'year' = 2020,
  'p0' = a20_full$X2020,
  'p1' = a20_full$X2021,
  'p2' = a20_full$new2022
)

s21_full <- read.csv(
  "~/ec_project/raw/Projections/Spring 2021/AMECO16.TXT",
  sep = ";"
)
s21_full$new2022 <- as.numeric(s21_full$X2022)

s21nt <- data.frame(
  'country' = s21_full$COUNTRY,
  'code' = s21_full$CODE,
  'title' = s21_full$TITLE,
  'subchapter' = s21_full$SUB.CHAPTER,
  'unit' = s21_full$UNIT,
  'date' = as.Date("2021-05-12"), #ymd
  'spring' = 1,
  'year' = 2021,
  'p0' = s21_full$X2021,
  'p1' = s21_full$new2022
)

a21_full <- read.csv(
  "~/ec_project/raw/Projections/Autumn 2021/AMECO16.TXT",
  sep = ";"
)
a21_full$new2023 <- as.numeric(a21_full$X2023)

a21nt <- data.frame(
  'country' = a21_full$COUNTRY,
  'code' = a21_full$CODE,
  'title' = a21_full$TITLE,
  'subchapter' = a21_full$SUB.CHAPTER,
  'unit' = a21_full$UNIT,
  'date' = as.Date("2021-11-11"), #ymd
  'spring' = 0,
  'year' = 2021,
  'p0' = a21_full$X2021,
  'p1' = a21_full$X2022,
  'p2' = a21_full$new2023
)

s22_full <- read.csv(
  "~/ec_project/raw/Projections/Spring 2022/AMECO16.TXT",
  sep = ";"
)
s22_full$new2023 <- as.numeric(s22_full$X2023)

s22nt <- data.frame(
  'country' = s22_full$COUNTRY,
  'code' = s22_full$CODE,
  'title' = s22_full$TITLE,
  'subchapter' = s22_full$SUB.CHAPTER,
  'unit' = s22_full$UNIT,
  'date' = as.Date("2022-05-16"), #ymd
  'spring' = 1,
  'year' = 2022,
  'p0' = s22_full$X2022,
  'p1' = s22_full$new2023
)

a22_full <- read.csv(
  "~/ec_project/raw/Projections/Autumn 2022/AMECO16.TXT",
  sep = ";"
)
a22_full$new2024 <- as.numeric(a22_full$X2024)

a22nt <- data.frame(
  'country' = a22_full$COUNTRY,
  'code' = a22_full$CODE,
  'title' = a22_full$TITLE,
  'subchapter' = a22_full$SUB.CHAPTER,
  'unit' = a22_full$UNIT,
  'date' = as.Date("2022-11-17"), #ymd
  'spring' = 0,
  'year' = 2022,
  'p0' = a22_full$X2022,
  'p1' = a22_full$X2023,
  'p2' = a22_full$new2024
)

a22nt$title <- replace(
  a22nt$title,
  a22nt$title == "Total current expenditure: general government :- ESA 2010 ",
  "Total expenditure: general government :- ESA 1995 (Including one-off proceeds (treated as negative expenditure) relative to the allocation of mobile phone licences (UMTS))"
)

s23_full <- read.delim("~/ec_project/raw/Projections/Spring 2023/AMECO16.TXT")
s23_full$new2024 <- as.numeric(s23_full$X2024)

s23nt <- data.frame(
  'country' = s23_full$COUNTRY,
  'code' = s23_full$CODE,
  'title' = s23_full$TITLE,
  'subchapter' = s23_full$SUB.CHAPTER,
  'unit' = s23_full$UNIT,
  'date' = as.Date("2023-05-15"), #ymd
  'spring' = 1,
  'year' = 2023,
  'p0' = s23_full$X2023,
  'p1' = s23_full$new2024
)

s24_full <- read.csv(
  "~/ec_project/raw/Projections/Spring 2024/AMECO16.TXT",
  sep = ";"
)
s24_full$new2025 <- as.numeric(s24_full$X2025)

#true values from s24
true3 <- data.frame(
  'country' = s24_full$COUNTRY,
  'code' = s24_full$CODE,
  'title' = s24_full$TITLE,
  'unit' = s24_full$UNIT,
  'true15' = s24_full$X2015,
  'true16' = s24_full$X2016,
  'true17' = s24_full$X2017,
  'true18' = s24_full$X2018,
  'true19' = s24_full$X2019,
  'true20' = s24_full$X2020,
  'true21' = s24_full$X2021,
  'true22' = s24_full$X2022,
  'true23' = s24_full$X2023
)

#merging
s15 <- merge(s15nt, true3, by = c("country", "title", "unit", "code"))
a15 <- merge(a15nt, true3, by = c("country", "title", "unit", "code"))

y15 <- bind_rows(s15, a15)
y15$true14 <- NULL
y15$true18 <- NULL
y15$true19 <- NULL
y15$true20 <- NULL
y15$true21 <- NULL
y15$true22 <- NULL
y15$true23 <- NULL

y15 <- y15 %>%
  dplyr::rename(true0 = true15, true1 = true16, true2 = true17)

test <- y15$p0 - y15$true0
summary(test)

test <- y15$p1 - y15$true1
summary(test)

test <- y15$p2 - y15$true2
summary(test)

s16 <- merge(s16nt, true3, by = c("country", "title", "unit", "code"))
a16 <- merge(a16nt, true3, by = c("country", "title", "unit", "code"))

y16 <- bind_rows(s16, a16)
y16$true15 <- NULL
y16$true19 <- NULL
y16$true20 <- NULL
y16$true21 <- NULL
y16$true22 <- NULL
y16$true23 <- NULL

#non_merged <- y16[is.na(y16$true16),]
#snm <- subset(non_merged, period == "spring")
#anm <- subset(non_merged, period == "autumn")

y16 <- y16 %>%
  dplyr::rename(true0 = true16, true1 = true17, true2 = true18)

test <- y16$p0 - y16$true0
summary(test)

test <- y16$p1 - y16$true1
summary(test)

test <- y16$p2 - y16$true2
summary(test)

s17 <- merge(s17nt, true3, by = c("country", "title", "unit", "code"))
a17 <- merge(a17nt, true3, by = c("country", "title", "unit", "code"))

y17 <- bind_rows(s17, a17)
y17$true15 <- NULL
y17$true16 <- NULL
y17$true20 <- NULL
y17$true21 <- NULL
y17$true22 <- NULL
y17$true23 <- NULL

y17 <- y17 %>%
  dplyr::rename(true0 = true17, true1 = true18, true2 = true19)

test <- y17$p0 - y17$true0
summary(test)

test <- y17$p1 - y17$true1
summary(test)

test <- y17$p2 - y17$true2
summary(test)

s18 <- merge(s18nt, true3, by = c("country", "title", "unit", "code"))
a18 <- merge(a18nt, true3, by = c("country", "title", "unit", "code"))

y18 <- bind_rows(s18, a18)
y18$true15 <- NULL
y18$true16 <- NULL
y18$true17 <- NULL
y18$true21 <- NULL
y18$true22 <- NULL
y18$true23 <- NULL

y18 <- y18 %>%
  dplyr::rename(true0 = true18, true1 = true19, true2 = true20)

test <- y18$p0 - y18$true0
summary(test)

test <- y18$p1 - y18$true1
summary(test)

test <- y18$p2 - y18$true2
summary(test)

s19 <- merge(s19nt, true3, by = c("country", "title", "unit", "code"))
a19 <- merge(a19nt, true3, by = c("country", "title", "unit", "code"))

y19 <- bind_rows(s19, a19)
y19$true15 <- NULL
y19$true16 <- NULL
y19$true17 <- NULL
y19$true18 <- NULL
y19$true22 <- NULL
y19$true23 <- NULL

y19 <- y19 %>%
  dplyr::rename(true0 = true19, true1 = true20, true2 = true21)

test <- y19$p0 - y19$true0
summary(test)

test <- y19$p1 - y19$true1
summary(test)

test <- y19$p2 - y19$true2
summary(test)

s20 <- merge(s20nt, true3, by = c("country", "title", "unit", "code"))
a20 <- merge(a20nt, true3, by = c("country", "title", "unit", "code"))

y20 <- bind_rows(s20, a20)
y20$true15 <- NULL
y20$true16 <- NULL
y20$true17 <- NULL
y20$true18 <- NULL
y20$true19 <- NULL
y20$true23 <- NULL

y20 <- y20 %>%
  dplyr::rename(true0 = true20, true1 = true21, true2 = true22)

test <- y20$p0 - y20$true0
summary(test)

test <- y20$p1 - y20$true1
summary(test)

test <- y20$p2 - y20$true2
summary(test)

s21 <- merge(s21nt, true3, by = c("country", "title", "unit", "code"))
a21 <- merge(a21nt, true3, by = c("country", "title", "unit", "code"))

y21 <- bind_rows(s21, a21)
y21$true15 <- NULL
y21$true16 <- NULL
y21$true17 <- NULL
y21$true18 <- NULL
y21$true19 <- NULL
y21$true20 <- NULL

y21 <- y21 %>%
  dplyr::rename(true0 = true21, true1 = true22, true2 = true23)

test <- y21$p0 - y21$true0
summary(test)

test <- y21$p1 - y21$true1
summary(test)

test <- y21$p2 - y21$true2
summary(test)

s22 <- merge(s22nt, true3, by = c("country", "title", "unit", "code"))
a22 <- merge(a22nt, true3, by = c("country", "title", "unit", "code"))

y22 <- bind_rows(s22, a22)
y22$true15 <- NULL
y22$true16 <- NULL
y22$true17 <- NULL
y22$true18 <- NULL
y22$true19 <- NULL
y22$true20 <- NULL
y22$true21 <- NULL

y22 <- y22 %>%
  dplyr::rename(true0 = true22, true1 = true23)

test <- y22$p0 - y22$true0
summary(test)

full_dataset15_23 <- bind_rows(y15, y16, y17, y18, y19, y20, y21, y22)

save(full_dataset15_23, file = "~/ec_project/data/full_dataset15_23.Rdata")
