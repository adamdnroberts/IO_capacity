#options(scipen = 999)

library(dplyr)

s11_full <- read.csv(
  "raw/Projections/Spring 2011/AMECO16.TXT",
  sep = ";"
)
s11_full$new2012 <- as.numeric(s11_full$X2012)

# create Spring 2011 predictions dataset
s11nt <- data.frame(
  'country' = s11_full$COUNTRY,
  'code' = s11_full$CODE,
  'title' = s11_full$TITLE,
  'subchapter' = s11_full$SUB.CHAPTER,
  'unit' = s11_full$UNIT,
  'date' = as.Date("2011-05-05"), #ymd
  'spring' = 1,
  'year' = 2011,
  'p0' = s11_full$X2011,
  'p1' = s11_full$new2012
)

a11_full <- read.csv(
  "raw/Projections/Autumn 2011/AMECO16.TXT",
  sep = ";"
)
a11_full$new2013 <- as.numeric(a11_full$X2013)

a11nt <- data.frame(
  'country' = a11_full$COUNTRY,
  'code' = a11_full$CODE,
  'title' = a11_full$TITLE,
  'subchapter' = a11_full$SUB.CHAPTER,
  'unit' = a11_full$UNIT,
  'date' = as.Date("2011-11-10"), #ymd
  'spring' = 0,
  'year' = 2011,
  'p0' = a11_full$X2011,
  'p1' = a11_full$X2012,
  'p2' = a11_full$new2013
)

s12_full <- read.csv(
  "raw/Projections/Spring 2012/AMECO16.TXT",
  sep = ";"
)
s12_full$new2013 <- as.numeric(s12_full$X2013)

s12nt <- data.frame(
  'country' = s12_full$COUNTRY,
  'code' = s12_full$CODE,
  'title' = s12_full$TITLE,
  'subchapter' = s12_full$SUB.CHAPTER,
  'unit' = s12_full$UNIT,
  'date' = as.Date("2012-05-08"), #ymd
  'spring' = 1,
  'year' = 2012,
  'p0' = s12_full$X2012,
  'p1' = s12_full$new2013
)

a12_full <- read.csv(
  "raw/Projections/Autumn 2012/AMECO16.TXT",
  sep = ";"
)
a12_full$new2013 <- as.numeric(a12_full$X2013)

a12nt <- data.frame(
  'country' = a12_full$COUNTRY,
  'code' = a12_full$CODE,
  'title' = a12_full$TITLE,
  'subchapter' = a12_full$SUB.CHAPTER,
  'unit' = a12_full$UNIT,
  'date' = as.Date("2012-11-08"), #ymd
  'spring' = 0,
  'year' = 2012,
  'p0' = a12_full$X2012,
  'p1' = a12_full$new2013
)

#Spring 2013
s13_full <- read.csv(
  "raw/Projections/Spring 2013/AMECO16.TXT",
  sep = ";"
)
s13_full$new2013 <- as.numeric(s13_full$X2013)

s13nt <- data.frame(
  'country' = s13_full$COUNTRY,
  'code' = s13_full$CODE,
  'title' = s13_full$TITLE,
  'subchapter' = s13_full$SUB.CHAPTER,
  'unit' = s13_full$UNIT,
  'date' = as.Date("2013-05-08"), #ymd
  'spring' = 1,
  'year' = 2013,
  'p0' = s13_full$new2013
)

a13_full <- read.csv(
  "raw/Projections/Autumn 2013/AMECO16.TXT",
  sep = ";"
)
a13_full$new2013 <- as.numeric(a13_full$X2013)

a13nt <- data.frame(
  'country' = a13_full$COUNTRY,
  'code' = a13_full$CODE,
  'title' = a13_full$TITLE,
  'subchapter' = a13_full$SUB.CHAPTER,
  'unit' = a13_full$UNIT,
  'date' = as.Date("2013-11-07"), #ymd
  'spring' = 0,
  'year' = 2013,
  'p0' = a13_full$new2013
)

s14_full <- read.csv(
  "raw/Projections/Spring 2014/AMECO16.TXT",
  sep = ";"
)
s14_full$new2015 <- as.numeric(s14_full$X2015)

s14nt <- data.frame(
  'country' = s14_full$COUNTRY,
  'code' = s14_full$CODE,
  'title' = s14_full$TITLE,
  'subchapter' = s14_full$SUB.CHAPTER,
  'unit' = s14_full$UNIT,
  'date' = as.Date("2014-05-08"), #ymd
  'spring' = 1,
  'year' = 2014,
  'p0' = s14_full$X2014,
  'p1' = s14_full$new2015
)

a14_full <- read.csv(
  "raw/Projections/Autumn 2014/AMECO16.TXT",
  sep = ";"
)
a14_full$new2016 <- as.numeric(a14_full$X2016)

a14nt <- data.frame(
  'country' = a14_full$COUNTRY,
  'code' = a14_full$CODE,
  'title' = a14_full$TITLE,
  'subchapter' = a14_full$SUB.CHAPTER,
  'unit' = a14_full$UNIT,
  'date' = as.Date("2014-11-06"), #ymd
  'spring' = 0,
  'year' = 2014,
  'p0' = a14_full$X2014,
  'p1' = a14_full$X2015,
  'p2' = a14_full$new2016
)

# Spring 2024 is the same release that supplies the actuals for every 2015-2023
# target in clean_data15_23.R (`true3`), so using it here puts all ESA 2010
# targets on a single benchmark.
#
# This replaces the Autumn 2017 release, which used to supply the 2014-2016
# actuals. Autumn 2017 was the newest vintage available when this script was
# written and is the earliest release in which 2016 is an outturn rather than a
# forecast, so it was a reasonable choice at the time. But it left the panel
# with three truth vintages where the paper describes two, and -- because target
# years 2015 and 2016 are reached both from the Autumn 2014 vintage and from the
# 2015+ vintages -- it gave 87% of the country x title cells for those years two
# different `true` values depending on which forecast round they came from,
# partially confounding the benchmark with the forecast horizon.
s24_full <- read.csv(
  "raw/Projections/Spring 2024/AMECO16.TXT",
  sep = ";"
)

# AMECO renamed "Czech Republic" to "Czechia" in the Spring 2019 release, so the
# Autumn 2014 vintage carries the old label and Spring 2024 the new one. Since
# `country` is part of the merge key, without this every Czech row would match
# nothing and be dropped by merge()'s default inner join.
standardize_country <- function(x) {
  x <- trimws(as.character(x))
  x[x == "Czech Republic"] <- "Czechia"
  x
}
a14nt$country <- standardize_country(a14nt$country)

#true values from s14
true1 <- data.frame(
  'country' = s14_full$COUNTRY,
  'code' = s14_full$CODE,
  'title' = s14_full$TITLE,
  'unit' = s14_full$UNIT,
  'true11' = s14_full$X2011,
  'true12' = s14_full$X2012,
  'true13' = s14_full$X2013
)

#true values from s24
true2 <- data.frame(
  'country' = standardize_country(s24_full$COUNTRY),
  'code' = s24_full$CODE,
  'title' = s24_full$TITLE,
  'unit' = s24_full$UNIT,
  'true14' = s24_full$X2014,
  'true15' = s24_full$X2015,
  'true16' = s24_full$X2016
)

## MERGING

# Define a function to reduce redundancy in merging and summarizing
merge_and_summarize <- function(s_nt_df, a_nt_df, true_df, suffixes) {
  s_merged_df <- merge(
    s_nt_df,
    true_df,
    by = c("country", "title", "unit", "code")
  )
  a_merged_df <- merge(
    a_nt_df,
    true_df,
    by = c("country", "title", "unit", "code")
  )
  y <- bind_rows(s_merged_df, a_merged_df)
  if (suffixes[2] == "") {
    y <- y %>%
      dplyr::rename(true0 = !!paste0("true", suffixes[1]))
  } else if (suffixes[3] == "") {
    y <- y %>%
      dplyr::rename(
        true0 = !!paste0("true", suffixes[1]),
        true1 = !!paste0("true", suffixes[2])
      )
  } else {
    y <- y %>%
      dplyr::rename(
        true0 = !!paste0("true", suffixes[1]),
        true1 = !!paste0("true", suffixes[2]),
        true2 = !!paste0("true", suffixes[3])
      )
  }
  return(y)
}

# Merge and summarize for s11 and a11
y11 <- merge_and_summarize(s11nt, a11nt, true1, c("11", "12", "13"))

# Merge and summarize for s12 and a12
y12 <- merge_and_summarize(s12nt, a12nt, true1, c("12", "13", ""))

# Merge and summarize for s13 and a13
y13 <- merge_and_summarize(s13nt, a13nt, true1, c("13", "", ""))

# Merge and summarize for a14. Spring 2014 is deliberately excluded; see below.
a14 <- merge(a14nt, true2, by = c("country", "title", "unit", "code"))
y14 <- a14 %>% rename(true0 = true14, true1 = true15, true2 = true16)

# No EU member state may vanish in that merge. This is the guard that would have
# caught the Czech drop described above.
eu_in_both <- intersect(
  intersect(a14nt$country, true2$country),
  c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
    "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
    "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
    "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
    "Slovenia", "Spain", "Sweden", "United Kingdom")
)
stopifnot(all(eu_in_both %in% y14$country))

# Why Spring 2014 (s14nt) is not in the panel, leaving ysp 2014 empty.
#
# AMECO switched from ESA 1995 to ESA 2010 in the Autumn 2014 release, so
# Spring 2014 is the last ESA 1995 vintage. Its titles carry the ":- ESA 1995"
# suffix: it shares all 47 title strings with the 2011-2013 vintages but only 5
# with Autumn 2014 onward.
#
# Its forecasts target 2014 and 2015 -- years for which no ESA 1995 outturn
# exists. true1 is built from the Spring 2014 file itself and so cannot contain
# them, and every subsequent release is ESA 2010. Scoring these forecasts would
# mean comparing ESA 1995 projections against ESA 2010 actuals, which measures
# an accounting-definition change as though it were forecast error. That is
# precisely what the two-database design avoids: true1 (Spring 2014, ESA 1995)
# for the 2011-2013 targets, and the later ESA 2010 releases thereafter.
#
# The file is still read above, because s14_full is the source of true1.

full_dataset11_14 <- bind_rows(y11, y12, y13, y14)
#df_full$true14 <- NULL
#df_full$true15 <- NULL

#write.csv(df_full, file = "data/full_dataset_13", row.names = FALSE)
save(full_dataset11_14, file = "data/full_dataset11_14.Rdata")
