#options(scipen = 999)

library(dplyr)

s11_full <- read.csv("~/ec_project/raw/Projections/Spring 2011/AMECO16.TXT", sep=";")
s11_full$new2012 <- as.numeric(s11_full$X2012)

# create Spring 2011 predictions dataset
s11nt <- data.frame('country' = s11_full$COUNTRY,
                    'code' = s11_full$CODE,
                    'title' = s11_full$TITLE,
                    'subchapter' = s11_full$SUB.CHAPTER,
                    'unit' = s11_full$UNIT,
                    'date' = as.Date("2011-05-05"), #ymd
                    'spring' = 1,
                    'year' = 2011,
                    'p0' = s11_full$X2011,
                    'p1' = s11_full$new2012)

a11_full <- read.csv("~/ec_project/raw/Projections/Autumn 2011/AMECO16.TXT", sep=";")
a11_full$new2013 <- as.numeric(a11_full$X2013)

a11nt <- data.frame('country' = a11_full$COUNTRY,
                    'code' = a11_full$CODE,
                    'title' = a11_full$TITLE,
                    'subchapter' = a11_full$SUB.CHAPTER,
                    'unit' = a11_full$UNIT,
                    'date' = as.Date("2011-11-10"), #ymd
                    'spring' = 0,
                    'year' = 2011,
                    'p0' = a11_full$X2011,
                    'p1' = a11_full$X2012,
                    'p2' = a11_full$new2013)

s12_full <- read.csv("~/ec_project/raw/Projections/Spring 2012/AMECO16.TXT", sep=";")
s12_full$new2013 <- as.numeric(s12_full$X2013)

s12nt <- data.frame('country' = s12_full$COUNTRY,
                    'code' = s12_full$CODE,
                    'title' = s12_full$TITLE,
                    'subchapter' = s12_full$SUB.CHAPTER,
                    'unit' = s12_full$UNIT,
                    'date' = as.Date("2012-05-08"), #ymd
                    'spring' = 1,
                    'year' = 2012,
                    'p0' = s12_full$X2012,
                    'p1' = s12_full$new2013)

a12_full <- read.csv("~/ec_project/raw/Projections/Autumn 2012/AMECO16.TXT", sep=";")
a12_full$new2013 <- as.numeric(a12_full$X2013)

a12nt <- data.frame('country' = a12_full$COUNTRY,
                    'code' = a12_full$CODE,
                    'title' = a12_full$TITLE,
                    'subchapter' = a12_full$SUB.CHAPTER,
                    'unit' = a12_full$UNIT,
                    'date' = as.Date("2012-11-08"), #ymd
                    'spring' = 0,
                    'year' = 2012,
                    'p0' = a12_full$X2012,
                    'p1' = a12_full$new2013)

#Spring 2013
s13_full <- read.csv("~/ec_project/raw/Projections/Spring 2013/AMECO16.TXT", sep=";")
s13_full$new2013 <- as.numeric(s13_full$X2013)

s13nt <- data.frame('country' = s13_full$COUNTRY,
                    'code' = s13_full$CODE,
                    'title' = s13_full$TITLE,
                    'subchapter' = s13_full$SUB.CHAPTER,
                    'unit' = s13_full$UNIT,
                    'date' = as.Date("2013-05-08"), #ymd
                    'spring' = 1,
                    'year' = 2013,
                    'p0' = s13_full$new2013)

a13_full <- read.csv("~/ec_project/raw/Projections/Autumn 2013/AMECO16.TXT", sep=";")
a13_full$new2013 <- as.numeric(a13_full$X2013)

a13nt <- data.frame('country' = a13_full$COUNTRY,
                    'code' = a13_full$CODE,
                    'title' = a13_full$TITLE,
                    'subchapter' = a13_full$SUB.CHAPTER,
                    'unit' = a13_full$UNIT,
                    'date' = as.Date("2013-11-07"), #ymd
                    'spring' = 0,
                    'year' = 2013,
                    'p0' = a13_full$new2013)

s14_full <- read.csv("~/ec_project/raw/Projections/Spring 2014/AMECO16.TXT", sep=";")
s14_full$new2015 <- as.numeric(s14_full$X2015)

s14nt <- data.frame('country' = s14_full$COUNTRY,
                    'code' = s14_full$CODE,
                    'title' = s14_full$TITLE,
                    'subchapter' = s14_full$SUB.CHAPTER,
                    'unit' = s14_full$UNIT,
                    'date' = as.Date("2014-05-08"), #ymd
                    'spring' = 1,
                    'year' = 2014,
                    'p0' = s14_full$X2014,
                    'p1' = s14_full$new2015)

a14_full <- read.csv("~/ec_project/raw/Projections/Autumn 2014/AMECO16.TXT", sep=";")
a14_full$new2016 <- as.numeric(a14_full$X2016)

a14nt <- data.frame('country' = a14_full$COUNTRY,
                    'code' = a14_full$CODE,
                    'title' = a14_full$TITLE,
                    'subchapter' = a14_full$SUB.CHAPTER,
                    'unit' = a14_full$UNIT,
                    'date' = as.Date("2014-11-06"), #ymd
                    'spring' = 0,
                    'year' = 2014,
                    'p0' = a14_full$X2014,
                    'p1' = a14_full$X2015,
                    'p2' = a14_full$new2016)

a17_full <- read.csv("~/ec_project/raw/Projections/Autumn 2017/AMECO16.TXT", sep=";")
a17_full$new2019 <- as.numeric(a17_full$X2019)

#true values from s14
true1 <- data.frame('country' = s14_full$COUNTRY,
                    'code' = s14_full$CODE,
                    'title' = s14_full$TITLE,
                    'unit' = s14_full$UNIT,
                    'true11' = s14_full$X2011,
                    'true12' = s14_full$X2012,
                    'true13' = s14_full$X2013)

#true values from a17
true2 <- data.frame('country' = a17_full$COUNTRY,
                    'code' = a17_full$CODE,
                    'title' = a17_full$TITLE,
                    'unit' = a17_full$UNIT,
                    'true14' = a17_full$X2014,
                    'true15' = a17_full$X2015,
                    'true16' = a17_full$X2016)

## MERGING

# Define a function to reduce redundancy in merging and summarizing
merge_and_summarize <- function(s_nt_df, a_nt_df, true_df, suffixes) {
  s_merged_df <- merge(s_nt_df, true_df, by = c("country", "title", "unit", "code"))
  a_merged_df <- merge(a_nt_df, true_df, by = c("country", "title", "unit", "code"))
  y <- bind_rows(s_merged_df, a_merged_df)
  if (suffixes[2] == "") {
    y <- y %>%
      dplyr::rename(true0 = !!paste0("true", suffixes[1]))
  } else if (suffixes[3] == "") {
    y <- y %>%
      dplyr::rename(true0 = !!paste0("true", suffixes[1]),
                    true1 = !!paste0("true", suffixes[2]))
  } else {
    y <- y %>%
      dplyr::rename(true0 = !!paste0("true", suffixes[1]),
                    true1 = !!paste0("true", suffixes[2]),
                    true2 = !!paste0("true", suffixes[3]))
  }
  return(y)
}

# Merge and summarize for s11 and a11
y11 <- merge_and_summarize(s11nt, a11nt, true1, c("11", "12", "13"))

# Merge and summarize for s12 and a12
y12 <- merge_and_summarize(s12nt, a12nt, true1, c("12", "13", ""))

# Merge and summarize for s13 and a13
y13 <- merge_and_summarize(s13nt, a13nt, true1, c("13", "", ""))

# Merge and summarize for a14 (s14 commented out)
a14 <- merge(a14nt, true2, by = c("country", "title", "unit", "code"))
y14 <- a14 %>% rename(true0 = true14, true1 = true15, true2 = true16)

full_dataset11_14 <- bind_rows(y11, y12, y13, y14)
#df_full$true14 <- NULL
#df_full$true15 <- NULL

#write.csv(df_full, file = "~/ec_project/data/full_dataset_13", row.names = FALSE)
save(full_dataset11_14,file="~/ec_project/data/full_dataset11_14.Rdata")
