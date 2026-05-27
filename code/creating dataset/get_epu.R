library(rvest)
library(dplyr)

base_url <- "https://www.policyuncertainty.com/"
outdir   <- "~/EU_capacity/raw/EPU/"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# Countries in dfpg with known pages on policyuncertainty.com.
# europe_monthly.html contains Germany, Italy, UK, France, and Spain in a
# single xlsx file, so it only needs to be downloaded once.
# Countries with no EPU page on the site are listed in the comment below.
country_pages <- c(
  "Belgium"        = "belgium_monthly.html",
  "Croatia"        = "croatia_monthly.html",
  "Denmark"        = "denmark_monthly.html",
  "France"         = "europe_monthly.html",
  "Germany"        = "europe_monthly.html",
  "Greece"         = "FKT_Monthly.html",
  "Ireland"        = "ireland_zalla.html",
  "Italy"          = "europe_monthly.html",
  "Netherlands"    = "netherlands_monthly.html",
  "Poland"         = "poland_monthly.html",
  "Portugal"       = "portugal_monthly.html",
  "Spain"          = "europe_monthly.html",
  "Sweden"         = "sweden_monthly.html",
  "United Kingdom" = "europe_monthly.html"
)

# EU countries in dfpg with no EPU page on policyuncertainty.com:
# Austria, Bulgaria, Cyprus, Czechia, Estonia, Finland, Hungary, Latvia,
# Lithuania, Luxembourg, Malta, Romania, Slovakia, Slovenia

# Scrape each unique page once and download all xlsx files found on it
unique_pages <- unique(country_pages)

for (page in unique_pages) {
  page_url <- paste0(base_url, page)
  message("Scraping: ", page_url)

  html <- tryCatch(
    read_html(page_url),
    error = function(e) { message("  Failed to load page: ", e$message); NULL }
  )
  if (is.null(html)) next

  xlsx_hrefs <- html %>%
    html_elements("a") %>%
    html_attr("href") %>%
    .[grepl("\\.(xlsx|xls|csv)$", ., ignore.case = TRUE)]

  if (length(xlsx_hrefs) == 0) {
    message("  No xlsx/csv links found on ", page)
    next
  }

  for (href in xlsx_hrefs) {
    file_url <- paste0(base_url, href)
    destfile <- file.path(outdir, basename(href))

    if (file.exists(destfile)) {
      message("  Already exists: ", basename(href))
      next
    }

    message("  Downloading: ", basename(href))
    tryCatch(
      download.file(file_url, destfile = destfile, mode = "wb", quiet = TRUE),
      error = function(e) message("  Download failed: ", e$message)
    )
  }
}

message("Done. Files saved to ", outdir)
