# EU member states in the analysis sample, 2011-2023.
#
# Defined once and sourced wherever the sample is restricted or asserted, so the
# definition cannot drift between scripts.
#
# Names are the post-standardisation AMECO labels -- in particular "Czechia",
# not "Czech Republic" (AMECO renamed it in the Spring 2019 release; see
# standardize_country() in clean_data15_23.R).
#
# The United Kingdom is included: it is a member state for most of the panel and
# appears in the forecast vintages through 2022.5, though it has no
# estimation-eligible observations after ysp 2020.5.

EU_MEMBERS <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
  "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
  "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
  "Slovenia", "Spain", "Sweden", "United Kingdom"
)
