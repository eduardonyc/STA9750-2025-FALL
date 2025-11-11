library(tidyverse)
library(tidycensus)

# 1. Define the NYC county NAMES (this is our new filter list)
nyc_counties <- c("New York", # Manhattan
                  "Kings",    # Brooklyn
                  "Bronx",    # The Bronx
                  "Queens",   # Queens
                  "Richmond"  # Staten Island
)

# Create a regex pattern: "New York|Kings|Bronx|Queens|Richmond"
nyc_pattern <- paste(nyc_counties, collapse = "|")

# 2. Get educational attainment data by PUMA for the *entire state*
attainment_data_puma <- get_acs(
  geography = "puma",
  state = "NY", 
  variables = c(
    total_pop_25_over = "B15003_001",
    bachelors = "B15003_022",
    masters = "B15003_023",
    professional = "B15003_024",
    doctorate = "B15003_025"
  ),
  year = 2023, 
  output = "wide"
)

# 3. Use tidyverse (dplyr) to filter and calculate
nyc_puma_summary <- attainment_data_puma %>%
  
  # --- THIS IS THE CORRECTED STEP ---
  # Filter using the NAME column, which contains the county name
  # e.g., "NYC-Queens Community District 7... PUMA, New York"
  filter(str_detect(NAME, nyc_pattern)) %>%
  # ----------------------------------

# Sum all post-secondary degrees
mutate(
  bachelors_or_higher_count = bachelorsE + mastersE + professionalE + doctorateE,
  pct_bachelors_or_higher = (bachelors_or_higher_count / total_pop_25_overE)
) %>%
  
  # Clean up the NAME field
  mutate(
    NAME = str_remove(NAME, ", New York"),
    NAME = str_remove(NAME, " PUMA")
  ) %>%
  
  # Select and rename the final columns
  select(
    puma_community_district = NAME,
    total_pop_25_over = total_pop_25_overE,
    bachelors_or_higher_count,
    pct_bachelors_or_higher
  ) %>%
  
  # Sort by the highest attainment
  arrange(desc(pct_bachelors_or_higher))

# 4. View the final, filtered table (this will now work)
print(nyc_puma_summary)

library(sf)
cd <- read_sf("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Community_Districts/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson")
# key fields: BoroCD (3-digit CD code), geometry in WGS84

library(readr)
# Pull a big chunk (adjust $limit if needed)
sales <- read_csv("https://data.cityofnewyork.us/resource/usep-8jbt.csv?$limit=500000")
# Tip: You'll likely need to geocode or join by BBL to map sales to CDs.

library(tidycensus); library(dplyr)
# one-time setup: census_api_key("YOUR_KEY", install = TRUE); restart R

counties <- c("005","047","061","081","085")  # Bronx, Kings, New York, Queens, Richmond
acs_tract <- get_acs(
  year = 2023, survey = "acs5",
  geography = "tract", table = "S1501",
  state = "36", county = counties, output = "wide", geometry = TRUE
)


library(sf); library(dplyr)

# Ensure both are same CRS
acs_tract <- st_transform(acs_tract, st_crs(cd))

# Spatial join: each tract polygon to its CD polygon
acs_cd <- st_join(acs_tract, cd["BoroCD"])

# Example: compute CD-level % bachelor's+ using S1501 vars
# S1501_C02_015E = Percent bachelor’s+; S1501_C01_015E = total base (percent denominator) varies by table design
# For a robust roll-up, recompute from counts if you pull “detailed” vars; here we average by tract population 25+
# If you only have percents, use a weighted mean:
acs_cd_summary <- acs_cd %>%
  st_drop_geometry() %>%
  # Replace 'S1501_C02_015E' with your chosen percent and 'S0101_C01_001E' or appropriate population 25+ as weight if pulled
  group_by(BoroCD) %>%
  summarize(
    pct_bach_plus_wtd = weighted.mean(S1501_C02_015E, w = S0101_C01_001E, na.rm = TRUE)
  )

library(dplyr)
library(sf)

# acs_tract: output from tidycensus get_acs(..., table = "S1501", output = "wide", geometry = TRUE)
# cd: community districts sf with column BoroCD in the same CRS

# 1) Build tract-level counts for 25+ and bachelor's+
acs_tract <- acs_tract %>%
  mutate(
    pop25_total = S1501_C01_007E + S1501_C01_008E + S1501_C01_009E +
      S1501_C01_010E + S1501_C01_011E + S1501_C01_012E +
      S1501_C01_013E,                                  # all 25+ categories
    bach_plus   = S1501_C01_012E + S1501_C01_013E                   # bachelor's + grad/prof.
  )

# 2) Join tracts to Community Districts
acs_cd <- st_join(st_transform(acs_tract, st_crs(cd)), cd["BoroCD"])

# 3) Aggregate counts to CD, then compute % bachelor's+ (CD-level)
attain_cd <- acs_cd %>%
  st_drop_geometry() %>%
  group_by(BoroCD) %>%
  summarise(
    pop25_total = sum(pop25_total, na.rm = TRUE),
    bach_plus   = sum(bach_plus,   na.rm = TRUE)
  ) %>%
  mutate(pct_bach_plus = 100 * bach_plus / pop25_total)

install.packages(c("readr","dplyr","lubridate","sf"))
library(readr); library(dplyr); library(lubridate); library(sf)

# Optional: set your Socrata app token to avoid throttling
# Sys.setenv(SOCRATA_APP_TOKEN = "your_token_here")

# Annualized sales (all years) — filter in R by date
sales <- read_csv(
  "https://data.cityofnewyork.us/resource/w2pb-icbu.csv?$limit=2000000"
)

sales <- sales %>%
  mutate(
    sale_date  = as.Date(sale_date),
    sale_price = suppressWarnings(as.numeric(sale_price)),
    year       = year(sale_date)
  ) %>%
  # your pre/post windows
  filter((year >= 2015 & year <= 2019) | (year >= 2021 & year <= 2024)) %>%
  # keep arms-length(ish) residential
  filter(!is.na(sale_price), sale_price > 0) %>%
  filter(tax_class_at_time_of_sale %in% c("1","2"))
