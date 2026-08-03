
# Setup -------------------------------------------------------------------

library("unvotes")
library("tidyr")
library("lubridate")
library("countrycode")
library("ggplot2")
library("rmarkdown")
library("dplyr")
library("knitr")
library("readxl")
library("scales")
library("ggrepel")
library(fixest)
library("readr")
library("stringr")
library("modelsummary")


# Import Voting Data in the UN ---------------------------------------------------

votes <- read.csv("data/UN_Votes.csv")

## Filtering: Countries in Central America; years of study; decisions marked as important by the US State Department

Cleaned_votes <- votes %>%
  filter(member_state %in% c("China", "United States", "Costa Rica", "Panama", "Guatemala", "El Salvador", "Honduras", "Nicaragua")) %>%
  filter (meeting_date > "2007-01-01") %>%
  filter (important == "important") %>%
  select (decision_id, meeting_date, member_state, amended_vote, important) %>%
  pivot_wider(
    id_cols = c(decision_id,meeting_date, important),
    names_from = member_state,
    values_from = amended_vote
  ) %>%
  mutate(year = year(ymd(meeting_date))) %>%
  rename ('US' = 'United States')

## Now let's create new columns for agreement with China or agreement with US

ca_countries <- c("Costa Rica", "Panama", "Guatemala", "El Salvador", "Honduras", "Nicaragua")

df_long <- Cleaned_votes %>%
  pivot_longer(
    cols = all_of(ca_countries),
    names_to = "country",
    values_to = "vote"
  )


df_long <- df_long %>%
  mutate(
    match = case_when(
      vote == China ~ "China",
      vote == US ~ "US",
      TRUE ~ "None"
    )
  )

master_df <- df_long %>%
  group_by(year, country) %>%
  summarise(
    n_obs = n(),                           # number of votes
    pct_china = mean(match == "China") * 100,
    pct_us    = mean(match == "US") * 100,
    pct_none  = mean(match == "None") * 100,
    .groups = "drop"
  )


# Foreign Aid -------------------------------------------------------------

# Importing data for Foreign Aid

Foreign_Aid <- read_excel("data/Chinese Foreign Aid.xlsx")

Foreign_Aid <- Foreign_Aid %>% select ("Recipient", "Commitment Year", "Implementation Start Year", "Completion Year", "Sector Name", "Amount (Constant USD 2021)") %>%
  filter (Recipient %in% ca_countries)

Foreign_Aid <- Foreign_Aid %>%
  filter(!is.na(`Amount (Constant USD 2021)`))


## Now, we are creating a table to summarize the aid committed each year.

aid_committed <- Foreign_Aid %>%
  filter(!is.na(`Commitment Year`)) %>%
  group_by(Recipient, year = `Commitment Year`) %>%
  summarise(
    aid_committed = sum(`Amount (Constant USD 2021)`, na.rm = TRUE),
    .groups = "drop"
  )



# Now we'll join the table to the main one (UN Votes)

master_df <- master_df %>%
  left_join(aid_committed, by = c("country" = "Recipient", "year" = "year"))


## I'll replace NAs with 0s


master_df <- master_df %>%
  mutate(
    aid_committed = coalesce(aid_committed, 0)
  )


# Foreign Direct Investment -----------------------------------------------

# Importing data

Chinese_FDI <- read_excel("data/Chinese FDI.xlsx")

# Cleaning it

fdi_country_year <- Chinese_FDI %>%
  filter(Country %in% c("Costa Rica", "Panama", "Guatemala", "El Salvador", "Honduras", "Nicaragua")) %>%
  mutate(
    `Investment (millions of dollars)` =
      as.numeric(`Investment (millions of dollars)`)) %>%
  group_by(Country, Year) %>%
  summarise(
    fdi_total = sum(`Investment (millions of dollars)`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    country = Country,
    year    = Year
  ) %>%
  mutate (year = as.integer(year
    )
  )


# Joining it to the main dataframe.

master_df <- master_df %>% 
  left_join(
    fdi_country_year,
    by = c("country", "year")
  )
  

# Replace NAs with Zeros

master_df <- master_df %>%
  mutate(
    fdi_total = coalesce(fdi_total, 0)
  )


# Trade -------------------------------------------------------------------

#Import Data from WITS

Exports_to_China <- read_excel("data/WITS Export Share.xlsx")
Imports_from_China <- read_excel("data/WITS Import Share.xlsx")

# Cleaning the data

exports_long <- Exports_to_China %>%
  pivot_longer(
    cols = -country,          # all columns except country
    names_to = "year",        # column names become years
    values_to = "exports_china"
  )

imports_long <- Imports_from_China %>%
  pivot_longer(
    cols = -country,          # all columns except country
    names_to = "year",        # column names become years
    values_to = "imports_china"
  )

trade <- exports_long %>%
  left_join(
    imports_long,
    by = c("country", "year"))

trade <- trade %>%
  mutate(
    country = as.character(country),
    year = as.integer(year),
    exports_china = as.numeric(exports_china),
    imports_china = as.numeric(imports_china)
  ) %>%
  select(country, year, exports_china, imports_china)

## Handling NAs

trade <- trade %>%
  mutate(
    exports_china = coalesce(exports_china, 0)
  ) 

trade <- trade %>%
  mutate (
    imports_china = coalesce(imports_china, 0)
  )

## Joining it with the master df


master_df <- master_df %>%
  left_join(
    trade,
    by = c("country", "year")
  )



# Adding control variables ------------------------------------------------


#1. Democracy Index

VDem_dataset <- readRDS("data/V-Dem-Dataset.rds")

VDem_dataset <- VDem_dataset %>% filter (year >= 2007)%>%
  filter (year <= 2022) %>% 
  filter(country_name %in% c("Costa Rica", "Panama", "Guatemala", "El Salvador", "Honduras", "Nicaragua")) %>%
  select (country_name, year, v2x_polyarchy) %>%
  group_by(country_name) 
  
# Note: I filled the NAs downward with the most recent election score


master_df <- master_df %>%
  left_join(
    VDem_dataset %>% select(country_name, year, v2x_polyarchy),
    by = c("country" = "country_name", "year" = "year")
  )


#2. Government Ideology

Government_Ideology <- read_excel("data/Government Ideology.xlsx")

Government_Ideology <- Government_Ideology %>%
  filter (countryname %in% c("Costa Rica", "Panama", "Guatemala", "El Salvador", "Honduras", "Nicaragua")) 

    
Government_Ideology <- Government_Ideology %>%
  mutate(
    ideology_num = case_when(
      execrlc == "Left"  ~ -1,
      execrlc == "Center"     ~  0,
      execrlc == "Right" ~  1,
      execrlc == "0" ~ NA,
      TRUE                ~ NA_real_ # handles missing or unexpected values safely
    )
  ) %>%
  mutate(
    year = year(ymd(year))) %>%
  mutate (country = countryname) %>%
  filter(year >= 2005 & year <= 2024) %>%
  select (country, ideology_num, year) %>%
  filter (year >= 2005) %>%
  filter (year <= 2024) 

  # Join to main dataset
  
  master_df <- master_df %>%
    left_join(
      Government_Ideology,
      by = c("country", "year")
    )
  
  # Adding missing government ideology
  
  master_df <- master_df %>%
    mutate(
      ideology_num = case_when(
        # Panama Full Coding
        country == "Panama" & year >= 2005 & year < 2009 ~ -1,
        country == "Panama" & year >= 2009 & year < 2019 ~  1,
        country == "Panama" & year >= 2019 & year <= 2024 ~ -1,
        
        #End-of-Series Gap Filling for other nations
        country == "Costa Rica" & year == 2021 ~  0, # Alvarado 
        country == "Costa Rica" & year == 2022 ~  1, # Chaves
        country == "Honduras"   & year == 2021 ~  1, # Hernández (Right)
        country == "Honduras"   & year == 2022 ~ -1, # Castro (Left)
        country == "Nicaragua"  & year >= 2021 ~ -1,
        country == "Guatemala"  & year >= 2021 ~ 1, # Giamattelli (Right)
        country == "El Salvador" & year >= 2021 ~ 1, # Nayib Bukele
        
        # Default: keep original dataset values
        TRUE ~ ideology_num
      )
    )
  
#3. World Bank Economic Indicators
  
  # 1. Clean and reshape the raw World Bank dataset
  
  wb_raw <- read_excel("data/World Bank Economic Control Variables.xlsx")
  
  wb_cleaned <- wb_raw %>%
    
    filter(`Country Name` %in% c("Costa Rica", "Panama", "Guatemala", "El Salvador", "Honduras", "Nicaragua")) %>%
  
    # Replace World Bank missing value placeholders ("..") with true NA
    
    mutate(across(everything(), ~ na_if(as.character(.), ".."))) %>%
    
    # Pivot year columns (e.g., "2005 [YR2005]", "2006 [YR2006]") into long format
    
    pivot_longer(
      cols = contains("[YR"),
      names_to = "year_raw",
      values_to = "value"
    ) %>%
    
    # Extract 4-digit numeric year from "2005 [YR2005]"
    mutate(
      year = as.integer(str_extract(year_raw, "\\d{4}")),
      value = as.numeric(value)
    ) %>%
    
    # Drop non-essential rows/columns if any exist
    filter(!is.na(year), !is.na(`Country Name`), !is.na(`Series Name`)) %>%
    
    # Pivot Series Name into wide columns (one column per indicator)
    pivot_wider(
      id_cols = c(`Country Name`, year),
      names_from = `Series Name`,
      values_from = value
    ) %>%
    
    # Standardize column names for easy analysis
    rename(
      country = `Country Name`,
      gdp_growth = `GDP growth (annual %)`,
      trade_pct_gdp = `Trade (% of GDP)`,
      debt_pct_gni = `External debt stocks (% of GNI)`,
      inflation = `Inflation, consumer prices (annual %)`,
      us_aid_usd = `Net bilateral aid flows from DAC donors, United States (current US$)`
    )
  
  # 2. Join into your main panel dataset (master_df)
  master_df <- master_df %>%
    left_join(wb_cleaned, by = c("country", "year"))
  
  

# Cleaning the dataset and running the regressions ------------------------

  master_df <- master_df %>%
    mutate(
      exports_china = ifelse(exports_china == 0, NA, exports_china),
      imports_china = ifelse(imports_china == 0, NA, imports_china)
    )  
  
  
  master_df <- master_df %>%
    mutate(
      aid_committed_m = aid_committed / 1e6,
      us_aid_m        = us_aid_usd / 1e6
    )
  
  
# Models
  
  m1 <- feols(pct_china ~ aid_committed_m + fdi_total + exports_china + imports_china | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m2 <- feols(pct_china ~ aid_committed_m + fdi_total + exports_china + imports_china + 
                ideology_num | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m3 <- feols(pct_china ~ aid_committed_m + fdi_total + exports_china + imports_china + 
                v2x_polyarchy + ideology_num | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m4 <- feols(pct_china ~ aid_committed_m + fdi_total + exports_china + imports_china + 
                v2x_polyarchy + ideology_num + gdp_growth | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m5 <- feols(pct_china ~ aid_committed_m + fdi_total + exports_china + imports_china + 
                v2x_polyarchy + ideology_num + gdp_growth + inflation | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m6 <- feols(pct_china ~ aid_committed_m + fdi_total + exports_china + imports_china + 
                v2x_polyarchy + gdp_growth + inflation + ideology_num + us_aid_m | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
 
  ## Table comparison
  
  models_list <- list(
    "Model 1: Core Engagement"  = m1,
    "Model 2: + Government Ideology"  = m2,
    "Model 3: + Polyarchy Index"       = m3,
    "Model 4: + GDP Growth"        = m4,
    "Model 5: + Inflation"         = m5,
    "Model 6: + US Aid"           = m6
  )
  
  # 2. Map exact dataset variable names -> Clean display labels
  #    (Any variable NOT listed in coef_map will automatically be hidden from the table)
  core_map <- c(
    "aid_committed_m" = "Chinese Aid Committed (Millions USD)",
    "fdi_total"     = "Chinese Total FDI (Millions USD)",
    "exports_china"   = "Exports to China (% of Total)",
    "imports_china"   = "Imports from China (% of Total)"
  )
  
  # 3. Generate side-by-side comparison table
 msummary(
    models_list,
    coef_map = core_map,
    stars = c('*' = .05, '**' = .01, '***' = .001),
    gof_omit = "IC|Log|F",
    title = "Impact of Chinese Economic Engagement on UNGA Voting Alignment with China (2007–2022)",
    notes = "All models include country and year fixed effects. Controls added progressively across models 2–6."
  )


  # Models for the US
  
  m7 <- feols(pct_us ~ aid_committed_m + fdi_total + exports_china + imports_china | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m8 <- feols(pct_us ~ aid_committed_m + fdi_total + exports_china + imports_china + 
                ideology_num | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m9 <- feols(pct_us ~ aid_committed_m + fdi_total + exports_china + imports_china + 
                v2x_polyarchy + ideology_num | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m10 <- feols(pct_us ~ aid_committed_m + fdi_total + exports_china + imports_china + 
                v2x_polyarchy + ideology_num + gdp_growth | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m11 <- feols(pct_us ~ aid_committed_m + fdi_total + exports_china + imports_china + 
                v2x_polyarchy + ideology_num + gdp_growth + inflation | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m12 <- feols(pct_us ~ aid_committed_m + fdi_total + exports_china + imports_china + 
                v2x_polyarchy + gdp_growth + inflation + ideology_num + us_aid_m | country + year, 
              data =  master_df, panel.id = ~country + year, vcov = "DK")
  
  
  models_list <- list(
    "Model 1: Core Engagement"  = m7,
    "Model 2: + Government Ideology"  = m8,
    "Model 3: + Polyarchy Index"       = m9,
    "Model 4: + GDP Growth"        = m10,
    "Model 5: + Inflation"         = m11,
    "Model 6: + US Aid"           = m12
  )
  
  
  # 2. Map exact dataset variable names -> Clean display labels
  #    (Any variable NOT listed in coef_map will automatically be hidden from the table)
  core_map <- c(
    "aid_committed_m" = "Chinese Aid Committed (Millions USD)",
    "fdi_total"     = "Chinese Total FDI (Millions USD)",
    "exports_china"   = "Exports to China (% of Total)",
    "imports_china"   = "Imports from China (% of Total)"
  )
  
  # 3. Generate side-by-side comparison table
  msummary(
    models_list,
    coef_map = core_map,
    stars = c('*' = .05, '**' = .01, '***' = .001),
    gof_omit = "IC|Log|F",
    title = "Impact of Chinese Economic Engagement on UNGA Voting Alignment with U.S. (2007–2022)",
    notes = "All models include country and year fixed effects. Controls added progressively across models 2–6."
  )
  
  
# Robustness Checks: Event-based indicators for Foreign Aid and FDI -------

  ## New column based on FDI event is created directly from cleaned data, as no observations were dropped because they were NAs
  
  master_df <- master_df %>% 
    mutate(
      FDI_event = (ifelse (fdi_total > 0, 1, 0))
    )
  
## For Foreign Aid, I will re-import the original data with 218 observations (before dropping the NAs)

  # 1. Re-import Chinese Aid dataset WITHOUT filtering NAs
  raw_aid_events <- read_excel("data/Chinese Foreign Aid.xlsx") %>%
    filter(Recipient %in% ca_countries) %>%
    filter(!is.na(`Commitment Year`)) %>%
    group_by(country = Recipient, year = `Commitment Year`) %>%
    summarise(aid_event = 1, .groups = "drop")
  
  # 2. Join event indicators back to master_df & replace NAs with 0
  master_df <- master_df %>%
    select(-any_of(c("aid_event"))) %>% # drop existing event columns if present
    left_join(raw_aid_events, by = c("country", "year")) %>%
    mutate(
      aid_event = coalesce(aid_event, 0)
    )  

  # Models
  
  m1_rob <- feols(pct_china ~ aid_event + FDI_event + exports_china + imports_china | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m2_rob <- feols(pct_china ~ aid_event + FDI_event + exports_china + imports_china + 
                ideology_num | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m3_rob <- feols(pct_china ~ aid_event + FDI_event + exports_china + imports_china + 
                v2x_polyarchy + ideology_num | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m4_rob <- feols(pct_china ~ aid_event + FDI_event + exports_china + imports_china + 
                v2x_polyarchy + ideology_num + gdp_growth | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m5_rob <- feols(pct_china ~ aid_event + FDI_event + exports_china + imports_china + 
                v2x_polyarchy + ideology_num + gdp_growth + inflation | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m6_rob <- feols(pct_china ~ aid_event + FDI_event + exports_china + imports_china + 
                v2x_polyarchy + gdp_growth + inflation + ideology_num + us_aid_m | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
  
  ## Table comparison
  
  models_list <- list(
    "Model 1: Core Engagement"  = m1_rob,
    "Model 2: + Government Ideology"  = m2_rob,
    "Model 3: + Polyarchy Index"       = m3_rob,
    "Model 4: + GDP Growth"        = m4_rob,
    "Model 5: + Inflation"         = m5_rob,
    "Model 6: + US Aid"           = m6_rob
  )
  
  # 2. Map exact dataset variable names -> Clean display labels
  #    (Any variable NOT listed in coef_map will automatically be hidden from the table)
  core_map <- c(
    "aid_event" = "Chinese Aid Event (1 if yes, 0 if not)",
    "FDI_event"     = "Chinese FDI Event (1 if yes, 0 if not)",
    "exports_china"   = "Exports to China (% of Total)",
    "imports_china"   = "Imports from China (% of Total)"
  )
  
  # 3. Generate side-by-side comparison table
  msummary(
    models_list,
    coef_map = core_map,
    stars = c('*' = .05, '**' = .01, '***' = .001),
    gof_omit = "IC|Log|F",
    title = "Robustness Check: Impact of Chinese Economic Engagement on UNGA Voting Alignment with China (2007–2022)",
    notes = "All models include country and year fixed effects. Controls added progressively across models 2–6."
  )
  
  
  # Models for the US
  
  m7_rob <- feols(pct_us ~ aid_event + FDI_event + exports_china + imports_china | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m8_rob <- feols(pct_us ~ aid_event + FDI_event + exports_china + imports_china + 
                ideology_num | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m9_rob <- feols(pct_us ~ aid_event + FDI_event + exports_china + imports_china + 
                v2x_polyarchy + ideology_num | country + year, 
              data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m10_rob <- feols(pct_us ~ aid_event + FDI_event + exports_china + imports_china + 
                 v2x_polyarchy + ideology_num + gdp_growth | country + year, 
               data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m11_rob <- feols(pct_us ~ aid_event + FDI_event + exports_china + imports_china + 
                 v2x_polyarchy + ideology_num + gdp_growth + inflation | country + year, 
               data = master_df, panel.id = ~country + year, vcov = "DK")
  
  m12_rob <- feols(pct_us ~ aid_event + FDI_event + exports_china + imports_china + 
                 v2x_polyarchy + gdp_growth + inflation + ideology_num + us_aid_m | country + year, 
               data = master_df, panel.id = ~country + year, vcov = "DK")
  
  ## Table comparison
  
  models_list <- list(
    "Model 1: Core Engagement"  = m7_rob,
    "Model 2: + Government Ideology"  = m8_rob,
    "Model 3: + Polyarchy Index"       = m9_rob,
    "Model 4: + GDP Growth"        = m10_rob,
    "Model 5: + Inflation"         = m11_rob,
    "Model 6: + US Aid"           = m12_rob
  )
  
  # 2. Map exact dataset variable names -> Clean display labels
  #    (Any variable NOT listed in coef_map will automatically be hidden from the table)
  core_map <- c(
    "aid_event" = "Chinese Aid Event (1 if yes, 0 if not)",
    "FDI_event"     = "Chinese FDI Event (1 if yes, 0 if not)",
    "exports_china"   = "Exports to China (% of Total)",
    "imports_china"   = "Imports from China (% of Total)"
  )
  
  # 3. Generate side-by-side comparison table
  msummary(
    models_list,
    coef_map = core_map,
    stars = c('*' = .05, '**' = .01, '***' = .001),
    gof_omit = "IC|Log|F",
    title = "Robustness Check: Impact of Chinese Economic Engagement on UNGA Voting Alignment with U.S. (2007–2022)",
    notes = "All models include country and year fixed effects. Controls added progressively across models 2–6."
  )

  
  
  
