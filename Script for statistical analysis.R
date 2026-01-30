
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
setwd("C:/Users/bgmau/OneDrive/Documents/R and R Studio/MRP")


# Voting Data in the UN ---------------------------------------------------

votes <- read.csv("UN_Votes.csv")

Cleaned_votes <- votes %>%
  filter(member_state %in% c("China", "United States", "Costa Rica", "Panama", "Guatemala", "El Salvador", "Honduras", "Nicaragua", "Belize", "Antigua and Barbuda", "Bahamas", "Barbados", "Cuba", "Dominica", "Dominican Republic", "Grenada", "Haiti", "Jamaica", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Trinidad and Tobago")) %>%
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

ca_countries <- c("Costa Rica", "Panama", "Guatemala", "El Salvador", "Honduras", "Nicaragua", "Belize", "Antigua and Barbuda", "Bahamas", "Barbados", "Cuba", "Dominica", "Dominican Republic", "Grenada", "Haiti", "Jamaica", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Trinidad and Tobago")

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

summary_df <- df_long %>%
  group_by(year, country) %>%
  summarise(
    n_obs = n(),                           # number of votes
    pct_china = mean(match == "China") * 100,
    pct_us    = mean(match == "US") * 100,
    pct_none  = mean(match == "None") * 100,
    .groups = "drop"
  )

# I'm adding a column by region

summary_df <- summary_df %>% mutate (region = ifelse(country %in% c("Costa Rica", "Panama", "Guatemala", "El Salvador", "Honduras", "Nicaragua", "Belize"), "Central America", "Caribbean"))



# Foreign Aid -------------------------------------------------------------

Foreign_Aid <- read_excel("Chiense Foreign Aid.xlsx")

Foreign_Aid <- Foreign_Aid %>% select ("Recipient", "Commitment Year", "Implementation Start Year", "Completion Year", "Sector Name", "Amount (Constant USD 2021)") %>%
  filter (Recipient %in% ca_countries)

Foreign_Aid <- Foreign_Aid %>%
  filter(!is.na(`Amount (Constant USD 2021)`))

## Note: 1103 entries originally. 573 after filtering NAs. This is a limitation, as we are only using investments for which thee amount is public.

## Now, we are creating three tables with a summary per country per year.One table would summarize the projects commited, the second the projects started, and the third the projects finished.

committed <- Foreign_Aid %>%
  filter(!is.na(`Commitment Year`)) %>%
  group_by(Recipient, year = `Commitment Year`) %>%
  summarise(
    total_committed = sum(`Amount (Constant USD 2021)`, na.rm = TRUE),
    .groups = "drop"
  )

started <- Foreign_Aid %>%
  filter(!is.na(`Implementation Start Year`)) %>%
  group_by(Recipient, year = `Implementation Start Year`) %>%
  summarise(
    total_started = sum(`Amount (Constant USD 2021)`, na.rm = TRUE),
    .groups = "drop"
  )


finished <- Foreign_Aid %>%
  filter(!is.na(`Completion Year`)) %>%
  group_by(Recipient, year = `Completion Year`) %>%
  summarise(
    total_finished = sum(`Amount (Constant USD 2021)`, na.rm = TRUE),
    .groups = "drop"
  )


# Now we'll join the tables to the main one (UN Votes)

summary_df2 <- summary_df %>%
  left_join(committed, by = c("country" = "Recipient", "year" = "year")) %>%
  left_join(started,   by = c("country" = "Recipient", "year" = "year")) %>%
  left_join(finished,  by = c("country" = "Recipient", "year" = "year"))


## I'll replace NAs with 0s


summary_df2 <- summary_df2 %>%
  mutate(
    total_committed = coalesce(total_committed, 0),
    total_started   = coalesce(total_started, 0),
    total_finished  = coalesce(total_finished, 0)
  )

## Finally, I will add a "Cumulative" column

summary_df2 <- summary_df2 %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    cumulative_committed = cumsum(total_committed),
    cumulative_started   = cumsum(total_started),
    cumulative_finished  = cumsum(total_finished)
  ) %>%
  ungroup()

## Regressions

library(fixest)

## standard lagged explanatory variable design:

aid_regressions <- summary_df2 %>%
  filter(year <= 2021) %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    aid_t = total_committed,
    aid_t1 =lag (total_committed, 1),
    aid_t3 = lag(total_committed, 3),
    aid_t5 = lag(total_committed, 5)
  ) %>%
  ungroup()


## Rescale to billions of dollars for readability

aid_regressions <- aid_regressions %>%
  mutate(
    aid_t_m  = aid_t  / 1e9,
    aid_t1_m = aid_t1 / 1e9,
    aid_t3_m = aid_t3 / 1e9,
    aid_t5_m = aid_t5 / 1e9
  )


## Now the regressions


# Same year (immediate effect)

model_China_t <- feols(
  pct_china ~ aid_t_m | country + year,
  data = aid_regressions,
  cluster = "country"
)

# One year lag

model_China_t1 <- feols(
  pct_china ~ aid_t1_m | country + year,
  data = aid_regressions,
  cluster = "country"
)


# Three years lag

model_China_t3 <- feols(
  pct_china ~ aid_t3_m | country + year,
  data = aid_regressions,
  cluster = "country"
)


# Five years lag

model_China_t5 <- feols(
  pct_china ~ aid_t5_m | country + year,
  data = aid_regressions,
  cluster = "country"
)

## Summarize them

etable(
  model_China_t, model_China_t1, model_China_t3, model_China_t5,
  dict = c(
    aid_t_m  = "Aid (t)",
    aid_t1_m = "Aid (t-1)",
    aid_t3_m = "Aid (t−3)",
    aid_t5_m = "Aid (t−5)"
  )
)




## There is no significant effect of foreign aid in the countries' alignment with China either the same year, three years ahead or five years ahead

## Finally, we'll run the same regression but with proportion of alignment with the US


# Same year (immediate effect)

model_US_t <- feols(
  pct_us ~ aid_t_m | country + year,
  data = aid_regressions,
  cluster = "country"
)

# One-year lag

model_US_t1 <- feols(
  pct_us ~ aid_t1_m | country + year,
  data = aid_regressions,
  cluster = "country"
)

# Three years lag

model_US_t3 <- feols(
  pct_us ~ aid_t3_m | country + year,
  data = aid_regressions,
  cluster = "country"
)

# Five years lag

model_US_t5 <- feols(
  pct_us ~ aid_t5_m | country + year,
  data = aid_regressions,
  cluster = "country"
)

# Summarize

etable(
  model_US_t, model_US_t3, model_US_t5,
  dict = c(
    aid_t_m  = "Aid (t)",
    aid_t1_m = "Aid (t-1)",
    aid_t3_m = "Aid (t−3)",
    aid_t5_m = "Aid (t−5)"
  )
)


## Conclusion: Foreign aid has no visible / substantively meaningful effect on voting patterns


## As a robustness check, I am going to add one more regression, accounting for the average of investment of the past three years

aid_checks <- summary_df2 %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    aid_avg_3yr = (
      lag(total_committed, 1) +
        lag(total_committed, 2) +
        lag(total_committed, 3)
    ) / 3
  ) %>%
  ungroup()

summary_df2 <- summary_df2 %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    aid_avg_3yr = (
      lag(total_committed, 1) +
        lag(total_committed, 2) +
        lag(total_committed, 3)
    ) / 3
  ) %>%
  ungroup()

aid_checks <- aid_checks %>%
  mutate(
    aid_avg_3yr  = aid_avg_3yr  / 1e9,
  )

# Regressions
 
m_avg3_China <- feols(
  pct_china ~ aid_avg_3yr | country + year,
  data = aid_checks,
  cluster = "country"
)

m_avg3_US <- feols(
  pct_us ~ aid_avg_3yr | country + year,
  data = aid_checks,
  cluster = "country"
)

m_avg3_China
m_avg3_US

## As a final robustness check, I am going to account for the fact that not all aid initiatives have publicly-available data on the dollar amount
## This means that I will create an event-based model

Foreign_Aid <- read_excel("Chiense Foreign Aid.xlsx")

Foreign_Aid <- Foreign_Aid %>% select ("Recipient", "Commitment Year", "Implementation Start Year", "Completion Year", "Sector Name", "Amount (Constant USD 2021)") %>%
  filter (Recipient %in% ca_countries)

## Now, we are creating three tables with a summary per country per year.One table would summarize the projects commited, the second the projects started, and the third the projects finished.

aid_counts <- Foreign_Aid %>%
  mutate(
    year = as.integer(`Commitment Year`),
    country = as.character(Recipient)
  ) %>%
  filter(!is.na(year), !is.na(country)) %>%
  group_by(country, year) %>%
  summarise(
    aid_commitment_count = n(),
    .groups = "drop"
  )

summary_df2 <- summary_df2 %>%
  left_join(aid_counts, by = c("country", "year")) %>%
  mutate(aid_commitment_count = if_else(is.na(aid_commitment_count), 0L, aid_commitment_count))


summary_df2 <- summary_df2 %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    aid_count_t1 = lag(aid_commitment_count, 1),
    aid_count_avg_3yr = (
      lag(aid_commitment_count, 1) +
        lag(aid_commitment_count, 2) +
        lag(aid_commitment_count, 3)
    ) / 3
  ) %>%
  ungroup()

# Regression for count the year before

feols(
  pct_china ~ aid_count_t1 | country + year,
  data = summary_df2 %>% filter(year <= 2021),
  cluster = "country"
)

# Regression for average three year count

feols(
  pct_china ~ aid_count_avg_3yr | country + year,
  data = summary_df2 %>% filter(year <= 2021),
  cluster = "country"
)

# Regressions for US alignment

feols(
  pct_us ~ aid_count_t1 | country + year,
  data = summary_df2 %>% filter(year <= 2021),
  cluster = "country"
)

# Regression for average three year count (US)

feols(
  pct_us ~ aid_count_avg_3yr | country + year,
  data = summary_df2 %>% filter(year <= 2021),
  cluster = "country"
)


# Foreign Direct Investment -----------------------------------------------

Chinese_FDI <- read_excel("Chinese FDI.xlsx")

fdi_country_year <- Chinese_FDI %>%
  filter(Country %in% c("Costa Rica", "Panama", "Guatemala", "El Salvador", "Honduras", "Nicaragua", "Belize", "Antigua and Barbuda", "Bahamas", "Barbados", "Cuba", "Dominica", "Republica Dominicana", "Grenada", "Haiti", "Jamaica", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "Trinidad y Tobago")) %>%
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
  mutate (year = as.integer(year)) %>%
  mutate(
    country = recode(country,
                     "Republica Dominicana" = "Dominican Republic",
                     "Trinidad y Tobago"    = "Trinidad and Tobago"
    )
  )


summary_df3 <- summary_df2 %>% 
  left_join(
    fdi_country_year,
    by = c("country", "year")
  )
  

# Replace NAs with Zeros

summary_df3 <- summary_df3 %>%
  mutate(
    fdi_total = coalesce(fdi_total, 0)
  )

# Given that FDI is very sporadic, and not present in all countries, the statistical power of a simple regression is not going to be good.
# Instead, I will treat FDI as events that might change behaviour, instead of a continuous flow
# The question is: “Does receiving FDI change subsequent voting behavior relative to periods without FDI?”


# Same year as FDI

feols(
  pct_china ~ fdi_total | country,
  data = summary_df3,
  cluster = "country"
)


# Lagged FDI Indicator


summary_df3 <- summary_df3 %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    fdi_avg_3yr = (
      lag(fdi_total, 1) +
        lag(fdi_total, 2) +
        lag(fdi_total, 3)
    ) / 3
  ) %>%
  ungroup()

## To compress extreme values, keep zeros meaningful and improve stability, I will us a log scale

summary_df3 <- summary_df3 %>%
  mutate(
    fdi_avg_3yr_log = log1p(fdi_avg_3yr)
  )

## Now I will run the regression: Within a country, do years with higher recent FDI exposure (averaged over the past 3 years and log-scaled) differ in their voting alignment with China?

feols(
  pct_china ~ fdi_avg_3yr_log | country,
  data = summary_df3,
  cluster = "country"
)


## “suggestive but inconclusive” evidence. Positive direction of FDI to voting alignment with China


## I will run the same regression but with alignment with the US

feols(
  pct_us ~ fdi_avg_3yr_log | country,
  data = summary_df3,
  cluster = "country"
)


## Conclusion: Points towards a smaller deviation away from the US. Still, FDI does not appear to exert a strong or robust influence on voting behavior.


## I will try one more model: Using an event-based indictor (1 or 0) per year, instead of the amount of the investment

summary_df3 <- summary_df3 %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(
    # 1 if FDI occurs in a year, 0 otherwise
    fdi_event = as.integer(fdi_total > 0),
    
    # average exposure over the last 3 years (t−1 to t−3)
    fdi_event_avg_3yr = (
      lag(fdi_event, 1) +
        lag(fdi_event, 2) +
        lag(fdi_event, 3)
    ) / 3
  ) %>%
  ungroup()


model_fdi_event <- feols(
  pct_china ~ fdi_event_avg_3yr | country,
  data = summary_df3,
  cluster = "country"
)

summary(model_fdi_event)

# Now for the US

model_fdi_event_us <- feols(
  pct_us ~ fdi_event_avg_3yr | country,
  data = summary_df3,
  cluster = "country"
)

summary(model_fdi_event_us)




## Conclusion: Moving from no FDI events to FDI in all three years is associated with about a 4 percentage-point increase in alignment — but this effect is imprecisely estimated and not statistically significant.


# Trade -------------------------------------------------------------------

Exports_to_China <- read_excel("Exports to China.xls")
Imports_from_China <- read_excel("Imports from China.xls")

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

# Cleaning the Data

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

## Joining it with the main summary df


summary_df4 <- summary_df3 %>%
  left_join(
    trade,
    by = c("country", "year")
  )


## Finding the total trade per country to do share of trade. Source: World Bank

Total_exports <- read_excel("Total exports by country.xlsx")
Total_imports <- read_excel("Total imports by country.xlsx")


Total_exports <- Total_exports %>%
  select(-`Country Code`, -`Series Name`, -`Series Code`) %>%
  mutate(across(-`Country Name`, ~ as.numeric(.)))


Total_imports <- Total_imports %>%
  select(-`Country Code`, -`Series Name`, -`Series Code`) %>%
  mutate(across(-`Country Name`, ~ as.numeric(.)))


Total_exports <- Total_exports %>%
  pivot_longer(
    cols = -`Country Name`,
    names_to = "year",
    values_to = "total_exports"
  ) %>%
  mutate(
    country = as.character(`Country Name`),
    year = as.integer(substr(year, 1, 4))
  ) %>%
  select(country, year, total_exports)

Total_imports <- Total_imports %>%
  pivot_longer(
    cols = -`Country Name`,
    names_to = "year",
    values_to = "total_imports"
  ) %>%
  mutate(
    country = as.character(`Country Name`),
    year = as.integer(substr(year, 1, 4))
  ) %>%
  select(country, year, total_imports)


## Cleaning the column names

Total_exports <- Total_exports %>%
  mutate(
    country = as.character(`country`),
    year = as.integer(substr(year, 1, 4)),
    total_exports = as.numeric(total_exports)
  ) %>%
  select(country, year, total_exports)

Total_imports <- Total_imports %>%
  mutate(
    country = as.character(`country`),
    year = as.integer(substr(year, 1, 4)),
    total_exports = as.numeric(total_imports)
  ) %>%
  select(country, year, total_imports)


## Joining them to the main dataset

# Making sure the country names match

Total_exports <- Total_exports %>%
  filter (country != "Bermuda") %>%
  mutate(
    country = recode(country,
                     "Bahamas, The" = "Bahamas",
                     "St. Kitts and Nevis" = "Saint Kitts and Nevis",
                     "St. Lucia" = "Saint Lucia",
                     "St. Vincent and the Grenadines" = "Saint Vincent and the Grenadines"
    )
  )

  
Total_imports <- Total_imports %>%
    filter (country != "Bermuda") %>%
  mutate(
    country = recode(country,
                     "Bahamas, The" = "Bahamas",
                     "St. Kitts and Nevis" = "Saint Kitts and Nevis",
                     "St. Lucia" = "Saint Lucia",
                     "St. Vincent and the Grenadines" = "Saint Vincent and the Grenadines"
    )
  )


# Joining imports and exports

imports_exports <- Total_exports %>%
  left_join(
    Total_imports,
    by = c("country", "year")
  )

summary_df4 <- summary_df4 %>%
  left_join(
    imports_exports,
    by = c("country", "year")
  )

# Handle NAs


sum(is.na(summary_df4$total_exports))
sum(is.na(summary_df4$total_imports))

## Constructing the trade share variable

# China exports and imports are in USD 10000. We have to adjust to USD.

summary_df4 <- summary_df4 %>%
  mutate(
    exports_china = exports_china * 10000,
    imports_china = imports_china * 10000
  )

# Now we create the variable

summary_df4 <- summary_df4 %>%
  mutate(
    export_share_china = 100 * exports_china / total_exports,
    import_share_china = 100 * imports_china / total_imports
  )


## Let's run the regression now

model_trade_exp <- feols(
  pct_china ~ export_share_china | country + year,
  data = summary_df4,
  cluster = "country"
)

summary(model_trade_exp)

model_trade_imp <- feols(
  pct_china ~ import_share_china | country + year,
  data = summary_df4,
  cluster = "country"
)

summary(model_trade_imp)


# Now for alignment with the US

model_trade_exp_us <- feols(
  pct_us ~ export_share_china | country + year,
  data = summary_df4,
  cluster = "country"
)

summary(model_trade_exp_us)

model_trade_imp_us <- feols(
  pct_us ~ import_share_china | country + year,
  data = summary_df4,
  cluster = "country"
)

summary(model_trade_imp_us)



# Testing for correlation ------------------------------------------------


cor(
  summary_df4 %>% 
    select(aid_avg_3yr, fdi_avg_3yr_log, export_share_china, import_share_china),
  use = "complete.obs"
)


## Conclusion: No multicolinearity concern


# Graphs for the written report -------------------------------------------


## Foreign Aid for Central America and Caribbean, with percentage of alignment with China


## Aid figure

# Figure 1: $ amount

summary_df4_aid <- summary_df4 %>%
  filter(year <= 2021)

aid_year <- summary_df4_aid %>%
  group_by(year) %>%
  summarise(
    pct_china = mean(pct_china, na.rm = TRUE),
    total_committed = sum(total_committed, na.rm = TRUE),
    aid_commitment_count = sum (aid_commitment_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(pct_china, total_committed, aid_commitment_count),
               names_to = "series", values_to = "value") %>%
  mutate(
    series = recode(series,
                    pct_china = "Voting alignment with China (%)",
                    total_committed = "Total committed aid (US$ billions)",
                    aid_commitment_count = "Total count of commitments"
    ),
    # convert only aid to billions for display
    value = ifelse(series == "Total committed aid (US$ billions)", value / 1e9, value)
  )

ggplot(aid_year, aes(x = year, y = value)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  facet_wrap(~series, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = pretty_breaks(6)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    x = "Year",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 8)),
    strip.background = element_rect(fill = "grey95", color = NA)
  )


## Foreign Direct Investment

fdi_year <- summary_df4 %>%
  group_by(year) %>%
  summarise(
    pct_china = mean(pct_china, na.rm = TRUE),
    fdi_total = sum(fdi_total, na.rm = TRUE),
    fdi_event = sum (fdi_event, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(pct_china, fdi_total, fdi_event),
               names_to = "series", values_to = "value") %>%
  mutate(
    series = recode(series,
                    pct_china = "Voting alignment with China (%)",
                    fdi_total = "Total FDI (US$ millions)",
                    fdi_event = "Amount of countries who received FDI"
    ),
  )

ggplot(fdi_year, aes(x = year, y = value)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  facet_wrap(~series, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = pretty_breaks(6)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    x = "Year",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 8)),
    strip.background = element_rect(fill = "grey95", color = NA)
  )



## Trade

trade_year <- summary_df4 %>%
  group_by(year) %>%
  summarise(
    pct_china = mean(pct_china, na.rm = TRUE),
    export_share_china = mean(export_share_china, na.rm = TRUE),
    import_share_china = mean(import_share_china, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(pct_china, export_share_china, import_share_china),
               names_to = "series", values_to = "value") %>%
  mutate(series = recode(series,
                         pct_china = "Voting alignment with China (%)",
                         export_share_china = "Average export share to China (%)",
                         import_share_china = "Average import share from China (%)"
  ))

ggplot(trade_year, aes(year, value)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  facet_wrap(~series, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = pretty_breaks(6)) +
  labs(x = "Year", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey95", color = NA))


## Finally, a summary graph of voting patterns for each country in the region

# First, alignment with China for Central America

df_ca <- summary_df4 %>%
  filter(region == "Central America") %>%
  select(country, year, pct_china)

ggplot(df_ca, aes(year, pct_china)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  facet_wrap(~country, ncol = 3) +
  scale_x_continuous(breaks = pretty_breaks(4)) +
  labs(
    x = "Year",
    y = "Percent of votes"
  ) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9)
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))
  
# Second, alignment with China for Caribbean
  
  df_ca <- summary_df4 %>%
    filter(region == "Caribbean") %>%
    select(country, year, pct_china)
  
  ggplot(df_ca, aes(year, pct_china)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.2) +
    facet_wrap(~country, ncol = 3) +
    scale_x_continuous(breaks = pretty_breaks(4)) +
    labs(
      x = "Year",
      y = "Percent of votes"
    ) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.9)
  theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold"))
