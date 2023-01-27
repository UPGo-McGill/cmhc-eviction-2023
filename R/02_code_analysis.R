#### 02 CODE ANALYSIS ##########################################################

source("R/01_code_import.R")


# Canadian population -----------------------------------------------------

library(cancensus)

# Provinces
get_census("CA21", regions = list(C = 01), level = "PR") |> 
  select(region = `Region Name`, population = Population) |> 
  filter(str_detect(region, "New Brunswick|Quebec|Ontario|British Columbia")) |> 
  mutate(pct = population / sum(population))

# Age
get_census("CA21", regions = list(C = 01), level = "PR", vectors = c(
  age_18 = "v_CA21_83", age_19 = "v_CA21_86", age_20_24 = "v_CA21_89",
  age_25_29 = "v_CA21_107", age_30_34 = "v_CA21_125", age_35_39 = "v_CA21_143",
  age_40_44 = "v_CA21_161", age_45_49 = "v_CA21_179", age_50_54 = "v_CA21_197",
  age_55_59 = "v_CA21_215", age_60_64 = "v_CA21_233", age_65 = "v_CA21_251")) |> 
  select(region = `Region Name`, age_18:age_65) |> 
  filter(str_detect(region, "New Brunswick|Quebec|Ontario|British Columbia")) |> 
  summarize(across(c(-region), sum)) |> 
  transmute(age_18_29 = age_18 + age_19 + age_20_24 + age_25_29,
            age_30_49 = age_30_34 + age_35_39 + age_40_44 + age_45_49,
            age_50_64 = age_50_54 + age_55_59 + age_60_64,
            age_65) |> 
  pivot_longer(everything(), names_to = "age", values_to = "n") |> 
  mutate(pct = n / sum(n))
  
# Household size
get_census("CA21", regions = list(C = 01), level = "PR", vectors = c(
  size_1 = "v_CA21_444", size_2 = "v_CA21_445", size_3 = "v_CA21_446",
  size_4 = "v_CA21_447", size_5 = "v_CA21_448")) |> 
  select(region = `Region Name`, size_1:size_5) |> 
  filter(str_detect(region, "New Brunswick|Quebec|Ontario|British Columbia")) |> 
  summarize(across(c(-region), sum)) |> 
  transmute(size_1, size_2 = size_2 * 2, size_3 = size_3 * 3, 
            size_4 = size_4 * 4 + size_5 * 5) |> 
  pivot_longer(everything(), names_to = "size", values_to = "n") |> 
  mutate(pct = n / sum(n))

# Gender
get_census("CA21", regions = list(C = 01), level = "PR", vectors = c(
  male = "v_CA21_9", female = "v_CA21_10")) |> 
  select(region = `Region Name`, male, female) |> 
  filter(str_detect(region, "New Brunswick|Quebec|Ontario|British Columbia")) |> 
  summarize(across(c(-region), sum)) |> 
  pivot_longer(everything(), names_to = "gender", values_to = "n") |> 
  mutate(pct = n / sum(n))
  
# Race/ethnicity
get_census("CA21", regions = list(C = 01), level = "PR", vectors = c(
  not_vis = "v_CA21_4914", white = "v_CA21_4968", black = "v_CA21_4884", 
  south_asian = "v_CA21_4878", chinese = "v_CA21_4881", 
  filipino = "v_CA21_4887", arab = "v_CA21_4890", 
  southeast_asian = "v_CA21_4896", west_asian = "v_CA21_4899", 
  korean = "v_CA21_4902", japanese = "v_CA21_4905", latin = "v_CA21_4893",
  indigenous = "v_CA21_4971")) |> 
  select(region = `Region Name`, not_vis:latin) |> 
  filter(str_detect(region, "New Brunswick|Quebec|Ontario|British Columbia")) |> 
  summarize(across(c(-region), sum)) |> 
  transmute(asian = south_asian + chinese + filipino + arab + southeast_asian +
              west_asian + korean + japanese, black, indigenous, latin, 
            white = not_vis + white) |> 
  pivot_longer(everything(), names_to = "race", values_to = "n") |> 
  mutate(pct = n / sum(n))
  

# Demographics ------------------------------------------------------------

snippets |> 
  group_by(transcript) |> 
  slice(1) |> 
  ungroup() |> 
  count(province) |> 
  mutate(pct = n / sum(n))

# Age
snippets |> 
  group_by(transcript) |> 
  slice(1) |> 
  ungroup() |> 
  summarize(
    age_18_29 = sum(age <= 29, na.rm = TRUE),
    age_30_49 = sum(age >= 30 & age <= 49, na.rm = TRUE),
    age_50_64 = sum(age >= 50 & age <= 64, na.rm = TRUE),
    age_65 = sum(age >= 65, na.rm = TRUE)) |> 
  pivot_longer(everything(), names_to = "age", values_to = "n") |> 
  mutate(pct = n / sum(n))

# HH size
snippets |> 
  group_by(transcript) |> 
  slice(1) |> 
  ungroup() |> 
  summarize(
    size_1 = sum(hh_size == 1, na.rm = TRUE),
    size_2 = sum(hh_size == 2, na.rm = TRUE),
    size_3 = sum(hh_size == 3, na.rm = TRUE),
    size_4 = sum(hh_size >= 4, na.rm = TRUE)) |> 
  pivot_longer(everything(), names_to = "size", values_to = "n") |> 
  mutate(pct = n / sum(n))

# Gender
snippets |> 
  group_by(transcript) |> 
  slice(1) |> 
  ungroup() |> 
  summarize(
    female = sum(gender == "Female", na.rm = TRUE),
    male = sum(gender == "Male", na.rm = TRUE),
    non_binary = sum(gender == "Nonbinary", na.rm = TRUE)) |> 
  pivot_longer(everything(), names_to = "gender", values_to = "n") |> 
  mutate(pct = n / sum(n))

# Race/ethnicity
snippets |> 
  group_by(transcript) |> 
  slice(1) |> 
  ungroup() |> 
  # count(race)
  summarize(
    asian = sum(race == "Asian", na.rm = TRUE),
    mixed = sum(str_detect(race, "Biracial|Mixed"), na.rm = TRUE),
    black = sum(race %in% c("Black", "Caribbean", "Haitian", "Nigerian"), 
                na.rm = TRUE),
    indigenous = sum(race == "Indigenous", na.rm = TRUE),
    hispanic = sum(race %in% c("Mexican", "Spanish"), na.rm = TRUE),
    white = sum(race %in% c("White/Caucasian", "Acadian"), na.rm = TRUE)) |> 
  pivot_longer(everything(), names_to = "race", values_to = "n") |> 
  mutate(pct = n / sum(n))

# Disability
snippets |> 
  group_by(transcript) |> 
  slice(1) |> 
  ungroup() |>
  summarize(
    physical = sum(physical == "Yes", na.rm = TRUE),
    intellectual = sum(intellectual == "Yes", na.rm = TRUE),
    neither = n() - physical - intellectual) |> 
  pivot_longer(everything(), names_to = "race", values_to = "n") |> 
  mutate(pct = n / sum(n))


# Eviction types ----------------------------------------------------------

# Landlord or tenant factor, single or multiple
transcripts |> 
  filter(category == "ET") |> 
  arrange(transcript) |> 
  group_by(transcript) |> 
  summarize(
    tenant = sum(code %in% c("ET-NP", "ET-OT")) >= 1,
    landlord = sum(code %in% c(
      "ET-OW", "ET-R", "ET-S", "ET-RT", "ET-OL")) >= 1,
    multiple = sum(code == "ET-ME") >= 1) |>
  group_by(multiple) |> 
  summarize(
    tenant_n = sum(tenant & !landlord),
    landlord_n = sum(!tenant & landlord),
    both_n = sum(tenant & landlord)) |> 
  mutate(
    tenant_pct = tenant_n / sum(tenant_n, landlord_n, both_n),
    landlord_pct = landlord_n / sum(tenant_n, landlord_n, both_n),
    both_pct = both_n / sum(tenant_n, landlord_n, both_n))

# Landlord factors
transcripts |> 
  filter(category == "ET") |> 
  arrange(transcript) |> 
  group_by(transcript) |> 
  filter(code %in% c("ET-OW", "ET-R", "ET-S", "ET-RT", "ET-OL")) |> 
  summarize(
    own_use = sum(code == "ET-OW"),
    reno = sum(code == "ET-R"),
    sale = sum(code == "ET-S"),
    retal = sum(code == "ET-RT"),
    other = sum(code == "ET-OL")) |> 
  summarize(across(c(own_use:other), mean))
  
# Geographical differences
transcripts |> 
  filter(category == "ET") |> 
  arrange(transcript) |> 
  group_by(transcript) |> 
  filter(code %in% c("ET-OW", "ET-R", "ET-S", "ET-RT", "ET-OL")) |> 
  summarize(
    province,
    own_use = sum(code == "ET-OW"),
    reno = sum(code == "ET-R"),
    sale = sum(code == "ET-S"),
    retal = sum(code == "ET-RT"),
    other = sum(code == "ET-OL"),
    .groups = "drop") |> 
  group_by(province) |> 
  summarize(across(c(own_use:other), mean))

# Own-use and multiple-unit
transcripts |> 
  filter(category == "ET") |> 
  arrange(transcript) |> 
  group_by(transcript) |> 
  summarize(
    own_use = sum(code == "ET-OW") >= 1,
    multiple = sum(code == "ET-ME") >= 1) |>
  count(own_use, multiple) |> 
  filter(multiple) |> 
  mutate(pct = n / sum(n))


# Court involvement -------------------------------------------------------

transcripts |> 
  filter(category == "ET") |> 
  arrange(transcript) |> 
  group_by(transcript) |> 
  summarize(court = sum(code == "ET-CI") >= 1,
            non_court = sum(code == "ET-NC") >= 1) |> 
  summarize(court_n = sum(court),
            court_pct = court_n / n())


# Details of eviction process ---------------------------------------------

# Landlord type
transcripts |> 
  filter(category == "LT") |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    individual = sum(code == "LT-I") >= 1,
    corporate = sum(code == "LT-C") >= 1,
    public = sum(code == "LT-PH") >= 1,
    non_profit = sum(code == "LT-NP") >= 1,
    owned_other = sum(code == "LT-RP") >= 1) |> 
  group_by(owned_other) |> 
  summarize(across(c(individual:non_profit), sum)) |> 
  mutate(across(individual:non_profit, \(x) x / sum(individual, corporate, public, non_profit), 
                .names = "{.col}_pct"))




# Impacts of eviction -----------------------------------------------------

# Mobility
transcripts |> 
  filter(category == "M") |> 
  arrange(transcript) |> 
  count(code) |> 
  mutate(pct = n / sum(n))

mob <- transcripts |> 
  filter(category == "M") |> 
  pull(transcript)

transcripts |> 
  filter(!transcript %in% mob) |> 
  pull(transcript) |> 
  unique()

ud_t <- transcripts |> 
  filter(category == "UD") |> 
  filter(str_detect(code, "UD-T")) |> 
  pull(transcript) |> 
  unique()

transcripts |> 
  filter(!transcript %in% ud_t) |> 
  pull(transcript) |> 
  unique()

et <- 
  transcripts |> 
  filter(category == "ET") |> 
  filter(code %in% c("ET-CI", "ET-NC")) |> 
  pull(transcript) |> 
  unique()

transcripts |> 
  filter(!transcript %in% et) |> 
  pull(transcript) |> 
  unique()
