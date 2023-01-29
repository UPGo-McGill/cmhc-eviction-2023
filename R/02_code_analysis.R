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

snippets |> 
  filter(is.na(province)) |> 
  pull(transcript) |> 
  unique()

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
    physical = sum(str_detect(physical, "Yes"), na.rm = TRUE),
    intellectual = sum(str_detect(intellectual, "Yes"), na.rm = TRUE),
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


# Landlord type -----------------------------------------------------------

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
  mutate(across(individual:non_profit, \(x) 
                x / sum(individual, corporate, public, non_profit), 
                .names = "{.col}_pct"))

# Landlord type vs. eviction type
transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |> 
  summarize(
    individual = sum(code == "LT-I") >= 1,
    corporate = sum(code == "LT-C") >= 1,
    public = sum(code == "LT-PH") >= 1,
    non_profit = sum(code == "LT-NP") >= 1,
    own_use = sum(code == "ET-OW") >= 1,
    reno = sum(code == "ET-R") >= 1,
    sale = sum(code == "ET-S") >= 1,
    retal = sum(code == "ET-RT") >= 1,
    other = sum(code == "ET-OL") >= 1) |> 
  filter(individual | corporate) |> 
  group_by(individual) |> 
  summarize(across(c(own_use:other), mean))


# Landlord type vs. eviction scale
transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |> 
  summarize(
    individual = sum(code == "LT-I") >= 1,
    corporate = sum(code == "LT-C") >= 1,
    public = sum(code == "LT-PH") >= 1,
    non_profit = sum(code == "LT-NP") >= 1,
    multiple = sum(code == "ET-ME") >= 1) |> 
  filter(individual | corporate) |> 
  group_by(individual) |> 
  summarize(across(c(multiple), mean))


# Landlord perception and behaviour ---------------------------------------

# Broader landlord perception
transcripts |> 
  filter(category == "BPL") |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    gender = first(gender),
    white = first(race) == "White/Caucasian",
    indifferent = sum(code == "BPL-I") >= 1,
    negative = sum(code == "BPL-N") >= 1,
    positive = sum(code == "BPL-P") >= 1,
    indif_only = sum(code == "BPL-I") >= 1 & 
      sum(code %in% c("BPL-N", "BPL-P")) == 0,
    neg_only = sum(code == "BPL-N") >= 1 & 
      sum(code %in% c("BPL-I", "BPL-P")) == 0,
    pos_only = sum(code == "BPL-P") >= 1 & 
      sum(code %in% c("BPL-N", "BPL-I")) == 0,
    mix = indifferent + negative + positive >= 2) |> 
  # group_by(gender) |> 
  # group_by(white) |> 
  summarize(across(c(indifferent:mix), c(sum, mean)))

# Broader landlord action
transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    absent = sum(code == "BLA-A") >= 1,
    control = sum(code == "BLA-C") >= 1,
    disresp = sum(code == "BLA-D") >= 1,
    harass = sum(code == "BLA-H") >= 1,
    neutral = sum(code == "BLA-N") >= 1) |> 
  summarize(across(c(absent:neutral), c(sum, mean)))

# Landlord action following eviction notice
transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    harass = sum(code == "LAF-H") >= 1,
    illegal = sum(code == "LAF-I") >= 1,
    ghost = sum(code == "LAF-G") >= 1,
    labour = sum(code == "LAF-TL") >= 1,
    neutral = sum(code == "LAF-N") >= 1) |> 
  summarize(any_1 = sum(harass + illegal + ghost + labour >= 1),
            any_2 = mean(harass + illegal + ghost + labour >= 1),
            across(c(harass:neutral), c(sum, mean)))


# Court involvement -------------------------------------------------------

transcripts |> 
  filter(category == "ET") |> 
  arrange(transcript) |> 
  group_by(transcript) |> 
  summarize(
    gender = first(gender),
    white = first(race) == "White/Caucasian",
    court = sum(code == "ET-CI") >= 1,
    non_court = sum(code == "ET-NC") >= 1) |> 
  # group_by(gender) |> 
  # group_by(white) |> 
  summarize(court_n = sum(court),
            court_pct = court_n / n())

transcripts |> 
  filter(category == "LA") |> 
  group_by(transcript) |> 
  count(code) |> 
  ungroup() |> 
  count(code) |> 
  mutate(pct_total = n / sum(n)) |> 
  group_by(no = str_detect(code, "-No")) |> 
  mutate(pct_group = n / sum(n)) |> 
  ungroup()

transcripts |> 
  filter(code == "LA-S") |> 
  count(province)



# Sources of support ------------------------------------------------------

# Sources of support
transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    gender = first(gender),
    white = first(race) == "White/Caucasian",
    family = sum(code == "SS-FM") >= 1,
    friends = sum(code == "SS-Fr") >= 1,
    social_media = sum(code == "SS-FM") >= 1,
    money = sum(code == "SS-M") >= 1) |> 
  # group_by(gender) |> 
  # group_by(white) |> 
  summarize(any_1 = sum(family + friends + social_media + money >= 1),
            any_2 = mean(family + friends + social_media + money >= 1),
            across(c(family:money), c(sum, mean)))

# No support
transcripts |> 
  group_by(transcript) |> 
  summarize(no_support = sum(category == "SS") == 0) |> 
  filter(no_support)

# Services used
transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    gender = first(gender),
    white = first(race) == "White/Caucasian",
    non_profit = sum(code == "Serv-HC") >= 1,
    legal_paid = sum(code == "Serv-L") >= 1,
    legal_free = sum(code == "Serv-F") >= 1,
    gov = sum(code == "Serv-G") >= 1) |> 
  # group_by(gender) |> 
  # group_by(white) |> 
  summarize(any_1 = sum(non_profit + legal_paid + legal_free + gov >= 1),
            any_2 = mean(non_profit + legal_paid + legal_free + gov >= 1),
            across(c(non_profit:gov), c(sum, mean)))


# Transition to stable housing --------------------------------------------

transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    gender = first(gender),
    white = first(race) == "White/Caucasian",
    shelter = sum(code == "T-S") >= 1,
    trans = sum(code == "T-TH") >= 1,
    short_term = sum(code == "T-ST") >= 1,
    car = sum(code == "T-C") >= 1,
    street = sum(code == "T-Street") >= 1,
    friends = sum(code == "T-Fr") >= 1,
    family = sum(code == "T-F") >= 1,
    other = sum(code == "T-O") >= 1) |> 
  # group_by(gender) |> 
  group_by(white) |> 
  summarize(any_1 = sum(shelter + trans + short_term + car + street +
                          friends + family + other >= 1),
            any_2 = mean(shelter + trans + short_term + car + street +
                           friends + family + other >= 1),
            homeless_1 = sum(shelter + trans + car + street + friends >= 1),
            homeless_2 = mean(shelter + trans + car + street + friends >= 1),
            across(c(shelter:other), c(sum, mean)))


# Covid -------------------------------------------------------------------

transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    visits = sum(code == "CVD-V") >= 1,
    court = sum(code == "CVD-C") >= 1,
    new_apart = sum(code == "CVD-N") >= 1,
    income = sum(code == "CVD-I") >= 1,
    health = sum(code == "CVD-H") >= 1,
    positive = sum(code == "CVD-P") >= 1,
    other = sum(code == "CVD-V") >= 1) |> 
  summarize(any_neg_1 = sum(visits + court + new_apart + income + health +
                              positive + other >= 1),
            any_neg_2 = mean(visits + court + new_apart + income + health +
                              positive + other >= 1),
            across(c(visits:other), c(sum, mean)))


# Up-down -----------------------------------------------------------------

# Cost
transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    up = sum(code == "UD-C+") >= 1,
    equal = sum(code == "UD-C=") >= 1,
    down = sum(code == "UD-C-") >= 1) |> 
  filter(up + equal + down == 1) |> 
  summarize(across(c(up:down), c(sum, mean)))

# Quality
transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    up = sum(code == "UD-Q+") >= 1,
    equal = sum(code == "UD-Q=") >= 1,
    down = sum(code == "UD-Q-") >= 1) |> 
  filter(up + equal + down == 1) |> 
  summarize(across(c(up:down), c(sum, mean)))

# Size
transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    up = sum(code == "UD-S+") >= 1,
    equal = sum(code == "UD-S=") >= 1,
    down = sum(code == "UD-S-") >= 1) |> 
  filter(up + equal + down == 1) |> 
  summarize(across(c(up:down), c(sum, mean)))

# Tenure
transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    private = sum(code == "UD-TP") >= 1,
    subsid = sum(code == "UD-TS") >= 1,
    non_market = sum(code == "UD-TNM") >= 1,
    owner = sum(code == "UD-TO") >= 1,
    family = sum(code == "UD-FAM") >= 1) |> 
  filter(private + subsid + non_market + owner == 1) |> 
summarize(across(c(private:family), c(sum, mean)))

# Location
transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    up = sum(code == "UD-L+") >= 1,
    equal = sum(code == "UD-L=") >= 1,
    down = sum(code == "UD-L-") >= 1) |> 
  filter(up + equal + down == 1) |> 
  summarize(across(c(up:down), c(sum, mean)))


# Interaction of factors --------------------------------------------------

# Net sentiment table
transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    cost_up = sum(code == "UD-C+") >= 1,
    cost_equal = sum(code == "UD-C=") >= 1,
    cost_down = sum(code == "UD-C-") >= 1,
    qual_up = sum(code == "UD-Q+") >= 1,
    qual_equal = sum(code == "UD-Q=") >= 1,
    qual_down = sum(code == "UD-Q-") >= 1,
    size_up = sum(code == "UD-S+") >= 1,
    size_equal = sum(code == "UD-S=") >= 1,
    size_down = sum(code == "UD-S-") >= 1,
    loc_up = sum(code == "UD-L+") >= 1,
    loc_equal = sum(code == "UD-L=") >= 1,
    loc_down = sum(code == "UD-L-") >= 1) |> 
  mutate(tot_up = cost_down + qual_up + size_up + loc_up,
         tot_equal = cost_equal + qual_equal + size_equal + loc_equal,
         tot_down = cost_up + qual_down + size_down + loc_down) |> 
  transmute(balance = tot_up - tot_down) |>
  count(balance) |> 
  mutate(pct = n / sum(n))
  
# Net sentiment figure
fig_1 <- 
  transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    cost_up = sum(code == "UD-C+") >= 1,
    cost_equal = sum(code == "UD-C=") >= 1,
    cost_down = sum(code == "UD-C-") >= 1,
    qual_up = sum(code == "UD-Q+") >= 1,
    qual_equal = sum(code == "UD-Q=") >= 1,
    qual_down = sum(code == "UD-Q-") >= 1,
    size_up = sum(code == "UD-S+") >= 1,
    size_equal = sum(code == "UD-S=") >= 1,
    size_down = sum(code == "UD-S-") >= 1,
    loc_up = sum(code == "UD-L+") >= 1,
    loc_equal = sum(code == "UD-L=") >= 1,
    loc_down = sum(code == "UD-L-") >= 1) |> 
  mutate(tot_up = cost_down + qual_up + size_up + loc_up,
         tot_equal = cost_equal + qual_equal + size_equal + loc_equal,
         tot_down = cost_up + qual_down + size_down + loc_down) |> 
  transmute(balance = tot_up - tot_down) |>
  ggplot(aes(balance)) +
  geom_bar() +
  scale_x_continuous(name = "Net housing sentiment after eviction",
                     breaks = -4:4, minor_breaks = NULL) +
  scale_y_continuous(name = "Observations") +
  theme_minimal()
  
# Cost faceting figure
fig_2 <- 
  transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    cost_up = sum(code == "UD-C+") >= 1,
    cost_equal = sum(code == "UD-C=") >= 1,
    cost_down = sum(code == "UD-C-") >= 1,
    qual_up = sum(code == "UD-Q+") >= 1,
    qual_equal = sum(code == "UD-Q=") >= 1,
    qual_down = sum(code == "UD-Q-") >= 1,
    size_up = sum(code == "UD-S+") >= 1,
    size_equal = sum(code == "UD-S=") >= 1,
    size_down = sum(code == "UD-S-") >= 1,
    loc_up = sum(code == "UD-L+") >= 1,
    loc_equal = sum(code == "UD-L=") >= 1,
    loc_down = sum(code == "UD-L-") >= 1) |> 
  mutate(tot_up = qual_up + size_up + loc_up,
         tot_equal = qual_equal + size_equal + loc_equal,
         tot_down = qual_down + size_down + loc_down,
         balance = tot_up - tot_down,
         cost = ordered(
           case_when(cost_up ~ "Higher", cost_equal ~ "Same", 
                     cost_down ~ "Lower"),
           levels = c("Lower", "Same", "Higher"))) |> 
  filter(!is.na(cost)) |> 
  ggplot(aes(cost, balance)) +
  geom_violin() +
  geom_jitter(width = 0.1, height = 0.1) +
  scale_x_discrete(name = "Cost of post-eviction housing") +
  scale_y_continuous(name = "Net housing sentiment after eviction") +
  theme_minimal()
  
# Cost faceting table
transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    cost_up = sum(code == "UD-C+") >= 1,
    cost_equal = sum(code == "UD-C=") >= 1,
    cost_down = sum(code == "UD-C-") >= 1,
    qual_up = sum(code == "UD-Q+") >= 1,
    qual_equal = sum(code == "UD-Q=") >= 1,
    qual_down = sum(code == "UD-Q-") >= 1,
    size_up = sum(code == "UD-S+") >= 1,
    size_equal = sum(code == "UD-S=") >= 1,
    size_down = sum(code == "UD-S-") >= 1,
    loc_up = sum(code == "UD-L+") >= 1,
    loc_equal = sum(code == "UD-L=") >= 1,
    loc_down = sum(code == "UD-L-") >= 1) |> 
  mutate(tot_up = qual_up + size_up + loc_up,
         tot_equal = qual_equal + size_equal + loc_equal,
         tot_down = qual_down + size_down + loc_down,
         balance = tot_up - tot_down,
         cost = ordered(
           case_when(cost_up ~ "Higher", cost_equal ~ "Same", 
                     cost_down ~ "Lower"),
           levels = c("Lower", "Same", "Higher"))) |> 
  group_by(cost) |> 
  summarize(n_pos = sum(balance > 0),
            n_neg = sum(balance < 0),
            n_neut = sum(balance == 0),
            balance = mean(balance))
  
# Tenure faceting figure
fig_3 <- 
  transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    cost_up = sum(code == "UD-C+") >= 1,
    cost_equal = sum(code == "UD-C=") >= 1,
    cost_down = sum(code == "UD-C-") >= 1,
    qual_up = sum(code == "UD-Q+") >= 1,
    qual_equal = sum(code == "UD-Q=") >= 1,
    qual_down = sum(code == "UD-Q-") >= 1,
    size_up = sum(code == "UD-S+") >= 1,
    size_equal = sum(code == "UD-S=") >= 1,
    size_down = sum(code == "UD-S-") >= 1,
    loc_up = sum(code == "UD-L+") >= 1,
    loc_equal = sum(code == "UD-L=") >= 1,
    loc_down = sum(code == "UD-L-") >= 1,
    tenure = case_when(sum(code == "UD-TP") >= 1 ~ "private",
                       sum(code %in% c("UD-TNM", "UD-TS")) >= 1 ~ "non_market",
                       sum(code == "UD-TO") >= 1 ~ "owner")) |> 
  mutate(tot_up = cost_down + qual_up + size_up + loc_up,
         tot_equal = qual_equal + size_equal + loc_equal,
         tot_down = cost_up + qual_down + size_down + loc_down,
         balance = tot_up - tot_down) |> 
  filter(!is.na(tenure)) |> 
  ggplot(aes(tenure, balance)) +
  geom_violin() +
  geom_jitter(width = 0.1, height = 0.1) +
  scale_x_discrete(name = "Post-eviction tenure type") +
  scale_y_continuous(name = "Net housing sentiment after eviction") +
  theme_minimal()

# Tenure faceting table
transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    cost_up = sum(code == "UD-C+") >= 1,
    cost_equal = sum(code == "UD-C=") >= 1,
    cost_down = sum(code == "UD-C-") >= 1,
    qual_up = sum(code == "UD-Q+") >= 1,
    qual_equal = sum(code == "UD-Q=") >= 1,
    qual_down = sum(code == "UD-Q-") >= 1,
    size_up = sum(code == "UD-S+") >= 1,
    size_equal = sum(code == "UD-S=") >= 1,
    size_down = sum(code == "UD-S-") >= 1,
    loc_up = sum(code == "UD-L+") >= 1,
    loc_equal = sum(code == "UD-L=") >= 1,
    loc_down = sum(code == "UD-L-") >= 1,
    tenure = case_when(sum(code == "UD-TP") >= 1 ~ "private",
                       sum(code %in% c("UD-TNM", "UD-TS")) >= 1 ~ "non_market",
                       sum(code == "UD-TO") >= 1 ~ "owner")) |> 
  mutate(tot_up = cost_down + qual_up + size_up + loc_up,
         tot_equal = qual_equal + size_equal + loc_equal,
         tot_down = cost_up + qual_down + size_down + loc_down,
         balance = tot_up - tot_down) |> 
  group_by(tenure) |> 
  summarize(balance = mean(balance))
  
# Gender faceting figure
fig_4 <- 
  transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    cost_up = sum(code == "UD-C+") >= 1,
    cost_equal = sum(code == "UD-C=") >= 1,
    cost_down = sum(code == "UD-C-") >= 1,
    qual_up = sum(code == "UD-Q+") >= 1,
    qual_equal = sum(code == "UD-Q=") >= 1,
    qual_down = sum(code == "UD-Q-") >= 1,
    size_up = sum(code == "UD-S+") >= 1,
    size_equal = sum(code == "UD-S=") >= 1,
    size_down = sum(code == "UD-S-") >= 1,
    loc_up = sum(code == "UD-L+") >= 1,
    loc_equal = sum(code == "UD-L=") >= 1,
    loc_down = sum(code == "UD-L-") >= 1,
    gender = first(gender)) |> 
  mutate(tot_up = cost_down + qual_up + size_up + loc_up,
         tot_equal = qual_equal + size_equal + loc_equal,
         tot_down = cost_up + qual_down + size_down + loc_down,
         balance = tot_up - tot_down) |> 
  filter(!is.na(gender), gender != "Prefer not to say") |> 
  ggplot(aes(gender, balance)) +
  geom_violin() +
  geom_jitter(width = 0.1, height = 0.1) +
  scale_x_discrete(name = "Gender") +
  scale_y_continuous(name = "Net housing sentiment after eviction") +
  theme_minimal()

# Gender faceting table
transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    cost_up = sum(code == "UD-C+") >= 1,
    cost_equal = sum(code == "UD-C=") >= 1,
    cost_down = sum(code == "UD-C-") >= 1,
    qual_up = sum(code == "UD-Q+") >= 1,
    qual_equal = sum(code == "UD-Q=") >= 1,
    qual_down = sum(code == "UD-Q-") >= 1,
    size_up = sum(code == "UD-S+") >= 1,
    size_equal = sum(code == "UD-S=") >= 1,
    size_down = sum(code == "UD-S-") >= 1,
    loc_up = sum(code == "UD-L+") >= 1,
    loc_equal = sum(code == "UD-L=") >= 1,
    loc_down = sum(code == "UD-L-") >= 1,
    gender = first(gender)) |> 
  mutate(tot_up = cost_down + qual_up + size_up + loc_up,
         tot_equal = qual_equal + size_equal + loc_equal,
         tot_down = cost_up + qual_down + size_down + loc_down,
         balance = tot_up - tot_down) |> 
  filter(!is.na(gender), gender != "Prefer not to say") |> 
  group_by(gender) |> 
  summarize(balance = mean(balance))

# Race faceting figure
# fig_4 <- 
  transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    cost_up = sum(code == "UD-C+") >= 1,
    cost_equal = sum(code == "UD-C=") >= 1,
    cost_down = sum(code == "UD-C-") >= 1,
    qual_up = sum(code == "UD-Q+") >= 1,
    qual_equal = sum(code == "UD-Q=") >= 1,
    qual_down = sum(code == "UD-Q-") >= 1,
    size_up = sum(code == "UD-S+") >= 1,
    size_equal = sum(code == "UD-S=") >= 1,
    size_down = sum(code == "UD-S-") >= 1,
    loc_up = sum(code == "UD-L+") >= 1,
    loc_equal = sum(code == "UD-L=") >= 1,
    loc_down = sum(code == "UD-L-") >= 1,
    white = first(race) == "White/Caucasian") |> 
  mutate(tot_up = cost_down + qual_up + size_up + loc_up,
         tot_equal = qual_equal + size_equal + loc_equal,
         tot_down = cost_up + qual_down + size_down + loc_down,
         balance = tot_up - tot_down) |> 
  filter(!is.na(white)) |> 
  ggplot(aes(white, balance)) +
  geom_violin() +
  geom_jitter(width = 0.1, height = 0.1) +
  scale_x_discrete(name = "White") +
  scale_y_continuous(name = "Net housing sentiment after eviction") +
  theme_minimal()

# Combined figure
library(patchwork)

fig_1 + fig_2 + fig_3 + fig_4 + plot_annotation(tag_levels = "A")


# Impacts of eviction -----------------------------------------------------

# Mobility
transcripts |> 
  group_by(transcript) |>
  summarize(
    gender = first(gender),
    white = first(race) == "White/Caucasian",
    stayed = sum(code == "M-S") >= 1,
    nbhd = sum(code == "M-N") >= 1,
    city = sum(code == "M-C") >= 1,
    region = sum(code == "M-R") >= 1,
    prov = sum(code == "M-P") >= 1,
    country = sum(code == "M-out") >= 1,
    other = sum(code == "M-O") >= 1,
    social_neg = sum(code == "EIL-SL") >= 1,
    employ_neg = sum(code == "EIL-AE") >= 1) |> 
  filter(stayed + nbhd + city + region + prov + country == 1) |> 
  # group_by(gender) |>
  # group_by(white) |>
  # group_by(social_neg) |> 
  # group_by(stayed) |> 
  summarize(any_1 = sum(nbhd + city + region + prov + country >= 1),
            any_2 = mean(nbhd + city + region + prov + country >= 1),
            across(where(is.logical), c(sum, mean)))

# Household composition change
transcripts |> 
  group_by(transcript) |>
  summarize(
    gender = first(gender),
    white = first(race) == "White/Caucasian",
    change = sum(code == "HCC-Y") >= 1,
    no_change = sum(code == "HCC-N") >= 1) |> 
  filter(change + no_change == 1) |> 
  # group_by(gender) |>
  # group_by(white) |>
  summarize(across(where(is.logical), c(sum, mean)))

# Emotional impact
transcripts |> 
  group_by(transcript) |>
  summarize(
    gender = first(gender),
    white = first(race) == "White/Caucasian",
    hope = sum(code == "E-H") >= 1,
    frust = sum(code == "E-F") >= 1,
    stress = sum(code == "E-S") >= 1,
    anxiety = sum(code == "E-Ax") >= 1,
    fear = sum(code == "E-FR") >= 1,
    isolat = sum(code == "E-I") >= 1,
    anger = sum(code == "E-An") >= 1,
    sad = sum(code == "E-Sa") >= 1,
    relief = sum(code == "E-R") >= 1,
    lucky = sum(code == "E-PL") >= 1) |> 
  # group_by(gender) |>
  # group_by(white) |>
  # group_by(gender, white) |> 
  summarize(across(where(is.logical), c(sum, mean)))

