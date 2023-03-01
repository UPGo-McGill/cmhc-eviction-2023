#### 8. IMPACTS OF EVICTION ####################################################

t_mb <- function(...) {
  transcripts |> 
    summarize(
      across(c(province, gender, race, children, pets, income, disability),
             first),
      stayed = sum(code == "M-S") >= 1,
      nbhd = sum(code == "M-N") >= 1,
      city = sum(code == "M-C") >= 1,
      region = sum(code == "M-R") >= 1,
      prov = sum(code == "M-P") >= 1,
      country = sum(code == "M-out") >= 1,
      other = sum(code == "M-O") >= 1, .by = transcript) |> 
    group_by(...) |> 
    summarize(across(c(stayed:other), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

t_hc <- function(...) {
  transcripts |> 
    summarize(
      across(c(province, gender, race, children, pets, income, disability),
             first),
      change = sum(code == "HCC-Y") >= 1, 
      no_change = sum(code == "HCC-N") >= 1, .by = transcript) |> 
    group_by(...) |> 
    summarize(across(c(change:no_change), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

t_ei <- function(...) {
  transcripts |> 
    summarize(
      across(c(province, gender, race, children, pets, income, disability),
             first),
      hope = sum(code == "E-H") >= 1,
      frust = sum(code == "E-F") >= 1,
      stress = sum(code == "E-S") >= 1,
      anxiety = sum(code == "E-Ax") >= 1,
      fear = sum(code == "E-FR") >= 1,
      isolat = sum(code == "E-I") >= 1,
      anger = sum(code == "E-An") >= 1,
      sad = sum(code == "E-Sa") >= 1,
      other = sum(code == "E-O") >= 1,
      relief = sum(code == "E-R") >= 1,
      lucky = sum(code == "E-PL") >= 1, 
      any_neg = sum(hope + frust + stress + anxiety + fear + isolat + anger +
                      sad + other) >= 1, .by = transcript) |> 
    group_by(...) |> 
    summarize(across(c(hope:any_neg), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

t_eil <- function(...) {
  transcripts |> 
    summarize(
      across(c(province, gender, race, children, pets, income, disability),
             first),
      difficulty = sum(code == "EIL-D") >= 1,
      security = sum(code == "EIL-PS") >= 1,
      health = sum(code == "EIL-H") >= 1,
      positive = sum(code == "EIL-PI") >= 1, .by = transcript) |> 
    group_by(...) |> 
    summarize(across(c(difficulty:positive), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}


# Mobility ----------------------------------------------------------------

t_mb()
t_mb(province)
t_mb(gender)
t_mb(race)
t_mb(white = race == "white")
t_mb(gender, white = race == "white")
t_mb(children)
t_mb(pets)
t_mb(income)
t_mb(high_stress = income == "50 - 100")
t_mb(disability)

# Figure 4
figure_4 <- 
  transcripts |> 
  filter(category == "M") |> 
  summarize(
    province = first(province),
    stayed = sum(code == "M-S") >= 1,
    nbhd = sum(code == "M-N") >= 1,
    city = sum(code == "M-C") >= 1,
    region = sum(code == "M-R") >= 1,
    prov = sum(code == "M-P") >= 1,
    country = sum(code == "M-out") >= 1, .by = transcript) |> 
  summarize(across(c(stayed:country), mean), .by = province) |> 
  pivot_longer(-province) |> 
  mutate(name = case_when(
    name == "stayed" ~ "Stayed in nbhd.",
    name == "nbhd" ~ "Neighbourhood",
    name == "city" ~ "City",
    name == "region" ~ "Region",
    name == "prov" ~ "Province",
    name == "country" ~ "Country")) |> 
  mutate(name = factor(name, levels = rev(c(
    "Stayed in nbhd.", "Neighbourhood", "City", "Region", "Province", 
    "Country")))) |> 
  ggplot(aes(province, value, fill = name)) +
  geom_col() +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "% of respondents", labels = scales::percent) +
  scale_fill_viridis_d(name = "Moved to different...", direction = -1) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("output/figure_4.png", figure_4, width = 7, height = 4, units = "in")

# Loss of social life
transcripts |> 
  filter(sum(category == "M") >= 1, .by = transcript) |> 
  summarize(
    stayed = sum(code == "M-S") >= 1,
    social = sum(code == "EIL-SL") >= 1, .by = transcript) |> 
  group_by(stayed) |> 
  summarize(across(where(is.logical), c(sum, mean)))

# Loss of amenities
transcripts |> 
  filter(sum(category == "M") >= 1, .by = transcript) |> 
  summarize(
    stayed = sum(code == "M-S") >= 1,
    amen = sum(code == "EIL-AE") >= 1, .by = transcript) |> 
  group_by(stayed) |> 
  summarize(across(where(is.logical), c(sum, mean)))


# Household composition change --------------------------------------------

t_hc()
t_hc(province)
t_hc(gender)
t_hc(race)
t_hc(white = race == "white")
t_hc(gender, white = race == "white")
t_hc(children)
t_hc(pets)
t_hc(income)
t_hc(high_stress = income == "50 - 100")
t_hc(disability)

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


# Emotional impact --------------------------------------------------------

t_ei()
t_ei(province)
t_ei(gender)
t_ei(race)
t_ei(white = race == "white")
t_ei(f_nw = gender == "Female" & race != "white")
t_ei(children)
t_ei(pets)
t_ei(income)
t_ei(high_stress = income == "50 - 100")
t_ei(disability)

# Luck by mobility
transcripts |> 
  summarize(
    lucky = sum(code == "E-PL") >= 1,
    stayed = sum(code == "M-S") >= 1,
    .by = transcript) |> 
  summarize(across(c(lucky), \(x) {
    paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}),
    .by = stayed)

# Luck/positive by housing outcome
transcripts |> 
  group_by(transcript) |>
  summarize(
    gender = first(gender),
    white = first(race) == "white",
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
    lucky = sum(code == "E-PL") >= 1,
    positive = sum(code == "EIL-PI") >= 1,
    tenure = case_when(sum(code == "UD-TP") >= 1 ~ "Private rental",
                       sum(code %in% c("UD-TNM", "UD-TS")) >= 1 ~ 
                         "Non-market rental",
                       sum(code == "UD-TO") >= 1 ~ "Ownership")) |> 
  mutate(tot_up = cost_down + qual_up + size_up + loc_up,
         tot_equal = qual_equal + size_equal + loc_equal,
         tot_down = cost_up + qual_down + size_down + loc_down,
         balance = tot_up - tot_down) |> 
  group_by(bal = balance >= 0) |>
  summarize(across(where(is.logical), c(sum, mean))) |> 
  select(bal, positive_1, positive_2, lucky_1, lucky_2)

# Luck/positive by tenure
transcripts |> 
  group_by(transcript) |>
  summarize(
    gender = first(gender),
    white = first(race) == "white",
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
    lucky = sum(code == "E-PL") >= 1,
    positive = sum(code == "EIL-PI") >= 1,
    tenure = case_when(sum(code == "UD-TP") >= 1 ~ "Private rental",
                       sum(code %in% c("UD-TNM", "UD-TS")) >= 1 ~ 
                         "Non-market rental",
                       sum(code == "UD-TO") >= 1 ~ "Ownership")) |> 
  mutate(tot_up = cost_down + qual_up + size_up + loc_up,
         tot_equal = qual_equal + size_equal + loc_equal,
         tot_down = cost_up + qual_down + size_down + loc_down,
         balance = tot_up - tot_down) |> 
  group_by(ten = tenure == "Non-market rental") |>
  summarize(across(where(is.logical), c(sum, mean))) |> 
  select(ten, positive_1, positive_2, lucky_1, lucky_2)

t_eil()
t_eil(province)
t_eil(gender)
t_eil(race)
t_eil(white = race == "white")
t_eil(f_nw = gender == "Female" & race != "white")
t_eil(children)
t_eil(pets)
t_eil(income)
t_eil(high_stress = income == "50 - 100")
t_eil(disability)


# Finding new accommodation
transcripts |> 
  group_by(transcript) |>
  summarize(
    gender = first(gender),
    white = first(race) == "White/Caucasian",
    difficulty = sum(code == "EIL-D") >= 1,
    security = sum(code == "EIL-PS") >= 1,
    health = sum(code == "EIL-H") >= 1,
    positive = sum(code == "EIL-PI") >= 1) |> 
  
  # group_by(gender) |>
  group_by(white) |>
  # filter(!is.na(white)) |>
  # group_by(f_nw = gender == "Female" & !white) |>
  summarize(across(where(is.logical), c(sum, mean)))

# Other impacts
transcripts |> 
  group_by(transcript) |>
  summarize(
    gender = first(gender),
    white = first(race) == "White/Caucasian",
    security = sum(code == "EIL-PS") >= 1,
    health = sum(code == "EIL-H") >= 1,
    positive = sum(code == "EIL-PI") >= 1) |> 
  # group_by(gender) |>
  # group_by(white) |>
  # filter(!is.na(white)) |>
  # group_by(f_nw = gender == "Female" & !white) |>
  summarize(across(where(is.logical), c(sum, mean)))

