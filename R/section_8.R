#### 8. IMPACTS OF EVICTION ####################################################

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
    employ_neg = sum(code == "EIL-AE") >= 1,
    isolat = sum(code == "E-I") >= 1,
    lucky = sum(code == "E-PL") >= 1) |> 
  filter(stayed + nbhd + city + region + prov + country == 1) |> 
  # group_by(gender) |>
  # group_by(white) |>
  # group_by(social_neg) |> 
  group_by(stayed) |>
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
  # filter(!is.na(white)) |>
  group_by(f_nw = gender == "Female" & !white) |>
  summarize(across(where(is.logical), c(sum, mean)))

# Luck by housing outcome/tenure
transcripts |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    gender = first(gender),
    white = first(race) == "White/Caucasian",
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
  group_by(ten = tenure == "Non-market rental") |>
  summarize(across(where(is.logical), c(sum, mean))) |> 
  # select(bal, positive_1, positive_2, lucky_1, lucky_2)
  select(ten, positive_1, positive_2, lucky_1, lucky_2)

# Finding new accommodation
transcripts |> 
  group_by(transcript) |>
  summarize(
    gender = first(gender),
    white = first(race) == "White/Caucasian",
    difficulty = sum(code == "EIL-D") >= 1) |> 
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

