#### 5. THE EVICTION PROCESS ###################################################

t_ci <- function(...) {
  transcripts |> 
    summarize(
      province = first(province),
      gender = first(gender),
      race = first(race),
      children = first(children),
      pets = first(pets),
      income = first(income),
      court = sum(code == "ET-CI") >= 1,
      non_court = sum(code == "ET-NC") >= 1, .by = transcript) |> 
    group_by(...) |> 
    summarize(across(c(court:non_court), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

t_la <- function(...) {
  transcripts |> 
    summarize(
      province = first(province),
      gender = first(gender),
      race = first(race),
      children = first(children),
      pets = first(pets),
      income = first(income),
      court = sum(code == "LA-C") >= 1,
      settle = sum(code == "LA-S") >= 1,
      pess = sum(code == "LA-NoP") >= 1,
      capacity = sum(code == "LA-NoC") >= 1,
      external = sum(code == "LA-NoE") >= 1,
      other = sum(code == "LA-NoO") >= 1, .by = transcript) |> 
    group_by(...) |> 
    summarize(across(c(court:other), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

t_su <- function(...) {
  transcripts |> 
    summarize(
      province = first(province),
      gender = first(gender),
      race = first(race),
      children = first(children),
      pets = first(pets),
      income = first(income),
      non_profit = sum(code == "Serv-HC") >= 1,
      legal_paid = sum(code == "Serv-L") >= 1,
      legal_free = sum(code == "Serv-F") >= 1,
      gov = sum(code == "Serv-G") >= 1,
      any = non_profit + legal_paid + legal_free + gov > 0, .by = transcript) |> 
    group_by(...) |> 
    summarize(across(c(non_profit:any), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}


# Court involvement -------------------------------------------------------

t_ci()
t_ci(province)
t_ci(gender)
t_ci(race)
t_ci(race == "white")
t_ci(gender, race == "white")
t_ci(children)
t_ci(pets)
t_ci(income)

transcripts |> 
  summarize(
    province = first(province),
    court = sum(code == "ET-CI") >= 1,
    own_use = sum(code == "ET-OW") > 0,
    reno = sum(code == "ET-R") > 0,
    sale = sum(code == "ET-S") > 0,
    retal = sum(code == "ET-RT") > 0,
    other = sum(code == "ET-OL") > 0, .by = transcript) |> 
  group_by(court) |> 
  summarize(across(c(own_use:other), \(x) {
    paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))


# Negotiated settlements --------------------------------------------------

t_la()
t_la(province)
t_la(gender)
t_la(race)
t_la(race == "white")
t_la(gender, race == "white")
t_la(children)
t_la(pets)
t_la(income)

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

# Services used
t_su()
t_su(province)
t_su(gender)
t_su(race)
t_su(race == "white")
t_su(gender, race == "white")
t_su(children)
t_su(pets)
t_su(income)


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

