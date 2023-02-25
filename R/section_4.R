#### 4. THE LANDLORD ###########################################################

t_lt <- 
  transcripts |> 
  filter(category == "LT") |> 
  arrange(transcript) |> 
  group_by(transcript) |>
  summarize(
    individual = sum(code == "LT-I") >= 1,
    corporate = sum(code == "LT-C") >= 1,
    public = sum(code == "LT-PH") >= 1,
    non_profit = sum(code == "LT-NP") >= 1,
    owned_other = sum(code == "LT-RP") >= 1)

t_bpl <- function(...) {
  transcripts |> 
    arrange(transcript) |> 
    group_by(transcript) |>
    summarize(
      province = first(province),
      gender = first(gender),
      race = first(race),
      children = first(children),
      pets = first(pets),
      indifferent = sum(code == "BPL-I") >= 1,
      negative = sum(code == "BPL-N") >= 1,
      positive = sum(code == "BPL-P") >= 1,
      indif_only = sum(code == "BPL-I") >= 1 & 
        sum(code %in% c("BPL-N", "BPL-P")) == 0,
      neg_only = sum(code == "BPL-N") >= 1 & 
        sum(code %in% c("BPL-I", "BPL-P")) == 0,
      pos_only = sum(code == "BPL-P") >= 1 & 
        sum(code %in% c("BPL-N", "BPL-I")) == 0,
      mix = indif_only + neg_only + pos_only == 0) |> 
    group_by(...) |> 
    summarize(across(c(indifferent:mix), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

t_bla <- function(...) {
  transcripts |> 
    arrange(transcript) |> 
    group_by(transcript) |>
    summarize(
      province = first(province),
      gender = first(gender),
      race = first(race),
      children = first(children),
      pets = first(pets),
      absent = sum(code == "BLA-A") >= 1,
      control = sum(code == "BLA-C") >= 1,
      disresp = sum(code == "BLA-D") >= 1,
      harass = sum(code == "BLA-H") >= 1,
      neutral = sum(code == "BLA-N") >= 1) |> 
    group_by(...) |> 
    summarize(across(c(absent:neutral), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

t_laf <- function(...) {
  transcripts |> 
    arrange(transcript) |> 
    group_by(transcript) |>
    summarize(
      province = first(province),
      gender = first(gender),
      race = first(race),
      children = first(children),
      pets = first(pets),
      harass = sum(code == "LAF-H") >= 1,
      illegal = sum(code == "LAF-I") >= 1,
      ghost = sum(code == "LAF-G") >= 1,
      labour = sum(code == "LAF-TL") >= 1,
      neutral = sum(code == "LAF-N") >= 1,
      any = harass + illegal + ghost + labour >= 1) |> 
    group_by(...) |> 
    summarize(across(c(harass:any), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

  
# Landlord type -----------------------------------------------------------

# Landlord type
t_lt |> 
  summarize(across(c(individual:non_profit), sum), .by = owned_other) |> 
  mutate(across(individual:non_profit, \(x) 
                x / sum(individual, corporate, public, non_profit), 
                .names = "{.col}_pct")) |> 
  reframe(
    owned_other = c(owned_other, NA),
    across(individual:non_profit_pct, \(x) c(x, sum(x)))) |> 
  transmute(
    owned_other = c("Landlord did not own other properties",
                    "Landlord did own other properties",
                    "Totals"),
    Individual = paste0(individual, " (", 
                        scales::percent(individual_pct, 0.1), ")"),
    Corporate = paste0(corporate, " (", 
                       scales::percent(corporate_pct, 0.1), ")"),
    `Public/non-profit` = paste0(public + non_profit, " (", scales::percent(
      public_pct + non_profit_pct, 0.1), ")"),
    Totals = paste0(individual + corporate + public + non_profit, " (", 
                    scales::percent(individual_pct + corporate_pct +
                                      public_pct + non_profit_pct, 0.1), ")"))


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
  summarize(across(c(own_use:other), \(x) paste0(sum(x), " (", scales::percent(
    mean(x), 0.1), ")")))

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
  summarize(across(c(multiple), \(x) paste0(sum(x), " (", scales::percent(
    mean(x), 0.1), ")")))


# Landlord perception and behaviour ---------------------------------------

# Broader landlord perception
t_bpl()
t_bpl(province)
t_bpl(gender)
t_bpl(race)
t_bpl(race == "white")
t_bpl(gender, race == "white")
t_bpl(children)
t_bpl(pets)

# Broader landlord action
t_bla()
t_bla(province)
t_bla(gender)
t_bla(race)
t_bla(race == "white")
t_bla(gender, race == "white")
t_bla(children)
t_bla(pets)

# Landlord action following eviction notice
t_laf()
t_laf(province)
t_laf(gender)
t_laf(race)
t_laf(race == "white")
t_laf(gender, race == "white")
t_laf(children)
t_laf(pets)
