#### 4. THE LANDLORD ###########################################################

t_lt <- function(...) {
  transcripts |> 
    summarize(
      across(c(province, gender, race, children, pets, income, disability),
             first),
      individual = sum(code == "LT-I") >= 1,
      corporate = sum(code == "LT-C") >= 1,
      public = sum(code == "LT-PH") >= 1,
      non_profit = sum(code == "LT-NP") >= 1,
      owned_other = sum(code == "LT-RP") >= 1, .by = id) |> 
    group_by(...) |> 
    summarize(across(c(individual:owned_other), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

t_bpl <- function(...) {
  transcripts |> 
    summarize(
      province = first(province),
      gender = first(gender),
      race = first(race),
      children = first(children),
      pets = first(pets),
      income = first(income),
      disability = first(disability),
      corporate = sum(code == "LT-C") >= 1,
      indifferent = sum(code == "BPL-I") >= 1,
      negative = sum(code == "BPL-N") >= 1,
      positive = sum(code == "BPL-P") >= 1,
      indif_only = sum(code == "BPL-I") >= 1 & 
        sum(code %in% c("BPL-N", "BPL-P")) == 0,
      neg_only = sum(code == "BPL-N") >= 1 & 
        sum(code %in% c("BPL-I", "BPL-P")) == 0,
      pos_only = sum(code == "BPL-P") >= 1 & 
        sum(code %in% c("BPL-N", "BPL-I")) == 0,
      mix = indif_only + neg_only + pos_only == 0, .by = id) |> 
    group_by(...) |> 
    summarize(across(c(indifferent:mix), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

t_bla <- function(...) {
  transcripts |> 
    summarize(
      province = first(province),
      gender = first(gender),
      race = first(race),
      children = first(children),
      pets = first(pets),
      income = first(income),
      corporate = sum(code == "LT-C") >= 1,
      disability = first(disability),
      absent = sum(code == "BLA-A") >= 1,
      control = sum(code == "BLA-C") >= 1,
      disresp = sum(code == "BLA-D") >= 1,
      harass = sum(code == "BLA-H") >= 1,
      neutral = sum(code == "BLA-N") >= 1, 
      neg = absent + control + disresp + harass >= 1,
      .by = id) |> 
    group_by(...) |> 
    summarize(across(c(absent:neg), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

t_laf <- function(...) {
  transcripts |> 
    summarize(
      province = first(province),
      gender = first(gender),
      race = first(race),
      children = first(children),
      pets = first(pets),
      income = first(income),
      disability = first(disability),
      harass = sum(code == "LAF-H") >= 1,
      illegal = sum(code == "LAF-I") >= 1,
      ghost = sum(code == "LAF-G") >= 1,
      labour = sum(code == "LAF-TL") >= 1,
      neutral = sum(code == "LAF-N") >= 1,
      any = harass + illegal + ghost + labour >= 1, .by = id) |> 
    group_by(...) |> 
    summarize(across(c(harass:any), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

  
# Landlord type -----------------------------------------------------------

t_lt()
t_lt(province)
t_lt(gender)
t_lt(race)
t_lt(race == "white")
t_lt(gender, race == "white")
t_lt(children)
t_lt(pets)
t_lt(income)
t_lt(disability)

# Landlord type
transcripts |> 
  filter(category == "LT") |> 
  summarize(
    individual = sum(code == "LT-I") >= 1,
    corporate = sum(code == "LT-C") >= 1,
    public = sum(code == "LT-PH") >= 1,
    non_profit = sum(code == "LT-NP") >= 1,
    owned_other = sum(code == "LT-RP") >= 1,
    .by = id) |> 
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
                                      public_pct + non_profit_pct, 0.1), ")")
    ) |> 
  gt::gt()

# Landlord type vs. eviction type
transcripts |> 
  group_by(id) |> 
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
  group_by(id) |> 
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
t_bpl(income)
t_bpl(disability)
t_bpl(corporate)

# Broader landlord action
t_bla()
t_bla(province)
t_bla(gender)
t_bla(race)
t_bla(race == "white")
t_bla(gender, race == "white")
t_bla(children)
t_bla(pets)
t_bla(income)
t_bla(disability)
t_bla(corporate)

transcripts |> 
  summarize(
    province = first(province),
    absent = sum(code == "BLA-A") >= 1,
    control = sum(code == "BLA-C") >= 1,
    disresp = sum(code == "BLA-D") >= 1,
    harass = sum(code == "BLA-H") >= 1,
    neutral = sum(code == "BLA-N") >= 1, .by = id) |> 
  group_by(nb = province == "New Brunswick") |> 
  summarize(neg = sum(absent + control + disresp + harass + neutral > 0),
            pct = mean(absent + control + disresp + harass + neutral > 0))
  
# Landlord action following eviction notice
t_laf()
t_laf(province)
t_laf(gender)
t_laf(race)
t_laf(race == "white")
t_laf(gender, race == "white")
t_laf(children)
t_laf(pets)
t_laf(income)
t_laf(disability)

# Landlord-factor evictions by landlord type and scale
transcripts |> 
  arrange(id) |> 
  group_by(id) |> 
  summarize(
    individual = sum(code == "LT-I") >= 1,
    corporate = sum(code == "LT-C") >= 1,
    tenant = sum(code %in% c("ET-NP", "ET-OT")) >= 1,
    landlord = sum(code %in% c(
      "ET-OW", "ET-R", "ET-S", "ET-RT", "ET-OL")) >= 1,
    multiple = sum(code == "ET-ME") >= 1) |>
  filter(!tenant) |> 
  group_by(multiple) |> 
  summarize(
    ind_n = sum(individual),
    corp_n = sum(corporate)) |>  
  mutate(
    ind_pct = ind_n / sum(ind_n, corp_n),
    corp_pct = corp_n / sum(ind_n, corp_n)) |> 
  reframe(
    multiple = c(multiple, NA),
    ind_n = c(ind_n, sum(ind_n)),
    corp_n = c(corp_n, sum(corp_n)),
    ind_pct = c(ind_pct, sum(ind_pct)),
    corp_pct = c(corp_pct, sum(corp_pct))) |> 
  transmute(
    multiple = c("Single-household eviction", "Multiple-household eviction", 
                 "Totals"),
    individual = paste0(ind_n, " (", scales::percent(ind_pct, 0.1), ")"),
    corporate = paste0(corp_n, " (", scales::percent(corp_pct, 0.1), ")"),
    total = paste0(ind_n + corp_n, " (", scales::percent(
      ind_pct + corp_pct, 0.1), ")")) |> 
  set_names(c(" ", "Individual landlord", "Corporate landlord", "Totals")) |> 
  gt::gt()
