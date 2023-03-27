#### 5. THE EVICTION PROCESS ###################################################

t_ci <- function(...) {
  transcripts |> 
    summarize(
      across(c(province, gender, race, children, pets, income, disability),
             first),
      corporate = sum(code == "LT-C") >= 1,
      court = sum(code == "ET-CI") >= 1,
      non_court = sum(code == "ET-NC") >= 1, .by = id) |> 
    group_by(...) |> 
    summarize(across(c(court:non_court), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

t_la <- function(...) {
  transcripts |> 
    summarize(
      across(c(province, gender, race, children, pets, income, disability),
             first),
      corporate = sum(code == "LT-C") >= 1,
      court = sum(code == "LA-C") >= 1,
      settle = sum(code == "LA-S") >= 1,
      pess = sum(code == "LA-NoP") >= 1,
      capacity = sum(code == "LA-NoC") >= 1,
      external = sum(code == "LA-NoE") >= 1,
      other = sum(code == "LA-NoO") >= 1, .by = id) |> 
    group_by(...) |> 
    summarize(across(c(court:other), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

t_cv <- function(...) {
  transcripts |> 
    summarize(
      across(c(province, gender, race, children, pets, income, disability),
             first),
      corporate = sum(code == "LT-C") >= 1,
      disability = first(disability),
      visits = sum(code == "CVD-V") >= 1,
      court = sum(code == "CVD-C") >= 1,
      new_apart = sum(code == "CVD-N") >= 1,
      inc = sum(code == "CVD-I") >= 1,
      health = sum(code == "CVD-H") >= 1,
      positive = sum(code == "CVD-P") >= 1,
      other = sum(code == "CVD-V") >= 1,
      any_neg = sum(visits + court + new_apart + inc + health + other) >= 1, 
      .by = id) |> 
    group_by(...) |> 
    summarize(across(c(visits:any_neg), \(x) {
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
t_ci(disability)
t_ci(corporate)

transcripts |> 
  summarize(
    province = first(province),
    court = sum(code == "ET-CI") >= 1,
    own_use = sum(code == "ET-OW") > 0,
    reno = sum(code == "ET-R") > 0,
    sale = sum(code == "ET-S") > 0,
    retal = sum(code == "ET-RT") > 0,
    other = sum(code == "ET-OL") > 0, .by = id) |> 
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
t_la(disability)
t_la(corporate)
t_la(province, corporate)

transcripts |> 
  filter(category == "LA") |> 
  group_by(id) |> 
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


# Covid -------------------------------------------------------------------

t_cv()
t_cv(province)
t_cv(gender)
t_cv(race)
t_cv(race == "white")
t_cv(gender, race == "white")
t_cv(children)
t_cv(pets)
t_cv(income)
t_cv(disability)
t_cv(corporate)
