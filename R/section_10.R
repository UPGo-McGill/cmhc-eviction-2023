#### 10. RECOMMENDATIONS #######################################################

source("R/002_code_checking.R")

t_rec <- function(...) {
  transcripts |> 
    summarize(
      province = first(province),
      gender = first(gender),
      race = first(race),
      children = first(children),
      pets = first(pets),
      income = first(income),
      rent_control = sum(code == "R-RC") >= 1,
      more_housing = sum(code == "R-M") >= 1,
      non_market = sum(code == "R-NM") >= 1,
      limit_private = sum(code == "R-LPO") >= 1,
      rent_reg = sum(code == "R-RR") >= 1,
      stronger_protect = sum(code == "R-SP") >= 1,
      better_ed = sum(code == "R-EI") >= 1,
      other = sum(code == "R-O") >= 1,
      any = rent_control + more_housing + non_market + limit_private + 
        rent_reg + stronger_protect + better_ed + other >= 1, 
      .by = transcript) |> 
    group_by(...) |> 
    summarize(across(c(rent_control:any), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

t_rec()
t_rec(province)
t_rec(qc = province == "Quebec")
t_rec(gender)
t_rec(race)
t_rec(white = race == "white")
t_rec(children)
t_rec(pets)
t_rec(income)
t_rec(high_stress = income == "50 - 100")



transcripts |> 
  summarize(
    non_market = sum(code == "UD-TNM") > 0,
    rent_control = sum(code == "R-RC") > 0,
    more_housing = sum(code == "R-M") > 0,
    non_market = sum(code == "R-NM") > 0,
    limit_private = sum(code == "R-LPO") > 0,
    rent_reg = sum(code == "R-RR") > 0,
    stronger_protect = sum(code == "R-SP") > 0,
    better_ed = sum(code == "R-EI") > 0,
    other = sum(code == "R-O") > 0, .by = transcript) |> 
  summarize(across(c(rent_control:other), \(x) {
    paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}), .by = non_market)

transcripts |> 
  summarize(
    non_market = sum(code %in% c("UD-TNM", "UD-TS")) > 0,
    rental = sum(code == "UD-TP") > 0,
    owner = sum(code == "UD-TO") > 0,
    insecure = sum(code == "EIL-PS") > 0, 
    .by = transcript) |> 
  mutate(tenure = case_when(
    non_market ~ "non-market",
    rental ~ "rental",
    owner ~ "owner")) |> 
  summarize(across(c(insecure), \(x) {
    paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}), .by = tenure)

transcripts |> 
  summarize(
    own_use = sum(code == "ET-OW") > 0,
    pess = sum(code == "LA-NoP") > 0, 
    .by = transcript) |> 
  summarize(across(pess, \(x) {
    paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}), .by = own_use)

transcripts |> 
  summarize(
    own_use = sum(code == "ET-OW") > 0,
    legal = sum(code == "LA-C") > 0, 
    .by = transcript) |> 
  summarize(across(legal, \(x) {
    paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}), .by = own_use)


transcripts |> 
  summarize(
    own_use = sum(code == "ET-OW") > 0,
    legal = sum(code == "ET-CI") > 0, 
    .by = transcript) |> 
  summarize(across(legal, \(x) {
    paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}), .by = own_use)


transcripts |> 
  summarize(
    province = first(province),
    gender = first(gender),
    race = first(race),
    children = first(children),
    pets = first(pets),
    income = first(income),
    legal = sum(code == "LA-C") > 0, 
    own_use = sum(code == "ET-OW") > 0,
    reno = sum(code == "ET-R") > 0,
    sale = sum(code == "ET-S") > 0,
    retal = sum(code == "ET-RT") > 0,
    other = sum(code == "ET-OL") > 0, .by = transcript) |> 
  group_by(nb = province == "New Brunswick") |> 
  summarize(across(c(own_use:other), \(x) {
    paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))


transcripts |> 
  summarize(
    province = first(province),
    gender = first(gender),
    race = first(race),
    children = first(children),
    pets = first(pets),
    income = first(income),
    legal = sum(code == "LA-C") > 0, 
    own_use = sum(code == "ET-OW") > 0,
    reno = sum(code == "ET-R") > 0,
    sale = sum(code == "ET-S") > 0,
    retal = sum(code == "ET-RT") > 0,
    other = sum(code == "ET-OL") > 0, 
    court = sum(code == "ET-CI") > 0, .by = transcript) |> 
  group_by(court) |> 
  summarize(across(c(own_use:other), \(x) {
    paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))


