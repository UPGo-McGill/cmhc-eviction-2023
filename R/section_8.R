#### 8. RECOMMENDATIONS ########################################################

source("R/002_code_checking.R")

t_rec <- function(field_1, field_2) {
  transcripts |> 
    filter(category == "R") |> 
    arrange(transcript) |> 
    summarize(
      {{ field_1 }} := first({{ field_1 }}),
      rent_control = sum(code == "R-RC"),
      more_housing = sum(code == "R-M"),
      non_market = sum(code == "R-NM"),
      limit_private = sum(code == "R-LPO"),
      rent_reg = sum(code == "R-RR"),
      stronger_protect = sum(code == "R-SP"),
      better_ed = sum(code == "R-EI"),
      other = sum(code == "R-O"), .by = transcript) |> 
    group_by({{ field_2 }}) |> 
    reframe(across(c(rent_control:other), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

transcripts |> 
  filter(category == "R") |> 
  arrange(transcript) |> 
  summarize(
    rent_control = sum(code == "R-RC"),
    more_housing = sum(code == "R-M"),
    non_market = sum(code == "R-NM"),
    limit_private = sum(code == "R-LPO"),
    rent_reg = sum(code == "R-RR"),
    stronger_protect = sum(code == "R-SP"),
    better_ed = sum(code == "R-EI"),
    other = sum(code == "R-O"), .by = transcript) |> 
  summarize(across(c(rent_control:other), \(x) {
    paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))

t_rec(province, province)
t_rec(gender, gender)
t_rec(race, race)
t_rec(race, race == "white")
t_rec(children, children)


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
    non_market = sum(code == "UD-TNM") > 0,
    insecure = sum(code == "EIL-PS") > 0, 
    .by = transcript) |> 
  summarize(across(c(insecure), \(x) {
    paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}), .by = non_market)

transcripts |> 
  summarize(
    own_use = sum(code == "ET-OW") > 0,
    pess = sum(code == "LA-NoP") > 0, 
    .by = transcript) |> 
  summarize(across(pess, \(x) {
    paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}), .by = own_use)

