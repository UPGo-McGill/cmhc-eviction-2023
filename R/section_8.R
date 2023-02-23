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
