#### 7. TRANSITION TO STABLE HOUSING ###########################################

t_t <- function(...) {
  transcripts |> 
    summarize(
      across(c(province, gender, race, children, pets, income, disability),
             first),
      corporate = sum(code == "LT-C") >= 1,
      shelter = sum(code == "T-S") >= 1,
      trans = sum(code == "T-TH") >= 1,
      short_term = sum(code == "T-ST") >= 1,
      car = sum(code == "T-C") >= 1,
      street = sum(code == "T-Street") >= 1,
      friends = sum(code == "T-Fr") >= 1,
      family = sum(code == "T-F") >= 1,
      other = sum(code == "T-O") >= 1, 
      homeless = sum(shelter + car + street + friends) >= 1,
      any = sum(shelter + trans + short_term + car + street + friends + family +
                  other) >= 1, .by = id) |> 
    group_by(...) |> 
    summarize(across(c(shelter:any), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}


# Transition to stable housing --------------------------------------------

t_t()
t_t(province)
t_t(gender)
t_t(race)
t_t(race == "white")
t_t(gender, race == "white")
t_t(children)
t_t(pets)
t_t(income)
t_t(disability)
t_t(corporate)
