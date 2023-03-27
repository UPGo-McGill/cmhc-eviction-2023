#### 5. PREVIOUS HOUSING INSTABILITY ###########################################

t_bhe <- function(...) {
  transcripts |> 
    summarize(
      across(c(province, gender, race, children, pets, income, disability),
             first),
      eviction = sum(code == "BHE-FM") >= 1,
      instability = sum(code == "BHE-HI") >= 1,
      discrimination = sum(code == "BHE-D") >= 1,
      any = eviction + instability + discrimination >= 1,
      .by = id) |> 
    group_by(...) |> 
    summarize(across(c(eviction:any), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}),
      .groups = "drop")
}

t_out <- function(...) {
  transcripts |> 
    summarize(
      across(c(province, gender, race, children, pets, income, disability),
             first),
      eviction = sum(code == "BHE-FM") >= 1,
      instability = sum(code == "BHE-HI") >= 1,
      discrimination = sum(code == "BHE-D") >= 1,
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
      tot_up = cost_down + qual_up + size_up + loc_up,
      tot_equal = cost_equal + qual_equal + size_equal + loc_equal,
      tot_down = cost_up + qual_down + size_down + loc_down,
      balance = tot_up - tot_down, 
      .by = id) |> 
    group_by(...) |> 
    summarize(balance = mean(balance), n = n())
}


# Instability -------------------------------------------------------------

t_bhe()
t_bhe(province)
t_bhe(gender)
t_bhe(race)
t_bhe(white = race == "white")
t_bhe(indig = race == "indigenous")
t_bhe(male = gender == "Male", white = race == "white")
t_bhe(non_white_male = gender == "Male" & race != "white")
t_bhe(children)
t_bhe(pets)
t_bhe(income)
t_bhe(high_stress = income == "50 - 100")
t_bhe(disability)

t_out(eviction)
t_out(instability)
t_out(discrimination)
t_out(any = eviction + instability + discrimination > 0)
