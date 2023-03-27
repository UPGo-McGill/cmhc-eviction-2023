#### 7. SOURCES OF SUPPORT #####################################################

t_su <- function(...) {
  transcripts |> 
    summarize(
      across(c(province, gender, race, children, pets, income, disability),
             first),
      corporate = sum(code == "LT-C") >= 1,
      non_profit = sum(code == "Serv-HC") >= 1,
      legal_paid = sum(code == "Serv-L") >= 1,
      legal_free = sum(code == "Serv-F") >= 1,
      gov = sum(code == "Serv-G") >= 1,
      any = non_profit + legal_paid + legal_free + gov > 0, .by = id) |> 
    group_by(...) |> 
    summarize(across(c(non_profit:any), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

t_ss <- function(...) {
  transcripts |> 
    summarize(
      across(c(province, gender, race, children, pets, income, disability),
             first),
      corporate = sum(code == "LT-C") >= 1,
      family = sum(code == "SS-FM") >= 1,
      friends = sum(code == "SS-Fr") >= 1,
      social_media = sum(code == "SS-FM") >= 1,
      money = sum(code == "SS-M") >= 1,
      any = family + friends + social_media + money > 0, .by = id) |> 
    group_by(...) |> 
    summarize(across(c(family:any), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}


# Services used -----------------------------------------------------------

t_su()
t_su(province)
t_su(gender)
t_su(race)
t_su(race == "white")
t_su(gender, race == "white")
t_su(children)
t_su(pets)
t_su(income)
t_su(disability)
t_su(corporate)

# Figure 2
figure_2 <- 
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
    any = non_profit + legal_paid + legal_free + gov > 0, .by = id) |> 
  group_by(income) |> 
  summarize(across(c(non_profit:any), mean)) |> 
  filter(!is.na(income)) |> 
  pivot_longer(cols = -income) |> 
  mutate(name = case_when(
    name == "any" ~ "Any services",
    name == "gov" ~ "Government",
    name == "legal_free" ~ "Legal (free)",
    name == "legal_paid" ~ "Legal (paid)",
    name == "non_profit" ~ "Non-profit organization"
  )) |> 
  mutate(income = paste0(income, "%")) |> 
  ggplot(aes(income, value)) +
  geom_col() +
  facet_wrap(~name) +
  scale_y_continuous(name = "% of respondents", labels = scales::percent) +
  scale_x_discrete(name = "% of income used for rent") + 
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

ggsave("output/figure_2.png", figure_2, width = 7, height = 4, units = "in")


# Informal support --------------------------------------------------------

t_ss()
t_ss(province)
t_ss(gender)
t_ss(race)
t_ss(race == "white")
t_ss(gender, race == "white")
t_ss(children)
t_ss(pets)
t_ss(income)
t_ss(income == "50 - 100")
t_ss(disability)
t_ss(corporate)
