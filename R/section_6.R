#### 6. SOURCES OF SUPPORT #####################################################

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
    any = non_profit + legal_paid + legal_free + gov > 0, .by = transcript) |> 
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
  theme_minimal()

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

