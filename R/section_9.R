#### 9. COMPARATIVE HOUSING QUALITY ############################################

t_uc <- function(...) {
  transcripts |> 
    summarize(
      across(c(province, gender, race, children, pets, income, disability),
             first),
      corporate = sum(code == "LT-C") >= 1,
      non_market = sum(code %in% c("UD-TNM", "UD-TS")) > 0,
      rental = sum(code == "UD-TP") > 0,
      owner = sum(code == "UD-TO") > 0,
      insecure = sum(code == "EIL-PS") > 0, 
      tenure = case_when(
        non_market ~ "non-market",
        rental ~ "rental",
        owner ~ "owner"),
      up = sum(code == "UD-C+") >= 1,
      equal = sum(code == "UD-C=") >= 1,
      down = sum(code == "UD-C-") >= 1, .by = id) |> 
    filter(up + equal + down >= 1) |> 
    group_by(...) |> 
    summarize(across(c(up:down), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

t_uq <- function(...) {
  transcripts |> 
    summarize(
      across(c(province, gender, race, children, pets, income, disability),
             first),
      corporate = sum(code == "LT-C") >= 1,
      up = sum(code == "UD-Q+") >= 1,
      equal = sum(code == "UD-Q=") >= 1,
      down = sum(code == "UD-Q-") >= 1, .by = id) |> 
    filter(up + equal + down >= 1) |> 
    group_by(...) |> 
    summarize(across(c(up:down), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

t_us <- function(...) {
  transcripts |> 
    summarize(
      across(c(province, gender, race, children, pets, income, disability),
             first),
      corporate = sum(code == "LT-C") >= 1,
      up = sum(code == "UD-S+") >= 1,
      equal = sum(code == "UD-S=") >= 1,
      down = sum(code == "UD-S-") >= 1, .by = id) |> 
    filter(up + equal + down >= 1) |> 
    group_by(...) |> 
    summarize(across(c(up:down), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

t_ul <- function(...) {
  transcripts |> 
    summarize(
      across(c(province, gender, race, children, pets, income, disability),
             first),
      corporate = sum(code == "LT-C") >= 1,
      up = sum(code == "UD-L+") >= 1,
      equal = sum(code == "UD-L=") >= 1,
      down = sum(code == "UD-L-") >= 1, .by = id) |> 
    filter(up + equal + down >= 1) |> 
    group_by(...) |> 
    summarize(across(c(up:down), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

t_ut <- function(...) {
  transcripts |> 
    summarize(
      across(c(province, gender, race, children, pets, income, disability),
             first),
      corporate = sum(code == "LT-C") >= 1,
      private = sum(code == "UD-TP") >= 1,
      non_market = sum(code %in% c("UD-TS", "UD-TNM")) >= 1,
      owner = sum(code == "UD-TO") >= 1,
      family = sum(code == "UD-FAM") >= 1, 
      .by = id) |> 
    filter(private + non_market + owner == 1) |> 
    group_by(...) |> 
    summarize(across(c(private:family), \(x) {
      paste0(sum(x), " (", scales::percent(mean(x), 0.1), ")")}))
}

t_ub <- function(...) {
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
                  other) >= 1,
      private = sum(code == "UD-TP") >= 1,
      non_market = sum(code %in% c("UD-TS", "UD-TNM")) >= 1,
      owner = sum(code == "UD-TO") >= 1,
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
    filter(private + non_market + owner == 1) |> 
    group_by(...) |> 
    count(balance) |> 
    mutate(pct = n / sum(n))
}

t_out <- function(...) {
  transcripts |> 
    summarize(
      across(c(province, gender, race, children, pets, income, disability),
             first),
      corporate = sum(code == "LT-C") >= 1,
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


# Cost --------------------------------------------------------------------

t_uc()
t_uc(province)
t_uc(gender)
t_uc(race)
t_uc(white = race == "white")
t_uc(gender, white = race == "white")
t_uc(children)
t_uc(pets)
t_uc(income)
t_uc(disability)
t_uc(high_stress = income == "50 - 100")
t_uc(tenure)
t_uc(corporate)



# Quality -----------------------------------------------------------------

t_uq()
t_uq(province)
t_uq(gender)
t_uq(race)
t_uq(white = race == "white")
t_uq(gender, white = race == "white")
t_uq(children)
t_uq(pets)
t_uq(income)
t_uq(disability)
t_uq(high_stress = income == "50 - 100")
t_uq(corporate)


# Size --------------------------------------------------------------------

t_us()
t_us(province)
t_us(gender)
t_us(race)
t_us(white = race == "white")
t_us(gender, white = race == "white")
t_us(children)
t_us(pets)
t_us(income)
t_us(disability)
t_us(low_stress = income == "0 - 29")
t_us(corporate)


# Location ----------------------------------------------------------------

t_ul()
t_ul(province)
t_ul(gender)
t_ul(race)
t_ul(white = race == "white")
t_ul(gender, white = race == "white")
t_ul(children)
t_ul(pets)
t_ul(income)
t_ul(disability)
t_ul(low_stress = income == "0 - 29")
t_ul(corporate)


# Tenure ------------------------------------------------------------------

t_ut()
t_ut(province)
t_ut(gender)
t_ut(race)
t_ut(white = race == "white")
t_ut(gender, white = race == "white")
t_ut(children)
t_ut(pets)
t_ut(income)
t_ut(disability)
t_ut(corporate)


# Interaction of factors --------------------------------------------------

t_out()
t_out(province)
t_out(gender)
t_out(race)
t_out(white = race == "white")
t_out(gender, white = race == "white")
t_out(children)
t_out(pets)
t_out(income)
t_out(disability)
t_out(high_stress = income == "50 - 100")
t_out(corporate)

t_ub()
t_ub(any) |> 
  ungroup() |> 
  summarize(avg = sum(balance * n) / sum(n), .by = any)

table_7 <- 
  transcripts |> 
  summarize(
    province = first(province),
    gender = first(gender),
    race = first(race),
    children = first(children),
    pets = first(pets),
    income = first(income),
    private = sum(code == "UD-TP") >= 1,
    non_market = sum(code %in% c("UD-TS", "UD-TNM")) >= 1,
    owner = sum(code == "UD-TO") >= 1,
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
  filter(private + non_market + owner == 1) |> 
  group_by(bal = case_when(
    balance < 0 ~ "neg",
    balance == 0 ~ "neutral",
    balance > 0 ~ "pos")) |>
  summarize(bc = sum(province == "British Columbia"),
            nb = sum(province == "New Brunswick"),
            on = sum(province == "Ontario"),
            qc = sum(province == "Quebec"),
            all = bc + nb + on + qc) |> 
  mutate(across(bc:all,\(x) {
    paste0(x, " (", scales::percent(x / sum(x), 0.1), ")")})) |> 
  relocate(all, .after = bal)

transcripts |> 
  summarize(
    province = first(province),
    gender = first(gender),
    race = first(race),
    children = first(children),
    pets = first(pets),
    income = first(income),
    private = sum(code == "UD-TP") >= 1,
    non_market = sum(code %in% c("UD-TS", "UD-TNM")) >= 1,
    owner = sum(code == "UD-TO") >= 1,
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
  filter(private + non_market + owner == 1) |> 
  summarize(
    bal = "avg", 
    bc = mean(balance[province == "British Columbia"]),
    nb = mean(balance[province == "New Brunswick"]),
    on = mean(balance[province == "Ontario"]),
    qc = mean(balance[province == "Quebec"]),
    all = mean(balance)) |> 
  mutate(across(bc:all, \(x) scales::comma(x, 0.01))) |> 
  relocate(all, .after = bal) |> 
  bind_rows(table_7)

# Net outcome figure
figure_3_1 <- 
  transcripts |> 
  summarize(
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
    balance = tot_up - tot_down, .by = id) |>
  ggplot(aes(balance)) +
  geom_bar() +
  scale_x_continuous(name = "Net housing outcome",
                     breaks = -4:4, minor_breaks = NULL) +
  scale_y_continuous(name = "Observations") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

# Province faceting figure
figure_3_2 <- 
  transcripts |> 
  summarize(
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
    province = first(province), 
    tot_up = cost_down + qual_up + size_up + loc_up,
    tot_equal = qual_equal + size_equal + loc_equal,
    tot_down = cost_up + qual_down + size_down + loc_down,
    balance = tot_up - tot_down, .by = id) |> 
  ggplot(aes(province, balance)) +
  geom_violin() +
  geom_jitter(width = 0.1, height = 0.1) +
  scale_x_discrete(name = "Province") +
  scale_y_continuous(name = "Net housing outcome",
                     breaks = 2 * -2:2) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

# Cost faceting figure
figure_3_3 <- 
  transcripts |> 
  summarize(
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
    tot_up = qual_up + size_up + loc_up,
    tot_equal = qual_equal + size_equal + loc_equal,
    tot_down = qual_down + size_down + loc_down,
    balance = tot_up - tot_down,
    cost = ordered(case_when(cost_up ~ "Higher", cost_equal ~ "Same", 
                             cost_down ~ "Lower"),
                   levels = c("Lower", "Same", "Higher")),
    .by = id) |> 
  filter(!is.na(cost)) |> 
  ggplot(aes(cost, balance)) +
  geom_violin() +
  geom_jitter(width = 0.1, height = 0.1) +
  scale_x_discrete(name = "Cost of post-eviction housing") +
  scale_y_continuous(name = "Net housing outcome") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

# Cost faceting table
transcripts |> 
  arrange(id) |> 
  group_by(id) |>
  summarize(
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
    loc_down = sum(code == "UD-L-") >= 1) |> 
  mutate(tot_up = qual_up + size_up + loc_up,
         tot_equal = qual_equal + size_equal + loc_equal,
         tot_down = qual_down + size_down + loc_down,
         balance = tot_up - tot_down,
         cost = ordered(
           case_when(cost_up ~ "Higher", cost_equal ~ "Same", 
                     cost_down ~ "Lower"),
           levels = c("Lower", "Same", "Higher"))) |> 
  group_by(cost) |> 
  summarize(n_pos = sum(balance > 0),
            n_neg = sum(balance < 0),
            n_neut = sum(balance == 0),
            balance = mean(balance))

# Tenure faceting figure
figure_3_4 <- 
  transcripts |> 
  summarize(
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
    tenure = case_when(sum(code == "UD-TP") >= 1 ~ "Private rental",
                       sum(code %in% c("UD-TNM", "UD-TS")) >= 1 ~ 
                         "Non-market rental",
                       sum(code == "UD-TO") >= 1 ~ "Ownership"),
    tot_up = cost_down + qual_up + size_up + loc_up,
    tot_equal = qual_equal + size_equal + loc_equal,
    tot_down = cost_up + qual_down + size_down + loc_down,
    balance = tot_up - tot_down, .by = id) |> 
  filter(!is.na(tenure)) |> 
  ggplot(aes(tenure, balance)) +
  geom_violin() +
  geom_jitter(width = 0.1, height = 0.1) +
  scale_x_discrete(name = "Post-eviction tenure type") +
  scale_y_continuous(name = "Net housing outcome",
                     breaks = 2 * -2:2) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

# Tenure faceting table
transcripts |> 
  arrange(id) |> 
  group_by(id) |>
  summarize(
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
    tenure = case_when(sum(code == "UD-TP") >= 1 ~ "Private rental",
                       sum(code %in% c("UD-TNM", "UD-TS")) >= 1 ~ 
                         "Non-market rental",
                       sum(code == "UD-TO") >= 1 ~ "Ownership")) |> 
  mutate(tot_up = cost_down + qual_up + size_up + loc_up,
         tot_equal = qual_equal + size_equal + loc_equal,
         tot_down = cost_up + qual_down + size_down + loc_down,
         balance = tot_up - tot_down) |> 
  group_by(tenure) |> 
  summarize(balance = mean(balance))

# Gender faceting figure
figure_3_5 <- 
  transcripts |> 
  summarize(
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
    gender = first(gender), 
    tot_up = cost_down + qual_up + size_up + loc_up,
    tot_equal = qual_equal + size_equal + loc_equal,
    tot_down = cost_up + qual_down + size_down + loc_down,
    balance = tot_up - tot_down, .by = id) |> 
  filter(!is.na(gender), gender != "Prefer not to say") |> 
  ggplot(aes(gender, balance)) +
  geom_violin() +
  geom_jitter(width = 0.1, height = 0.1) +
  scale_x_discrete(name = "Gender") +
  scale_y_continuous(name = "Net housing outcome",
                     breaks = 2 * -2:2) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

# Gender faceting table
transcripts |> 
  arrange(id) |> 
  group_by(id) |>
  summarize(
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
    gender = first(gender)) |> 
  mutate(tot_up = cost_down + qual_up + size_up + loc_up,
         tot_equal = qual_equal + size_equal + loc_equal,
         tot_down = cost_up + qual_down + size_down + loc_down,
         balance = tot_up - tot_down) |> 
  filter(!is.na(gender), gender != "Prefer not to say") |> 
  group_by(gender) |> 
  summarize(balance = mean(balance))

# Race faceting figure
figure_3_6 <- 
  transcripts |> 
  summarize(
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
    white = if_else(first(race) == "white", "White", "Non-white"), 
    tot_up = cost_down + qual_up + size_up + loc_up,
    tot_equal = qual_equal + size_equal + loc_equal,
    tot_down = cost_up + qual_down + size_down + loc_down,
    balance = tot_up - tot_down, .by = id) |> 
  filter(!is.na(white)) |> 
  mutate(white = if_else(white == "White", "White", 
                         "Racialized or Indigenous")) |> 
  ggplot(aes(white, balance)) +
  geom_violin() +
  geom_jitter(width = 0.1, height = 0.1) +
  scale_x_discrete(name = "Race") +
  scale_y_continuous(name = "Net housing outcome",
                     breaks = 2 * -2:2) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

# Race faceting table
transcripts |> 
  summarize(
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
    white = first(race) == "white", 
    tot_up = cost_down + qual_up + size_up + loc_up,
    tot_equal = qual_equal + size_equal + loc_equal,
    tot_down = cost_up + qual_down + size_down + loc_down,
    balance = tot_up - tot_down, .by = id) |> 
  filter(!is.na(white)) |> 
  summarize(balance = mean(balance), .by = white)

# Instability faceting figure
figure_3_7 <- 
  transcripts |> 
  summarize(
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
    province = first(province), 
    eviction = sum(code == "BHE-FM") >= 1,
    instability = sum(code == "BHE-HI") >= 1,
    discrimination = sum(code == "BHE-D") >= 1,
    any = eviction + instability + discrimination >= 1,
    tot_up = cost_down + qual_up + size_up + loc_up,
    tot_equal = qual_equal + size_equal + loc_equal,
    tot_down = cost_up + qual_down + size_down + loc_down,
    balance = tot_up - tot_down, .by = id) |> 
  mutate(any = if_else(any, "True", "False")) |> 
  ggplot(aes(any, balance)) +
  geom_violin() +
  geom_jitter(width = 0.1, height = 0.1) +
  scale_x_discrete(name = "Previous housing instability") +
  scale_y_continuous(name = "Net housing outcome",
                     breaks = 2 * -2:2) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

# Instability faceting table
transcripts |> 
  summarize(
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
    white = first(race) == "white", 
    eviction = sum(code == "BHE-FM") >= 1,
    instability = sum(code == "BHE-HI") >= 1,
    discrimination = sum(code == "BHE-D") >= 1,
    any = eviction + instability + discrimination >= 1,
    tot_up = cost_down + qual_up + size_up + loc_up,
    tot_equal = qual_equal + size_equal + loc_equal,
    tot_down = cost_up + qual_down + size_down + loc_down,
    balance = tot_up - tot_down, .by = id) |> 
  summarize(balance = mean(balance), .by = any)

# Combined figure
library(patchwork)

layout <- "
AA
BC
DE
FG
"

figure_3 <- 
  figure_3_1 + figure_3_2 + figure_3_3 + figure_3_4 + figure_3_5 + figure_3_6 + 
  figure_3_7 +
  plot_annotation(tag_levels = "A") + plot_layout(design = layout)

ggsave("output/figure_3.png", figure_3, width = 9, height = 11, units = "in")
