#### 3. EVICTION TYPE ##########################################################

source("R/02_code_checking.R")

t_evict <- function(field_1, field_2) {
  transcripts |> 
    filter(category == "ET") |> 
    arrange(transcript) |> 
    filter(code %in% c("ET-OW", "ET-R", "ET-S", "ET-RT", "ET-OL")) |> 
    reframe(
      {{ field_1 }},
      own_use = sum(code == "ET-OW"),
      reno = sum(code == "ET-R"),
      sale = sum(code == "ET-S"),
      retal = sum(code == "ET-RT"),
      other = sum(code == "ET-OL"), .by = transcript) |> 
    group_by({{ field_2 }}) |> 
    reframe(across(c(own_use:other), \(x) scales::percent(mean(x), 0.1)))
}


# Eviction types ----------------------------------------------------------

# Landlord or tenant factor, single or multiple
transcripts |> 
  filter(category == "ET") |> 
  arrange(transcript) |> 
  group_by(transcript) |> 
  summarize(
    tenant = sum(code %in% c("ET-NP", "ET-OT")) >= 1,
    landlord = sum(code %in% c(
      "ET-OW", "ET-R", "ET-S", "ET-RT", "ET-OL")) >= 1,
    multiple = sum(code == "ET-ME") >= 1) |>
  group_by(multiple) |> 
  summarize(
    tenant_n = sum(tenant & !landlord),
    landlord_n = sum(!tenant & landlord),
    both_n = sum(tenant & landlord)) |> 
  mutate(
    tenant_pct = tenant_n / sum(tenant_n, landlord_n, both_n),
    landlord_pct = landlord_n / sum(tenant_n, landlord_n, both_n),
    both_pct = both_n / sum(tenant_n, landlord_n, both_n)) |> 
  reframe(
    multiple = c(multiple, NA),
    tenant_n = c(tenant_n, sum(tenant_n)),
    landlord_n = c(landlord_n, sum(landlord_n)),
    both_n = c(both_n, sum(both_n)),
    tenant_pct = c(tenant_pct, sum(tenant_pct)),
    landlord_pct = c(landlord_pct, sum(landlord_pct)),
    both_pct = c(both_pct, sum(both_pct))) |> 
  transmute(
    multiple = c("Single-household eviction", "Multiple-household eviction", 
                 "Totals"),
    tenant = paste0(tenant_n, " (", scales::percent(tenant_pct, 0.1), ")"),
    landlord = paste0(landlord_n, " (", scales::percent(landlord_pct, 0.1), ")"),
    both = paste0(both_n, " (", scales::percent(both_pct, 0.1), ")"),
    total = paste0(tenant_n + landlord_n + both_n, " (", scales::percent(
      tenant_pct + landlord_pct + both_pct, 0.1), ")")) |> 
  set_names(c(" ", "Tenant factor", "Landlord factor", "Both factors", 
              "Totals")) |> 
  gt::gt()

# Landlord factors
transcripts |> 
  filter(category == "ET") |> 
  arrange(transcript) |> 
  group_by(transcript) |> 
  filter(code %in% c("ET-OW", "ET-R", "ET-S", "ET-RT", "ET-OL")) |> 
  summarize(
    own_use = sum(code == "ET-OW"),
    reno = sum(code == "ET-R"),
    sale = sum(code == "ET-S"),
    retal = sum(code == "ET-RT"),
    other = sum(code == "ET-OL")) |> 
  summarize(across(c(own_use:other), \(x) scales::percent(mean(x), 0.1)))

# Geographical differences
t_evict(province, province)

# Gender differences
t_evict(gender, gender)
  
# Race differences
t_evict(race, race)
t_evict(race, race == "white")

# Single- or multiple-unit
transcripts |> 
  filter(category == "ET") |> 
  arrange(transcript) |> 
  group_by(transcript) |> 
  summarize(
    own_use = sum(code == "ET-OW") >= 1,
    multiple = sum(code == "ET-ME") >= 1) |>
  count(multiple) |> 
  mutate(pct = n / sum(n)) |> 
  mutate(pct = scales::percent(pct, 0.1))

# Own-use and multiple-unit
transcripts |> 
  filter(category == "ET") |> 
  arrange(transcript) |> 
  group_by(transcript) |> 
  summarize(
    own_use = sum(code == "ET-OW") >= 1,
    multiple = sum(code == "ET-ME") >= 1) |>
  count(own_use, multiple) |> 
  filter(multiple) |> 
  mutate(pct = n / sum(n)) |> 
  mutate(pct = scales::percent(pct, 0.1))
