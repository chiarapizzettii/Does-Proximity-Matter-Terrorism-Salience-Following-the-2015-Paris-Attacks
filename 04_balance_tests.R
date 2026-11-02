# Part 4. Balance tests
# This section assesses whether treatment and control groups are balanced
# in terms of observable individual characteristics around the event date.
# The analysis compares respondents interviewed before and after the Paris
# attacks within the ±1-day window, excluding the day of the attack.

# Balance is assessed for age, gender, rural/urban residence, and current
# occupation. Continuous and binary variables are compared using t-tests,
# while categorical occupation is assessed using chi-squared tests.
# The tests are performed separately for France, neighboring countries,
# the rest of Europe, and countries with previous terrorism exposure.


### FRANCE

fr_balance <- eb %>%
  filter(
    isocntry == "FR",
    p1_date >= as.Date("2015-11-12"),
    p1_date <= as.Date("2015-11-14"),
    p1_date != event_date
  ) %>%
  mutate(
    treatment = if_else(p1_date > event_date, 1, 0)
  )

fr_balance %>%
  count(treatment)


fr_balance %>% select(treatment, age, female, rural, current_occupation) %>% summary()

names(fr_balance)
fr_balance %>% select(treatment, age, female, rural, current_occupation) %>% summary()


# balance test su variabile continua age
t_age <- t.test(age ~ treatment, data = fr_balance)
t_age

fr_balance %>%
  group_by(treatment) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age   = sd(age, na.rm = TRUE),
    n        = n()
  )

# balance test su variabili binarie/dummy   
t_female <- t.test(female ~ treatment, data = fr_balance)
t_rural  <- t.test(rural  ~ treatment, data = fr_balance)

t_female
t_rural



# variabili categoriche, usare test chi qaudro
tab_occ <- table(fr_balance$treatment, fr_balance$current_occupation)
tab_occ
chisq.test(tab_occ)


fr_balance %>%
  group_by(treatment, current_occupation) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(treatment) %>%
  mutate(pct = round(100 * n / sum(n), 1))



### NEIGHBORING COUNTRIES

neighbors_balance <- eb %>%
  filter(
    isocntry %in% c("BE", "DE-W", "DE-E", "LU", "ES", "IT"),
    p1_date >= as.Date("2015-11-12"),
    p1_date <= as.Date("2015-11-14"),
    p1_date != event_date
  ) %>%
  mutate(
    treatment = if_else(p1_date > event_date, 1, 0)
  )

neighbors_balance %>%
  count(treatment)

# Check sample
summary(neighbors_balance)

neighbors_balance %>%
  count(isocntry, treatment)



t_age_neighbors <- t.test(
  age ~ treatment,
  data = neighbors_balance
)

t_age_neighbors

neighbors_balance %>%
  group_by(treatment) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age   = sd(age, na.rm = TRUE),
    n        = sum(!is.na(age))
  )

# female

t_female_neighbors <- t.test(
  female ~ treatment,
  data = neighbors_balance
)

t_female_neighbors


neighbors_balance %>%
  group_by(treatment) %>%
  summarise(
    proportion_female = mean(female, na.rm = TRUE),
    n = sum(!is.na(female))
  )

# rural

t_rural_neighbors <- t.test(
  rural ~ treatment,
  data = neighbors_balance
)

t_rural_neighbors

neighbors_balance %>%
  group_by(treatment) %>%
  summarise(
    proportion_rural = mean(rural, na.rm = TRUE),
    n = sum(!is.na(rural))
  )

# current occupation

tab_occ_neighbors <- table(
  neighbors_balance$treatment,
  neighbors_balance$current_occupation
)


tab_occ_neighbors

chisq.test(tab_occ_neighbors)

neighbors_balance %>%
  group_by(treatment, current_occupation) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(treatment) %>%
  mutate(
    pct = round(100 * n / sum(n), 1)
  )


### ROE

roe_balance <- eb %>%
  filter(
    !isocntry %in% c("FR", "BE", "DE-W", "DE-E", "LU", "ES", "IT"),
    p1_date >= as.Date("2015-11-12"),
    p1_date <= as.Date("2015-11-14"),
    p1_date != event_date
  ) %>%
  mutate(
    treatment = if_else(p1_date > event_date, 1, 0)
  )

roe_balance %>%
  count(treatment)

# age

t_age_roe <- t.test(
  age ~ treatment,
  data = roe_balance
)

t_age_roe

roe_balance %>%
  group_by(treatment) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age   = sd(age, na.rm = TRUE),
    n        = sum(!is.na(age))
  )

# female

t_female_roe <- t.test(
  female ~ treatment,
  data = roe_balance
)

t_female_roe

roe_balance %>%
  group_by(treatment) %>%
  summarise(
    proportion_female = mean(female, na.rm = TRUE),
    n = sum(!is.na(female))
  )

# rural

t_rural_roe <- t.test(
  rural ~ treatment,
  data = roe_balance
)

t_rural_roe


roe_balance %>%
  group_by(treatment) %>%
  summarise(
    proportion_rural = mean(rural, na.rm = TRUE),
    n = sum(!is.na(rural))
  )

# current occupation

tab_occ_roe <- table(
  roe_balance$treatment,
  roe_balance$current_occupation
)

tab_occ_roe

chisq.test(tab_occ_roe)

roe_balance %>%
  group_by(treatment, current_occupation) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(treatment) %>%
  mutate(
    pct = round(100 * n / sum(n), 1)
  )

# MISSING VALUES

roe_balance %>%
  summarise(
    age_missing = sum(is.na(age)),
    female_missing = sum(is.na(female)),
    rural_missing = sum(is.na(rural)),
    occupation_missing = sum(is.na(current_occupation))
  )


### PREVIOUS EXPOSURE CPOUNTRIES

neighbors_balance <- eb %>%
  filter(
    isocntry %in% c( "ES", "GB", "DE-W", "DE-E", "BE", "BG", "DK", "NL"),
    p1_date >= as.Date("2015-11-12"),
    p1_date <= as.Date("2015-11-14"),
    p1_date != event_date
  ) %>%
  mutate(
    treatment = if_else(p1_date > event_date, 1, 0)
  )

neighbors_balance %>%
  count(treatment)

# Check sample
summary(neighbors_balance)

neighbors_balance %>%
  count(isocntry, treatment)



t_age_neighbors <- t.test(
  age ~ treatment,
  data = neighbors_balance
)

t_age_neighbors

neighbors_balance %>%
  group_by(treatment) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age   = sd(age, na.rm = TRUE),
    n        = sum(!is.na(age))
  )

# female

t_female_neighbors <- t.test(
  female ~ treatment,
  data = neighbors_balance
)

t_female_neighbors


neighbors_balance %>%
  group_by(treatment) %>%
  summarise(
    proportion_female = mean(female, na.rm = TRUE),
    n = sum(!is.na(female))
  )

# rural

t_rural_neighbors <- t.test(
  rural ~ treatment,
  data = neighbors_balance
)

t_rural_neighbors

neighbors_balance %>%
  group_by(treatment) %>%
  summarise(
    proportion_rural = mean(rural, na.rm = TRUE),
    n = sum(!is.na(rural))
  )

# current occupation

tab_occ_neighbors <- table(
  neighbors_balance$treatment,
  neighbors_balance$current_occupation
)


tab_occ_neighbors

chisq.test(tab_occ_neighbors)

neighbors_balance %>%
  group_by(treatment, current_occupation) %>%
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(treatment) %>%
  mutate(
    pct = round(100 * n / sum(n), 1)
  )


