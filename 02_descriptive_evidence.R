# Part 2. Descriptive Evidence
# This section provides preliminary descriptive evidence for the four samples
# considered in the analysis, focusing on terrorism salience before and after
# the 13 November 2015 Paris attacks. The analysis describes the number of
# interviews and the percentage of respondents identifying terrorism as one
# of the most important issues, both overall and by interview date.


### 1. DATA PREPARATION AND INTERVIEW DATE ###

# Inspect the dataset and the terrorism salience variable
View(eb)
eb$qa3a_6

# Convert the interview date into a standard Date format
Sys.setlocale("LC_TIME", "C")

eb$p1_date <- eb$p1 |>
  as_factor() |>
  as.character() |>
  str_remove("^[A-Za-z]+\\s+") |>        # remove the weekday
  str_replace_all("(st|nd|rd|th)", "") |> # remove ordinal suffixes
  dmy(locale = "C")


# Define the date of the Paris attacks
event_date <- as.Date("2015-11-13")


### 2. DEFINE THE FOUR SAMPLES ####

# France
fr <- eb %>%
  filter(
    isocntry == "FR",
    !is.na(p1_date)
  )

# Neighboring countries
neighbors <- eb %>%
  filter(
    isocntry %in% c("BE", "DE-W", "DE-E", "LU", "ES", "IT"),
    !is.na(p1_date)
  )

# Rest of Europe: all countries excluding France and neighboring countries
roe <- eb %>%
  filter(
    !isocntry %in% c("FR", "BE", "DE-W", "DE-E", "LU", "ES", "IT"),
    !is.na(p1_date)
  )

# Countries with previous terrorism exposure:
neighbors_2 <- eb %>%
  filter(
    isocntry %in% c("ES", "GB", "DE-W", "DE-E", "BE", "BG", "DK", "NL"),
    !is.na(p1_date)
  )


### 3. INTERVIEW COUNTS BY DATE ####

# Number of interviews by date in each sample

fr %>%
  count(p1_date)

neighbors %>%
  count(p1_date)

roe %>%
  count(p1_date)

neighbors_2 %>%
  count(p1_date)


### 4. DEFINE THE MAIN ANALYTICAL WINDOW ####

# Main analytical window: three days before and three days after
# the attacks, excluding the day of the event itself.
# The treatment group consists of respondents interviewed after the attacks,
# while the control group consists of respondents interviewed before the attacks.

fr <- fr %>%
  filter(
    p1_date >= as.Date("2015-11-10"),
    p1_date <= as.Date("2015-11-16"),
    p1_date != event_date,
    !is.na(qa3a_6)
  ) %>%
  mutate(
    treatment = if_else(p1_date > event_date, 1, 0)
  )

neighbors <- neighbors %>%
  filter(
    p1_date >= as.Date("2015-11-10"),
    p1_date <= as.Date("2015-11-16"),
    p1_date != event_date,
    !is.na(qa3a_6)
  ) %>%
  mutate(
    treatment = if_else(p1_date > event_date, 1, 0)
  )

roe <- roe %>%
  filter(
    p1_date >= as.Date("2015-11-10"),
    p1_date <= as.Date("2015-11-16"),
    p1_date != event_date,
    !is.na(qa3a_6)
  ) %>%
  mutate(
    treatment = if_else(p1_date > event_date, 1, 0)
  )

neighbors_2 <- neighbors_2 %>%
  filter(
    p1_date >= as.Date("2015-11-10"),
    p1_date <= as.Date("2015-11-16"),
    p1_date != event_date,
    !is.na(qa3a_6)
  ) %>%
  mutate(
    treatment = if_else(p1_date > event_date, 1, 0)
  )


### 5. DESCRIPTIVE EVIDENCE: FRANCE ####

# Number of respondents interviewed before and after the attacks
fr %>%
  mutate(
    group = ifelse(treatment == 0, "Before attack", "After attack")
  ) %>%
  count(group)

# Terrorism salience before and after the attacks
# qa3a_6 is a binary indicator equal to 1 if terrorism is mentioned
# among the most important issues.
fr %>%
  mutate(
    group = ifelse(treatment == 0, "Before attack", "After attack")
  ) %>%
  group_by(group) %>%
  summarise(
    respondents = n(),
    terrorism_salience = mean(qa3a_6) * 100
  )

# Terrorism salience by interview date
fr %>%
  group_by(p1_date) %>%
  summarise(
    N = n(),
    terrorism_salience = mean(qa3a_6) * 100
  )


### 6. DESCRIPTIVE EVIDENCE: NEIGHBORING COUNTRIES ####

# Number of respondents interviewed before and after the attacks
neighbors %>%
  mutate(
    group = ifelse(treatment == 0, "Before attack", "After attack")
  ) %>%
  count(group)

# Terrorism salience before and after the attacks
neighbors %>%
  mutate(
    group = ifelse(treatment == 0, "Before attack", "After attack")
  ) %>%
  group_by(group) %>%
  summarise(
    respondents = n(),
    terrorism_salience = mean(qa3a_6) * 100
  )

# Terrorism salience by interview date
neighbors %>%
  group_by(p1_date) %>%
  summarise(
    N = n(),
    terrorism_salience = mean(qa3a_6) * 100
  )


### 7. DESCRIPTIVE EVIDENCE: REST OF EUROPE ####

# Number of respondents interviewed before and after the attacks
roe %>%
  mutate(
    group = ifelse(treatment == 0, "Before attack", "After attack")
  ) %>%
  count(group)

# Terrorism salience before and after the attacks
roe %>%
  mutate(
    group = ifelse(treatment == 0, "Before attack", "After attack")
  ) %>%
  group_by(group) %>%
  summarise(
    respondents = n(),
    terrorism_salience = mean(qa3a_6) * 100
  )

# Terrorism salience by interview date
roe %>%
  group_by(p1_date) %>%
  summarise(
    N = n(),
    terrorism_salience = mean(qa3a_6) * 100
  )


### 8. DESCRIPTIVE EVIDENCE: PREVIOUS TERRORISM EXPOSURE ####

# Number of respondents interviewed before and after the attacks
neighbors_2 %>%
  mutate(
    group = ifelse(treatment == 0, "Before attack", "After attack")
  ) %>%
  count(group)

# Terrorism salience before and after the attacks
neighbors_2 %>%
  mutate(
    group = ifelse(treatment == 0, "Before attack", "After attack")
  ) %>%
  group_by(group) %>%
  summarise(
    respondents = n(),
    terrorism_salience = mean(qa3a_6) * 100
  )

# Terrorism salience by interview date
neighbors_2 %>%
  group_by(p1_date) %>%
  summarise(
    N = n(),
    terrorism_salience = mean(qa3a_6) * 100
  )


### 9. INTERVIEW DISTRIBUTION IN THE FULL FIELDWORK PERIOD ####

# Number of interviews by date during the full Eurobarometer fieldwork period
# (7–17 November 2015) for the previous-exposure sample.

eb %>%
  filter(
    isocntry %in% c(
      "ES", "GB", "DE-W", "DE-E",
      "BE", "BG", "DK", "NL"
    )
  ) %>%
  filter(
    p1_date >= as.Date("2015-11-07"),
    p1_date <= as.Date("2015-11-17")
  ) %>%
  count(p1_date, name = "n_interviews") %>%
  complete(
    p1_date = seq(
      as.Date("2015-11-07"),
      as.Date("2015-11-17"),
      by = "day"
    ),
    fill = list(n_interviews = 0)
  ) %>%
  arrange(p1_date)


### 10. CONTROL AND TREATMENT GROUP SIZE ####

# Number of respondents assigned to the control and treatment groups
# in the previous-exposure sample within the main ±3-day window.

neighbors_2 %>%
  count(treatment) %>%
  mutate(
    group = ifelse(treatment == 0, "Control", "Treatment")
  ) %>%
  select(group, n)

