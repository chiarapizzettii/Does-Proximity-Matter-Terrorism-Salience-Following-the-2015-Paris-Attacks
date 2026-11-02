# Part 1. The code first loads and prepares the EB 84.3 data, including
# the interview date and the outcome variable measuring terrorism salience.
# The individual-level covariates are then defined.
# For France and the neighboring countries, I estimate logit models, with and 
# without controls, separately for the one-day and three-day windows around the event.
# I also compute odds ratios and average marginal effects (AMEs).


library(haven)       
library(tidyverse)   
library(labelled)   
library(janitor)    
library(conflicted)
library(dplyr)
library(stringr)
library(fixest)
library(ggplot2)
library(lubridate)

setwd("C://Users//chiar//Desktop//prova_tesi")
gwd <- getwd()

# 1. load dataset
eb <- read_dta("ZA6643_v4-0-0.dta") 
View(eb)

# 2. variables 
class(eb$uniqid) 
class(eb$isocntry) # country
class(eb$p1_date) # date of the interview 

# 3. Y variables
# what do you think are the most two important issues facing our country?
# dummy = 1 if terrorism, = 0 oterhwise 
eb$qa3a_6 # per terrorism 
eb$qa3a_9 # per immigration (simultaneous event)

# Convert the p1 variable into a date variable
Sys.setlocale("LC_TIME", "C")
eb$p1_date <- eb$p1 |>
  as_factor() |>
  as.character() |>
  str_remove("^[A-Za-z]+\\s+") |>      
  str_replace_all("(st|nd|rd|th)", "") |>  
  dmy(locale = "C")


### DESCRIPTIVE Y VARIABLES

# variable qa3a_6
eb %>%
  filter(!is.na(qa3a_6)) %>%
  count(qa3a_6) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = factor(qa3a_6), y = n)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(n, "\n(", round(pct, 1), "%)")), 
            vjust = -0.3, size = 4) +
  scale_x_discrete(labels = c("0" = "No terrorism", "1" = "Terrorism")) +
  labs(
    title = "What do you think are the two most 
important issues facing our country?",
    x = "Risposta",
    y = "Frequenza",
    caption = paste("N =", sum(!is.na(eb$qa3a_6)))
  ) +
  theme_minimal() +
  ylim(0, max(table(eb$qa3a_6)) * 1.15)  

# variable qa3a_9
eb %>%
  filter(!is.na(qa3a_9)) %>%
  count(qa3a_9) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = factor(qa3a_9), y = n)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(n, "\n(", round(pct, 1), "%)")), 
            vjust = -0.3, size = 4) +
  scale_x_discrete(labels = c("0" = "No Immigration", "1" = "Immigration")) +
  labs(
    title = "What do you think are the two most 
important issues facing our country?",
    x = "Risposta",
    y = "Frequenza",
    caption = paste("N =", sum(!is.na(eb$qa3a_9)))
  ) +
  theme_minimal() +
  ylim(0, max(table(eb$qa3a_9)) * 1.15)  

# Date range by country
date_range_country <- eb %>%
  filter(!is.na(p1_date)) %>%
  group_by(isocntry) %>%
  summarise(
    data_inizio = min(p1_date),
    data_fine   = max(p1_date),
    n_giorni    = as.numeric(data_fine - data_inizio) + 1,
    n_interviste = n(),
    .groups = "drop"
  ) %>%
  arrange(data_inizio)

print(date_range_country, n = Inf)

ggplot(date_range_country, aes(y = reorder(isocntry, data_inizio))) +
  geom_segment(aes(x = data_inizio, xend = data_fine, yend = isocntry),
               linewidth = 2, color = "steelblue") +
  geom_vline(xintercept = ymd("2015-11-13"), linetype = "dashed", color = "red") +
  labs(
    title = "Periodo di rilevazione per paese — Eurobarometro 84.3",
    subtitle = "Linea rossa tratteggiata = attentato di Parigi (13/11/2015)",
    x = "Interview date",
    y = "Country"
  ) +
  theme_minimal()



######################################################
### MAIN RESULTS: FRANCE AND NEIGHBORING COUNTRIES ###
######################################################

### France
fr <- eb %>%
  filter(isocntry == "FR", !is.na(p1_date))

fr %>%
  count(p1_date)

# Nieghboring countries
neighbors <- eb %>%
  filter(
    isocntry %in% c("BE", "DE-W", "DE-E", "LU", "ES", "IT"),
    !is.na(p1_date)
  )

neighbors %>%
  count(p1_date)

# ROE
roe <- eb %>%
  filter(
    !isocntry %in% c("FR", "BE", "DE-W", "DE-E", "LU", "ES", "IT"),
    !is.na(p1_date)
  )

roe %>%
  count(p1_date)



library(dplyr)

# Date of  the Paris attacks
event_date <- as.Date("2015-11-13")

# Select the French sample within the event window.
# The analysis excludes the day of the attack and defines treatment
# as interviews conducted after the attack.
# The same procedure is repeated for the ±1-day window.
fr <- eb %>%
  filter(
    isocntry == "FR",
    p1_date >= as.Date("2015-11-12"),
    p1_date <= as.Date("2015-11-14"),
    p1_date != event_date,
    !is.na(qa3a_6)
  ) %>%
  mutate(
    treatment = if_else(p1_date > event_date, 1, 0)
  )

# Number of respondents interviewed before and after the attack
fr %>%
  mutate(
    Gruppo = ifelse(treatment == 0,
                    "Prima attentato",
                    "Dopo attentato")
  ) %>%
  count(Gruppo)

# Percentage of respondents identifying terrorism as an important issue
# before and after the attack
fr %>%
  mutate(
    Gruppo = ifelse(treatment == 0,
                    "Prima attentato",
                    "Dopo attentato")
  ) %>%
  group_by(Gruppo) %>%
  summarise(
    Intervistati = n(),
    Percentuale_terrorismo = mean(qa3a_6)*100
  )

# plot
fr %>%
  group_by(p1_date) %>%
  summarise(
    Percentuale = mean(qa3a_6)*100,
    n = n()
  ) %>%
  ggplot(aes(p1_date, Percentuale)) +
  geom_point(size = 3) +
  geom_line() +
  geom_vline(xintercept = as.Date("2015-11-13"),
             linetype = "dashed",
             colour = "red") +
  labs(
    x = "Data intervista",
    y = "Percentuale che cita il terrorismo"
  ) +
  theme_minimal()



#################################
### INDIVIDUAL-LEVEL CONTROLS ###
#################################

eb$age <- eb$d11 # age
mean(eb$age)

eb$d10 # gender
eb$female <- ifelse(eb$d10 == 2, 1, 0) # dummy female = 1 if women, 0 otherwise

eb$d25 # rural area
eb$rural <- ifelse(is.na(eb$d25), NA, ifelse(eb$d25 == 1, 1, 0))

# current_occupation
eb$d15a

library(dplyr)

eb <- eb %>%
  mutate(current_occupation = case_when(
    d15a == 2 ~ "student",
    d15a == 4 ~ "retired",
    d15a == 3 ~ "unemployed",
    d15a %in% c(1,5,6,7,8,9,10,11,12,13,14,15,16,17,18) ~ "workers",
    TRUE ~ NA_character_
  ))
class(eb$current_occupation)

eb <- eb %>%
  mutate(
    current_occupation = factor(
      current_occupation,
      levels = c("workers", "student", "unemployed", "retired")
    )
  )
levels(eb$current_occupation)

eb$current_occupation
table(eb$current_occupation, useNA = "ifany")

eb %>%
  filter(qa3a_6 == 1) %>%
  count(current_occupation) %>%
  mutate(percentuale = round(100 * n / sum(n), 1))

# questo se vuoi analisi
eb %>%
  group_by(current_occupation) %>%
  summarise(
    n_tot = n(),
    n_risponde_1 = sum(qa3a_6 == 1, na.rm = TRUE),
    perc = round(100 * n_risponde_1 / n_tot, 1)
  )


# France: models without controls
# probit 
model1_probit <- glm(qa3a_6 ~ treatment, family = binomial(link = "logit"), data = fr)
summary(model1_probit)
nobs(model1_probit)

# odds ration
exp(cbind(
  OR = coef(model1_probit),
  confint(model1_probit)
))

# AME
#install.packages("marginaleffects")
library("marginaleffects")
avg_slopes(model1_probit)


# France: with controls
fr <- eb %>%
  filter(
    isocntry == "FR",
    p1_date >= as.Date("2015-11-12"),
    p1_date <= as.Date("2015-11-14"),
    p1_date != event_date,
    !is.na(qa3a_6),
    !is.na(age),
    !is.na(female),
    !is.na(rural),
    !is.na(current_occupation)
  ) %>%
  mutate(
    treatment = if_else(p1_date > event_date, 1, 0)
  )


# probit 
model2_probit <- glm(qa3a_6 ~ treatment + age + female + rural + current_occupation, family = binomial(link = "logit"), data = fr)
summary(model2_probit)
nobs(model2_probit)

# odds ration
exp(cbind(
  OR = coef(model2_probit),
  confint(model2_probit)
))

# AME
avg_slopes(model2_probit)



### Neighboring countries
neighbors <- eb %>%
  filter(
    isocntry %in% c("BE", "DE-W", "DE-E", "LU", "ES", "IT"),
    !is.na(p1_date)
  )

event_date <- as.Date("2015-11-13")

neighbors <- eb %>%
  filter(
    isocntry %in% c("BE", "DE-W", "DE-E", "LU", "ES", "IT"),
    p1_date >= as.Date("2015-11-12"),
    p1_date <= as.Date("2015-11-14"),
    p1_date != event_date,
    !is.na(qa3a_6)
  ) %>%
  mutate(
    treatment = if_else(p1_date > event_date, 1, 0)
  )

# Number of respondents interviewed before and after the attack
neighbors %>%
  mutate(
    Gruppo = ifelse(treatment == 0,
                    "Prima attentato",
                    "Dopo attentato")
  ) %>%
  count(Gruppo)

# Percentage of respondents identifying terrorism as an important issue
# before and after the attack
neighbors %>%
  mutate(
    Gruppo = ifelse(treatment == 0,
                    "Prima attentato",
                    "Dopo attentato")
  ) %>%
  group_by(Gruppo) %>%
  summarise(
    Intervistati = n(),
    Percentuale_terrorismo = mean(qa3a_6)*100
  )

# plot
neighbors %>%
  group_by(p1_date) %>%
  summarise(
    Percentuale = mean(qa3a_6)*100,
    n = n()
  ) %>%
  ggplot(aes(p1_date, Percentuale)) +
  geom_point(size = 3) +
  geom_line() +
  geom_vline(xintercept = as.Date("2015-11-13"),
             linetype = "dashed",
             colour = "red") +
  labs(
    x = "Data intervista",
    y = "Percentuale che cita il terrorismo"
  ) +
  theme_minimal()


# add controls
neighbors <- eb %>%
  filter(
    isocntry %in% c("BE", "DE-W", "DE-E", "LU", "ES", "IT"),
    p1_date >= as.Date("2015-11-12"),
    p1_date <= as.Date("2015-11-14"),
    p1_date != event_date,
    !is.na(qa3a_6),
    !is.na(age),
    !is.na(female),
    !is.na(rural),
    !is.na(current_occupation)
  ) %>%
  mutate(
    treatment = if_else(p1_date > event_date, 1, 0)
  )


# probit 
model1_probit <- glm(qa3a_6 ~ treatment, family = binomial(link = "logit"), data = neighbors)
summary(model1_probit)
nobs(model1_probit)

# odds ration
exp(cbind(
  OR = coef(model1_probit),
  confint(model1_probit)
))

# AME
avg_slopes(model1_probit)


# with controls
# probit 
model1_probit <- glm(qa3a_6 ~ treatment + age + female + rural + current_occupation, family = binomial(link = "logit"), data = neighbors)
summary(model1_probit)
nobs(model1_probit)

# odds ration
exp(cbind(
  OR = coef(model1_probit),
  confint(model1_probit)
))

# AME
avg_slopes(model1_probit)

