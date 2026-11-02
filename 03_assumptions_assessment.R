# Part 3. Simultaneous event assessment & unrelated time trends
# This section performs two checks to assess the validity of the UESD design.
# First, it examines whether the observed change in terrorism salience could
# be explained by a simultaneous change in another issue, focusing on
# immigration salience. This check is performed for France and, in the
# appendix, for the other samples.
# Second, it examines whether terrorism salience was already changing over
# time before the Paris attacks. A placebo test splits the pre-event
# observations at the median interview date and tests for differences in
# terrorism salience between the two periods.


#### Simultaneous event assessment ###

fr <- eb %>%
  filter(isocntry == "FR", !is.na(p1_date))

fr %>%
  count(p1_date)

library(dplyr)

event_date <- as.Date("2015-11-13")

fr <- eb %>%
  filter(
    isocntry == "FR",
    p1_date >= as.Date("2015-11-10"),
    p1_date <= as.Date("2015-11-16"),
    p1_date != event_date,
    !is.na(qa3a_9)
  ) %>%
  mutate(
    treatment = if_else(p1_date > event_date, 1, 0)
  )

fr %>%
  mutate(
    Gruppo = ifelse(treatment == 0,
                    "Prima attentato",
                    "Dopo attentato")
  ) %>%
  count(Gruppo)


fr %>%
  mutate(
    Gruppo = ifelse(treatment == 0,
                    "Prima attentato",
                    "Dopo attentato")
  ) %>%
  group_by(Gruppo) %>%
  summarise(
    Intervistati = n(),
    Percentuale_terrorismo = mean(qa3a_9)*100
  )

# plot
fr %>%
  group_by(p1_date) %>%
  summarise(
    Percentuale = mean(qa3a_9)*100,
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


library(tidyr)

# comparison between terrorism and immigration salience
fr %>%
  group_by(p1_date) %>%
  summarise(
    Terrorism = mean(qa3a_6, na.rm = TRUE) * 100,
    Immigration = mean(qa3a_9, na.rm = TRUE) * 100,
    n = n()
  ) %>%
  pivot_longer(
    cols = c(Terrorism, Immigration),
    names_to = "Tema",
    values_to = "Percentuale"
  ) %>%
  ggplot(aes(x = p1_date, y = Percentuale, color = Tema)) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = as.Date("2015-11-13"),
             linetype = "dashed",
             colour = "red") +
  scale_color_manual(values = c("Terrorism" = "#2a78d6", "Immigration" = "#eb6834")) +
  labs(
    x = "Date of Interview",
    y = "Percentage of respondents mentioning the issue (%)",
    color = ""
  ) +
  theme_minimal()

# test 
summary(glm(qa3a_9 ~ treatment, data = fr, family = binomial))
summary(glm(qa3a_6 ~ treatment, data = fr, family = binomial))



# In appendix, do the same for the other samples 
library(dplyr)
library(tidyr)
library(ggplot2)

event_date <- as.Date("2015-11-13")

# Define samples
france_codes      <- c("FR")
neighboring_codes <- c("BE", "DE-W", "DE-E", "LU", "ES", "IT")  
roe_codes <- eb %>%
  filter(!isocntry %in% c("FR", "BE", "DE-W", "DE-E", "LU", "ES", "IT"))%>%
  pull(isocntry) %>%
  unique()
previous_exposure_codes <- c("ES", "GB", "DE-W", "DE-E", "BE", "BG", "DK", "NL")


run_simultaneous_event <- function(data, country_codes, sample_label) {
  
  df <- data %>%
    filter(
      isocntry %in% country_codes,
      p1_date >= as.Date("2015-11-10"),
      p1_date <= as.Date("2015-11-16"),
      p1_date != event_date,
      !is.na(qa3a_9),
      !is.na(qa3a_6)
    ) %>%
    mutate(treatment = if_else(p1_date > event_date, 1, 0))
  
  # N per gruppo
  n_summary <- df %>%
    mutate(Gruppo = ifelse(treatment == 0, "Prima attentato", "Dopo attentato")) %>%
    count(Gruppo)
  
  # Percentuali terrorismo/immigrazione prima e dopo
  pct_summary <- df %>%
    mutate(Gruppo = ifelse(treatment == 0, "Prima attentato", "Dopo attentato")) %>%
    group_by(Gruppo) %>%
    summarise(
      Intervistati = n(),
      Percentuale_terrorismo   = mean(qa3a_6) * 100,
      Percentuale_immigrazione = mean(qa3a_9) * 100
    )
  
  # Plot giorno-per-giorno
  plot_data <- df %>%
    group_by(p1_date) %>%
    summarise(
      Terrorism   = mean(qa3a_6, na.rm = TRUE) * 100,
      Immigration = mean(qa3a_9, na.rm = TRUE) * 100,
      n = n()
    ) %>%
    pivot_longer(cols = c(Terrorism, Immigration), names_to = "Tema", values_to = "Percentuale")
  
  p <- ggplot(plot_data, aes(x = p1_date, y = Percentuale, color = Tema)) +
    geom_point(size = 3) +
    geom_line(linewidth = 0.8) +
    geom_vline(xintercept = event_date, linetype = "dashed", colour = "red") +
    scale_color_manual(values = c("Terrorism" = "#2a78d6", "Immigration" = "#eb6834")) +
    labs(
      title = sample_label,
      x = "Date of Interview",
      y = "Percentage of respondents mentioning the issue (%)",
      color = ""
    ) +
    theme_minimal()
  
  # Modelli logistici
  model_terrorism   <- glm(qa3a_6 ~ treatment, data = df, family = binomial)
  model_immigration <- glm(qa3a_9 ~ treatment, data = df, family = binomial)
  
  list(
    n_summary = n_summary,
    pct_summary = pct_summary,
    plot = p,
    model_terrorism = model_terrorism,
    model_immigration = model_immigration
  )
}


res_france      <- run_simultaneous_event(eb, france_codes, "France")
res_neighboring <- run_simultaneous_event(eb, neighboring_codes, "Neighboring Countries")
res_roe         <- run_simultaneous_event(eb, roe_codes, "Rest of Europe")
res_previous_exposure <- run_simultaneous_event(
  eb,
  previous_exposure_codes,
  "Previous Terrorism Exposure"
)


# risultati
res_france$pct_summary
res_france$plot
summary(res_france$model_terrorism)
summary(res_france$model_immigration)

res_neighboring$pct_summary
res_neighboring$plot
summary(res_neighboring$model_terrorism)
summary(res_neighboring$model_immigration)

res_roe$pct_summary
res_roe$plot
summary(res_roe$model_terrorism)
summary(res_roe$model_immigration)

res_previous_exposure$pct_summary
res_previous_exposure$plot
summary(res_previous_exposure$model_terrorism)
summary(res_previous_exposure$model_immigration)


library(broom)

extract_coef <- function(model, outcome_label, sample_label) {
  tidy(model) %>%
    filter(term == "treatment") %>%
    transmute(
      Sample = sample_label,
      Outcome = outcome_label,
      Treatment = estimate,
      SE = std.error,
      P_value = p.value
    )
}

summary_table <- bind_rows(
  extract_coef(res_france$model_terrorism, "Terrorism", "France"),
  extract_coef(res_france$model_immigration, "Immigration", "France"),
  extract_coef(res_neighboring$model_terrorism, "Terrorism", "Neighboring Countries"),
  extract_coef(res_neighboring$model_immigration, "Immigration", "Neighboring Countries"),
  extract_coef(res_roe$model_terrorism, "Terrorism", "Rest of Europe"),
  extract_coef(res_roe$model_immigration, "Immigration", "Rest of Europe"),
  extract_coef(res_previous_exposure$model_terrorism, "Terrorism", "Previous Terrorism Exposure"),
  extract_coef(res_previous_exposure$model_immigration, "Immigration", "Previous Terrorism Exposure")
)

summary_table


###############################################################################

#### Unrelated time trends ###

eb %>%
  filter(isocntry == "FR", p1_date < as.Date("2015-11-13")) %>%
  count(p1_date) %>%
  print(n = 50)

library(dplyr)

# 1. Isola SOLO gli intervistati pre-evento (già presenti in fr con treatment == 0)
pre_event <- eb %>%
  filter(treatment == 0)

pre_event <- eb %>%
  filter(
    isocntry == "FR",
    p1_date < as.Date("2015-11-13")
  )

pre_event %>% count(p1_date)

# 2. Trova la mediana della data di fieldwork nel sottocampione pre-evento
median_date <- median(pre_event$p1_date)
median_date

# 3. Crea la variabile placebo:
#    1 = intervistato dopo la mediana (ma sempre prima del 13/11)
#    0 = intervistato prima della mediana
pre_event <- pre_event %>%
  mutate(placebo = if_else(p1_date > median_date, 1, 0))

# controllo numerosità dei due sottogruppi placebo
pre_event %>% count(placebo)

# 4. Regredisci la salienza del terrorismo (qa3a_6) sulla variabile placebo
m_placebo_terrorism <- glm(qa3a_6 ~ placebo, data = pre_event, family = binomial)
summary(m_placebo_terrorism)


# NEIGHBORING COUNTRIES
eb %>%
  filter(isocntry %in% c("BE", "DE-W", "DE-E", "LU", "ES", "IT"), 
         p1_date < as.Date("2015-11-13")) %>%
  count(p1_date) %>%
  print(n = 50)

library(dplyr)

# 1. Isola SOLO gli intervistati pre-evento (già presenti in fr con treatment == 0)
#pre_event <- eb %>%
  #filter(treatment == 0)

pre_event <- eb %>%
  filter(
    isocntry %in% c("BE", "DE-W", "DE-E", "LU", "ES", "IT"),
    p1_date < as.Date("2015-11-13")
  )

# controllo rapido
pre_event %>% count(p1_date)

# 2. Trova la mediana della data di fieldwork nel sottocampione pre-evento
median_date <- median(pre_event$p1_date)
median_date

# 3. Crea la variabile placebo:
#    1 = intervistato dopo la mediana (ma sempre prima del 13/11)
#    0 = intervistato prima della mediana
pre_event <- pre_event %>%
  mutate(placebo = if_else(p1_date > median_date, 1, 0))

# controllo numerosità dei due sottogruppi placebo
pre_event %>% count(placebo)

# 4. Regredisci la salienza del terrorismo (qa3a_6) sulla variabile placebo
m_placebo_terrorism <- glm(qa3a_6 ~ placebo, data = pre_event, family = binomial)
summary(m_placebo_terrorism)


# REST OF EUROPE
eb %>%
  filter(!isocntry %in% c("FR", "BE", "DE-W", "DE-E", "LU", "ES", "IT"), 
         p1_date < as.Date("2015-11-13")) %>%
  count(p1_date) %>%
  print(n = 50)

library(dplyr)

# 1. Isola SOLO gli intervistati pre-evento (già presenti in fr con treatment == 0)
#pre_event <- eb %>%
#filter(treatment == 0)

pre_event <- eb %>%
  filter(
    !isocntry %in% c("FR", "BE", "DE-W", "DE-E", "LU", "ES", "IT"),
    p1_date < as.Date("2015-11-13")
  )

# controllo rapido
pre_event %>% count(p1_date)

# 2. Trova la mediana della data di fieldwork nel sottocampione pre-evento
median_date <- median(pre_event$p1_date)
median_date

# 3. Crea la variabile placebo:
#    1 = intervistato dopo la mediana (ma sempre prima del 13/11)
#    0 = intervistato prima della mediana
pre_event <- pre_event %>%
  mutate(placebo = if_else(p1_date > median_date, 1, 0))

# controllo numerosità dei due sottogruppi placebo
pre_event %>% count(placebo)

# 4. Regredisci la salienza del terrorismo (qa3a_6) sulla variabile placebo
m_placebo_terrorism <- glm(qa3a_6 ~ placebo, data = pre_event, family = binomial)
summary(m_placebo_terrorism)


# PREVIOUS EXPOSURE COUNTRIES 
eb %>%
  filter(isocntry %in% c( "ES", "GB", "DE-W", "DE-E", "BE", "BG", "DK", "NL"), 
         p1_date < as.Date("2015-11-13")) %>%
  count(p1_date) %>%
  print(n = 50)

library(dplyr)

# 1. Isola SOLO gli intervistati pre-evento (già presenti in fr con treatment == 0)
#pre_event <- eb %>%
#filter(treatment == 0)

pre_event <- eb %>%
  filter(
    isocntry %in% c( "ES", "GB", "DE-W", "DE-E", "BE", "BG", "DK", "NL"),
    p1_date < as.Date("2015-11-13")
  )

# controllo rapido
pre_event %>% count(p1_date)

# 2. Trova la mediana della data di fieldwork nel sottocampione pre-evento
median_date <- median(pre_event$p1_date)
median_date

# 3. Crea la variabile placebo:
#    1 = intervistato dopo la mediana (ma sempre prima del 13/11)
#    0 = intervistato prima della mediana
pre_event <- pre_event %>%
  mutate(placebo = if_else(p1_date > median_date, 1, 0))

# controllo numerosità dei due sottogruppi placebo
pre_event %>% count(placebo)

# 4. Regredisci la salienza del terrorismo (qa3a_6) sulla variabile placebo
m_placebo_terrorism <- glm(qa3a_6 ~ placebo, data = pre_event, family = binomial)
summary(m_placebo_terrorism)
