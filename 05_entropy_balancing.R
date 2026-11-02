# Part 5. Entropy Balancing
# This section applies entropy balancing to improve covariate balance between
# treatment and control groups for the Rest of Europe and previous terrorism
# exposure samples. Categorical covariates are converted into dummy variables,
# while observations with missing values on the balancing covariates are
# excluded.

# Entropy balancing is used to obtain weights for the control group so that
# its observed characteristics match those of the treatment group.
# The resulting weights are then incorporated into a survey design and used
# to estimate the final logit models, with and without individual-level
# controls. Odds ratios and average marginal effects (AMEs) are also computed.


#install.packages("ebal")
#install.packages("fastDummies")
library(ebal)
library(dplyr)
library(fastDummies) 

# variabili categoriche vanno trasformate in dummy, perchè ebal lavora su matrici numeriche
# gestire i missing, rimuovere colonna con NA

roe_balance <- eb %>%
  filter(
    isocntry %in% c("ES", "GB", "DE-W","DE-E", "BE", "BG", "DK", "NL"),
    p1_date >= as.Date("2015-11-12"),
    p1_date <= as.Date("2015-11-14"),
    p1_date != event_date
  ) %>%
  mutate(
    treatment = if_else(p1_date > event_date, 1, 0)
  )

# ------------------------------------------------------------
# 1. Pulizia dati: rimuovere missing sulle variabili di interesse
# ------------------------------------------------------------
roe_eb <- roe_balance %>%
  filter(
    !is.na(age),
    !is.na(female),
    !is.na(rural),
    !is.na(current_occupation)
  )

# ------------------------------------------------------------
# 2. Creare le dummy per current_occupation
# ------------------------------------------------------------

roe_eb <- fastDummies::dummy_cols(
  roe_eb,
  select_columns = "current_occupation",
  remove_first_dummy = FALSE,  # ebal non richiede intercetta, quindi le teniamo tutte
  remove_selected_columns = FALSE
)

# ------------------------------------------------------------
# 3. Costruire la matrice delle covariate (X) e il vettore treatment
# ------------------------------------------------------------
covariate_names <- c(
  "age", "female", "rural",
  grep("^current_occupation_", names(roe_eb), value = TRUE)
)

# Fase 1: dummy — ESCLUDI una categoria di riferimento (es. workers)
roe_eb <- fastDummies::dummy_cols(
  roe_eb,
  select_columns = "current_occupation",
  remove_first_dummy = FALSE,
  remove_selected_columns = FALSE
)

# Rimuovi manualmente la dummy della categoria di riferimento (workers)
# così restano solo: students, unemployed, retired
occupation_dummies <- grep("^current_occupation_", names(roe_eb), value = TRUE)
occupation_dummies <- occupation_dummies[!grepl("workers", occupation_dummies, ignore.case = TRUE)]

covariate_names <- c(
  "age", "female", "rural",
  occupation_dummies
)

X <- as.matrix(roe_eb[, covariate_names])
Treatment <- roe_eb$treatment


# ------------------------------------------------------------
# 4. Applicare entropy balancing
# ------------------------------------------------------------
eb_out <- ebalance(Treatment = Treatment, X = X)
# eb_out$w contiene i pesi stimati per le unità di CONTROLLO
# eb_out$maxdiff mostra la massima differenza residua tra i momenti (dovrebbe essere ~0)

summary(eb_out) ##ok

# Vedi tutti i nomi dei componenti dell'oggetto
names(eb_out)

# I pesi stimati per i controlli (dovrebbero essere 11198, uno per ogni controllo)
head(eb_out$w)
length(eb_out$w)
summary(eb_out$w)   # per vedere min, max, media dei pesi

# Controlla se ci sono pesi estremi (molto grandi rispetto agli altri)
# --> il paper avverte che pesi molto sbilanciati aumentano la varianza (Sezione 3.6, p. 34-35)
par(mar = c(4, 4, 2, 1))
hist(eb_out$w)
sd(eb_out$w) / mean(eb_out$w)  # coefficiente di variazione dei pesi

# La deviazione massima dai vincoli (già riassunta nel summary come "max moment deviation")
# nel pacchetto ebal è spesso salvata così:
eb_out$maxdiff



### Estimation models for ROE and previous exposure countries 

### STEP 1 — Assegna i pesi finali al dataset completo 

# ordina roe_eb per treatment, così l'ordine corrisponde a come ebalance ha processato i controlli
roe_eb <- roe_eb %>% arrange(treatment)

# separa treated e control, assegna i pesi
roe_eb$eb_weight <- NA
roe_eb$eb_weight[roe_eb$treatment == 1] <- 1                # trattati: peso 1
roe_eb$eb_weight[roe_eb$treatment == 0] <- eb_out$w         # controlli: pesi entropy balancing

# controllo veloce: nessun NA rimasto nei pesi, lunghezza corretta
summary(roe_eb$eb_weight)
sum(is.na(roe_eb$eb_weight))

### STEP 2 — Specifica preferita: survey design con pesi di calibrazione 

library(survey)

# current_occupation come categoriale (non dummy) per il modello finale, come specificato nel testo
roe_design <- svydesign(
  ids = ~1,                       # nessun clustering, unità indipendenti
  weights = ~eb_weight,
  data = roe_eb
)

model_roe_svy <- svyglm(
  qa3a_6 ~ treatment + age + female + rural + current_occupation,
  design = roe_design,
  family = quasibinomial()
)

summary(model_roe_svy)
nobs(model_roe_svy)


### STEP 3 — Odds Ratios e AME 

# Odds ratio del trattamento (dalla specifica preferita)
exp(coef(model_roe_svy)["treatment"])
exp(confint(model_roe_svy))["treatment", ]   # intervallo di confidenza sull'OR

# Average Marginal Effect del trattamento
library(marginaleffects)

ame_roe_correct <- avg_slopes(model_roe_svy, variables = "treatment")
ame_roe_correct


### Modello senza controlli — survey design con pesi entropy balancing 

model_roe_svy_nocontrols <- svyglm(
  qa3a_6 ~ treatment,
  design = roe_design,
  family = quasibinomial()
)

summary(model_roe_svy_nocontrols)
nobs(model_roe_svy_nocontrols)

# Odds ratio
exp(coef(model_roe_svy_nocontrols)["treatment"])
exp(confint(model_roe_svy_nocontrols))["treatment", ]

# AME
library(marginaleffects)
ame_roe_nocontrols <- avg_slopes(model_roe_svy_nocontrols, variables = "treatment")
ame_roe_nocontrols

nobs(model_roe_svy)              # con controlli
nobs(model_roe_svy_nocontrols)   # senza controlli

