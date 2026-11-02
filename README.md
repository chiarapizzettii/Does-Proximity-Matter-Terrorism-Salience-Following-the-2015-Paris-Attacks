# Terrorism Salience and Proximity: Evidence from the 2015 Paris Attacks

Master's thesis in Analytics and Data Science for Economics and Management, University of Brescia & University of Strasbourg, 2025/2026
**Author:** Chiara Pizzetti · **Supervisor:** Prof. Nicola Pontarollo · **Co-supervisor:** Prof. Giovanni Maria Abbiati

## Overview

Terrorism is statistically rare, yet it can generate fear and political concern far beyond its objective risk. This thesis studies **terrorism salience**, defined as the extent to which citizens identify terrorism as one of the most important problems facing their country, and asks:

> **Does proximity to a terrorist attack, geographical or experiential, condition its effect on terrorism salience?**

## Contribution

Prior work on the indirect effects of attacks is mixed. This thesis adds two things:

- **Geographical proximity:** neighbouring countries are included as an intermediate group between France and the rest of Europe.
- **Experiential proximity:** previous exposure to terrorism is introduced as a second dimension of proximity.

## Data and Design

- **Event:** the terrorist attacks in Paris on 13 November 2015.
- **Data:** Standard Eurobarometer 84.3 (fieldwork 7–17 November 2015).
- **Outcome:** binary variable equal to 1 if the respondent mentions terrorism as one of the most important problems facing their country, 0 otherwise.
- **Identification:** Unexpected Event During Survey Design (UESD). Because the attacks occurred during fieldwork, respondents interviewed shortly before and after are compared. Treatment is being interviewed after the attacks.
- **Time window:** 3 days before vs. 3 days after, excluding 13 November. A 1-day window is used as a robustness check.

### Samples

| Sample | Definition |
|---|---|
| France | Directly targeted country |
| Neighbouring countries | Countries sharing a border with France |
| Rest of Europe | All other countries in the survey |
| Previous Exposure Countries (PEC) | Countries with at least one terrorist attack between 2000 and 2015 (overlaps with the other three) |

### Methods

- Assumption checks following Muñoz et al.: **excludability** and **ignorability** (balance tests on socio-demographic controls).
- **Entropy balancing** for samples with covariate imbalance (rest of Europe, PEC).
- **Logit models**, with and without socio-demographic controls, reported as odds ratios and **average marginal effects (AMEs)**.

## Main Findings

- The Paris attacks increased terrorism salience in **all samples**, but the magnitude varies.
- **France** shows the largest increase, consistent with direct targeting and with the heightened salience already present after Charlie Hebdo (January 2015).
- There is **no clear geographical gradient**: neighbouring countries look similar to the rest of Europe.
- The effect is **larger in countries with previous exposure** to terrorism.
- Results are robust to a narrower 1-day window.

**Takeaway:** previous exposure to terrorism appears to matter more than geographical proximity to the attack.

## Limitations and Future Research

- The analysis covers a single, exceptional event.
- It captures only the immediate reaction.
- Future work could examine other attacks and longer time horizons, to test whether the effect is a temporary shock or becomes a lasting policy priority.

## Repository Structure

```
├── data/          # [Eurobarometer 84.3 is available from GESIS; see Data Access]
├── code/          # [R scripts]
├── thesis.pdf     # Full thesis text
└── slides.pdf     # Defense presentation
```

## Data Access

Standard Eurobarometer 84.3 is publicly available from [GESIS](https://www.gesis.org/en/eurobarometer-data-service). 

## Replication

```bash
# [Add your steps, e.g.]
# 1. Download Eurobarometer 84.3 and place it in data/
# 2. Run code/01_clean.R
# 3. Run code/02_models.R
```
