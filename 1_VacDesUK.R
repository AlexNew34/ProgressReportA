# This code does Data cleaning for the United Kingdom vaccine usage.

# Load the datasets.
library(tidyverse)
library(lubridate)
library(viridis)
library(readr)
# Read the data file from https://github.com/YouGov-Data/covid-19-tracker. Encoding was required due to difference in '-'.
df1 <- read_csv("./data/unitedkingdom.csv", locale = locale(encoding = "Windows-1252"))

# Change missing values to NA (eg.: NA, ,__NA__).
df1[df1 == "__NA__"] <- NA
df1[df1 == " "] <- NA
df1[df1 == ""] <- NA

# Convert string to date time.
df1$endtime <- as_date(dmy_hm(df1$endtime))

# Get as many of the vaccine variables in the dataframe. Google AI was used to help with this step.
vaccine_cols <- grep("vaccine|vaccin|dose|booster|vac_boost|vac14", names(df1), 
                     ignore.case = TRUE, value = TRUE)

# Remove columns with more than 20% missing values except the vaccine columns.
df1 <- df1[, colMeans(is.na(df1)) < 0.2 | names(df1) %in% vaccine_cols]

# Cleaning similar to the face mask predictors reproduced work.
df1$household_size[df1$household_size == "8 or more"] <- "8"
df1$household_size[df1$household_size == "Prefer not to say"] <- NA
df1$household_size[df1$household_size == "Don't know"] <- NA

df1$r1_1[df1$r1_1 == "7 - Agree"] <- "7"
df1$r1_1[df1$r1_1 == "1 – Disagree"] <- "1"
df1$r1_2[df1$r1_2 == "7 - Agree"] <- "7"
df1$r1_2[df1$r1_2 == "1 – Disagree"] <- "1"

protective_cols <- grep("^i12_", names(df1), value = TRUE)

df1[protective_cols][df1[protective_cols] == "Always"] <- "5"
df1[protective_cols][df1[protective_cols] == "Frequently"] <- "4"
df1[protective_cols][df1[protective_cols] == "Sometimes"] <- "3"
df1[protective_cols][df1[protective_cols] == "Rarely"] <- "2"
df1[protective_cols][df1[protective_cols] == "Not at all"] <- "1"

df1[c("PHQ4_1", "PHQ4_3", "PHQ4_4")][is.na(df1[c("PHQ4_1", "PHQ4_3", "PHQ4_4")])] <- "N/A"

df1 <- df1 %>% 
  mutate(across(c(i2_health, household_size, all_of(protective_cols)), as.integer))

# Drop NA values in the vaccine columns.
df1 <- df1 %>%
  drop_na(-all_of(vaccine_cols))

# Perform cleaning in the vaccine columns (manually).
df1$vac_boost_beyond[df1$vac_boost_beyond == "1 - Strongly agree"] <- "1"
df1$vac_boost_beyond[df1$vac_boost_beyond == "5 – Strongly disagree"] <- "5"
df1$vac_booster[df1$vac_booster == "1 - Strongly agree"] <- "1"
df1$vac_booster[df1$vac_booster == "5 – Strongly disagree"] <- "5"
df1$vac_boost_2[df1$vac_boost_2 == "1 - Strongly agree"] <- "1"
df1$vac_boost_2[df1$vac_boost_2 == "5 – Strongly disagree"] <- "5"
df1 <- subset(df1, select = -vac12_booster_other)
vac12_cols <- grep("^vac12_", names(df1), value = TRUE)
df1[vac12_cols][df1[vac12_cols] == "Yes"] <- "1"
df1[vac12_cols][df1[vac12_cols] == "No"] <- "0"
df1$vac_boost_1[df1$vac_boost_1 == "Yes"] <- "1"
df1$vac_boost_1[df1$vac_boost_1 == "No"] <- "0"
vac14_cols <- grep("^vac14_", names(df1), value = TRUE)
df1[vac14_cols][df1[vac14_cols] == "More likely to get vaccinated for Covid-19"] <- "1"
df1[vac14_cols][df1[vac14_cols] == "Neither more or less likely to get vaccinated for Covid-19"] <- "N/A"
df1[vac14_cols][df1[vac14_cols] == "Not sure"] <- "N/A"
df1[vac14_cols][df1[vac14_cols] == "Less likely to get vaccinated for Covid-19"] <- "0"
vac_cols <- grep("vac", names(df1), value = TRUE)
df1 <- df1 %>%
  mutate(across(all_of(vac_cols), as.numeric))

# Cleaning similar to reproduced work on face mask predictors.
df1$protective_behaviour_scale <- apply(df1[protective_cols], 1, median, na.rm = TRUE)

df1$protective_behaviour_binary <- ifelse(
  df1$protective_behaviour_scale >= 4, "Yes", "No"
)

d1_cols <- grep("^d1_", names(df1), value = TRUE)

df1$d1_comorbidities <- "Yes"
df1$d1_comorbidities[df1$d1_health_99 == "Yes"] <- "No"
df1$d1_comorbidities[df1$d1_health_99 == "N/A"] <- NA
df1$d1_comorbidities[df1$d1_health_98 == "Yes"] <- "Prefer_not_to_say"

df1 <- df1[, !names(df1) %in% d1_cols]

df1 <- df1 %>%
  mutate(
    start_date = min(endtime, na.rm = TRUE),
    week_number = as.numeric(difftime(endtime, start_date, units = "days")) %/% 14 + 1
  )

df1 <- subset(df1, select = -qweek)

# Looked through definitions of some of the variables in the https://github.com/YouGov-Data/covid-19-tracker data.
# Decided to use vaccine desirability as an alternative for vaccine usage.
# Depending on certain rows completion filter the dataframe (to not get rid of too much data).
df1 <- df1 %>%
  filter(
    rowSums(is.na(.)) == 0 |
      !is.na(vac_boost_1) |
      !is.na(vac_boost_2) |
      !is.na(vac14_1) |
      !is.na(vac14_2) |
      !is.na(vac14_3)
  )

# Create a vaccine desire binary value column depending on certain conditions met within certain observations found.
df1 <- df1 %>%
  mutate(
    vaccine_desire = ifelse(
      (ifelse(is.na(vac14_1), 0, vac14_1) == 1) |
        (ifelse(is.na(vac14_2), 0, vac14_2) == 1) |
        (ifelse(is.na(vac14_3), 0, vac14_3) == 1) |
        (ifelse(is.na(vac_boost_1), 0, vac_boost_1) == 1) |
        (ifelse(is.na(vac_boost_2), 0, vac_boost_2) %in% c(4, 5)) |
        (ifelse(is.na(vac_booster), 0, vac_booster) %in% c(4, 5)),
      1, 0
    )
  )

# Make vaccine desire a factor variable.
df1$vaccine_desire <- factor(df1$vaccine_desire, ordered = TRUE)

### EDA ###
# Create a histogram of vaccine desire in the UK.
# Use viridis package for colour blind friendly graphs.
ggplot(df1, aes(x = vaccine_desire, fill = factor(vaccine_desire))) +
  geom_bar(position = "dodge") +
  scale_fill_viridis_d(option = "cividis")+
  labs(x = "Vaccine Desire", y = "Count", fill = "Vaccine Desire", title = "Frequency of Vaccine Desire in the UK") +
  theme_minimal()

# Small table to know proportions of vaccine desire.
df1 %>%
  count(vaccine_desire) %>%
  mutate(prop = n / sum(n))

# Histogram based on gender vaccine desire.
ggplot(df1, aes(x = gender, fill = vaccine_desire)) +
  geom_bar(position = "fill") +
  theme_minimal()

# Histogram based on week number vaccine desire. 
ggplot(df1, aes(x = week_number, fill = factor(vaccine_desire))) +
  geom_bar(position = "dodge") + 
  scale_fill_viridis_d(option = "cividis") +
  labs(x = "Week Number", y = "Count", fill = "Vaccine Desire", title = "Vaccine Desirability in the UK During COVID-19") +
  theme_minimal()

# Save the dateframe.
write_csv(df1, "vac_des_uk.csv")

