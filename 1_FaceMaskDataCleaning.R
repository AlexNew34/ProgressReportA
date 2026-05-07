# This code does data cleaning and EDA for face mask predictors in Australia.

# Some code from https://github.com/Matthew-Ryan1995/face_mask_predictors has been adapted and used in this code.
# Parts that have used Matthew's code will be mentioned in a comment.

# Load the libraries.
library(tidyverse)
library(lubridate)
library(zoo)
library(fastDummies)

# Read the csv files (downloaded) from https://github.com/YouGov-Data/covid-19-tracker, 
# and https://doi.org/10.1038/s41562-021-01079-8
df1<-read.csv('./data/australia.csv')
df2<-read.csv('./data_policy/Australia/OxCGRT_AUS_latest.csv')

# Change missing values to NA (eg.: NA, ,__NA__).
df1[df1 == "__NA__"] <- NA
df1[df1 == " "] <- NA
df1[df1 == ""] <- NA

# Convert string to date time.
df1$endtime <- as_date(dmy_hm(df1$endtime))

#Remove columns with more than 20% missing values.
df1 <- df1[, colMeans(is.na(df1)) < 0.2]

# Convert household size.
df1$household_size[df1$household_size == "8 or more"] <- "8"
df1$household_size[df1$household_size == "Prefer not to say"] <- NA
df1$household_size[df1$household_size == "Don't know"] <- NA

# Convert scale values.
df1$r1_1[df1$r1_1 == "7 - Agree"] <- "7"
df1$r1_1[df1$r1_1 == "1 – Disagree"] <- "1"
df1$r1_2[df1$r1_2 == "7 - Agree"] <- "7"
df1$r1_2[df1$r1_2 == "1 – Disagree"] <- "1"

# Change the scale values for the i12_health values (protective behaviours).
protective_cols <- grep("^i12_", names(df1), value = TRUE)
df1[protective_cols][df1[protective_cols] == "Always"] <- "5"
df1[protective_cols][df1[protective_cols] == "Frequently"] <- "4"
df1[protective_cols][df1[protective_cols] == "Sometimes"] <- "3"
df1[protective_cols][df1[protective_cols] == "Rarely"] <- "2"
df1[protective_cols][df1[protective_cols] == "Not at all"] <- "1"

# Used part of Matthew's code face mask predictors to alter values PHQ4_1, PHQ4_3, PHQ4_4 into N/A values.
df1[c("PHQ4_1", "PHQ4_3", "PHQ4_4")][is.na(df1[c("PHQ4_1", "PHQ4_3", "PHQ4_4")])] <- "N/A"
df1 <- df1 %>% drop_na()

# Change to integer values.
df1 <- df1 %>% 
  mutate(across(c(i2_health, i12_health_1, i12_health_2,  i12_health_3,
                  i12_health_4,  i12_health_5,  i12_health_6,  i12_health_7, 
                  i12_health_8, i12_health_11, i12_health_12, i12_health_13,
                  i12_health_14, i12_health_15, i12_health_16, i12_health_22,
                  i12_health_23, i12_health_25, household_size), as.integer))

# Calculate face mask and protective behaviour scales (got the idea from Matthew's code).
mask_behaviour_cols <- c("i12_health_1", "i12_health_22", "i12_health_23", "i12_health_25")
df1$face_mask_behaviour_scale <- apply(df1[mask_behaviour_cols], 1, median, na.rm = TRUE)

# Face mask binary
df1$face_mask_behaviour_binary <- ifelse(df1$face_mask_behaviour_scale >= 4, "Yes", "No")

# Protective behaviour scale
df1$protective_behaviour_scale <- apply(df1[protective_cols], 1, median, na.rm = TRUE)

# Protective behaviour binary
df1$protective_behaviour_binary <- ifelse(df1$protective_behaviour_scale >= 4, "Yes", "No")

# Difference of protective behaviours and mask behaviour columns (got the idea from Matthew's code).
protective_nomask_cols <- setdiff(protective_cols, mask_behaviour_cols)

# Row-wise median
df1$protective_behaviour_nomask_scale <- apply(df1[protective_nomask_cols],1,median,na.rm = TRUE)
df1$protective_behaviour_nomask_binary <- ifelse(df1$protective_behaviour_nomask_scale >= 4, "Yes", "No")

# All d1 columns
d1_cols <- grep("^d1_", names(df1), value = TRUE)

# Create default variable and apply the following conditions (from Matthew's code).
df1$d1_comorbidities <- "Yes"
df1$d1_comorbidities[df1$d1_health_99 == "Yes"] <- "No"
df1$d1_comorbidities[df1$d1_health_99 == "N/A"] <- NA
df1$d1_comorbidities[df1$d1_health_98 == "Yes"] <- "Prefer_not_to_say"

# Remove columns
df1 <- df1[, !names(df1) %in% d1_cols]
df1 <- df1[, !names(df1) %in% protective_cols]

# Used part of Matthew's code for face mask predictors to get new week numbers based on the days.
df1 <- df1 %>%
  mutate(
    start_date = min(endtime, na.rm = TRUE),
    week_number = as.numeric(difftime(endtime, start_date, units = "days")) %/% 14 + 1
  )

# Remove qweek.
df1 <- subset(df1, select = -qweek)


### EDA ###
# Skim the dataframe.
skimr::skim_without_charts(df1)

# Basic histograms for numeric scales.
hist(df1$face_mask_behaviour_scale, main="Face mask behaviour scale", xlab="Scale", col="lightblue")
hist(df1$protective_behaviour_scale, main="Protective behaviour scale", xlab="Scale", col="lightgreen")
hist(df1$protective_behaviour_nomask_scale, main="Protective behaviour (no mask)", xlab="Scale", col="lightcoral")

# Barplots for categorical variables
barplot(table(df1$face_mask_behaviour_binary), main="Face mask behaviour binary", col="lightblue")
barplot(table(df1$protective_behaviour_binary), main="Protective behaviour binary", col="lightgreen")
barplot(table(df1$d1_comorbidities), main="Comorbidities", col="lightcoral")

# Histograms of weekly score distributions for face mask wearing in Australia.
ggplot(df1, aes(x = face_mask_behaviour_scale)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  facet_wrap(~ week_number) +  # Creates a sub-plot for each week
  labs(title = "Weekly Score Distributions For Face Mask Wearing in Australia During COVID-19 Pandemic", x = "Score", y = "Count")

### Extra data cleaning for model development.
df1 <- subset(df1, select = -c(start_date))
df2 <- subset(df2, select = c(RegionName, Date, H6M_Facial.Coverings))
df2$Date <- ymd(df2$Date)

# Got this from Matthew's code face mask predictors. These steps are to get the mandates periods by using a 'rolling' mean for fortnightly averages.
result <- df2 %>%
  group_by(RegionName) %>%
  arrange(Date) %>%
  mutate(fort_avg = rollmean(H6M_Facial.Coverings, k = 14, fill = NA, align = "right"))
# Get the averages grouped by RegionName (so only 1 State is used per mandate period).
df_mandates <- result %>%
  filter(fort_avg >= 3) %>%
  group_by(RegionName) %>%
  slice(1) %>%
  ungroup()

# Add the mandate periods and intergrate them in a final dataframe (all data merged into one dataset).
df_mandates <- subset(df_mandates, select = c(RegionName, Date, fort_avg))
df_mandates <- df_mandates[df_mandates$RegionName != "", ]
df_mandates <- rename(df_mandates, state = RegionName, start_date = Date)

df_final <- left_join(df1, df_mandates, by = "state")
df_final <- df_final %>%
  mutate(within_mandates = ifelse(endtime >= start_date, 1, 0))
df_final <- subset(df_final, select = -c(fort_avg,start_date))

# Create dummy variables for ease of model development in the final dataframe.
df_final <- dummy_cols(df_final, select_columns = c('i9_health','i11_health', 'gender','state', 'employment_status','WCRex2',
                                                  'WCRex1', 'PHQ4_1', 'PHQ4_3', 'PHQ4_4',
                                                  'd1_comorbidities'), remove_first_dummy = TRUE, remove_selected_columns = TRUE)
# Save the final dataframe for model development.
write_csv(df_final, "face_mask_ausmand.csv")
