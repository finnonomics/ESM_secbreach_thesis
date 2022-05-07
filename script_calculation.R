#### Calculation - Thesis project #####

# This script conducts an event study based on various data sets that have been  
# wrangled in "script_wrangling.R". Consult this script 
# to track data processing

# Creator: Finn Hagemann
# First version: 2022-03-06
# This version: 2022-04-23

# Make sure your folders are set up
# data_input (only raw data!)
# data_output (only modified data!)
# figures and tables

# The terms ticker and stock-symbol are used interchangeably

#### Packages & setup ####

#Load necessary tools
library(tidyverse)
library(lubridate)
library(tidyquant)
library(stargazer)
library(knitr)
library(kableExtra)

#Prevent scientific notations
options(scipen=999)

#set working directory
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/02 I Studium/4. Semester/Thesis/Thesis_Data")

#### Load breach data ####

#Load "Breaches" data set, that has been created in "script_wrangling.R"
Breaches <- read_csv("data_output/Breaches.csv") %>%
  arrange(ID, Date) 

#### Wrangle Trading Days ####

#Create dataset fro trading days only with EventTime for trading days only
Breaches_trdg <- Breaches %>%
  mutate(BreachDateObs = ifelse(BreachDate == Date | !is.na(Return_adj), 1, 0)) %>% 
  filter(BreachDateObs == 1) %>%
  group_by(ID) %>% 
  mutate(ft = row_number()) %>% 
  mutate(event_ft = ifelse(BreachDate == Date, ft, NA)) %>% 
  fill(event_ft, .direction = "updown") %>% 
  mutate(EventTime_trdg = ft - event_ft) %>% 
  select(-event_ft, -ft, -BreachDateObs) %>% 
  ungroup()

#### Descriptive Analysis ####

#Frequency of type of attacks
Breaches_trdg %>% 
  distinct(ID, .keep_all = TRUE) %>% 
  mutate(Type = ifelse(Ransomware == T, "Ransomware \nincidents", 
                       ifelse(Type == "HACK", "Cybersecurity \nincidents",
                              ifelse(Type == "DISC", "Unintended \ndisclosure",  
                                     ifelse(Type == "PORT", "Lost, discarded \nor stolen device", "Other type \nof breaches"))))) %>%
  group_by(Type) %>% 
  summarize(no_types = length(Type)) %>% 
  ungroup()

#Frequency of type of attacks in histogram
Breaches_trdg %>% 
  distinct(ID, .keep_all = TRUE) %>%
  mutate(Type = ifelse(Ransomware == T, "Ransomware \nincidents", 
                       ifelse(Type == "HACK", "Cybersecurity \nincidents",
                              ifelse(Type == "DISC", "Unintended \ndisclosure",  
                                     ifelse(Type == "PORT", "Lost, discarded \nor stolen device", "Other type \nof breaches"))))) %>% 
  ggplot(aes(x = Type, fill = Type)) +
  geom_bar(alpha = 0.75) +
  scale_fill_manual(values = c("#90c9df", "#ba0020", "#d5c29d", "#90c9df", "#5b6770")) +
  labs(x = "",
       y = "Frequency",
       caption = "*Ransomware incidents do also belong to cybersecurity incidents") +
  guides(fill = "none") +
  theme_classic()

ggsave("figures_tables/histogram_incidenttypes.png",
       width = 2000,
       height = 1250,
       units = "px")

#Create Density Plot for Breach Dates
Breaches_trdg %>% 
  distinct(ID, .keep_all = TRUE) %>% 
  mutate(InciType = ifelse(Ransomware == T, "Ransomware incidents",
                           ifelse(Type == "HACK", "Cybersecurity incidents",
                           "Other incidents"))) %>% 
  mutate(BreachYear = as.numeric(format(.$BreachDate, "%Y"))) %>% 
  ggplot(aes(x = BreachYear, fill = InciType)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_fill_manual(values = c("#ba0020", "#5b6770", "#90c9df")) +
  labs(x = "Breach dates per year",
       y = "Density",
       fill = "") +
  theme_classic()

ggsave("figures_tables/density_breachdates.png",
       width = 2000,
       height = 1250,
       units = "px")

#Frequency of industries 
Breaches_trdg %>% 
  distinct(ID, .keep_all = TRUE) %>% 
  group_by(Industry) %>% 
  summarize(no_industry = length(Industry)) %>% 
  ungroup() %>% 
  arrange(desc(no_industry))

#### 1.1 Event Study: Abnormal Returns S&P 500 ####

#Calculate Model Parameters for Abnormal Returns
model_market <- Breaches_trdg %>% 
  filter(EventTime_trdg >= -375 & #Cut down to estimation window
           EventTime_trdg <=  -11) %>%
  nest(data = -ID) %>% 
  mutate(model = map(data, ~lm(Return_adj ~ Return_sp500, data = .)), 
         tidied = map(model, broom::tidy)) %>% 
  unnest(tidied) %>% 
  select(-model, -data) %>% 
  mutate(term = gsub("\\(", "", .$term)) %>% 
  mutate(term = gsub("\\)", "", .$term)) %>% 
  pivot_wider(id_cols = ID, 
              names_from = term,
              values_from = estimate) %>% 
  rename("beta_estimate" = Return_sp500,
         "alpha_estimate" = Intercept)

#Join dataframes
Breaches_trdg <- Breaches_trdg %>% 
  left_join(model_market, 
            by = c(ID = "ID"))

#Calculate Abnormal Returns (ARs)(i.e., apply model parameters)
Breaches_estudy <- Breaches_trdg %>%
  #Filter for event window
  filter(EventTime_trdg >= -10 & 
           EventTime_trdg <=  30) %>%
  #Calculate ARs
  mutate(AR = Return_adj - (alpha_estimate + beta_estimate*Return_sp500)) %>%
  #Select relevant variables
  select(ID, Company, Date, EventTime_trdg, BreachDate,
         Return_adj, Return_sp500, AR, Ransomware, Type,
         alpha_estimate, beta_estimate)

#### 1.2 Event Study: Abnormal Returns NYSE ####

#Calculate Model Parameters for Abnormal Returns
model_nyse <- Breaches_trdg %>% 
  filter(EventTime_trdg >= -375 & #Cut down to estimation window
           EventTime_trdg <=  -11) %>%
  nest(data = -ID) %>% 
  mutate(model = map(data, ~lm(Return_adj ~ Return_nyse, data = .)), 
         tidied = map(model, broom::tidy)) %>% 
  unnest(tidied) %>% 
  select(-model, -data) %>% 
  mutate(term = gsub("\\(", "", .$term)) %>% 
  mutate(term = gsub("\\)", "", .$term)) %>% 
  pivot_wider(id_cols = ID, 
              names_from = term,
              values_from = estimate) %>% 
  rename("beta_nyse" = Return_nyse,
         "alpha_nyse" = Intercept)

#Join dataframes
Breaches_trdg <- Breaches_trdg %>% 
  left_join(model_nyse, 
            by = c(ID = "ID"))

#Calculate Abnormal Returns (ARs)(i.e., apply model parameters)
Breaches_estudy_nyse <- Breaches_trdg %>%
  #Filter for event window
  filter(EventTime_trdg >= -10 & 
           EventTime_trdg <=  30) %>%
  #Calculate ARs
  mutate(AR_nyse = Return_adj - (alpha_nyse + beta_nyse*Return_nyse)) %>%
  #Select relevant variables
  select(ID, Company, Date, EventTime_trdg, BreachDate,
         Return_adj, Return_nyse, AR_nyse, Ransomware, Type,
         alpha_nyse, beta_nyse)

#### 2.1 Event Study: full dataset  ####

#Calculate AARs
AAR <- Breaches_estudy %>% 
  group_by(EventTime_trdg) %>% 
  summarize(AAR = mean(AR, na.rm = T))

#Calculating a t.test for AARs. 
AAR_ttest <- Breaches_estudy %>% 
  mutate(Exp_Return = alpha_estimate + beta_estimate*Return_sp500) %>% 
  nest(data = -EventTime_trdg) %>% 
  mutate(model = map(data, ~t.test(x = .$AR)), 
         tidied = map(model, broom::tidy)) %>% 
  unnest(tidied)

#Create dataframe with AARs
df_AAR <- AAR %>% 
  left_join(., AAR_ttest, by = c(EventTime_trdg = "EventTime_trdg")) %>% 
  mutate(across(where(is.numeric), round, 4)) %>% 
  mutate(significance = ifelse(p.value < 0.01, "***",
                        ifelse(p.value < 0.05, "**",
                               ifelse(p.value < 0.1, "*", "")))) %>% 
  select(EventTime_trdg, 
         AAR, 
         statistic, p.value, significance) %>%
  arrange(EventTime_trdg)

#Create time-series plot for AARs
AAR %>% 
  left_join(., AAR_ttest, by = c(EventTime_trdg = "EventTime_trdg")) %>% 
  mutate(dataset = "All Incidents") %>% 
  mutate(significance = ifelse(p.value < 0.01, "p < 0.01",
                               ifelse(p.value < 0.05, "p < 0.05",
                                      ifelse(p.value < 0.1, "p < 0.1", "not significant")))) %>% 
  ggplot(aes(x = EventTime_trdg, y = AAR)) +
  geom_line() +
  geom_point(aes(x = EventTime_trdg, y = AAR, color = significance), size = 2) +
  scale_colour_manual(name="", values=c("black", "#ba0020", "#90c9df", "#d5c29d")) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, 
             color = "#d11647") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  facet_wrap(~dataset, scales = "fixed") +
  labs(x = "Days around breach announcement", 
       y = "AARs") +
  guides(color = guide_legend(title = "T-Test results")) + 
  theme_classic()

ggsave("figures_tables/plot_AAR_full.png",
       width = 2000,
       height = 1250,
       units = "px")

#Calculate CARs with varying length
CAR5 <- Breaches_estudy %>% 
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  5) %>%
  group_by(ID) %>% 
  summarize(CAR = sum(AR, na.rm = T))

CAR10 <- Breaches_estudy %>% 
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  10) %>%
  group_by(ID) %>% 
  summarize(CAR = sum(AR, na.rm = T))

CAR30 <- Breaches_estudy %>% 
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  30) %>%
  group_by(ID) %>% 
  summarize(CAR = sum(AR, na.rm = T))

#Calculate CAARs and t.test for CAARs with varying length
CAAR5 <- AAR %>%
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  5) %>%
  summarize(CAAR = sum(AAR, na.rm = T)) %>% 
  mutate(Start = 0, 
         End = 5) %>%
  nest(data = c(-CAAR, -Start, -End)) %>% 
  mutate(model = map(data, ~t.test(x = CAR5$CAR)), 
         tidied = map(model, broom::tidy)) %>%
  unnest(tidied) %>% 
  select(CAAR, Start, End, statistic, p.value)

CAAR10 <- AAR %>%
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  10) %>%
  summarize(CAAR = sum(AAR, na.rm = T)) %>% 
  mutate(Start = 0, 
         End = 10) %>%
  nest(data = c(-CAAR, -Start, -End)) %>% 
  mutate(model = map(data, ~t.test(x = CAR10$CAR)), 
         tidied = map(model, broom::tidy)) %>%
  unnest(tidied) %>% 
  select(CAAR, Start, End, statistic, p.value)

CAAR30 <- AAR %>%
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  30) %>%
  summarize(CAAR = sum(AAR, na.rm = T)) %>% 
  mutate(Start = 0, 
         End = 30) %>%
  nest(data = c(-CAAR, -Start, -End)) %>% 
  mutate(model = map(data, ~t.test(x = CAR30$CAR)), 
         tidied = map(model, broom::tidy)) %>%
  unnest(tidied) %>% 
  select(CAAR, Start, End, statistic, p.value)

#Bind CAARs for varying time spans together
rbind(CAAR5, CAAR10, CAAR30) %>% 
  mutate(across(where(is.numeric), round, 4)) %>% 
  mutate(significance = ifelse(p.value < 0.01, "***",
                               ifelse(p.value < 0.05, "**",
                                      ifelse(p.value < 0.1, "*", "")))) %>% 
  arrange(End) %>% 
  kable(col.names = c("CAAR's", "Start day", "End day", "T-Value", "P-Value", "")) %>%
  kable_styling() %>% 
  add_header_above(c("CAAR's across varying time spans for full data set" = 6)) %>% 
  save_kable(file = "figures_tables/table_CAARs.html",
             bs_theme = "readable")

#### 2.2 Event Study: full dataset with NYSE (AARs only) ####

#Calculate AARs on basis of NYSE
AAR_nyse <- Breaches_estudy_nyse %>% 
  group_by(EventTime_trdg) %>% 
  summarize(AAR_nyse = mean(AR_nyse, na.rm = T))

#Calculating a t.test for AARs on basis of NYSE
AAR_ttest_nyse <- Breaches_estudy_nyse %>% 
  mutate(Exp_Return = alpha_nyse + beta_nyse*Return_nyse) %>% 
  nest(data = -EventTime_trdg) %>% 
  mutate(model = map(data, ~t.test(x = .$AR_nyse)), 
         tidied = map(model, broom::tidy)) %>% 
  unnest(tidied)

#Create dataframe with AARs on basis of NYSE
df_AAR_nyse <- AAR_nyse %>% 
  left_join(., AAR_ttest_nyse, by = c(EventTime_trdg = "EventTime_trdg")) %>% 
  mutate(across(where(is.numeric), round, 4)) %>% 
  mutate(significance = ifelse(p.value < 0.01, "***",
                               ifelse(p.value < 0.05, "**",
                                      ifelse(p.value < 0.1, "*", "")))) %>% 
  select(EventTime_trdg, 
         AAR_nyse, 
         statistic, p.value, significance) %>%
  arrange(EventTime_trdg)

#### 3.1 Event Study: cybersecurity incidents ####

#Subset for cyber security incidents only
Breaches_cysec <- Breaches_estudy %>% 
  filter(Type == "HACK")

#Calculate AARs for cyber security incidents
AAR_cysec <- Breaches_cysec %>% 
  group_by(EventTime_trdg) %>% 
  summarize(AAR = mean(AR, na.rm = T))

#Calculating a t.test for AARs of cyber security incidents
AAR_ttest_cysec <- Breaches_cysec %>% 
  mutate(Exp_Return = alpha_estimate + beta_estimate*Return_sp500) %>% 
  nest(data = -EventTime_trdg) %>% 
  mutate(model = map(data, ~t.test(x = .$AR)), 
         tidied = map(model, broom::tidy)) %>% 
  unnest(tidied)

#Create dataframe with AARs
df_AAR_cysec <- AAR_cysec %>% 
  left_join(., AAR_ttest_cysec, by = c(EventTime_trdg = "EventTime_trdg")) %>% 
  mutate(across(where(is.numeric), round, 4)) %>% 
  mutate(significance = ifelse(p.value < 0.01, "***",
                               ifelse(p.value < 0.05, "**",
                                      ifelse(p.value < 0.1, "*", "")))) %>% 
  select(EventTime_trdg, 
         AAR, 
         statistic, p.value, significance) %>%
  arrange(EventTime_trdg)

#Create time-series plot for AARs for cybersecurity incidents
AAR_cysec %>% 
  left_join(., AAR_ttest_cysec, by = c(EventTime_trdg = "EventTime_trdg")) %>% 
  mutate(dataset = "Cybersecurity incidents") %>% 
  mutate(significance = ifelse(p.value < 0.01, "p < 0.01",
                               ifelse(p.value < 0.05, "p < 0.05",
                                      ifelse(p.value < 0.1, "p < 0.1", "not significant")))) %>% 
  ggplot(aes(x = EventTime_trdg, y = AAR)) +
  geom_line() +
  geom_point(aes(x = EventTime_trdg, y = AAR, color = significance), size = 2) +
  scale_colour_manual(name="", values=c("black", "#ba0020", "#90c9df", "#d5c29d")) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, 
             color = "#d11647") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  facet_wrap(~dataset, scales = "fixed") +
  labs(x = "Days around breach announcement", 
       y = "AARs") +
  guides(color = guide_legend(title = "T-Test results")) + 
  theme_classic()

ggsave("figures_tables/plot_AAR_cysec.png",
       width = 2000,
       height = 1250,
       units = "px")

#Calculate CARs for cybersecurit incidents with varying length
CAR5_cysec <- Breaches_cysec %>% 
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  5) %>%
  group_by(ID) %>% 
  summarize(CAR = sum(AR, na.rm = T))

CAR10_cysec <- Breaches_cysec %>% 
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  10) %>%
  group_by(ID) %>% 
  summarize(CAR = sum(AR, na.rm = T))

CAR30_cysec <- Breaches_cysec %>% 
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  30) %>%
  group_by(ID) %>% 
  summarize(CAR = sum(AR, na.rm = T))


#Calculate CAARs and t.test for CAARs with varying length for cybersecurity incidents
CAAR5_cysec <- AAR_cysec %>%
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  5) %>%
  summarize(CAAR = sum(AAR, na.rm = T)) %>% 
  mutate(Start = 0, 
         End = 5) %>%
  nest(data = c(-CAAR, -Start, -End)) %>% 
  mutate(model = map(data, ~t.test(x = CAR5_cysec$CAR)), 
         tidied = map(model, broom::tidy)) %>%
  unnest(tidied) %>% 
  select(CAAR, Start, End, statistic, p.value)

CAAR10_cysec <- AAR_cysec %>%
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  10) %>%
  summarize(CAAR = sum(AAR, na.rm = T)) %>% 
  mutate(Start = 0, 
         End = 10) %>%
  nest(data = c(-CAAR, -Start, -End)) %>% 
  mutate(model = map(data, ~t.test(x = CAR10_cysec$CAR)), 
         tidied = map(model, broom::tidy)) %>%
  unnest(tidied) %>% 
  select(CAAR, Start, End, statistic, p.value)

CAAR30_cysec <- AAR_cysec %>%
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  30) %>%
  summarize(CAAR = sum(AAR, na.rm = T)) %>% 
  mutate(Start = 0, 
         End = 30) %>%
  nest(data = c(-CAAR, -Start, -End)) %>% 
  mutate(model = map(data, ~t.test(x = CAR30_cysec$CAR)), 
         tidied = map(model, broom::tidy)) %>%
  unnest(tidied) %>% 
  select(CAAR, Start, End, statistic, p.value)

#Bind CAARs for varying time spans together for cybersecurit incidents
rbind(CAAR5_cysec, CAAR10_cysec, CAAR30_cysec) %>% 
  mutate(across(where(is.numeric), round, 4)) %>% 
  mutate(significance = ifelse(p.value < 0.01, "***",
                               ifelse(p.value < 0.05, "**",
                                      ifelse(p.value < 0.1, "*", "")))) %>% 
  arrange(End) %>% 
  kable(col.names = c("CAAR's", "Start day", "dnd Day", "T-Value", "P-Value", "")) %>%
  kable_styling() %>% 
  add_header_above(c("CAAR's across varying time spans for cybersecurity incidents" = 6)) %>% 
  save_kable(file = "figures_tables/table_CAARs_cysec.html",
             bs_theme = "readable")

#### 3.2 Event Study: cybersecurity incidents with NYSE ####

#Subset for cyber security incidents only
Breaches_cysec_nyse <- Breaches_estudy_nyse %>% 
  filter(Type == "HACK")

#Calculate AARs for cyber security incidents
AAR_cysec_nyse <- Breaches_cysec_nyse %>% 
  group_by(EventTime_trdg) %>% 
  summarize(AAR = mean(AR_nyse, na.rm = T))

#Calculating a t.test for AARs of cyber security incidents
AAR_ttest_cysec_nyse <- Breaches_cysec_nyse %>% 
  mutate(Exp_Return = alpha_nyse + beta_nyse*Return_nyse) %>% 
  nest(data = -EventTime_trdg) %>% 
  mutate(model = map(data, ~t.test(x = .$AR_nyse)), 
         tidied = map(model, broom::tidy)) %>% 
  unnest(tidied)

#Create dataframe with AARs
df_AAR_cysec_nyse <- AAR_cysec_nyse %>% 
  left_join(., AAR_ttest_cysec_nyse, by = c(EventTime_trdg = "EventTime_trdg")) %>% 
  mutate(across(where(is.numeric), round, 4)) %>% 
  mutate(significance = ifelse(p.value < 0.01, "***",
                               ifelse(p.value < 0.05, "**",
                                      ifelse(p.value < 0.1, "*", "")))) %>% 
  select(EventTime_trdg, 
         AAR, 
         statistic, p.value, significance) %>%
  arrange(EventTime_trdg)

#### 4.1 Event Study: Ransomware ####

#Subset for cyber security breaches only
Breaches_ransom <- Breaches_estudy %>% 
  filter(Ransomware == T)

#Calculate AARs
AAR_ransom <- Breaches_ransom %>% 
  group_by(EventTime_trdg) %>% 
  summarize(AAR = mean(AR, na.rm = T))

#Calculating a t.test for AARs. 
AAR_ttest_ransom <- Breaches_ransom %>% 
  mutate(Exp_Return = alpha_estimate + beta_estimate*Return_sp500) %>% 
  nest(data = -EventTime_trdg) %>% 
  mutate(model = map(data, ~t.test(x = .$AR)), 
         tidied = map(model, broom::tidy)) %>% 
  unnest(tidied)

#Create dataframe with AARs
df_AAR_ransom <- AAR_ransom %>% 
  left_join(., AAR_ttest_ransom, by = c(EventTime_trdg = "EventTime_trdg")) %>% 
  mutate(across(where(is.numeric), round, 4)) %>% 
  mutate(significance = ifelse(p.value < 0.01, "***",
                               ifelse(p.value < 0.05, "**",
                                      ifelse(p.value < 0.1, "*", "")))) %>% 
  select(EventTime_trdg, 
         AAR, 
         statistic, p.value, significance) %>%
  arrange(EventTime_trdg)

#Create graphic for AARs
df_AAR_ransom %>% 
  mutate(significance = ifelse(p.value < 0.01, "p < 0.01",
                               ifelse(p.value < 0.05, "p < 0.05",
                                      ifelse(p.value < 0.1, "p < 0.1", "not significant")))) %>% 
  mutate(dataset = "Ransomware incidents") %>% 
  ggplot(aes(x = EventTime_trdg, y = AAR)) +
  geom_line() +
  geom_point(aes(x = EventTime_trdg, y = AAR, color = significance), size = 2) +
  scale_colour_manual(name="", values=c("black", "#ba0020", "#90c9df", "#d5c29d")) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, 
             color = "#d11647") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
  facet_wrap(~dataset, scales = "fixed") +
  labs(x = "Days around breach announcement", 
       y = "AARs") +
  guides(color = guide_legend(title = "T-Test results")) + 
  theme_classic()

ggsave("figures_tables/plot_AAR_ransom.png",
       width = 2000,
       height = 1250,
       units = "px")

#Calculate CARs with varying length
CAR5_ransom <- Breaches_ransom %>% 
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  5) %>%
  group_by(ID) %>% 
  summarize(CAR = sum(AR, na.rm = T))

CAR10_ransom <- Breaches_ransom %>% 
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  10) %>%
  group_by(ID) %>% 
  summarize(CAR = sum(AR, na.rm = T))

CAR30_ransom <- Breaches_ransom %>% 
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  30) %>%
  group_by(ID) %>% 
  summarize(CAR = sum(AR, na.rm = T))

#Calculate CAARs and t.test for CAARs with varying length
CAAR5_ransom <- AAR_ransom %>%
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  5) %>%
  summarize(CAAR = sum(AAR, na.rm = T)) %>% 
  mutate(Start = 0, 
         End = 5) %>%
  nest(data = c(-CAAR, -Start, -End)) %>% 
  mutate(model = map(data, ~t.test(x = CAR5_ransom$CAR)), 
         tidied = map(model, broom::tidy)) %>%
  unnest(tidied) %>% 
  select(CAAR, Start, End, statistic, p.value)

CAAR10_ransom <- AAR_ransom %>%
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  10) %>%
  summarize(CAAR = sum(AAR, na.rm = T)) %>% 
  mutate(Start = 0, 
         End = 10) %>%
  nest(data = c(-CAAR, -Start, -End)) %>% 
  mutate(model = map(data, ~t.test(x = CAR10_ransom$CAR)), 
         tidied = map(model, broom::tidy)) %>%
  unnest(tidied) %>% 
  select(CAAR, Start, End, statistic, p.value)

CAAR30_ransom <- AAR_ransom %>%
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  30) %>%
  summarize(CAAR = sum(AAR, na.rm = T)) %>% 
  mutate(Start = 0, 
         End = 30) %>%
  nest(data = c(-CAAR, -Start, -End)) %>% 
  mutate(model = map(data, ~t.test(x = CAR30_ransom$CAR)), 
         tidied = map(model, broom::tidy)) %>%
  unnest(tidied) %>% 
  select(CAAR, Start, End, statistic, p.value)

#Bind CAARs for varying time spans together
rbind(CAAR5_ransom, CAAR10_ransom, CAAR30_ransom) %>% 
  mutate(across(where(is.numeric), round, 4)) %>% 
  mutate(significance = ifelse(p.value < 0.01, "***",
                               ifelse(p.value < 0.05, "**",
                                      ifelse(p.value < 0.1, "*", "")))) %>% 
  arrange(End) %>% 
  kable(col.names = c("CAAR's", "Start day", "End day", "T-Value", "P-Value", "")) %>%
  kable_styling() %>% 
  add_header_above(c("CAAR's across varying time spans for ransomware incidents" = 6)) %>% 
  save_kable(file = "figures_tables/table_CAARs_ransom.html",
             bs_theme = "readable")

#### 4.2 Moderator Analysis: Ransomware ####

#Create dataframes for 5-day, 10-day and 30-day period after breach
Breaches_estudy_moderator5 <- Breaches_estudy %>% 
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  5) %>% 
  mutate(Index = "(0;5)") %>% 
  select(ID, EventTime_trdg, AR, Ransomware, Index)

Breaches_estudy_moderator10 <- Breaches_estudy %>% 
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  10) %>% 
  mutate(Index = "(0;10)") %>% 
  select(ID, EventTime_trdg, AR, Ransomware, Index)

Breaches_estudy_moderator30 <- Breaches_estudy %>% 
  filter(EventTime_trdg >= 0 & 
           EventTime_trdg <=  30) %>% 
  mutate(Index = "(0;30)") %>% 
  select(ID, EventTime_trdg, AR, Ransomware, Index)

#Create table with regression results in knitr
rbind(Breaches_estudy_moderator5, 
      Breaches_estudy_moderator10, 
      Breaches_estudy_moderator30) %>% 
  nest(data = -Index) %>% 
  mutate(model = map(data, ~lm(AR ~ Ransomware, data = .)), 
         tidied = map(model, broom::tidy)) %>% 
  unnest(tidied) %>% 
  select(-model, -data) %>% 
  mutate(term = gsub("\\(", "", .$term)) %>% 
  mutate(term = gsub("\\)", "", .$term)) %>%
  mutate(term = gsub("TRUE", "", .$term)) %>% 
  mutate(across(where(is.numeric), round, 4)) %>% 
  mutate(significance = ifelse(p.value < 0.01, "***",
                               ifelse(p.value < 0.05, "**",
                                      ifelse(p.value < 0.1, "*", "")))) %>% 
  filter(term == "Ransomware") %>% 
  select(-term) %>% 
  kable(col.names = c("Time span", "Estimate", "Std. error", "T-Value", "P-Value", "")) %>%
  kable_styling() %>% 
  add_header_above(c("OLS: AR ~ Ransomware (varying time periods)" = 6)) %>% 
  save_kable(file = "figures_tables/table_moderator_ransom.html",
             bs_theme = "readable")

#### 4.3 Event Study: Ransomware incidents with NYSE ####

#Subset for cyber security incidents only
Breaches_ransom_nyse <- Breaches_estudy_nyse %>% 
  filter(Ransomware == T)

#Calculate AARs for cyber security incidents
AAR_ransom_nyse <- Breaches_ransom_nyse %>% 
  group_by(EventTime_trdg) %>% 
  summarize(AAR = mean(AR_nyse, na.rm = T))

#Calculating a t.test for AARs of cyber security incidents
AAR_ttest_ransom_nyse <- Breaches_ransom_nyse %>% 
  mutate(Exp_Return = alpha_nyse + beta_nyse*Return_nyse) %>% 
  nest(data = -EventTime_trdg) %>% 
  mutate(model = map(data, ~t.test(x = .$AR_nyse)), 
         tidied = map(model, broom::tidy)) %>% 
  unnest(tidied)

#Create dataframe with AARs
df_AAR_ransom_nyse <- AAR_ransom_nyse %>% 
  left_join(., AAR_ttest_ransom_nyse, by = c(EventTime_trdg = "EventTime_trdg")) %>% 
  mutate(across(where(is.numeric), round, 4)) %>% 
  mutate(significance = ifelse(p.value < 0.01, "***",
                               ifelse(p.value < 0.05, "**",
                                      ifelse(p.value < 0.1, "*", "")))) %>% 
  select(EventTime_trdg, 
         AAR, 
         statistic, p.value, significance) %>%
  arrange(EventTime_trdg)

#### 5. AARs for all subsets ####

df_AAR_cysec <- df_AAR_cysec %>% 
  rename(Event_Time_cysec = EventTime_trdg, 
         AAR_cysec = AAR, 
         statistic_cysec = statistic, 
         p.value_cysec = p.value, 
         significance_cysec = significance)

df_AAR_ransom <- df_AAR_ransom %>% 
  rename(Event_Time_ransom = EventTime_trdg, 
         AAR_ransom = AAR, 
         statistic_ransom = statistic, 
         p.value_ransom = p.value, 
         significance_ransom = significance)

df_AAR %>% 
  left_join(., df_AAR_cysec, by = c(EventTime_trdg = "Event_Time_cysec")) %>%
  left_join(., df_AAR_ransom, by = c(EventTime_trdg = "Event_Time_ransom")) %>% 
  mutate(across(where(is.numeric), round, 4)) %>% 
  kable(col.names = c("", "AAR", "T-Value", "P-Value", "",
                      "AAR", "T-Value", "P-Value", "",
                      "AAR", "T-Value", "P-Value", "")) %>%
  kable_styling() %>% 
  add_header_above(c("Time" = 1, 
                     "Full sample" = 4, 
                     "Cybersec. incidents" = 4, 
                     "Ransomware incidents" = 4)) %>% 
  save_kable(file = "figures_tables/table_full_AARs.html",
             bs_theme = "readable")
