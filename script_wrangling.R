#### Wrangling - Thesis project #####

# This script wrangles all data inputs into a common data base  
# that is used to conduct an event study on the effect of data breaches & ransomware
# on companies market value in the US

# Creator: Finn Hagemann
# First version: 2022-03-06
# This version: 2022-05-07

# Make sure your folders are set up
# data_input (only raw data!)
# data_output (only modified data!)
# figures and tables

# The terms ticker and stock-symbol are used interchangeably

#### Packages & setup ####

#Load necessary tools
library(tidyverse)
library(lubridate)
library(fuzzyjoin)
library(tidyquant)
library(readxl)

#Prevent scientific notations
options(scipen=999)

#set working directory
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/02 I Studium/4. Semester/Thesis/Data")

#### Loading data ####

#Load Privacy Rights Clearinghouse dataset (security breaches)
prc <- read_csv("data_input/PRC.csv") %>% 
  mutate(Date = as.POSIXct(`Date Made Public`,format='%m/%d/%Y')) %>% 
  drop_na(`Date Made Public`, Company) %>% 
  mutate(Type = `Type of breach`,
         Organization = `Type of organization`, 
         Description = `Description of incident`,
         Location = State,
         Vector = NA,
         RansomPaid = NA,
         RansomScale = NA) %>% 
  mutate(Companyfull = Company) %>%
  mutate(Company = tolower(Company)) %>% 
  select(Companyfull, Company, Date, State, Location, 
         Type, Vector, Organization, Description)

#Load "Ransomware Repository" by the University of Temple (ransomware incidents)
rwr <- read_excel("data_input/RW-Repository.xlsx") %>% 
  drop_na(Date_Began, OrgName) %>% 
  mutate(Company = OrgName,
         Date = Date_Began, 
         Location = `Location (State)`,
         Duration = `Duration (days, unless specified)`,
         Organization = `Primary CI Sector targeted`,
         Type = "HACK", 
         Vector = "Ransomware",
         RansomPaid = PaidStatus,
         Description = Comments) %>%
  mutate(Companyfull = Company) %>%
  mutate(Company = tolower(Company)) %>% 
  select(Companyfull, Company, Date, State, Location, 
         Type, Vector, Organization, Description)

#Load Capital IQ stock-symbol list
ciq <- read_csv("data_input/ciqUS.csv") %>% 
  drop_na(Company, Ticker)%>% 
  mutate(Companyfull = Company) %>%
  mutate(Company = tolower(.$Company)) %>% 
  select(Companyfull, Company, Ticker)

#Load Alpha Vantage stock-symbol list
av <- read_csv("data_input/AlphaVantage_Listing.csv", na = "null") %>% 
  filter(assetType == "Stock") %>% 
  drop_na(name, symbol) %>%
  mutate(Company = name) %>% 
  mutate(Ticker = symbol) %>%
  filter(str_detect(symbol, '-', negate = T)) %>% 
  mutate(Companyfull = Company) %>% 
  mutate(Company = tolower(Company)) %>%
  select(Companyfull, Company, Ticker, ipoDate)

#### Pre-processing company name layout ####

#Change company layout to prepare datasets for matching
#by deleting punctuation, deleting spaces, deleting legal notations
#and transforming all names into lowercase symbols.

#Change Ticker and Company Layout for CIQ
ciq <- ciq %>% mutate(Ticker = gsub(".*:", "", .$Ticker))
ciq <- ciq %>% mutate(Company = gsub("\\s*\\([^\\)]+\\)", "", .$Company))
ciq <- ciq %>% mutate(Companyfull = gsub("\\s*\\([^\\)]+\\)", "", .$Company))
ciq <- ciq %>% mutate(Company = gsub(", l.p.", "", .$Company))
ciq <- ciq %>% mutate(Company = gsub(" l.p.", "", .$Company))
ciq <- ciq %>% mutate(Company = gsub(" lp", "", .$Company))
ciq <- ciq %>% mutate(Company = gsub(", lp", "", .$Company))
ciq <- ciq %>% mutate(Company = gsub(", inc.", "", .$Company))
ciq <- ciq %>% mutate(Company = gsub(" inc.", "", .$Company))  
ciq <- ciq %>% mutate(Company = gsub(" inc", "", .$Company))
ciq <- ciq %>% mutate(Company = gsub(" corporation", "", .$Company))
ciq <- ciq %>% mutate(Company = gsub("\\& company", "", .$Company))
ciq <- ciq %>% mutate(Company = gsub(" corp", "", .$Company))  
ciq <- ciq %>% mutate(Company = gsub(", corp", "", .$Company))
ciq <- ciq %>% mutate(Company = gsub(" co.", "", .$Company))
ciq <- ciq %>% mutate(Company = gsub(" llc", "", .$Company))     
ciq <- ciq %>% mutate(Company = gsub(", llc", "", .$Company))         
ciq <- ciq %>% mutate(Company = gsub(", llc,", "", .$Company))       
ciq <- ciq %>% mutate(Company = gsub(", ltd.", "", .$Company))       
ciq <- ciq %>% mutate(Company = gsub(" ltd.", "", .$Company))       
ciq <- ciq %>% mutate(Company = gsub(" ltd", "", .$Company))
ciq <- ciq %>% mutate(Company = gsub("\\.", "", .$Company)) 
ciq <- ciq %>% mutate(Company = gsub("\\,", "", .$Company)) 
ciq <- ciq %>% mutate(Company = gsub(" i", "", .$Company)) 
ciq <- ciq %>% mutate(Company = gsub("holdings", "", .$Company))
ciq <- ciq %>% mutate(Company = gsub("pany", "", .$Company))

ciq <- ciq %>% mutate(Company = gsub("\\`", "", .$Company))
ciq <- ciq %>% mutate(Company = gsub("\\'", "", .$Company))
ciq <- ciq %>% mutate(Company = gsub("é", "e", .$Company))
ciq <- ciq %>% mutate(Company = gsub("\\.", "", .$Company))
ciq <- ciq %>% mutate(Company = gsub("\\& ", "\\&", .$Company))
ciq <- ciq %>% mutate(Company = gsub("\\-", "", .$Company))
ciq <- ciq %>% mutate(Company = gsub(" ", "", .$Company))
ciq <- ciq %>% mutate(Company = gsub("é", "e", .$Company))
ciq <- ciq %>% mutate(Company = gsub("é", "e", .$Company))

ciq <- ciq %>% distinct(Company, .keep_all = TRUE)

#Change Ticker and Company Layout for AV
av <- av %>% mutate(Company = gsub("\\-.*","", .$Company))
av <- av %>% distinct(Ticker, .keep_all = T)
av <- av %>% mutate(Company = gsub(", l.p.", "", .$Company))
av <- av %>% mutate(Company = gsub(" l.p.", "", .$Company))
av <- av %>% mutate(Company = gsub(" lp", "", .$Company))
av <- av %>% mutate(Company = gsub(", lp", "", .$Company))
av <- av %>% mutate(Company = gsub(", inc.", "", .$Company))
av <- av %>% mutate(Company = gsub(" inc.", "", .$Company))  
av <- av %>% mutate(Company = gsub(" inc", "", .$Company))
av <- av %>% mutate(Company = gsub(" corporation", "", .$Company))
av <- av %>% mutate(Company = gsub("\\& company", "", .$Company))
av <- av %>% mutate(Company = gsub(" corp", "", .$Company))  
av <- av %>% mutate(Company = gsub(", corp", "", .$Company))
av <- av %>% mutate(Company = gsub(" co.", "", .$Company))
av <- av %>% mutate(Company = gsub(" llc", "", .$Company))     
av <- av %>% mutate(Company = gsub(", llc", "", .$Company))         
av <- av %>% mutate(Company = gsub(", llc,", "", .$Company))       
av <- av %>% mutate(Company = gsub(", ltd.", "", .$Company))       
av <- av %>% mutate(Company = gsub(" ltd.", "", .$Company))       
av <- av %>% mutate(Company = gsub(" ltd", "", .$Company))
av <- av %>% mutate(Company = gsub("\\.", "", .$Company)) 
av <- av %>% mutate(Company = gsub("\\,", "", .$Company)) 
av <- av %>% mutate(Company = gsub(" i", "", .$Company))
av <- av %>% mutate(Company = gsub("holdings", "", .$Company))
av <- av %>% mutate(Company = gsub("pany", "", .$Company))

av <- av %>% mutate(Company = gsub("\\`", "", .$Company))
av <- av %>% mutate(Company = gsub("\\'", "", .$Company))
av <- av %>% mutate(Company = gsub("\\.", "", .$Company))
av <- av %>% mutate(Company = gsub("\\& ", "\\&", .$Company))
av <- av %>% mutate(Company = gsub("\\-", "", .$Company))
av <- av %>% mutate(Company = gsub(" ", "", .$Company))
av <- av %>% mutate(Company = gsub("é", "e", .$Company))

av <- av %>% distinct(Company, .keep_all = TRUE)

#Change Ticker and Company Layout for PRC
prc <- prc %>% mutate(Company = gsub(", l.p.", "", .$Company))
prc <- prc %>% mutate(Company = gsub(" l.p.", "", .$Company))
prc <- prc %>% mutate(Company = gsub(" lp", "", .$Company))
prc <- prc %>% mutate(Company = gsub(", lp", "", .$Company))
prc <- prc %>% mutate(Company = gsub(", inc.", "", .$Company))
prc <- prc %>% mutate(Company = gsub(" inc.", "", .$Company))  
prc <- prc %>% mutate(Company = gsub(" inc", "", .$Company))
prc <- prc %>% mutate(Company = gsub(" corporation", "", .$Company))
prc <- prc %>% mutate(Company = gsub(" corp", "", .$Company))  
prc <- prc %>% mutate(Company = gsub(", corp", "", .$Company))
prc <- prc %>% mutate(Company = gsub(" co.", "", .$Company))
prc <- prc %>% mutate(Company = gsub(" llc", "", .$Company))     
prc <- prc %>% mutate(Company = gsub(", llc", "", .$Company))         
prc <- prc %>% mutate(Company = gsub(", llc,", "", .$Company))       
prc <- prc %>% mutate(Company = gsub(", ltd.", "", .$Company))       
prc <- prc %>% mutate(Company = gsub(" ltd.", "", .$Company))       
prc <- prc %>% mutate(Company = gsub(" ltd", "", .$Company))
prc <- prc %>% mutate(Company = gsub("\\.", "", .$Company)) 
prc <- prc %>% mutate(Company = gsub("\\,", "", .$Company)) 
prc <- prc %>% mutate(Company = gsub(" i", "", .$Company))
prc <- prc %>% mutate(Company = gsub("holdings", "", .$Company))

prc <- prc %>% mutate(Company = gsub("\\`", "", .$Company))
prc <- prc %>% mutate(Company = gsub("\\'", "", .$Company))
prc <- prc %>% mutate(Company = gsub("\\.", "", .$Company))
prc <- prc %>% mutate(Company = gsub("\\& ", "\\&", .$Company))
prc <- prc %>% mutate(Company = gsub("\\-", "", .$Company))
prc <- prc %>% mutate(Company = gsub(" ", "", .$Company))
prc <- prc %>% mutate(Company = gsub("é", "e", .$Company))

#Change Ticker and Company Layout for RWR
rwr <- rwr %>% mutate(Company = gsub(", l.p.", "", .$Company))
rwr <- rwr %>% mutate(Company = gsub(" l.p.", "", .$Company))
rwr <- rwr %>% mutate(Company = gsub(" lp", "", .$Company))
rwr <- rwr %>% mutate(Company = gsub(", lp", "", .$Company))
rwr <- rwr %>% mutate(Company = gsub(", inc.", "", .$Company))
rwr <- rwr %>% mutate(Company = gsub(" inc.", "", .$Company))  
rwr <- rwr %>% mutate(Company = gsub(" inc", "", .$Company))
rwr <- rwr %>% mutate(Company = gsub(" corporation", "", .$Company))
rwr <- rwr %>% mutate(Company = gsub(" corp", "", .$Company))  
rwr <- rwr %>% mutate(Company = gsub(", corp", "", .$Company))
rwr <- rwr %>% mutate(Company = gsub(" co.", "", .$Company))
rwr <- rwr %>% mutate(Company = gsub(" llc", "", .$Company))     
rwr <- rwr %>% mutate(Company = gsub(", llc", "", .$Company))         
rwr <- rwr %>% mutate(Company = gsub(", llc,", "", .$Company))       
rwr <- rwr %>% mutate(Company = gsub(", ltd.", "", .$Company))       
rwr <- rwr %>% mutate(Company = gsub(" ltd.", "", .$Company))       
rwr <- rwr %>% mutate(Company = gsub(" ltd", "", .$Company))
rwr <- rwr %>% mutate(Company = gsub("\\.", "", .$Company)) 
rwr <- rwr %>% mutate(Company = gsub("\\,", "", .$Company)) 
rwr <- rwr %>% mutate(Company = gsub(" i", "", .$Company))
rwr <- rwr %>% mutate(Company = gsub("holdings", "", .$Company))

rwr <- rwr %>% mutate(Company = gsub("\\`", "", .$Company))
rwr <- rwr %>% mutate(Company = gsub("\\'", "", .$Company))
rwr <- rwr %>% mutate(Company = gsub("\\.", "", .$Company))
rwr <- rwr %>% mutate(Company = gsub("\\& ", "\\&", .$Company))
rwr <- rwr %>% mutate(Company = gsub("\\-", "", .$Company))
rwr <- rwr %>% mutate(Company = gsub(" ", "", .$Company))
rwr <- rwr %>% mutate(Company = gsub("é", "e", .$Company))

#Correct left-overspelling mistakes manually in the PRC-data and align with AV ticker list
prc$Company <- str_replace(prc$Company, "verifone", "paymentus")
prc$Company <- str_replace(prc$Company, "choicehotelsnternationals", "choicehotelsnternational")
prc$Company <- str_replace(prc$Company, "northropgrunman", "northropgrumman")
prc$Company <- str_replace(prc$Company, "junipernetwork", "junipernetworks")
prc$Company <- str_replace(prc$Company, "northropgrunman", "northropgrumman")
prc$Company <- str_replace(prc$Company, "cloudfare", "cloudflare")
prc$Company <- str_replace(prc$Company, "molinahealthcaren", "molinahealthcare")
prc$Company <- str_replace(prc$Company, "northropgrummansystems", "northropgrumman")
prc$Company <- str_replace(prc$Company, "tdbank", "torontodominionbank")
prc$Company <- str_replace(prc$Company, "tdbankna", "torontodominionbank")
prc$Company <- str_replace(prc$Company, "citi", "citigroup")
prc$Company <- str_replace(prc$Company, "at&tmobility", "at&t")
prc$Company <- str_replace(prc$Company, "appleat&t", "apple")
prc$Company <- str_replace(prc$Company, "jpmorgan", "jpmorganchase")
prc$Company <- str_replace(prc$Company, "advancedmicrodevices(amd)nvidia", "nvidia")
prc$Company <- str_replace(prc$Company, "hsbcbankusanationalassociation", "hsbcplc")
prc$Company <- str_replace(prc$Company, "hsbcfinance", "hsbcplc")
prc$Company <- str_replace(prc$Company, "poloralphlaurenhsbc", "ralphlauren")
prc$Company <- str_replace(prc$Company, "snapchat", "snap")
prc$Company <- str_replace(prc$Company, "google", "alphabet")
prc$Company <- str_replace(prc$Company, "cisco", "ciscosystems")
prc$Company <- str_replace(prc$Company, "theboeing", "boeing")
prc$Company <- str_replace(prc$Company, "boeingco", "boeing")
prc$Company <- str_replace(prc$Company, "thomsonreuters", "thomson")
prc$Company <- str_replace(prc$Company, "tmobileus", "tmobileustradeablerightsjune2020")
prc$Company <- str_replace(prc$Company, "tmobile", "tmobileustradeablerightsjune2020")
prc$Company <- str_replace(prc$Company, "advancedautoparts", "advanceautoparts")
prc$Company <- str_replace(prc$Company, "atlantisplastic", "atlantisplastics")

#### Match security breaches with stock-symbol lists ####

#Match PRC and AV data
match_prc_av <- prc %>%
  stringdist_inner_join(av, 
                        by = c(Company = "Company"), 
                        max_dist = 0,
                        method = "lv",
                        distance_col = "distance") %>%
  mutate(Company_prc = Company.x,Company_av = Company.y,
         Company = Companyfull.y,
         Company_ciq = NA,
         Company_rwr = NA) %>% 
  select(Company_prc, Company_rwr, Company_av, Company_ciq,
         Ticker, Company,
         Date, State, Location, Type, Vector,
         Organization, Description, ipoDate) %>% 
  mutate(Vector = ifelse(Company_prc == "mongodb", "Ransomware", NA))

#Match PRC and CIQ data that have not been matched with the AV data
match_prc_ciq <- prc %>%
  stringdist_inner_join(ciq, 
                        by = c(Company = "Company"), 
                        max_dist = 0,
                        method = "lv",
                        distance_col = "distance") %>%
  mutate(Company_prc = Company.x, 
         Company_ciq = Company.y,
         Company = Companyfull.y,
         Company_av = NA,
         Company_rwr = NA,
         ipoDate = NA) %>% 
  select(Company_prc, Company_rwr, Company_av, Company_ciq,
         Ticker, Company,
         Date, State, Location, Type, Vector,
         Organization, Description, ipoDate) %>% 
  stringdist_anti_join(match_prc_av, 
                       by = c(Company_prc = "Company_prc"), 
                       max_dist = 0,
                       method = "lv",
                       distance_col = "distance")

#### Match ransomware incidents with stock-symbol lists ####

#Match Ransomware Repsitory with AV data
match_rwr_av <- rwr %>%
  stringdist_inner_join(av, 
                        by = c(Company= "Company"), 
                        max_dist = 0,
                        method = "lv",
                        distance_col = "distance") %>% 
  mutate(Company_prc = NA,
         Company_rwr = Company.x,
         Company_av = Company.y,
         Company_ciq = NA,
         Company = Companyfull.x) %>% 
  select(Company_prc, Company_rwr, Company_av, Company_ciq,
         Ticker, Company,
         Date, State, Location, Type, Vector,
         Organization, Description, ipoDate)

#Check manually via MsExcel for non-matched companies and add to existing dataset
rwr_add <- read_csv("data_input/RansomwareExtra.csv") %>% 
  mutate(Ticker = gsub(" ", "", .$Ticker)) %>% 
  mutate(Company_rwr = gsub(" ", "", .$Company_rwr)) %>% 
  stringdist_inner_join(rwr, 
                        by = c(Company_rwr = "Company"), 
                        max_dist = 0,
                        method = "lv",
                        distance_col = "distance") %>%
  mutate(Company_prc = NA,
         Company_av = NA,
         Company_ciq = NA,
         ipoDate = NA,
         Company = Companyfull,
         Location = Location.x) %>% 
  select(Company_prc, Company_rwr, Company_av, Company_ciq,
         Ticker, Company,
         Date, State, Location, Type, Vector,
         Organization, Description, ipoDate)

#Match ransomware incidents and CIQ data that have not been matched with the AV data
match_rwr_ciq <- rwr %>%
  stringdist_inner_join(ciq, 
                        by = c(Company = "Company"), 
                        max_dist = 0,
                        method = "lv",
                        distance_col = "distance") %>%
  mutate(Company_rwr = Company.x, 
         Company_ciq = Company.y,
         Company = Companyfull.y,
         Company_av = NA,
         Company_prc = NA,
         ipoDate = NA) %>% 
  select(Company_prc, Company_rwr, Company_av, Company_ciq,
         Ticker, Company,
         Date, State, Location, Type, Vector, 
         Organization, Description, ipoDate) %>% 
  stringdist_anti_join(match_rwr_av, 
                       by = c(Company_rwr = "Company_rwr"), 
                       max_dist = 0,
                       method = "lv",
                       distance_col = "distance")

#### Connect subsets into one coherent dataframe ####

#Bind all datasets
Breaches <- rbind(match_prc_av, match_prc_ciq,
                  match_rwr_av, rwr_add, match_rwr_ciq) %>% 
  #Add variables that indicate origin
  mutate(AlphaVantage = ifelse(is.na(Company_av), FALSE, TRUE)) %>%
  mutate(CapitalIQ = ifelse(is.na(Company_ciq), FALSE, TRUE)) %>% 
  mutate(PRC = ifelse(is.na(Company_prc), FALSE, TRUE)) %>% 
  mutate(UTemple = ifelse(is.na(Company_rwr), FALSE, TRUE)) %>% 
  #Correct the date-format for all variables
  mutate(Date = as.Date(Date)) %>% 
  #Select relevant variables in convenient order
  select(Ticker, Company, ipoDate,
         Date, Location, Type, Vector,
         Organization, Description)

#### Prepare all variables for processing ####

#Create coherent categories for the "Location" variable (all 50 US states)
Breaches$Location <- str_replace(Breaches$Location, "UK (London)", "United Kingdom")
Breaches$Location <- str_replace(Breaches$Location, "AL", "Alabama")
Breaches$Location <- str_replace(Breaches$Location, "AK", "Alaska")
Breaches$Location <- str_replace(Breaches$Location, "AZ", "Arizona")
Breaches$Location <- str_replace(Breaches$Location, "AR", "Arkansas")
Breaches$Location <- str_replace(Breaches$Location, "CA", "California")
Breaches$Location <- str_replace(Breaches$Location, "CO", "Colorado")
Breaches$Location <- str_replace(Breaches$Location, "CT", "Connecticut")
Breaches$Location <- str_replace(Breaches$Location, "DE", "Delaware")
Breaches$Location <- str_replace(Breaches$Location, "FL", "Florida")
Breaches$Location <- str_replace(Breaches$Location, "GA", "Georgia")
Breaches$Location <- str_replace(Breaches$Location, "HI", "Hawaii")
Breaches$Location <- str_replace(Breaches$Location, "ID", "Idaho")
Breaches$Location <- str_replace(Breaches$Location, "IL", "Illinois")
Breaches$Location <- str_replace(Breaches$Location, "IN", "Indiana")
Breaches$Location <- str_replace(Breaches$Location, "IA", "Iowa")
Breaches$Location <- str_replace(Breaches$Location, "KS", "Kansas")
Breaches$Location <- str_replace(Breaches$Location, "KY", "Kentucky")
Breaches$Location <- str_replace(Breaches$Location, "LA", "Louisiana")
Breaches$Location <- str_replace(Breaches$Location, "ME", "Maine")
Breaches$Location <- str_replace(Breaches$Location, "MD", "Maryland")
Breaches$Location <- str_replace(Breaches$Location, "MA", "Massachusetts")
Breaches$Location <- str_replace(Breaches$Location, "MI", "Michigan")
Breaches$Location <- str_replace(Breaches$Location, "MN", "Minnesota")
Breaches$Location <- str_replace(Breaches$Location, "MS", "Mississippi")
Breaches$Location <- str_replace(Breaches$Location, "MO", "Missouri")
Breaches$Location <- str_replace(Breaches$Location, "MT", "Montana")
Breaches$Location <- str_replace(Breaches$Location, "NE", "Nebraska")
Breaches$Location <- str_replace(Breaches$Location, "NV", "Nevada")
Breaches$Location <- str_replace(Breaches$Location, "NH", "New Hampshire")
Breaches$Location <- str_replace(Breaches$Location, "NJ", "New Jersey")
Breaches$Location <- str_replace(Breaches$Location, "NM", "New Mexico")
Breaches$Location <- str_replace(Breaches$Location, "NY", "New York")
Breaches$Location <- str_replace(Breaches$Location, "NC", "North Carolina")
Breaches$Location <- str_replace(Breaches$Location, "ND", "North Dakota")
Breaches$Location <- str_replace(Breaches$Location, "OH", "Ohio")
Breaches$Location <- str_replace(Breaches$Location, "OK", "Oklahoma")
Breaches$Location <- str_replace(Breaches$Location, "OR", "Oregon")
Breaches$Location <- str_replace(Breaches$Location, "PA", "Pennsylvania")
Breaches$Location <- str_replace(Breaches$Location, "RI", "Rhode Island")
Breaches$Location <- str_replace(Breaches$Location, "SC", "South Carolina")
Breaches$Location <- str_replace(Breaches$Location, "SD", "South Dakota")
Breaches$Location <- str_replace(Breaches$Location, "TN", "Tennessee")
Breaches$Location <- str_replace(Breaches$Location, "TX", "Texas")
Breaches$Location <- str_replace(Breaches$Location, "UT", "Utah")
Breaches$Location <- str_replace(Breaches$Location, "VT", "Vermont")
Breaches$Location <- str_replace(Breaches$Location, "VA", "Virginia")
Breaches$Location <- str_replace(Breaches$Location, "WA", "Washington")
Breaches$Location <- str_replace(Breaches$Location, "WV", "West Virginia")
Breaches$Location <- str_replace(Breaches$Location, "WI", "Wisconsin")
Breaches$Location <- str_replace(Breaches$Location, "WY", "Wyoming")

#Correct the indication for unknown breach type
Breaches$Type <- str_replace(Breaches$Type, "#N/A", "UNKN")

#Create coherent categories for organization type (taken from PRC dataset)
Breaches$Organization <- str_replace(Breaches$Organization, "Chemical", "BSO")
Breaches$Organization <- str_replace(Breaches$Organization, "Commercial Facilities", "BSO")
Breaches$Organization <- str_replace(Breaches$Organization, "Communications", "BSO")
Breaches$Organization <- str_replace(Breaches$Organization, "Critical Manufacturing", "BSO")
Breaches$Organization <- str_replace(Breaches$Organization, "Emergency Services", "GOV")
Breaches$Organization <- str_replace(Breaches$Organization, "Financial Services", "BSF")
Breaches$Organization <- str_replace(Breaches$Organization, "Food and Agriculture", "BSO")
Breaches$Organization <- str_replace(Breaches$Organization, "Healthcare and Public Health", "MED")
Breaches$Organization <- str_replace(Breaches$Organization, "Information Technology", "BSO")
Breaches$Organization <- str_replace(Breaches$Organization, "Transportation Systems", "BSO")

#Correct wrong tickers manually by looking them up in Yahoo Finance
Breaches$Ticker <- str_replace(Breaches$Ticker, "TMUSR", "TMUS")
Breaches$Ticker <- str_replace(Breaches$Ticker, "GOOG", "GOOGL")
Breaches$Ticker <- str_replace(Breaches$Ticker, "GOOGLL", "GOOGL")
Breaches$Ticker <- str_replace(Breaches$Ticker, "CHSCL", "CHSCP")

#Pull company data from ssga.com (via tidyquant) to match industries
#(companies listed on NASDAQ, AMEX & NYSE)
NASDAQ <- tq_exchange("NASDAQ") %>% select(symbol, company, country, industry)
AMEX <- tq_exchange("AMEX") %>% select(symbol, company, country, industry)
NYSE <- tq_exchange("NYSE") %>% select(symbol, company, country, industry)
exchange_data <- rbind(NASDAQ, AMEX, NYSE)

#Match exchange data with dataset
Breaches <- Breaches %>%
  left_join(exchange_data, 
            by = c(Ticker = "symbol"))

#Change missing Industry values to NA
Breaches$industry <- na_if(Breaches$industry, "")

#Prepare variables for processing
Breaches <- Breaches %>%
  mutate(BreachDate = Date) %>% 
  mutate(Ransomware = ifelse(is.na(Vector), FALSE, TRUE)) %>%
  mutate(Country = country,
         Industry = industry)

#### Drop companies that cannot be processed ####

#Drop company that has incomplete data (CHANGE LATER IN WRANGLING SET)
Breaches <- Breaches %>% filter(Ticker != "CCZ" &
                                  Ticker != "MGLN") 

#Delete organizations that had an IPO before the estimation window
#(window of 90 days + 30 days before incident)
Breaches <- Breaches %>% 
  mutate(ipo_early = ifelse(ipoDate > (Date-120), TRUE, FALSE)) %>% 
  filter(ipo_early == FALSE)

#Add unique ID for each incident and select relevant variables
Breaches <- Breaches %>%
  arrange(BreachDate) %>% 
  mutate(ID = row_number()) %>%
  #Select all relevant variables in relevant order
  select(ID, Ticker, Company, ipoDate,
         BreachDate, Location, Country,
         Type, Ransomware,
         Organization, Industry, Description)

#### Download Time Series Data ####

#Pull daily adjusted stock prices for incidents since earliest data breach
stocks <- tq_get(Breaches$Ticker,
                 from = "2004-01-01", #1y before earliest breach
                 to = "2022-01-01",  #30d after latest breach
                 get = "stock.prices")

#Add daily return for adjusted prices
stocks <- stocks %>% 
  #Select adj. prices and rename
  select(symbol, date, adjusted) %>%
  rename(Price_adj = adjusted,
         Date = date,
         Symbol = symbol) %>% 
  group_by(Symbol) %>%
  #Only keep unique date per ticker (relevant when company with x breaches)
  distinct(Date, .keep_all = TRUE) %>%
  #Calculate daily returns
  tq_mutate(select = Price_adj, 
            mutate_fun = periodReturn,
            period = "daily",
            col_rename = "Return_adj")

#Merch stock data with full breach dataset
Breaches <- stocks %>% 
  group_by(Symbol) %>%
  left_join(Breaches, 
            by = c(Symbol = "Ticker"))

#### Order Time Series ####

#Fill non-trading days with NA
Breaches <- Breaches %>% 
    group_by(ID) %>% 
    complete(Date = seq.Date(min(Date), max(Date), by="day"),
        Symbol, Company, ipoDate, BreachDate, 
        Location, Country, 
        Type, Ransomware, Organization, Industry, Description) %>% 
    ungroup()

#Add variable with days from breach date
Breaches <- Breaches %>% 
  group_by(ID) %>% 
  mutate(ft = row_number()) %>% 
  mutate(event_ft = ifelse(BreachDate == Date, ft, NA)) %>% 
  fill(event_ft, .direction = "updown") %>% 
  mutate(EventTime = ft - event_ft) %>% 
  select(-event_ft, -ft) %>% 
  ungroup()

#Add dummy variable as indicator if before or after breach
Breaches <- Breaches %>% 
  mutate(PrePost = ifelse(EventTime < 0, 0, 1))

#Order variables 
Breaches <- Breaches %>% 
  select(ID, Symbol, Date, Price_adj, Return_adj, 
         Company, ipoDate, BreachDate, 
         EventTime, PrePost,
         Location, Country, 
         Type, Ransomware, Organization, Industry, Description)

#### Pull benchmark data ####

#Pull adjusted prices for S&P 500
sp500 <- tq_get("^GSPC",
                from = "2004-01-01", #year of earliest breach minus estimation window
                to = "2022-01-01", #year of latest breach
                get = "stock.prices") %>% 
  select(date, adjusted) %>%
  rename(Price_sp500 = adjusted,
         Date = date) %>%
  #Only keep one date per ticker
  distinct(Date, .keep_all = TRUE) %>%
  #Calculate daily returns
  tq_mutate(select = Price_sp500, 
            mutate_fun = periodReturn,
            period = "daily",
            col_rename = "Return_sp500")

#Merch stock data with full breach dataset
Breaches <- Breaches %>% 
  left_join(sp500, 
            by = c(Date = "Date"))

#Pull adjusted prices for NYSE
nyse <- tq_get("^NYA",
               from = "2004-01-01", #year of earliest breach minus estimation window
               to = "2022-01-01", #year of latest breach
               get = "stock.prices") %>% 
  select(date, adjusted) %>%
  rename(Price_nyse = adjusted,
         Date = date) %>%
  #Only keep one date per ticker
  distinct(Date, .keep_all = TRUE) %>%
  #Calculate daily returns
  tq_mutate(select = Price_nyse, 
            mutate_fun = periodReturn,
            period = "daily",
            col_rename = "Return_nyse")

#Merch stock data with full breach dataset
Breaches <- Breaches %>% 
  left_join(nyse, 
            by = c(Date = "Date"))

#Order variables
Breaches <- Breaches %>% 
  select(ID, Symbol, Company, Date,
         Price_adj, Return_adj, 
         Price_sp500, Return_sp500, Price_nyse, Return_nyse,
         EventTime, PrePost, BreachDate, ipoDate, 
         Location, Country, 
         Type, Ransomware, Organization, Industry, Description)

#Calculate Average R-squared value for all events to check S&P500 as benchmark fit 
Breaches %>% 
  drop_na(Return_adj) %>% 
  summarize(rsq = cor(Return_adj, Return_sp500)^2) %>% 
  ungroup() %>%
  summarize(rsq_mean = mean(rsq))

#Calculate Average R-squared value for all events to check NYSE as benchmark fit 
Breaches %>% 
  drop_na(Return_adj) %>% 
  summarize(rsq = cor(Return_adj, Return_nyse)^2) %>% 
  ungroup()
  
#write time series dataset to csv
Breaches %>% write_csv("data_output/Breaches.csv")
