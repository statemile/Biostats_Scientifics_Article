
# set working directory
setwd("D:/Exp_Pro/Biostats_Scientifics_Article/Leishmaniasis_case_control_study")

# Packages loading
library(dplyr)
library(tidyverse)
library(readxl)
library(openxlsx)
library(kableExtra)
library(knitr)
library(gtsummary)
library(writexl)
library(gt)
library(lubridate)


# 2. Patient flow diagram


# data loading and preliminary checking
dataNgere2016 <- read_excel("data_Ngere2016.xlsx", 
                            sheet = "Case_Control")
glimpse(dataNgere2016)
# data contains 174 samples and 180 columns

# Features processing
## sex
dataNgere2016$Sex <- as.factor(dataNgere2016$Sex)

## age group knowing study period is 20th January - 3rd February 2016

dataNgere2016$Dateofbirth <- as.Date(dataNgere2016$Dateofbirth,
                                     formart="%Y-%m-%d")


dataNgere2016$End_date <- as.Date(rep("2016-02-03", 
                                      times=dim(dataNgere2016)[1]),
                                  formart="%Y-%m-%d")

dataNgere2016$age <- ifelse(test = format(dataNgere2016$End_date, "%m-%d") < 
                                    format(dataNgere2016$Dateofbirth, "%m-%d"), 
                                 # Subtract 1 if birthday hasn't happened yet this year
                                 as.numeric(format(dataNgere2016$End_date, "%Y")) -
                                 as.numeric(format(dataNgere2016$Dateofbirth, "%Y")),
                                 as.numeric(format(dataNgere2016$End_date, "%Y")) -
                                 as.numeric(format(dataNgere2016$Dateofbirth, "%Y")) -1)


dataNgere2016$age2 <- interval(dataNgere2016$Dateofbirth, dataNgere2016$End_date) |> time_length("years") 


                                                                                               
                                                                                            
dataNgere2016$class_age[dataNgere2016$age<5] <- "Under 5 years"
dataNgere2016$class_age[dataNgere2016$age>=5 & dataNgere2016$age<=15 ] <- "5-15"
dataNgere2016$class_age[dataNgere2016$age>=16 & dataNgere2016$age<=34 ] <- "16-34"
dataNgere2016$class_age[dataNgere2016$age>=35 & dataNgere2016$age<=59 ] <- "35-59"
dataNgere2016$class_age[dataNgere2016$age>=60] <- "60+"
dataNgere2016$End_date<-NULL # delete a temporary created feature

dataNgere2016$class_age <- as.factor(dataNgere2016$class_age)
                                  

# level of education
dataNgere2016$Educationlevel_groupe[dataNgere2016$Educationlevel %in% c("Complete Primary", 
                                                                        "Continuing Primary",
                                                                        "None",
                                                                        "Some Primary")] <- "Primary level and below"


dataNgere2016$Educationlevel_groupe[dataNgere2016$Educationlevel %in% c("Complete Secondary", 
                                                                        "Continuing Secondary",
                                                                        "Tertiary")] <- "Secondary level or more"



dataNgere2016$Educationlevel_groupe <- as.factor(dataNgere2016$Educationlevel_groupe)

# Occupation
dataNgere2016$Occupation_groupe[dataNgere2016$Occupation=="Student/School"] <- "In school"


dataNgere2016$Occupation_groupe[dataNgere2016$Occupation %in% c("Charcoal Burning", 
                                                                "Herdsman",
                                                                "Hunting",
                                                                "Mining/Stone Mason",
                                                                "Other")] <- "Involving forest visit"

dataNgere2016$Occupation_groupe[dataNgere2016$Occupation %in% c("Crop Farming", 
                                                                "Livestock Farming",
                                                                "Mixed Farming")] <- "Farming"

dataNgere2016$Occupation_groupe[dataNgere2016$Occupation %in% c("Business", 
                                                                "Housewife")] <- "Other occupation"


dataNgere2016$Occupation_groupe <- as.factor(dataNgere2016$Occupation_groupe)

dataNgere2016$CaseStatus <- as.factor(dataNgere2016$CaseStatus)

# table 2
table_2 <- dataNgere2016 %>%
  select(Sex,class_age, Educationlevel_groupe, Occupation_groupe,CaseStatus) %>%
  tbl_summary(by = CaseStatus,
              label = list(Sex ~ "Sex",
                           class_age ~ "Age Group",
                           Educationlevel_groupe ~ "Level of education",
                           Occupation_groupe ~ "Occupation"),
              sort = all_categorical() ~ "frequency",
              digits = list(all_categorical() ~ c(0, 1)))%>%
  add_p() %>% 
  modify_header(label="**Variables**") %>% 
  modify_caption("**Demographic characteristics of cases and controls in Gilgil, Kenya 2016**") %>% 
  bold_labels() %>%
  italicize_labels()

gtsave(as_gt(table_2),file="table2.docx")
table_2

# Modeling of risk factors 
## factors related to the individual = f1
f1 <- c("Sex", # Participant is male
        "Educationlevel_groupe", # Education level is primary level and below
        "spendtimeoutsidehouse", # Spending time outside home after sunset
        "Aretheremosquitonets",  # Individual using bed net
        "Occupation_groupe", # Occupation involves forest visit
        "travelaway" # History of travel
        )
  




  
## factors related to indoor dwelling environment = f2

f2 <- c("RoofCeilingtype",  # Roofing made of other materials
        "Natureofthewalls",
        "NatureofFloor",
        "Totalnumberofpersons",
        "HistoryofContact", "Householdmember"
        )


## factors related to outdoor environment = f3

f3 <- c("RockHyraxes", # Sighting rock hyrax near residence
        "WildJackals", # Sighting wild jackal near residence
        "Porcupine",   # Sighting porcupine near residence
        "Mongoose",    # Sighting mongoose near residence
        "DomesticDog", # Domestic dogs in the residence
        "NearbyforestThicket",  # Presence of a nearby forest or thicket
        "Nearbyriveropenwatersource", # Presence of a nearby open water source
        "Immediateneighborwithin150meters", # Immediate neighbor has typical skin lesions
        "DistantneigbourOutside150m", # Distant neighbor has typical skin lesions
        "GarbagepitOpenwastepit" # Presence of garbage mound near residence
)

# Protective factor = f4
f4 <- c("CropfarmCultivatedarea" # Presence of cultivated crop farm near residence
        )
