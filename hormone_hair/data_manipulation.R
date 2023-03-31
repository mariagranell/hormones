# ---------------
# Title: Data manipulation hait samples 2022
# Date: 31 mar 2023
# Author: mgranellruiz
# Goal: To merge all the different data types together
# ---------------

# path ------------------------
setwd("/Users/mariagranell/Repositories/hormones/hormone_hair")

# library ---------------------
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
source("/Users/mariagranell/Repositories/male_services_index/functions.R")

# load data -------------
indv <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/data/physiology_darting_2022_darted_individuals_corrected.csv")
extraction <- read.csv("/Users/mariagranell/Repositories/hormones/hormone_hair/data/Hair_extraction_2023.csv")
hormones <- read.csv2("/Users/mariagranell/Repositories/hormones/hormone_hair/data/hairdarting22samples_230316_simplify.csv")

# standarize indv ---------------
indv2 <- indv %>%
  mutate( AnimalID = gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", code_name, perl = TRUE),
          Group = toupper(group),
          Sex = toupper(sex),
          Group = case_when(Group == "IFAM" ~ "IF", TRUE ~ Group),
          Date_darting = as.character(dmy(date)),
          Mass = mass.kg.,
          Forearm = forearm_lenght.cm.,
          Canines = canine_lenght_mean,
          Testicles = testicle_size,
          DartingID = id_number,
          Age = age

  ) %>%
  select(DartingID, AnimalID, name_individual, Group,Sex,Age, Date_darting, Mass, Forearm, Canines, Testicles)

# standarize extraction ---------------
extraction2 <- extraction %>%
  mutate(
    DartingID = gsub("c","",Sample_ID),
    NPAC_ID = as.character(NPAC_ID),
    Date_extraction = dmy(Date_extraction),
    Weight_extraction = Weight,
    Comments_extraction = Comments
  ) %>%
  select(DartingID, NPAC_ID,Date_extraction,Weight_extraction,Replicates,Comments_extraction)

# standarize hormones ---------------
hormones2 <- hormones %>%
  rename(NPAC_ID = SampleID) %>%
  filter(NPAC_ID != 54) %>% # removing the control
  pivot_longer(cols = c(Cortisone, Cortisol, Corticosterone, DHEA.Q, Androstenedione.Q, Testosterone, Progesterone),
               values_to = "concentration",
               names_to = "hormones") %>%
  mutate(hormone_family = ifelse(hormones %in% c("Cortisone", "Cortisol", "Corticosterone"), "glucocorticoids", "steroids"))


# merge -----------------------------

data <- hormones2 %>%
  left_join(., extraction2, by= "NPAC_ID") %>%
  mutate(Replicates = ifelse(is.na(Replicates), "Standard",Replicates))
data <- data%>%
  left_join(., indv2, by = "DartingID") %>%
  group_by(hormones)%>%
  mutate(concen_centered = ifelse(Replicates == "Standard", concentration, concentration - mean(concentration[Replicates != "Standard"])))

rm(indv,extraction,hormones)
rm(indv2,extraction2,hormones2)
# write csv -------------------------

write.csv(data, "/Users/mariagranell/Repositories/hormones/hormone_hair/data/merged_hormone_date_2022.csv", row.names = F)


# data organization --------------
data$hormone_family <- as.factor(data$hormone_family)
data$concentration <- as.numeric(data$concentration)

# TODO explore the hormones ---------

# TOdo explore how many grmas of hair you put. If the tube broke, if there was only one extraction round!
{hormones <- c("Cortisone", "Cortisol", "DHEA.Q", "Androstenedione.Q", "Testosterone", "Progesterone")
color_map <- c("yes" = "red", "steroids" = "blue")

plots <- lapply(hormones, function(hormone) {
  data %>%
    filter(Replicates != "Standard" & hormones == hormone) %>%
    ggplot(aes(x = as.numeric(NPAC_ID), y = concentration, fill = Replicates)) +
    geom_col() +
    scale_fill_manual(values = color_map) +
    coord_flip() +
    xlab("") +
    ylab(hormone) +
    theme(legend.position = "none")
})

gridExtra::grid.arrange(grobs = plots, ncol = 2)}



# Explore data!!! -----------------

# TODO male an average of the replicates!!!!!

data %>%
  filter(Replicates != "Standard" & hormones != "Corticosterone") %>%
  ggplot(.,aes(x = AnimalID, y = concentration, fill = hormones)) +
  geom_bar(stat="identity") +
  coord_flip() +
  ylab("Hormone concentration") +
  ggtitle("Hormone Concentration by Animal ID") +
  labs(fill = "Hormone") +
  theme_bw()

data %>%
  filter(Replicates != "Standard" & hormones != "Corticosterone") %>%
  group_by(AnimalID, hormones) %>%
  summarize(concen_centered = mean(concen_centered), .groups = "drop") %>%
  ggplot(aes(x = reorder(AnimalID, -concen_centered), y = concen_centered, fill = hormones)) +
  geom_col() +
  coord_flip() +
  ylab("Hormone concentration centered") +
  ggtitle("Centered Hormone Concentration by Animal ID") +
  labs(fill = "Hormone") +
  theme_bw()

{hormones <- c("Cortisone", "Cortisol", "DHEA.Q", "Androstenedione.Q", "Testosterone", "Progesterone")
color_map <- c("glucocorticoids" = "red", "steroids" = "blue")

plots <- lapply(hormones, function(hormone) {
  data %>%
    filter(Replicates != "Standard" & hormones == hormone) %>%
    arrange(desc(concentration)) %>%
    ggplot(aes(x = AnimalID, y = concentration, fill = hormone_family)) +
    geom_col() +
    scale_fill_manual(values = color_map) +
    coord_flip() +
    xlab("") +
    ylab(hormone) +
    theme(legend.position = "none")
})

gridExtra::grid.arrange(grobs = plots, ncol = 2)}


hormones <- c("Cortisone", "Cortisol", "DHEA.Q", "Androstenedione.Q", "Testosterone", "Progesterone")
color_map <- c("glucocorticoids" = "red", "steroids" = "blue")

plots <- lapply(hormones, function(hormone) {
  ggbarplot(data = data %>%
              filter(Replicates != "Standard" & hormones == hormone),
            aes(x = AnimalID, y = concentration, fill = hormone_family),
            stat = "identity",
            position = position_stack(reverse = TRUE),
            ylab = hormone) +
    scale_fill_manual(values = color_map) +
    xlab("") +
    theme_bw()
})

gridExtra::grid.arrange(grobs = plots, ncol = 2)



data %>%
  filter(Replicates != "Standard" & hormones != "Progesterone" & Sex == "M" & hormones != "Corticosterone") %>%
  group_by(AnimalID, hormone_family) %>%
  summarize(concen = mean(concen_centered), .groups = "drop") %>%
  ggplot(aes(x = reorder(AnimalID, -concen_centered), y = concen_centered, fill = hormone_family)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  theme_bw()

data %>%
  filter(Replicates != "Standard"  & Sex == "F" & hormones != "Corticosterone") %>%
  group_by(AnimalID, hormone_family) %>%
  summarize(concen_centered = mean(concen_centered), .groups = "drop") %>%
  ggplot(aes(x = reorder(AnimalID, -concen_centered), y = concen_centered, fill = hormone_family)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  theme_bw()
