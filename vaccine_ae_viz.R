# COVID-19 vaccine systematic adverse events heatmap viz

library(tidyverse)
library(scales)
library(ggthemes)

# get data from csv and reshape
systematic_ae_data <- read_csv("systemic_adverse_effects.csv", skip = 1, trim_ws = TRUE) %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  rename(study.full_name = 1) %>%
  pivot_longer(!study.full_name, names_to = "symptom.name",
               values_to = "symptom.percentage_of_patients") %>%
  mutate(
    # strip whitespace and newlines
    study.full_name = gsub("-\\s", "-", study.full_name),
    study.full_name = gsub("\\n|\\r|\\r\\n", " ", study.full_name),
    symptom.name = gsub("-\\s", "", symptom.name),
    symptom.percentage_of_patients = as.numeric(symptom.percentage_of_patients),
    
    # replace NAs with zero (if left in, NAs will show as grey on the heat map, which
    # shows greater contrast between low frequency symptoms and those not present at all)
    symptom.percentage_of_patients = replace_na(symptom.percentage_of_patients, 0),
    
    # extract info from study name
    study.total_patients = str_extract(study.full_name, "(?<=\\(n=)\\d*(?=\\))"),
    study.author_name = str_extract(study.full_name, "\\w*(?=,)"),
    vaccine.name = str_extract(study.full_name, "(?<=\\w{1,50},).*(?=\\()"),
    
    # add vaccine categories
    vaccine.type = case_when(str_detect(study.full_name, "ChAd") ~ "ChAd",
                             str_detect(study.full_name, "AdH|Ad26") ~ "hAd",
                             str_detect(study.full_name, "BNT|mRNA") ~ "RNA",
                             str_detect(study.full_name, regex("BB|CoronaVac|inactivated SARS-COV-2",
                                                               ignore_case = T)) ~ "Inactivated",
                             str_detect(study.full_name, "NVX|INO|SCB") ~ "SARS-COV-2 Other",
                             str_detect(study.full_name, "MERS|SARS\\s") ~ "MERS/SARS"))

# check data looks ok            
View(systematic_ae_data)

# create ggplot theme
my_heatmap_theme <- theme_tufte(base_size = 15, base_family = "Georgia") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title=element_blank(),
        plot.title.position = "plot",
        plot.subtitle = element_text(margin=margin(0,0,30,0)),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, margin=margin(30,0,0,0))) 

# draw the heatmap
ggplot(systematic_ae_data, aes(y = symptom.name, x = vaccine.name, fill = symptom.percentage_of_patients)) +
  geom_tile() +
  facet_grid(~vaccine.type, scales = "free") +
  scale_fill_distiller("Percentage of patients", palette = "RdPu", na.value = "grey90",
                       direction = 1, labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Systematic adverse events in coronavirus vaccine studies",
    subtitle = "Interstudy analysis of individual systematic adverse events reported in vaccine group (excluding control group) of coronavirus vaccine studies.",
    caption = bquote(bold("Source:") ~ "McDonald et al. (2021). Comparative systematic review and meta-analysis of reactogenicity, immunogenicity and efficacy of vaccines against SARS-CoV-2.")) +
  my_heatmap_theme

# Additional notes

# Systematic adverse events in coronavirus vaccine studies
# Interstudy analysis of individual systematic adverse events reported in vaccine group
# (excluding control group) of coronavirus vaccine studies. Data represent % of patients
# in experimental group reporting any grade of symptomatic adverse event. 

# Source: McDonald, I., Murray, S.M., Reynolds, C.J. et al. Comparative systematic
# review and meta-analysis of reactogenicity, immunogenicity and efficacy of vaccines
# against SARS-CoV-2. npj Vaccines 6, 74 (2021). https://doi.org/10.1038/s41541-021-00336-1