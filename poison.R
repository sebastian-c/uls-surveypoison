#### Load in packages ####

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(FactoMineR)
library(factoextra)

raw_poison = read_xlsx("raw_data/poison.xlsx", sheet = "Data")

#### Look at link

#' Look at food compared to sickness

food_graph_data <- 
raw_poison %>% 
  group_by(Sick) %>%
  summarise(across(Fish:Icecream, \(x) sum(grepl("y$", x)))) %>% #for foods, get number of cases
  pivot_longer(!Sick, names_to = "food", values_to = "n_sick") #stack data by food and n_sick
  

ggplot(food_graph_data, aes(x = food, y = n_sick, fill = Sick)) +
  geom_col(position = "dodge")

# No obvious problems

#### MCA ####

quali_sup_vars <- c("Sex", "Sick", "Nausea", "Vomiting", "Abdominals", "Fever", "Diarrhae")
quanti_sup_vars <- c("ID", "Age", "Time")

poison_mca <- MCA(raw_poison, quali.sup = quali_sup_vars, quanti.sup = quanti_sup_vars)

fviz_screeplot(poison_mca)

dimdesc(poison_mca)
#### Hierarchical clustering ####


poison_cluster <- HCPC(poison_mca, method = "ward", nb.clust = 3)
