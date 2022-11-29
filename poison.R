#### Load in packages ####

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(FactoMineR)
library(factoextra)
#library(FactoInvestigate) #I can see why you didn't tell us about this package

raw_poison = read_xlsx("raw_data/poison.xlsx", sheet = "Data")

#### Look at link ####

#' Look at food compared to sickness

food_graph_data <- 
raw_poison %>% 
  group_by(Sick) %>% # Split groups sick and not sick
  summarise(across(Potato:Icecream, \(x) sum(grepl("y$", x)))) %>% #for foods, get number of yeses
  pivot_longer(!Sick, names_to = "food", values_to = "num") #stack data by food and n_sick
  
ggplot(food_graph_data, aes(x = food,  y = num, fill = Sick)) +
  geom_col(position = "fill") +
  labs(y = "Proportion sick/not sick",
       x = "Food")

# No obvious problems

#### MCA ####

quali_sup_vars <- which(names(raw_poison) %in% c("Sex", "Sick", "Nausea", "Vomiting", "Abdominals", "Fever", "Diarrhae"))
quanti_sup_vars <- which(names(raw_poison) %in% c("ID", "Age", "Time"))
#excl_vars <- which(names(raw_poison) %in% "ID")

poison_mca <- MCA(raw_poison, quali.sup = quali_sup_vars, quanti.sup = quanti_sup_vars)

quali_sup_vars <- which(names(raw_poison) %in% c("Sex", "Sick"))
poison_mca <- MCA(raw_poison, quali.sup = quali_sup_vars, quanti.sup = quanti_sup_vars)

#Investigate(poison_mca, keepRmd = TRUE)
fviz_screeplot(poison_mca)
# Only worth keeping one dimension

#fviz_mca_biplot(poison_mca,repel = TRUE)

dimdesc(poison_mca)
#### Hierarchical clustering ####

poison_HCPC <- HCPC(poison_mca, method = "ward", nb.clust = -1)
fviz_dend(poison_HCPC)
poison_HCPC$desc.var
poison_HCPC$desc.var$category$`3`
plot(poison_HCPC)

# https://www.r-bloggers.com/2020/05/binary-logistic-regression-with-r/

#### Binomial regression ####
library(caret)

quali_sup_vars <- which(names(raw_poison) %in% c("Sex", "Nausea", "Vomiting", "Abdominals", "Fever", "Diarrhae"))
quanti_sup_vars <- which(names(raw_poison) %in% c("ID", "Age", "Time"))


chr_cols <- sapply(raw_poison, is.character)
glm_poison_data <- as.data.frame(lapply(raw_poison, \(x) if(is.character(x)) as.factor(x) else x))
glm_poison_data$Sick <- as.integer(glm_poison_data$Sick) - 1

poison_glm <- glm(Sick ~ ., family = binomial(), data=glm_poison_data[, -c(quali_sup_vars, quanti_sup_vars)])
summary(poison_glm)
