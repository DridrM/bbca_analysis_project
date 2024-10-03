library(ggplot2)
library(FactoMineR)
library(Factoshiny)
library(dplyr)
library(tidyr)
library(corrplot)

## Définition du dossier de travail----
# setwd("D:/M2 GEI/02_Alternance/Projets/30-08-2022 Analyse de données BBCA/Analyse")


## Ouverture du fichier----
dtf_bbca <- read.csv("data/bbca_data.csv", sep = ";", header = T)


## Suppression des lignes en double----
dtf_bbca_index <- dtf_bbca %>% distinct(Index, .keep_all = TRUE) ## Ne garder que les lignes uniques

#row.names(dtf_bbca_index) <- dtf_bbca_index$Index ## Set le nom des lignes comme ?gal ? la colonne Index

#dtf_bbca_index <- dtf_bbca_index %>% select(-Index) ## Supprimer la colonne Index qui ne sert plus ? rien


## Filtrer les variables qui nous int?ressent----
variables_analyse_1 <- c("Index", "Consommation", "Co?t.des.travaux", "Surface", "Travaux", 
                         "Type.b?timent", "Usage.principal", "Ville", "Zone.climatique", 
                         "X_graph.pie.contrib_Chantier", "X_graph.pie.contrib_Eau", "X_graph.pie.contrib_Energie",
                         "X_graph.pie.contrib_Produits.de.Constructions.et.Equipements") ## Nom des variables qui nous int?ressent dans un vecteur pour l'analyse 1

variables_analyse_lots_pce <- c("X_graph.pie.decomp.composant_Appareils.?l?vateurs", 
                             "X_graph.pie.decomp.composant_Chauffage..ventilation..ECS..froid", 
                             "X_graph.pie.decomp.composant_Cloisonnement..Doublage..Menuiseries.int?rieures", 
                             "X_graph.pie.decomp.composant_Couverture.Charpente..?tanch?it?", 
                             "X_graph.pie.decomp.composant_Equipements.de.production.d.?nergie", 
                             "X_graph.pie.decomp.composant_Fa?ades.et.menuiseries.ext?rieures", 
                             "X_graph.pie.decomp.composant_Fluides.frigorig?nes", 
                             "X_graph.pie.decomp.composant_Fondations.et.infrastructures", 
                             "X_graph.pie.decomp.composant_Ma?onnerie..Superstructure", 
                             "X_graph.pie.decomp.composant_Rev?tements.sol.mur.et.plafonds..chapes..d?coration", 
                             "X_graph.pie.decomp.composant_R?seaux.de.communication", 
                             "X_graph.pie.decomp.composant_R?seaux.d.?nergie", 
                             "X_graph.pie.decomp.composant_Sanitaire", 
                             "X_graph.pie.decomp.composant_VRD") ## Vecteur des variables relatives aux lots PCE

variables_analyse_usages_energie <- c("X_graph.pie.eges_obs_Chauffage", 
                                      "X_graph.pie.eges_obs_ECS", 
                                      "X_graph.pie.eges_obs_Eclairage", 
                                      "X_graph.pie.eges_obs_Refroidissement", 
                                      "X_graph.pie.eges_obs_VMC...Auxiliaires") ## Variables relatives aux 5 usages r?glementaires RT 2012 (non utilis? pour le moment)

variables_analyse_2 <- c(variables_analyse_1, variables_analyse_lots_pce)

#variables_analyse_3 <- c("Index", "Co?t.des.travaux", "Surface", "Travaux", 
#                         "Type.b?timent", "Usage.principal") ## Non utilis? pour le moment

variables_analyse_4 <- c(variables_analyse_1, variables_analyse_usages_energie) ## Variables telles que d?crites dans l'analyse 4

dtf_bbca_fltrd_1 <- dtf_bbca_index %>% select(variables_analyse_1) ## Selection des variables qui nous int?ressent selon la description de l'analyse 1

dtf_bbca_fltrd_2 <- dtf_bbca_index %>% select(variables_analyse_2) ## Selection des variables qui nous int?ressent selon la description de l'analyse 2

#dtf_bbca_fltrd_3 <- dtf_bbca_index %>% select(variables_analyse_3) ## Non utilis? pour le moment

dtf_bbca_fltrd_4 <- dtf_bbca_index %>% select(variables_analyse_4)


## Renommer les variables----
dtf_bbca_fltrd_1 <- dtf_bbca_fltrd_1 %>% rename(Co?t.travaux = Co?t.des.travaux, 
                          Contrib.chantier = X_graph.pie.contrib_Chantier, 
                          Contrib.eau = X_graph.pie.contrib_Eau, 
                          Contrib.energie = X_graph.pie.contrib_Energie, 
                          Contrib.pce = X_graph.pie.contrib_Produits.de.Constructions.et.Equipements)

dtf_bbca_fltrd_2 <- dtf_bbca_fltrd_2 %>% rename(Co?t.travaux = Co?t.des.travaux, 
                          Contrib.chantier = X_graph.pie.contrib_Chantier, 
                          Contrib.eau = X_graph.pie.contrib_Eau, 
                          Contrib.energie = X_graph.pie.contrib_Energie, 
                          Contrib.pce = X_graph.pie.contrib_Produits.de.Constructions.et.Equipements, 
                          pce.elev = X_graph.pie.decomp.composant_Appareils.?l?vateurs, 
                          pce.cvc = X_graph.pie.decomp.composant_Chauffage..ventilation..ECS..froid, 
                          pce.cloi.int = X_graph.pie.decomp.composant_Cloisonnement..Doublage..Menuiseries.int?rieures, 
                          pce.couv.charp = X_graph.pie.decomp.composant_Couverture.Charpente..?tanch?it?,
                          pce.prod.ener = X_graph.pie.decomp.composant_Equipements.de.production.d.?nergie, 
                          pce.facade = X_graph.pie.decomp.composant_Fa?ades.et.menuiseries.ext?rieures, 
                          pce.fl.frigo = X_graph.pie.decomp.composant_Fluides.frigorig?nes, 
                          pce.fond = X_graph.pie.decomp.composant_Fondations.et.infrastructures, 
                          pce.str = X_graph.pie.decomp.composant_Ma?onnerie..Superstructure, 
                          pce.sols = X_graph.pie.decomp.composant_Rev?tements.sol.mur.et.plafonds..chapes..d?coration, 
                          pce.cfaible = X_graph.pie.decomp.composant_R?seaux.de.communication, 
                          pce.cfort = X_graph.pie.decomp.composant_R?seaux.d.?nergie, 
                          pce.sani = X_graph.pie.decomp.composant_Sanitaire, 
                          pce.vrd = X_graph.pie.decomp.composant_VRD)

#dtf_bbca_fltrd_3 <- dtf_bbca_fltrd_3 %>% rename(Co?t.travaux = Co?t.des.travaux) ## Non utilis? pour le moment

dtf_bbca_fltrd_4 <- dtf_bbca_fltrd_4 %>% rename(Co?t.travaux = Co?t.des.travaux, 
                                                Contrib.chantier = X_graph.pie.contrib_Chantier, 
                                                Contrib.eau = X_graph.pie.contrib_Eau, 
                                                Contrib.energie = X_graph.pie.contrib_Energie, 
                                                Contrib.pce = X_graph.pie.contrib_Produits.de.Constructions.et.Equipements,
                                                Chauffage = X_graph.pie.eges_obs_Chauffage, 
                                                ECS = X_graph.pie.eges_obs_ECS,
                                                Eclairage = X_graph.pie.eges_obs_Eclairage, 
                                                Clim = X_graph.pie.eges_obs_Refroidissement, 
                                                Auxilliaires = X_graph.pie.eges_obs_VMC...Auxiliaires)


## Cr?er un nouveau df pour connaitre plus en d?tail les usages des b?timents tertiaires----
dtf_bbca_fltrd_ter_use <- dtf_bbca_fltrd_1 %>% filter(Type.b?timent %in% c("Tertiaire - Public", "Tertiaire - Priv?")) ## Se cantonner aux b?timents tertiaires avec un filtre sur les lignes

dtf_bbca_fltrd_ter_pub_use <- dtf_bbca_fltrd_1 %>% filter(Type.b?timent == "Tertiaire - Public") ## Se cantonner aux b?timents tertiaires publics avec un filtre sur les lignes

dtf_bbca_fltrd_ter_priv_use <- dtf_bbca_fltrd_1 %>% filter(Type.b?timent == "Tertiaire - Priv?") ## Se cantonner aux b?timents tertiaires priv?s avec un filtre sur les lignes


## Filtrer les lignes qui nous int?ressent----
dtf_bbca_fltrd_line_1 <- dtf_bbca_fltrd_1[(dtf_bbca_fltrd_1$Travaux == "Neuf - RT 2012 - E+C-"), ] ## Filtrer les lignes selon le libell? "Neuf - RT 2012 - E+C-" de la colonne Travaux

dtf_bbca_fltrd_line_1 <- dtf_bbca_fltrd_line_1[complete.cases(dtf_bbca_fltrd_line_1), ] ## Eliminer les lignes qui contiennent des NA

dtf_bbca_fltrd_line_1 <- dtf_bbca_fltrd_line_1 %>% filter(!Type.b?timent %in% c("Tertiaire", "Maisons group?es - Priv?", 
                                                                            "Maison en secteur diffus - Priv?", "Maisons group?es - Public")) ## Eliminer les lignes qui ne contiennent qu'un individu d'une cat?gorie de la variable "Type.b?timent"

dtf_bbca_fltrd_line_2 <- dtf_bbca_fltrd_2[(dtf_bbca_fltrd_2$Travaux == "Neuf - RT 2012 - E+C-"), ] ## Filtrer les lignes selon le libell? "Neuf - RT 2012 - E+C-" de la colonne Travaux

dtf_bbca_fltrd_line_2 <- dtf_bbca_fltrd_line_2[complete.cases(dtf_bbca_fltrd_line_2), ] ## Eliminer les lignes qui contiennent des NA

dtf_bbca_fltrd_line_2 <- dtf_bbca_fltrd_line_2 %>% filter(!Type.b?timent %in% c("Tertiaire", "Maisons group?es - Priv?", 
                                                                            "Maison en secteur diffus - Priv?", "Maisons group?es - Public")) ## Eliminer les lignes qui ne contiennent qu'un individu d'une cat?gorie de la variable "Type.b?timent"

dtf_bbca_fltrd_line_2 <- dtf_bbca_fltrd_line_2 %>% rowwise() %>% 
  mutate(check_sum = sum(c_across(cols = c(12:25)))) ## Cr?er une colonne somme de contr?le contributeur PCE, pour v?rifier les contribs pce corrects

dtf_bbca_fltrd_line_2a <- dtf_bbca_fltrd_line_2 %>% filter(!Index %in% c(6633, 10581)) %>% select(-check_sum) ## Jetter les lignes d'index n?6633 et 10581

#dtf_bbca_fltrd_line_3 <- dtf_bbca_fltrd_3[(dtf_bbca_fltrd_3$Travaux == "Neuf - RT 2012 - E+C-"), ] ## Filter les lignes selon le libell? "Neuf - RT 2012 - E+C-" de la colonne Travaux (non utilis? pour le moment)

#dtf_bbca_fltrd_line_3 <- dtf_bbca_fltrd_line_3[complete.cases(dtf_bbca_fltrd_line_3), ] ## Eliminer les lignes qui contiennent des NA (non utilis? pour le moment)

#dtf_bbca_fltrd_line_3 <- dtf_bbca_fltrd_line_3 %>% filter(!Type.b?timent %in% c("Tertiaire", "Maisons group?es - Priv?", 
#                                                                               "Maison en secteur diffus - Priv?", "Maisons group?es - Public")) ## Eliminer les lignes qui ne contiennent qu'un individu d'une cat?gorie de la variable "Type.b?timent" (non utilis? pour le moment)

#dtf_bbca_fltrd_line_3 <- dtf_bbca_fltrd_line_3 %>% mutate(cout.m2 = Co?t.travaux/Surface) ## Rajout de la colonne co?t au m? (non utilis? pour le moment)

dtf_bbca_fltrd_line_4 <- dtf_bbca_fltrd_4[(dtf_bbca_fltrd_4$Travaux == "Neuf - RT 2012 - E+C-"), ] ## Filtrer les lignes selon le libell? "Neuf - RT 2012 - E+C-" de la colonne Travaux

dtf_bbca_fltrd_line_4 <- dtf_bbca_fltrd_line_4[complete.cases(dtf_bbca_fltrd_line_4), ] ## Eliminer les lignes qui contiennent des NA

dtf_bbca_fltrd_line_4 <- dtf_bbca_fltrd_line_4 %>% filter(!Type.b?timent %in% c("Tertiaire", "Maisons group?es - Priv?", 
                                                                                "Maison en secteur diffus - Priv?", "Maisons group?es - Public")) ## Eliminer les lignes qui ne contiennent qu'un individu d'une cat?gorie de la variable "Type.b?timent"


## Cr?er un df "tidy" pour l'analyse statistique descriptive----
dtf_bbca_fltrd_tidy_1 <- gather(dtf_bbca_fltrd_line_1, "Impact.type", "Impact.c", 8:11) ## Cr?ation d'un df tidy

dtf_bbca_fltrd_tidy_mean_std_1 <- dtf_bbca_fltrd_tidy_1 %>% group_by(Impact.type, Type.b?timent) %>% 
  summarise(mean.impact = mean(Impact.c), std.impact = sd(Impact.c)) ## Cr?ation d'un df tidy des moyennes et ecart-types des impact carbone par type d'impact et type de b?timent

dtf_bbca_fltrd_tidy_2 <- dtf_bbca_fltrd_line_2a %>% select(-c("Contrib.chantier", "Contrib.eau", "Contrib.energie", "Contrib.pce", "Usage.principal")) %>% 
  gather("Impact.type", "Impact.c", 8:21) ## Cr?ation d'un df tidy

dtf_bbca_fltrd_tidy_mean_std_2 <- dtf_bbca_fltrd_tidy_2 %>% group_by(Impact.type, Type.b?timent) %>% 
  summarise(mean.impact = mean(Impact.c), std.impact = sd(Impact.c)) ## Cr?ation d'un df tidy des moyennes et ecart-types des impact carbone par type d'impact et type de b?timent

dtf_bbca_fltrd_tidy_mean_std_2a <- dtf_bbca_fltrd_tidy_mean_std_2 %>% group_by(Type.b?timent) %>%
  mutate(ratio.mean.impact = mean.impact/sum(mean.impact)) ## Version du df pr?c?dent avec la moyenne des impacts par lot exprim? en ratio, par type de b?timent

dtf_bbca_fltrd_tidy_mean_std_2 <- dtf_bbca_fltrd_tidy_mean_std_2[order(dtf_bbca_fltrd_tidy_mean_std_2$Type.b?timent,
                                                                       dtf_bbca_fltrd_tidy_mean_std_2$mean.impact), ]

#dtf_bbca_fltrd_mean_std_3 <- dtf_bbca_fltrd_line_3 %>% select(c("Type.b?timent", "cout.m2")) %>% group_by(Type.b?timent) %>% 
#  summarise(mean.cout.m2 = mean(cout.m2), std.cout.m2 = sd(cout.m2)) ## Cr?ation d'un df des moyennes et des ?cart-types des ratios de prix au m? (non utilis? pour le moment)


## Cr?ation d'un vecteur de classification des lots les plus ?metteurs (Analyse 2)----
dtf_bbca_fltrd_tidy_mean_mean_std_2 <- dtf_bbca_fltrd_tidy_2 %>% group_by(Impact.type) %>% 
  summarise(mean.impact = mean(Impact.c), std.impact = sd(Impact.c)) ## Cr?ation d'un df tidy des moyennes et ecart-types des impact carbone par type d'impact seulement

dtf_bbca_fltrd_tidy_mean_mean_std_2 <- dtf_bbca_fltrd_tidy_mean_mean_std_2[order(dtf_bbca_fltrd_tidy_mean_mean_std_2$mean.impact), ] ## Classement dans l'ordre croissant des moyennes des impacts carbone

f_emission_lots <- factor(rep(dtf_bbca_fltrd_tidy_mean_mean_std_2$Impact.type, 3), 
                          levels = dtf_bbca_fltrd_tidy_mean_mean_std_2$Impact.type) ## Obtention du facteur des lots les plus ?metteurs dans l'ordre croissant
                                                                                    ## Ce facteur servira ? afficher dans l'ordre voulu l'impact des lots dans les graphiques empil?s
                                                                                    ## L'ordre de classement choisi est l'ordre croissant des impacts moyens par lot, sur l'ensemble du jeu de donn?es sans distinction du type de b?timent

## Statistiques descriptives----
ggplot(dtf_bbca_fltrd_line_1) + aes(x = Type.b?timent, y = Contrib.pce) + geom_boxplot() ## Essai pour un boxplot

ggplot(dtf_bbca_fltrd_line_1) + aes(x = Contrib.pce) + geom_histogram() ## Essai pour un histogramme

ggplot(dtf_bbca_fltrd_tidy_1[(dtf_bbca_fltrd_tidy_1$Impact.type == c("Contrib.pce", "Contrib.energie")), ]) + 
  aes(fill = Impact.type, x = Type.b?timent, y = Impact.c) + geom_boxplot() ## Boxplot par type de b?timent pour les contributeurs ?nergie et pce

ggplot(dtf_bbca_fltrd_tidy_mean_std_1) + aes(fill = Impact.type, x = Type.b?timent, y = mean.impact) + 
  geom_bar(position = "stack", stat = "identity") ## Analyse 1 : Barplot empil? de la somme des moyennes de l'impact carbone par contributeur et par type de b?timent

ggplot(dtf_bbca_fltrd_tidy_mean_std_1) + aes(fill = Impact.type, x = Type.b?timent, y = mean.impact) + 
  geom_bar(position = "fill", stat = "identity") ## Analyse 1 : Barplot empil? du pourcentage des moyennes de l'impact carbone par contributeur et par type de b?timent

ggplot(dtf_bbca_fltrd_tidy_mean_std_1) + aes(fill = Impact.type, x = Type.b?timent, y = mean.impact) + 
  geom_bar(position = "dodge", stat = "identity") + 
  geom_errorbar(aes(ymin = mean.impact - std.impact, ymax = mean.impact + std.impact), width = .2, position = position_dodge(.9)) ## Analyse 1 : Barplot non empil? des moyennes de l'impact carbone par contributeur et par type de b?timent avec ?cart-types

ggplot(dtf_bbca_fltrd_tidy_mean_std_2) + aes(fill = f_emission_lots, x = Type.b?timent, y = mean.impact) + 
  geom_bar(position = "stack", stat = "identity") ## Analyse 2 : Barplot empil? de la somme des moyennes de l'impact carbone par lot et par type de b?timent

ggplot(dtf_bbca_fltrd_tidy_mean_std_2) + aes(fill = f_emission_lots, x = Type.b?timent, y = mean.impact) + 
  geom_bar(position = "fill", stat = "identity") ## Analyse 2 : Barplot empil? du pourcentage des moyennes de l'impact carbone par lot et par type de b?timent

#ggplot(dtf_bbca_fltrd_mean_std_3) + aes(x = Type.b?timent, y = mean.cout.m2) + geom_bar(stat = "identity") + 
#  geom_errorbar(aes(ymin = mean.cout.m2 - std.cout.m2, ymax = mean.cout.m2 + std.cout.m2), width = .2) ## Analyse 3 non utilis? pour le moment

ggplot(dtf_bbca_fltrd_ter_use) + aes(x = "", fill = Usage.principal) + geom_bar(position = "fill") + coord_polar("y", start=0) ## Camembert des usages des b?timents tertiaires

ggplot(dtf_bbca_fltrd_ter_pub_use) + aes(x = "", fill = Usage.principal) + geom_bar(position = "fill") + coord_polar("y", start=0) ## Camembert des usages des b?timents tertiaires publics

ggplot(dtf_bbca_fltrd_ter_priv_use) + aes(x = "", fill = Usage.principal) + geom_bar(position = "fill") + coord_polar("y", start=0) ## Camembert des usages des b?timents tertiaires priv?s


## Obtenir une matrice des corr?lations entre variables quantitatives----
variables_analyse_1_quanti <- c("Consommation", "Co?t.travaux", "Surface", 
                                "Contrib.chantier", "Contrib.eau", 
                                "Contrib.energie", "Contrib.pce")

dtf_bbca_fltrd_quanti_all <- dtf_bbca_fltrd_line_1 %>% select(variables_analyse_1_quanti) ## Selection des variables quantitatives, toutes cat?gories de b?timents confondues

dtf_bbca_fltrd_quanti_hab <- dtf_bbca_fltrd_line_1 %>% filter(Type.b?timent %in% c("Logements collectifs - Public", "Logements collectifs - Priv?")) %>% 
  select(variables_analyse_1_quanti) ## Selection des variables quantitatives, logements publics et priv?s seuls

dtf_bbca_fltrd_quanti_ter <- dtf_bbca_fltrd_line_1 %>% filter(Type.b?timent %in% c("Tertiaire - Public", "Tertiaire - Priv?")) %>% 
  select(variables_analyse_1_quanti) ## Selection des variables quantitatives, b?timents tertiaires publics et priv?s seuls

bbca_cor_var_quanti_all <- cor(dtf_bbca_fltrd_quanti_all) ## Obtention de la matrice des corr?lations entre vatiables quantitatives, toutes cat?gories de b?timents confondues

bbca_cor_var_quanti_hab <- cor(dtf_bbca_fltrd_quanti_hab) ## Obtention de la matrice des corr?lations entre vatiables quantitatives, logements publics et priv?s seuls

bbca_cor_var_quanti_ter <- cor(dtf_bbca_fltrd_quanti_ter) ## Obtention de la matrice des corr?lations entre vatiables quantitatives, b?timents tertiaires publics et priv?s seuls

p_values_all <- cor.mtest(dtf_bbca_fltrd_quanti_all) ## Obtention des p values des corr?lations entre variables quanti, toutes cat?gories de b?timents confondues

p_values_hab <- cor.mtest(dtf_bbca_fltrd_quanti_hab) ## Obtention des p values des corr?lations entre variables quanti, logements publics et priv?s seuls

p_values_ter <- cor.mtest(dtf_bbca_fltrd_quanti_ter) ## Obtention des p values des corr?lations entre variables quanti, b?timents tertiaires publics et priv?s seuls

bbca_corplot_var_quanti_all <- corrplot(bbca_cor_var_quanti_all, p.mat = p_values_all$p, method = 'color', diag = FALSE, type = 'upper',
                                sig.level = c(0.001, 0.01, 0.05), insig = 'label_sig', pch.col = 'grey20', order = 'AOE') ## Obtention du diagramme des corr?lations entre variables, toutes cat?gories de b?timents confondues

bbca_corplot_var_quanti_hab <- corrplot(bbca_cor_var_quanti_hab, p.mat = p_values_hab$p, method = 'color', diag = FALSE, type = 'upper',
                                sig.level = c(0.001, 0.01, 0.05), insig = 'label_sig', pch.col = 'grey20', order = 'AOE') ## Obtention du diagramme des corr?lations entre variables, logements publics et priv?s seuls

bbca_corplot_var_quanti_ter <- corrplot(bbca_cor_var_quanti_ter, p.mat = p_values_ter$p, method = 'color', diag = FALSE, type = 'upper',
                                sig.level = c(0.001, 0.01, 0.05), insig = 'label_sig', pch.col = 'grey20', order = 'AOE') ## Obtention du diagramme des corr?lations entre variables, b?timents tertiaires publics et priv?s seuls


## Analyse factorielle et classification----
dtf_bbca_fltrd_line_1a <- dtf_bbca_fltrd_line_1[, ] ## Deep copy du df dtf_bbca_fltrd_line_1

row.names(dtf_bbca_fltrd_line_1a) <- dtf_bbca_fltrd_line_1a$Index ## Set Index comme le nom des lignes

dtf_bbca_fltrd_line_1a <- dtf_bbca_fltrd_line_1a %>% select(-Index) ## Supprimer la colonne Index

analyse_bbca_acp <- Factoshiny::Factoshiny(dtf_bbca_fltrd_line_1a) ## Lancement de l'analyse en composantes principales

res.PCA <- PCA(dtf_bbca_fltrd_line_1a, quali.sup = c(4,5,6,7,8), graph = FALSE) ## Ligne de code pour l'ACP

res.HCPC <- HCPC(res.PCA, nb.clust = 4, consol = FALSE, graph = FALSE) ## Ligne de code pour la classification

dtf_bbca_fltrd_line_1a_clust <- res.HCPC$data.clust ## R?cup?ration des donn?es de classification
