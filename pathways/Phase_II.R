##Phase II Analyses
library(dplyr)
library(tidyr)

load("~/Documents/CI_Projects_LA/Knowledge_Base/evidence_based_11_2.RData")
setwd("~/Documents/CI_Projects_LA/Knowledge_Base/")

##Conceptual models - exploring trends and informing pathways
models <- filter(data.pathways,Concept_mod == 1)
models <- models %>% select(aid,Concept_mod,Concept_mod_name) %>% distinct()
model_types <- count(models,Concept_mod_name)
model_aids <- select(models,aid)
model_aids <- distinct(model_aids)
nm_aids <- data.biblio %>% select(aid) 
nm_aids <- nm_aids %>% anti_join(model_aids,by="aid") %>% distinct()
##Disaggregating dataset into models and non-models
m.data.biblio <- left_join(model_aids,data.biblio,by="aid")
m.data.biomes <- left_join(model_aids,data.biomes,by="aid")
m.data.interv <- left_join(model_aids,data.interv,by="aid")
m.data.outcome <- left_join(model_aids,data.outcome,by="aid")
m.data.outhwb <- left_join(model_aids,data.outhwb,by="aid")
m.data.pathways <- left_join(model_aids,data.pathways,by="aid")
m.data.study <- left_join(model_aids,data.study,by="aid")

nm.data.biblio <- left_join(nm_aids,data.biblio,by="aid")
nm.data.biomes <- left_join(nm_aids,data.biomes,by="aid")
nm.data.interv <- left_join(nm_aids,data.interv,by="aid")
nm.data.outcome <- left_join(nm_aids,data.outcome,by="aid")
nm.data.outhwb <- left_join(nm_aids,data.outhwb,by="aid")
nm.data.pathways <- left_join(nm_aids,data.pathways,by="aid")
nm.data.study <- left_join(nm_aids,data.study,by="aid")

#Comparing models and non-models - data needs to be normalized (present percentages)
#1. Linkages examined

#2a. Biomes

#2b.Regions
##Plot countries
#load in full country list
country <- read.csv("~/Documents/CI_Projects_LA/Knowledge_Base/country_list2.csv", head=TRUE, sep=",")
names(country)<- c("Study_country", "Region", "Code")
regions <- arrange(country,Region)

##Count number of studies for all countries and arrange by region
country_count <- matrix(nrow=nrow(regions), ncol=3)
rownames(country_count) <- regions$Study_country
colnames(country_count) <- c("Study_country", "m_counts","nm_counts")
#Calculate in for loop and write to blank matrix
for (c in regions$Study_country){
  subset <- filter(m.data.study, Study_country == c)
  country_count[c,1] <- c
  country_count[c,2] <- as.numeric(n_distinct(subset$aid))
  subset <- filter(nm.data.study, Study_country == c)
  country_count[c,3] <- as.numeric(n_distinct(subset$aid))
}
#Remove rownames and reformat data types
rownames(country_count) = NULL
country_count <- as.data.frame(country_count, stringsAsFactors=FALSE)
countries_only <- inner_join(country_count,regions,by="Study_country")
countries_only <- filter(countries_only, Code != "")
countries_only <- arrange(countries_only,Region)

countries_only$m_counts <- as.numeric(countries_only$m_counts)
countries_only$nm_counts <- as.numeric(countries_only$nm_counts)
countries_only <- as.data.frame(countries_only)

#Calculate percent of overall studies that include a model
c_perc_mod <- matrix(nrow=nrow(countries_only),ncol=1)
rownames(c_perc_mod) <- countries_only$Study_country
colnames(c_perc_mod) <- c("perc_mod")
d <- countries_only
rownames(d) <- d$Study_country

for (c in countries_only$Study_country){
  perc <- ((d[c,2])/(d[c,2]+d[c,3]))*100
  c_perc_mod[c,1] <- as.numeric(perc)
}

c_perc_mod <- as.data.frame(c_perc_mod)
c_perc_mod$perc_mod <- as.numeric(c_perc_mod$perc_mod)
countries_only <- bind_cols(countries_only,c_perc_mod)
countries_data <- as.data.frame(filter(countries_only, perc_mod != "NaN"))
countries_nodata <- as.data.frame(filter(countries_only,perc_mod == "NaN"))
countries_nodata <- as.data.frame(select(countries_nodata,Study_country, Code))

#Plot distribution to PDF
library(rgeos)
library(maptools)
library(gpclib)
library(ggplot2)
library(RColorBrewer)
oecd <- read.csv("~/Documents/CI_Projects_LA/Knowledge_Base/oecd.csv",header=TRUE)
map <- readShapeSpatial("~/Documents/CI_Projects_LA/Knowledge_Base/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
plot(map)
map <- fortify(map, region="ISO3")
pdf(file="Percent_Models_Country_Map.pdf", width=16, height=8.5)
ggplot() + geom_map(data=countries_data, aes(map_id=Code, fill=perc_mod),map=map) + geom_map(data=oecd, aes(map_id=CODE),fill="#f0f0f0",map=map) + geom_map(data=countries_nodata, aes(map_id=Code),fill="#bbbbbb",map=map) + expand_limits(x=map$long,y=map$lat) + scale_fill_gradient2(low="#d9f0a3",mid="#41ab5d",high="#004529",midpoint=50,limits=c(0,100))
dev.off()

#3. Study design/quality

#4. Researcher/affiliations

#5. Outcomes examined and complexity of outcomes
##Generate term document matrix (as standard matrix) of article IDs (aids) and outcome types
outcome_tdm <- matrix(nrow=12,ncol=n_distinct(nm_aids$aid))
colnames(outcome_tdm) <- as.character(nm_aids$aid)
out_type = c("env", "mat_liv_std", "eco_liv_std", "health", "education", "soc_rel", "sec_saf", "gov", "sub_well", "culture", "free_choice", "other")
rownames(outcome_tdm) <- out_type
m_aid <- as.vector(as.character(nm_aids$aid))
for (n in nm_aids$aid){
  sub <- filter(nm.data.outcome,aid == n)
  for (o in out_type){
    if (o %in% sub$Outcome == TRUE){
      state <- as.numeric(1)
    } else
      state <- as.numeric(0)
    outcome_tdm[as.character(o),as.character(n)] <- state
    }
  }

##Calculate number of outcomes studies
nm_to <- as.data.frame(colSums(outcome_tdm))
colnames(nm_to) <- c("tot_outcome")
nm_outcome_avg <- nm_to %>% filter(tot_outcome != 0) %>% summarise(avg = mean(tot_outcome))
m_to <- as.data.frame(colSums(outcome_tdm_models))
colnames(m_to) <- c("tot_outcome")
m_outcome_avg <- m_to %>% filter(tot_outcome != 0) %>% summarise(avg = mean(tot_outcome))
#Compare means with t-test - first testing for equal variances
nm_to <- filter(nm_to, tot_outcome !=0)
m_to <- filter(m_to, tot_outcome !=0)
var.test(nm_to$tot_outcome,m_to$tot_outcome)
t.test(m_to$tot_outcome,nm_to$tot_outcome)

#Compare distributions
par(mfrow=c(1,2))
hist(m_to$tot_outcome,main="Studies employing a conceptual model", xlab="Total number of outcomes studied")
hist(nm_to$tot_outcome,main="Studies without a conceptual model", xlab="Total number of outcomes studied")

##Check tdm for missing data
class(outcome_tdm) <- "numeric"
outcome_tdm <- as.data.frame(outcome_tdm)
missing_data <- outcome_tdm[, colSums(outcome_tdm) == 0]
##13 articles w/ models with NO outcome information - need to go back into data and check
##32 articles w/ no models with NO outcome information

#Build term-term adjacency matrix
outcome_tdm <- as.matrix(outcome_tdm)
outcome_tdm[outcome_tdm>=1] <- 1
outcome_term_matrix <- outcome_tdm %*% t(outcome_tdm)

#Build graph of adjacent terms (co-occurence of two terms)
library(igraph)
g <- graph.adjacency(outcome_term_matrix,weighted=T,mode="undirected")
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g,layout=layout1)

##Scale based on degrees to make common terms standout
V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
plot(g, layout=layout1)


##Building actual term document matrix
library(tm)
library(SnowballC)

dist <- dist(scale(outcome_tdm))
fit <- hclust(dist,method="ward.D")
plot(fit, cex=0.9,hang=1)

#Comparing within models
#1. Complexity - # outcomes, #interventions
#Distribution of # of outcomes and # interventions per study
#2. Robustness of data - is this biased based on the scientific capacity of the country?

#3. Types of models - standard vs. novel - different linkages? different complexity?
#Separate out novel vs standard models 
nov_mod <- filter(models, Concept_mod_name == "in situ")
nov_mod <- distinct(nov_mod)
std_mod <- anti_join(models,nov_mod,by="aid")
std_mod <- distinct(std_mod)
std_mod_aids <- distinct(as.data.frame(std_mod$aid))
nov_mod_aids <- distinct(as.data.frame(nov_mod$aid))

library(gplots)
library(RColorBrewer)
#Create new dataframe with intervention and outcome data
int_out <- left_join(data.interv, data.outcome, by = "aid")
int_out <- select(int_out,aid,Int_type,Outcome)
int_out <- distinct(int_out)

#Create new matrix for intervention groups
rows <- c(1:nrow(int_out))
int_groups <- matrix(nrow=nrow(int_out),ncol=1)
rownames(int_groups) <- rows
colnames(int_groups) <- c("int_group")

#Assign intervention groups
for (i in rows){
  int <- int_out$Int_type[i]
  if (int == "area_mgmt" | int == "sp_control" | int == "restoration") {
    group <- "land_wat_mgmt"
  } else if (int == "sp_mgmt" | int == "sp_recov" | int == "sp_reint" | int == "ex_situ") {
    group <- "species_mgmt"
  } else if (int == "form_ed" | int == "training" | int == "aware_comm") {
    group <- "education"
  } else if (int == "legis" | int == "pol_reg" | int == "priv_codes" | int == "compl_enfor") {
    group <- "law_policy"
  } else if (int == "liv_alt" | int == "sub" | int == "market" | int == "non_mon") {
    group <- "liv_eco_inc"
  } else if (int == "inst_civ_dev" | int == "part_dev" | int == "cons_fin") {
    group <- "ext_cap_build"
  } else
    group <- int
  int_groups[i,"int_group"] <- group
}

int_groups <- as.data.frame(int_groups)
int_out_final <- bind_cols(int_out,int_groups)
int_out_final <- filter(int_out_final,!is.na(int_groups))

#Create blank data matrix with labeled rows and columns
int_groups_type = c("area_protect", "land_wat_mgmt", "res_mgmt", "species_mgmt", "education", "law_policy", "liv_eco_inc", "ext_cap_build", "sus_use", "other")
out_type = c("eco_liv_std", "mat_liv_std", "health", "education", "soc_rel", "sec_saf", "gov", "sub_well", "culture", "free_choice", "other")
ios_counts = matrix(nrow=11, ncol=10)
rownames(ios_counts) <- out_type
colnames(ios_counts) <- int_groups_type
#Calculate number of unique studies for each linkage cell between intervention and outcome
#Calculate in for loop and write to blank matrix
for (i in int_groups_type){
  for (j in out_type){
    subset <- distinct(filter(int_out_sys_final, Outcome == j, int_group == i))
    sys_rev <- distinct(filter(subset, Part_sys_rev == 1))
    ios_counts[j,i] <- n_distinct(sys_rev$aid)
  }
}
#Relabel rows and columns
int_group_labels = c("Area protection", "Land/Water management", "Resource management", "Species management", "Education", "Law & Policy", "Livelihood, economic & other incentives", "External capacity building", "Sustainable use", "Other")
out_labels = c("Economic living standards", "Material living standards", "Health", "Education", "Social relations", "Security & safety", "Governance & empowerment", "Subjective well-being", "Culture & Spiritual", "Freedom of choice/action", "Other")
rownames(ios_counts) <- out_labels
colnames(ios_counts) <- int_group_labels
#Define color palette for heatmap
#Color1 - #d9f0a3 #41ab5d #004529

palette_final <- colorRampPalette(c("#e5f5f9", #d9f0a3","#41ab5d", "#004529")) (n=50)
#Write heatmap and legend to PDF
pdf(file="Interventions_Outcomes_Condensed_Sys_Rev_Heatmap.pdf", width=11, height=8.5)
heatmap.2(ios_counts, Colv=NA, dendrogram="none", col=palette_final, cellnote=ios_counts, notecol="black", notecex=1.0, trace="none", cexRow=1.5, cexCol=1.5, key=TRUE, Rowv=NA)
dev.off()

#4. Looking at models versus examining explanatory variables/factors


