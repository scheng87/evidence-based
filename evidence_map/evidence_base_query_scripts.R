##Query scripts for evidence based conservation data frame
#Load required libraries and dataframe
library(dplyr)
library(tidyr)
path <- "~/Documents/CI_Projects_LA/Knowledge_Base/"
load("~/Documents/CI_Projects_LA/Knowledge_Base/evidence_based_11_2.RData")


##Create set variables for study design, these can be called later when interested in filtering datasets for quality of data
BA_C_I <- filter(data.study, Comps == 1, Design.control == 1)
BA_C_I <- filter(BA_C_I, Comps.time == "Punctuated" | Comps.time == "Yes, punctuated")
BA_C_I <- select(BA_C_I, aid, Comps.time,Design.control,Data_type)
BA_C_I <- distinct(BA_C_I)
BA_C_I_titles <- left_join(BA_C_I, data.biblio, by="aid")
BA_C_I_titles <- distinct(BA_C_I_titles)
BA_C_I_count <- n_distinct(BA_C_I$aid)
BA_C_I_qual <- count(BA_C_I,Data_type)

BA <- filter(data.study, Comps.time == "Punctuated" | Comps.time == "Yes, punctuated")
BA <- select(BA, aid, Comps.time,Data_type)
BA <- distinct(BA)
BA_count <- n_distinct(BA$aid)
BA_qual <- count(BA,Data_type)

COT <- filter(data.study, Comps.time == "Continual" | Comps.time == "Yes, continual" | Comps.time == "Continuous")
COT <- select(COT, aid, Comps.time,Data_type)
COT <- distinct(COT)
COT_count <- n_distinct(COT$aid)
COT_qual <- count(COT,Data_type)

CG <- filter(data.study, Comps.type == "Cultural/ethnic groups" | Comps.type == "Demographic groups" | Comps.type == "Projects" | Comps.type == "Sites" | Comps.type == "Socio/economic groups/levels" | Comps.type == "User groups" | Comps.type == "cultural/ethnic groups" | Comps.type == "demographic groups" | Comps.type == "projects" | Comps.type == "project" | Comps.type == "sites" | Comps.type == "socio-economic" | Comps.type == "socio-economic groups" | Comps.type == "user groups" | Comps.type == "Presence/absence of intervention" | Comps.type == "presence/absence of intervention" | Comps.type == "Presence/Absence of intervention")
CG <- select(CG, aid, Comps.type,Data_type)
CG <- distinct(CG)
CG_count <- n_distinct(CG$aid)
CG_Q <- distinct(select(CG,aid,Data_type))
CG_qual <- count(CG_Q,Data_type)

CG_BA <- filter(data.study, Comps.type == "Cultural/ethnic groups" | Comps.type == "Demographic groups" | Comps.type == "Projects" | Comps.type == "Sites" | Comps.type == "Socio/economic groups/levels" | Comps.type == "User groups" | Comps.type == "cultural/ethnic groups" | Comps.type == "demographic groups" | Comps.type == "projects" | Comps.type == "project" | Comps.type == "sites" | Comps.type == "socio-economic" | Comps.type == "socio-economic groups" | Comps.type == "user groups" | Comps.type == "Presence/absence of intervention" | Comps.type == "presence/absence of intervention" | Comps.type == "Presence/Absence of intervention")
CG_BA <- filter(CG_BA, Comps.time == "Yes, punctuated" | Comps.time == "Punctuated")
CG_BA <- select(CG_BA, aid, Comps.type, Comps.time,Data_type)
CG_BA <- distinct(CG_BA)
CG_BA_count <- n_distinct(CG_BA$aid)
CG_BA_Q <- distinct(select(CG_BA,aid,Data_type))
CG_BA_qual <- count(CG_BA_Q,Data_type)

CI_CG <- filter(data.study, Comps.type == "Cultural/ethnic groups" | Comps.type == "Demographic groups" | Comps.type == "Projects" | Comps.type == "Sites" | Comps.type == "Socio/economic groups/levels" | Comps.type == "User groups" | Comps.type == "cultural/ethnic groups" | Comps.type == "demographic groups" | Comps.type == "projects" | Comps.type == "project" | Comps.type == "sites" | Comps.type == "socio-economic" | Comps.type == "socio-economic groups" | Comps.type == "user groups" | Comps.type == "Presence/absence of intervention" | Comps.type == "presence/absence of intervention" | Comps.type == "Presence/Absence of intervention")
CI_CG <- filter(CI_CG, Comps.time == "No" | Comps.time == "Unspecified")
CI_CG <- filter(CI_CG, Design.control == 1)
CI_CG <- select(CI_CG, aid, Design.control, Comps.type, Comps.time,Data_type)
CI_CG <- distinct(CI_CG)
CI_CG_count <- n_distinct(CI_CG$aid)
CI_CG_Q <- distinct(select(CI_CG,aid,Data_type))
CI_CG_qual <- count(CI_CG_Q,Data_type)

NO_CG <- filter(data.study, Comps == 0)
NO_CG <- select(NO_CG, aid, Comps,Data_type)
NO_CG <- distinct(NO_CG)
NO_CG_count <- n_distinct(NO_CG$aid)
NO_CG_Q <- distinct(select(NO_CG,aid,Data_type))
NO_CG_qual <- count(NO_CG_Q,Data_type)

QUANT_EXP <- filter(data.study, Design.assigned == 1)
QUANT_EXP <- filter(QUANT_EXP, Design.control == 1)
QUANT_EXP <- select(QUANT_EXP, aid, Data_type, Design.assigned, Design.control)
QUANT_EXP <- distinct(QUANT_EXP)
EXP_count <- count(QUANT_EXP,Data_type)
QUANT_EXP <- filter(QUANT_EXP, Data_type == "Quant" | Data_type == "NA")
QUANT_EXP <- distinct(QUANT_EXP)
QUANT_EXP_count <- n_distinct(QUANT_EXP$aid)
QUANT_EXP_qual <- count(QUANT_EXP, Data_type)
QUANT_EXP_full <- left_join(QUANT_EXP,data.biblio,by="aid")
QUANT_EXP_full <- distinct(QUANT_EXP_full)
QUANT_EXP_full <- select(QUANT_EXP_full,aid,Authors,Pub_year,Title)
QUANT_EXP_full2 <- left_join(QUANT_EXP_full,data.study,by="aid")
QUANT_EXP_full2 <- select(QUANT_EXP_full2,aid,Authors,Pub_year,Title,Study_country)
QUANT_EXP_full2 <- distinct(QUANT_EXP_full2)
QUANT_EXP_full3 <- left_join(QUANT_EXP_full2,data.interv,by="aid")
QUANT_EXP_full3 <- select(QUANT_EXP_full3,aid,Authors,Pub_year,Title,Study_country,Int_type)
QUANT_EXP_full3 <- distinct(QUANT_EXP_full3)
QUANT_EXP_final <- left_join(QUANT_EXP_full3,data.outcome,by="aid")
QUANT_EXP_final <- select(QUANT_EXP_final,aid,Authors,Pub_year,Title,Study_country,Int_type,Outcome)
QUANT_EXP_final <- distinct(QUANT_EXP_final)
write.csv(QUANT_EXP_final,"Quantitative_experimental_studies.csv")

QUANT_QUASI <- filter(data.study, Design.control == 1)
QUANT_QUASI <- select(QUANT_QUASI,aid,Data_type,Design.control)
QUANT_QUASI <- distinct(QUANT_QUASI)
QUANT_QUASI_qual <- count(QUANT_QUASI, Data_type)
QUANT_QUASI <- filter(QUANT_QUASI, Design.qual_only == 0 | Design.qual_only == "Unspecified")
QUANT_QUASI <- distinct(QUANT_QUASI)
QUANT_QUASI_count <- n_distinct(QUANT_QUASI$aid)
count(QUANT_QUASI,Design.qual_only)
QUANT_QUASI_full <- left_join(QUANT_QUASI,data.biblio,by="aid")
QUANT_QUASI_full <- distinct(QUANT_QUASI_full)
QUANT_QUASI_full <- select(QUANT_QUASI_full,aid,Authors,Pub_year,Title)
QUANT_QUASI_full2 <- left_join(QUANT_QUASI_full,data.study,by="aid")
QUANT_QUASI_full2 <- select(QUANT_QUASI_full2,aid,Authors,Pub_year,Title,Study_country)
QUANT_QUASI_full2 <- distinct(QUANT_QUASI_full2)
QUANT_QUASI_full3 <- left_join(QUANT_QUASI_full2,data.interv,by="aid")
QUANT_QUASI_full3 <- select(QUANT_QUASI_full3,aid,Authors,Pub_year,Title,Study_country,Int_type)
QUANT_QUASI_full3 <- distinct(QUANT_QUASI_full3)
QUANT_QUASI_final <- left_join(QUANT_QUASI_full3,data.outcome,by="aid")
QUANT_QUASI_final <- select(QUANT_QUASI_final,aid,Authors,Pub_year,Title,Study_country,Int_type,Outcome)
QUANT_QUASI_final <- distinct(QUANT_QUASI_final)
write.csv(QUANT_QUASI_final,"Quantitative_quasi-experimental_studies.csv")

QUANT_NONEXP <- filter(data.study, Comps == 1)
QUANT_NONEXP <- filter(QUANT_NONEXP, Design.control == 0 | Design.control == "Unspecified")
QUANT_NONEXP <- select(QUANT_NONEXP,aid,Comps,Comps.type,Comps.time,Data_type,Design.control)
QUANT_NONEXP <- distinct(QUANT_NONEXP)
QUANT_NONEXP_count <- n_distinct(QUANT_NONEXP$aid)
QUANT_NONEXP_qual <- count(QUANT_NONEXP,Data_type)

QUAL_STRAT <- filter(data.study,Comps == 1 | Design.control == 1)
QUAL_STRAT <- select(QUAL_STRAT,aid,Data_type,Comps, Design.control)
QUAL_STRAT <- distinct(QUAL_STRAT)
QUAL_STRAT_count <- n_distinct(QUAL_STRAT$aid)
QUAL_STRAT_qual <- count(QUAL_STRAT,Data_type)

ALL_REM <- filter(data.study, Comps == 0)
ALL_REM <- select(ALL_REM,aid,Comps,Comps.type,Comps.time,Data_type,Design.control,Design.assigned)
ALL_REM <- distinct(ALL_REM)
ALL_REM_count <- n_distinct(ALL_REM$aid)
ALL_REM_qual <- count(ALL_REM,Data_type)

#Create quantitative, exp, quasi_exp, non_exp data subset
QUANT_EXP_sub <- select(QUANT_EXP,aid,Data_type)
QUANT_EXP_sub <- distinct(filter(QUANT_EXP_sub,Data_type == "Quant"))
QUANT_QUASI_sub <- select(QUANT_QUASI,aid,Data_type)
QUANT_QUASI_sub <- distinct(filter(QUANT_QUASI_sub,Data_type == "Quant"))
QUANT_NONEXP_sub <- select(QUANT_NONEXP,aid,Data_type)
QUANT_NONEXP_sub <- distinct(filter(QUANT_NONEXP_sub,Data_type == "Quant"))
QUANT_sub <- bind_rows(QUANT_EXP_sub,QUANT_QUASI_sub,QUANT_NONEXP_sub)
QUANT_sub <- left_join(QUANT_sub,data.interv,by="aid")
QUANT_sub <- distinct(select(QUANT_sub,aid,Int_type))
QUANT_sub <- left_join(QUANT_sub,data.outcome,by="aid")
QUANT_sub <- distinct(select(QUANT_sub,aid,Int_type,Outcome))
int_out <- QUANT_sub

##CHECKING OVERLAPS
QUANT_EXP_aid <- distinct(as.data.frame(QUANT_EXP$aid))
QUANT_QUASI_aid <- distinct(as.data.frame(QUANT_QUASI$aid))
QUANT_NONEXP_aid <- distinct(as.data.frame(QUANT_NONEXP$aid))
QUAL_STRAT_aid <- distinct(as.data.frame(QUAL_STRAT$aid))
ALL_REM_aid <- distinct(as.data.frame(ALL_REM$aid))

check <- bind_rows(QUANT_EXP_aid,QUANT_QUASI_aid,ALL_REM_aid,QUANT_NONEXP_aid,QUAL_STRAT_aid)
duplicated(check)

#Summarize counts
study_type_counts <- matrix(nrow=7, ncol=1)
rownames(study_type_counts) <- c("Non-comparative study", "Experimental study", "Observational study", "Experimental before and after study/interrupted time series", "Observational before and after study/interrupted time series", "Experimental with control", "Observational with control")
colnames(study_type_counts) <- c("counts")
#Calculate in for loop and write to blank matrix
study_type_counts[1,1] <- n_distinct(NCS$aid)
study_type_counts[2,1] <- n_distinct(EXP$aid)
study_type_counts[3,1] <- n_distinct(OBS$aid)
study_type_counts[4,1] <- n_distinct(EXP_BAC$aid)
study_type_counts[5,1] <- n_distinct(OBS_BAC$aid)
study_type_counts[6,1] <- n_distinct(EXP_CON$aid)
study_type_counts[7,1] <- n_distinct(OBS_CON$aid)
#Remove rownames and reformat data types
study_type_counts <- as.data.frame(study_type_counts)
stcounts <- as.numeric(study_type_counts$counts)
st_labels <- c("Non-comparative study", "Experimental study", "Observational study", "Experimental before and after study/interrupted time series", "Observational before and after study/interrupted time series", "Experimental with control", "Observational with control")
#Plot barplot
pdf(file="Summary_study_type.pdf")
par(mar = c(7, 4, 2, 2) + 0.2)
end_points = 0.5 + nrow(study_type_counts) + nrow(study_type_counts)-1
barplot(stcounts, main="Study type", axes=TRUE, ylim = c(0,10+max(stcounts)), ylab = "Number of studies", xlab = "", col="purple", angle=45, border=NA, space=1)
text(seq(1.5,end_points,by=2), par("usr")[3]-0.25, srt=60, adj=1.1, xpd=TRUE, labels = paste(st_labels), cex=0.65)
box()
dev.off()

##Plot countries
country_file <- "country_list.csv"
country_file_reg <- "country_list2.csv"
country_list <- paste(path,country_file, sep="")
reg_list <- paste(path,country_file_reg, sep="")
#load in full country list
country <- read.csv(country_list, head=TRUE, sep=",")
names(country)<- c("Study_country", "Long", "Lat")
country <- arrange(country,Study_country)
regions <- read.csv(reg_list, head=TRUE, sep=",")
names(regions) <- c("Study_country", "Region", "Code")
regions <- arrange(regions,Continent)

##Count number of studies for all countries and arrange by region
country_count <- matrix(nrow=nrow(regions), ncol=2)
rownames(country_count) <- regions$Study_country
colnames(country_count) <- c("Study_country", "counts")
#Calculate in for loop and write to blank matrix
for (c in regions$Study_country){
  subset <- filter(data.study, Study_country == c)
  country_count[c,1] <- c
  country_count[c,2] <- as.numeric(n_distinct(subset$aid))
}
#Remove rownames and reformat data types
rownames(country_count) = NULL
country_count <- as.data.frame(country_count, stringsAsFactors=FALSE)
countries_only <- inner_join(country_count,regions,by="Study_country")
countries_only <- filter(countries_only, Code != "")
countries_only_no_zero <- filter(countries_only, counts != 0)
countries_only_no_zero <- arrange(countries_only_no_zero,Region)

require(gdata)
countries_only_no_zero$Region <- reorder.factor(countries_only_no_zero$Region, new.order=test_order)
require(dplyr)
countries_only_no_zero <- countries_only_no_zero %>% arrange(desc(Study_country)) %>% arrange(desc(Region))

c_counts <- as.numeric(as.vector(countries_only_no_zero$counts))
countries_only_no_zero$counts <- as.numeric(countries_only_no_zero$counts)
countries_only$counts <- as.numeric(countries_only$counts)
c_labels <- countries_only_no_zero$Study_country
countries_only <- as.data.frame(countries_only)



pdf(file="IE_counts.pdf", width=20, height=20)
barplot(c_counts,horiz=TRUE,xlim=c(0,10),names.arg=c_labels,las=1,xlab="Number of impact evaluations by region",cex.axis=1.5)
box()
dev.off()

reg <- c("Asia","Africa","Europe","Latin America","Oceania")
reg_summary <- matrix(nrow=5,ncol=1)
rownames(reg_summary) <- reg
colnames(reg_summary) <- c("counts")
for (r in reg){
  sub <- filter(countries_only,Region == r)
  sum <- sum(sub$counts)
  reg_summary[r,1] <- sum
} 
reg_summary <- as.data.frame(reg_summary)
write.csv(reg_summary,"reg_summary.csv")
reg_summary <- read.csv("reg_summary.csv")
r_counts <- as.numeric(as.vector(reg_summary$counts))

pdf(file="Country_distribution_region.pdf", width=20, height=8.5)
barplot(r_counts,horiz=TRUE,xlim=c(0,525),names.arg=reg_summary$X,las=1,xlab="Number of studies by region",cex.axis=1.5)
box()
dev.off()

continent <- distinct(as.data.frame(countries_only$Continent))
continent <- unlist(continent)
continent_summary <- matrix(nrow=5,ncol=1)
rownames(continent_summary) <- continent
colnames(continent_summary) <- c("counts")
for (c in continent){
  sub <- filter(countries_only,Continent == c)
  sum <- sum(sub$counts)
  continent_summary[c,1] <- sum
} 
continent_summary <- as.data.frame(continent_summary)
cont_counts <- as.numeric(as.vector(continent_summary$counts))

pdf(file="Country_distribution_continent.pdf", width=20, height=8.5)
end_point1 = 0.5 + nrow(cont_counts) + nrow(cont_counts)-1
barplot(cont_counts, main="Distribution of studies for non-OECD countries by continent", axes=TRUE, ylim = c(0,5+max(cont_counts)), ylab = "Number of studies", xlab = "", col="turquoise", angle=45, border=NA, space=1)
text(seq(1.5,end_point1,by=1), par("usr")[3]-0.25, srt=60, adj=1, xpd=TRUE, labels = paste(continents), cex=0.5)
box()
dev.off()

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
pdf(file="resource_mgmt_economic_Country_Map.pdf", width=16, height=8.5)
ggplot() + geom_map(data=countries_only, aes(map_id=Code, fill=counts),map=map) + geom_map(data=oecd, aes(map_id=CODE),fill="#f0f0f0",map=map) + expand_limits(x=map$long,y=map$lat) + scale_fill_gradient2(low="#d9f0a3",mid="#41ab5d",high="#004529",midpoint=47,limits=c(0,94))
dev.off()

#Color1 - #d9f0a3 #41ab5d #004529
#Color2 - #c7e9b4 #1d91c0 #081d58
#Greyscale - #bdbdbd #525252 #000000


#Count number of studies for each country and input into blank data matrix - for map
country_counts <- matrix(nrow=95, ncol=3)
rownames(country_counts) <- country$Study_country
colnames(country_counts) <- c("Study_country", "counts", "counts_for_map")
#Calculate in for loop and write to blank matrix
for (c in country$Study_country){
  subset <- filter(data.study, Study_country == c)
  country_counts[c,1] <- c
  country_counts[c,2] <- as.numeric(n_distinct(subset$aid))
  country_counts[c,3] <- as.numeric((n_distinct(subset$aid))^2)
}
#Remove rownames and reformat data types
rownames(country_counts) = NULL
country_counts <- as.data.frame(country_counts, stringsAsFactors=FALSE)
#Combine with lat, long, convert data types
final_country <- left_join(country, country_counts, by="Study_country")
ccounts <- as.numeric(as.vector(final_country$counts_for_map))
clabels <- final_country$Study_country

plot_set <- data.frame(counts=integer(),countsformap=integer())
range <- c(1:95)
for (i in range){
  plot_set[i,1] <- as.numeric(final_country[i,4])
  plot_set[i,2] <- as.numeric(final_country[i,5])
}
newset <- bind_cols(country,plot_set)
newset <- arrange(newset,desc(counts))
total_counts <- as.numeric(as.vector(newset$counts))
map_counts <- as.numeric(as.vector(newset$countsformap))
colnames(newset) <- c("Study_country", "Long", "Lat", "counts","countsformap")
newlabs <- newset$Study_country

#Load required libraries
library(ggplot2)
library(maps)
#Map points and write to PDF
pdf(file="Country_distribution_map_scaled.pdf", width=11, height=8.5)
mdat <- map_data('world')
ggplot() + 
  geom_polygon(dat=mdat, aes(long, lat, group=group), fill="grey50") +
  geom_point(data=newset, aes(x = Lat, y = Long, size=countsformap), alpha = .5, color="turquoise1")
#scale_size_area(breaks = c(1,10,50,88), labels = c("1", "< 10", "< 50", "< 100"), name = "Number of studies")
dev.off()


##Plot summaries 
##Summarize biomes
biome_cat <- c("T_TSTMBF", "T_TSTDBF", "T_TSTCF", "T_TBMF", "T_TCF", "T_BFT", "T_TSTGSS", "T_TGSS", "T_FGS", "T_MGS", "T_T", "T_MFWS", "T_DXS", "T_M", "FW_LL", "FW_LRD", "FW_PF", "FW_MF","FW_TCR","FW_TFRW","FW_TUR","FW_TSTCR","FW_TSTFRW","FW_TSTUR","FW_XFEB","FW_OI", "M_P", "M_TSS", "M_TU", "M_TRU", "M_TRC", "M_TSTSS")
#Create blank data matrix with labeled rows and columns
biome_counts <- matrix(nrow=32, ncol=2)
rownames(biome_counts) <- biome_cat
colnames(biome_counts) <- c("Code", "counts")
#Calculate number of unique studies for each linkage cell between intervention and outcome
#Calculate in for loop and write to blank matrix
for (b in biome_cat){
  subset <- filter(data.biomes, Biome. == b)
  biome_counts[b,1] <- b
  biome_counts[b,2] <- n_distinct(subset$aid)
  }
#Remove rownames and reformat data types
rownames(biome_counts) <- NULL
biome_counts <- as.data.frame(biome_counts)
bcounts <- as.numeric(as.vector(biome_counts$counts))
#Create barplot
biome_labels <- c("Tropical/sub-tropical moist broadleaf forests", "Tropical/sub-tropical dry broadleaf forests", "Tropical/sub-tropical coniferous forests", "Temperate broadleaf and mixed forests", "Temperate coniferous forests", "Boreal forests & taiga", "Tropical/sub-tropical grasslands, savannas & shrublands", "Temperate grasslands, savannas & shrublands", "Flooded grasslands and savannas", "Montane grasslands & shrublands", "Tundra", "Mediterranean forests, woodlands & scrubs", "Deserts & xeric shrublands", "Mangroves", "Large lakes", "Large river deltas", "Polar freshwaters", "Montane freshwaters", "Temperate coastal rivers", "Temperate floodplain rivers and wetlands", "Temperate upland rivers", "Tropical and subtropical coastal rivers", "Tropical and subtropical floodplain rivers and wetland complexes", "Tropical and subtropical upland rivers", "Xeric freshwaters and endorheic (closed) basins", "Oceanic Islands", "Marine-polar", "Marine-temperate shelfs and seas", "Marine-temperate upwelling", "Marine-tropical upwelling", "Marine-tropical corals", "Marine-tropical shelfs and seas")
write.csv(biome_counts,"biome_counts.csv")
biome_counts <- as.data.frame(read.csv("biome_counts.csv",header=TRUE))
pdf(file="Summary_biomes.pdf")
par(mar = c(7, 4, 2, 2) + 0.2)
barplot(as.matrix(biome_counts),horiz=TRUE,beside=TRUE,xlim=c(0,215),names.arg=c("Forests","Grasslands","Tundra","Deserts","Mangroves","Freshwater","Marine"),las=1,xlab="Number of studies by biome",cex.axis=1.5)
box()
dev.off()

##Biomes count by unit area
unit_area <- left_join(biome_counts,biome_summary,by="Code")
colnames(unit_area) <- c("code","counts","biome","area")
unit_area$counts <- as.numeric(as.vector(unit_area$counts))
unit_area$area <- as.numeric(as.vector(unit_area$area))
unit_area <- mutate(unit_area,studies_area = counts/area)

barplot(as.matrix(biome_counts[,2:3]),beside=TRUE,ylim=c(0,350),names.arg=as.vector(biome_counts$Biome),legend.text = c("Models","No Models"), args.legend = list(x="topleft", bty = "n"), las=1,xlab="Number of studies by biome",cex.axis=1.5)

##Summarize Bibliographic information
##Publication type
pub_cat <- c("Peer-reviewed published literature", "Conference proceedings", "Book/book chapter", "Unpublished grey literature", "Other")
#Create blank data matrix with labeled rows and columns
pub_counts <- matrix(nrow=5, ncol=1)
rownames(pub_counts) <- pub_cat
colnames(pub_counts) <- c("counts")
#Calculate in for loop and write to blank matrix
for (p in pub_cat){
  subset <- filter(data.biblio, Pub_type == p)
  pub_counts[p,1] <- n_distinct(subset$aid)
}
#Remove rownames and reformat data types
pub_counts <- as.data.frame(pub_counts)
pcounts <- as.numeric(as.vector(pub_counts$counts))

##Author affiiation
affil_cat <- c("Academic", "Pub_sec", "Res_int", "Cons", "Non_prof", "Priv_sec")
#Create blank data matrix with labeled rows and columns
affil_counts <- matrix(nrow=6, ncol=1)
rownames(affil_counts) <- affil_cat
colnames(affil_counts) <- c("counts")
#Calculate in for loop and write to blank matrix
for (a in affil_cat){
  subset <- filter(data.biblio, Affil_type == a)
  affil_counts[a,1] <- n_distinct(subset$aid)
}
#Remove rownames and reformat data types
affil_counts <- as.data.frame(affil_counts)
acounts <- as.numeric(as.vector(affil_counts$counts))

##Publications by year
#years <- as.character(c(1970:2014))
#Create blank data matrix with labeled rows and columns
#year_counts <- matrix(nrow=45, ncol=1)
#rownames(year_counts) <- years
#colnames(year_counts) <- c("counts")
3data.biblio$Pub_year <- as.character(data.biblio$Pub_year)
#Calculate in for loop and write to blank matrix
#for (y in years){
#  subset <- filter(data.biblio, Pub_year == y)
#  year_counts[y,1] <- n_distinct(subset$aid)
#}
#Remove rownames and reformat data types
#year_counts <- as.data.frame(year_counts)
#ycounts <- as.numeric(as.vector(year_counts$counts))
year_counts<-read.csv("Year_Counts_Total_IE.csv",header=TRUE)
rownames(year_counts) <- year_counts$X
year_counts <- select(year_counts,-X)
years <- c("1990-1994","1995-1999","2000-2004","2005-2009","2010-2014")
ycounts <- as.matrix(year_counts)
colnames(ycounts) <- NULL

##Author affiiation
affil_cat <- c("Academic", "Pub_sec", "Res_int", "Cons", "Non_prof", "Priv_sec")
#Create blank data matrix with labeled rows and columns
affil_counts <- matrix(nrow=6, ncol=1)
rownames(affil_counts) <- affil_cat
colnames(affil_counts) <- c("counts")
#Calculate in for loop and write to blank matrix
for (a in affil_cat){
  subset <- filter(data.biblio, Affil_type == a)
  affil_counts[a,1] <- n_distinct(subset$aid)
}
#Remove rownames and reformat data types
affil_counts <- as.data.frame(affil_counts)
acounts <- as.numeric(as.vector(affil_counts$counts))
affil_labels <- c("Academic", "Public sector", "Research institute", "Consultant", "Non-profit", "Private sector")

#Create barplots in PDFs
end_point = 0.5 + nrow(pub_counts) + nrow(pub_counts)-1
end_point_2 = 0.5 + nrow(affil_counts) + nrow(affil_counts)-1
end_point_3 = 0.5 + ncol(year_counts) + ncol(year_counts)-1
pdf(file="Summary_Year.pdf")
layout(matrix(c(1,1,2,3), 2, 2, byrow=TRUE))
barplot(ycounts, main="Publications by year", axes=TRUE, ylim = c(0,600), ylab = "Number of studies", xlab = "", col=c("black","white"), angle=45, border=TRUE, space=1)
text(seq(1.5,end_point_3,by=2), par("usr")[3]-0.25, srt=0, pos=1, xpd=TRUE, labels = paste(years), cex=0.65)
box()
barplot(pcounts, main="Publication type", axes=TRUE, ylim = c(0,5+max(pcounts)), ylab = "Number of studies", xlab = "", col="purple", border=NA, space=1)
text(seq(1.5,end_point,by=2), par("usr")[3]-0.25, srt=60, adj=1, xpd=TRUE, labels = paste(pub_cat), cex=0.65)
box()
barplot(acounts, main="Affiliation of first author", axes=TRUE, ylim = c(0,5+max(acounts)), ylab = "Number of studies", xlab = "", col="purple", angle=45, border=NA, space=1)
text(seq(1.5,end_point_2,by=2), par("usr")[3]-0.25, srt=60, adj=1, xpd=TRUE, labels = paste(affil_labels), cex=0.65)
box()
dev.off()

##Summarize intervention and outcome types
#Intervention types
int_type = c("area_protect", "area_mgmt", "sp_control", "restoration", "res_mgmt", "sp_mgmt", "sp_recov", "sp_reint", "ex_situ", "form_ed", "training", "aware_comm", "legis", "pol_reg", "priv_codes", "compl_enfor", "liv_alt", "sub", "market", "non_mon", "inst_civ_dev", "part_dev", "cons_fin", "sus_use", "other")
group_type = c("area_protect", "res_mgmt", "land_wat_mgmt", "species_mgmt", "education", "law_policy", "liv_eco_inc", "ext_cap_build", "sus_use", "other")
#Create blank data matrix with labeled rows and columns
int_counts <- matrix(nrow=25, ncol=2)
rownames(int_counts) <- int_type
colnames(int_counts) <- c("Int_type","counts")
#Calculate in for loop and write to blank matrix
for (i in int_type){
  subset <- filter(data.interv, Int_type == i)
  int_counts[i,1] <- i
  int_counts[i,2] <- n_distinct(subset$aid)
}
#Remove rownames and reformat data types
int_counts <- as.data.frame(int_counts)
icounts <- as.numeric(as.vector(int_counts$counts))
int_labels = c("Area protection", "Area management", "Species control", "Restoration", "Resource management/protection", "Species management", "Species recovery", "Species reintroduction", "Ex-situ conservation", "Formal education", "Training", "Awareness & Communications", "Legislation", "Policies & Regulations", "Private sector standards and codes", "Compliance & enforcement", "Enterprises & livelihood alternatives", "Substitution", "Market-based forces", "Non-monetary values", "Institutional & civil society development", "Alliance & partnership development", "Conservation finance", "Sustainable use", "Other")
group_labels = c("Area protection", "Resource protection/management","Land/Water management",  "Species management", "Education", "Law & Policy", "Livelihood, economic & other incentives", "External capacity building", "Sustainable use", "Other")
pdf(file="Summary_int_subcat.pdf")
par(mar = c(7, 4, 2, 2) + 0.2)
barplot(as.numeric(icounts),horiz=TRUE,xlim=c(0,450),names.arg=int_labels,las=1,xlab="Number of studies by IUCN-CMP intervention subcategories",cex.axis=1.5)
box()
dev.off()


write.csv(int_counts,"int_counts.csv")
int_groups <- read.csv("int_counts.csv",header=TRUE)

#Assign to groups
#rows <- c(1:nrow(int_counts))
#int_groups <- matrix(nrow=nrow(int_counts),ncol=1)
#rownames(int_groups) <- rows
#colnames(int_groups) <- c("int_group")

#Assign intervention groups ##NOT WORKING - wrote to CSV, manually added and read back in
for (i in rows){
  int <- int_counts$Int_type[i]
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
int_groups_final <- matrix(nrow=10,ncol=1)
rownames(int_groups_final) <- group_type
colnames(int_groups_final) <- c("counts")

for (g in group_type){
  subset <- filter(int_groups, int_group == g)
  int_groups_final[g,1] <- sum(subset$counts)
}
int_groups_final <- as.data.frame(int_groups_final)
ig_counts <- as.numeric(as.vector(int_groups_final$counts))

#Create barplot
pdf(file="Summary_interventions_groups.pdf")
par(mar = c(7, 4, 2, 2) + 0.2)
barplot(as.matrix(int_groups),horiz=TRUE,xlim=c(0,500),names.arg=c("Area protection","Land/Water management", "Resource protection/management", "Species management", "Education", "Law & Policy", "Livelihood, economic & other incentives", "External capacity building", "Sustainable use", "Other"),las=1,xlab="Number of studies by intervention group",cex.axis=1.5)
box()
dev.off()

#Outcome types
out_type = c("env", "mat_liv_std", "eco_liv_std", "health", "education", "soc_rel", "sec_saf", "gov", "sub_well", "culture", "free_choice", "other")
#Create blank data matrix with labeled rows and columns
out_counts <- matrix(nrow=12, ncol=1)
rownames(out_counts) <- out_type
colnames(out_counts) <- c("counts")
#Calculate in for loop and write to blank matrix
for (o in out_type){
  subset <- filter(data.outcome, Outcome == o)
  out_counts[o,1] <- n_distinct(subset$aid)
}
#Remove rownames and reformat data types
out_counts <- as.data.frame(out_counts)
out_counts_noenv <- slice(out_counts,2:12)
rownames(out_counts_noenv) <- out_type[2:12]
ocounts <- as.numeric(as.vector(out_counts$counts))
ocounts2 <- as.numeric(as.vector(out_counts_noenv$counts))
out_labels = c("Environmental", "Material living standards", "Economic living standards", "Health", "Education", "Social relations", "Security & safety", "Governance & empowerment", "Subjective well-being", "Culture & Spiritual", "Freedom of choice/action", "Other")
out_labels2 = c("Material living standards", "Economic living standards", "Health", "Education", "Social relations", "Security & safety", "Governance & empowerment", "Subjective well-being", "Culture & Spiritual", "Freedom of choice/action", "Other")
ocounts2 <- t(out_counts_noenv)
#Create barplot
pdf(file="Summary_outcomes.pdf")
par(mar = c(7, 4, 2, 2) + 0.2)
barplot(as.numeric(ocounts2),horiz=TRUE,xlim=c(0,700),names.arg=out_labels2,las=1,xlab="Number of studies by domain of human well-being",cex.axis=1.5)
box()
dev.off()

##Heatmap of incidences between intervention + HWB that have studies included in a systematic review
#Load required packages
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

##Systematic reviews - bind
sysrev <- select(data.pathways,aid,Part_sys_rev)
int_out_sys_final <- left_join(int_out_final,sysrev,by="aid")
int_out_sys_final <- distinct(int_out_sys_final)
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


#Calculate number of studies for each linkage cell between intervention and outcome
#Create blank data matrix with labeled rows and columns
io_counts = matrix(nrow=11, ncol=10)
rownames(io_counts) <- out_type
colnames(io_counts) <- int_groups_type
#Calculate in for loop and write to blank matrix
for (i in int_groups_type){
  for (j in out_type){
    subset <- distinct(filter(int_out_final, Outcome == j, int_group == i))
    io_counts[j,i] <- n_distinct(subset$aid)
  }
}
#Relabel rows and columns
rownames(io_counts) <- out_labels
colnames(io_counts) <- int_group_labels
#Write heatmap and legend to PDF
pdf(file="Interventions_Outcomes_Condensed_No_Models_Heatmap.pdf", width=11, height=8.5)
heatmap.2(io_counts, Colv=NA, dendrogram="none", col=palette_final, cellnote=io_counts, notecol="black", notecex=1.5, trace="none", cexRow=1.5, cexCol=1.5, key=TRUE, Rowv=NA)
dev.off()

#Create blank data matrix with labeled rows and columns for NON-CONDENSED
int_type = c("area_protect", "area_mgmt", "res_mgmt", "sp_control", "restoration", "sp_mgmt", "sp_recov", "sp_reint", "ex_situ", "form_ed", "training", "aware_comm", "legis", "pol_reg", "priv_codes", "compl_enfor", "liv_alt", "sub", "market", "non_mon", "inst_civ_dev", "part_dev", "cons_fin", "sus_use", "other")
out_type = c("env", "mat_liv_std", "eco_liv_std", "health", "education", "soc_rel", "sec_saf", "gov", "sub_well", "culture", "free_choice", "other")
io_counts = matrix(nrow=12, ncol=25)
rownames(io_counts) <- out_type
colnames(io_counts) <- int_type
#Calculate number of unique studies for each linkage cell between intervention and outcome
#Calculate in for loop and write to blank matrix
for (i in int_type){
  for (j in out_type){
    subset <- filter(int_out, Outcome == j, Int_type == i)
    io_counts[j,i] <- n_distinct(subset$aid)
  }
}
#Relabel rows and columns
int_labels = c("Area protection", "Area management", "Resource management/protection", "Species control", "Restoration", "Species management", "Species recovery", "Species reintroduction", "Ex-situ conservation", "Formal education", "Training", "Awareness & Communications", "Legislation", "Policies & Regulations", "Private sector standards and codes", "Compliance & enforcement", "Enterprises & livelihood alternatives", "Substitution", "Market-based forces", "Non-monetary values", "Institutional & civil society development", "Alliance & partnership development", "Conservation finance", "Sustainable use", "Other")
out_labels = c("Environmental", "Material living standards", "Economic living standards", "Health", "Education", "Social relations", "Security & safety", "Governance & empowerment", "Subjective well-being", "Culture & Spiritual", "Freedom of choice/action", "Other")
rownames(io_counts) <- out_labels
colnames(io_counts) <- int_labels
#Define color palette for heatmap
palette <- colorRampPalette(c("red", "yellow", "green")) (n=299)
palette1 <- colorRampPalette(c("#ffffcc", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")) (n=299)
palette2 <- colorRampPalette(c("#762a83", "#af8dc3", "#c7eae5", "#5ab4ac", "#01665e")) (n=299)
palette3 <- colorRampPalette(c("#b2182b", "#ef8a62", "#fddbc7", "#f7f7f7", "#d1e5f0", "#67a9cf", "#2166ac")) (n=299)
palette4 <- colorRampPalette(c("#cef3f7", "#9ce7ee", "#39cedd", "#3796a0", "#04616a", "#023035")) (n=299)
palette5 <- colorRampPalette(c("#e66101", "#fdb863", "#f7f7f7","#f7f7f7", "#b2abd2", "#5e3c99")) (n=50)
palette6 <- colorRampPalette(c("#636363", "#CCCCCC", "#f7f7f7","#f7f7f7", "#80cdc1", "#018571")) (n=50)

#Write heatmap and legend to PDF
pdf(file="Interventions_Outcomes_Heatmap_full_impact_eval.pdf", width=11, height=8.5)
heatmap.2(io_counts, Colv=NA, dendrogram="none", col=palette_final, cellnote=io_counts, notecol="black", notecex=1.0, trace="none", cexRow=1.5, cexCol=1.5, key=TRUE, Rowv=NA)
dev.off()

##Heatmap by intervention group and biome occurence
#
int_out_b <- left_join(data.interv, data.outcome,by = "aid")
int_out_b <- distinct(int_out_b)
int_out_b <- left_join(int_out_b,data.biomes,by="aid")
int_out_b <- select(int_out_b,aid,Int_type,Outcome,Biome.)
int_out_b <- distinct(int_out_b)

#Assign intervention groups
rows <- c(1:nrow(int_out_b))
int_groups <- matrix(nrow=nrow(int_out_b),ncol=1)
rownames(int_groups) <- rows
colnames(int_groups) <- c("int_group")
for (i in rows){
  int <- int_out_b$Int_type[i]
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
int_out_b_final <- bind_cols(int_out_b,int_groups)
int_out_b_final <- filter(int_out_b_final,!is.na(int_groups))

#Create blank data matrix with labeled rows and columns
int_groups_type = c("area_protect", "land_wat_mgmt", "res_mgmt", "species_mgmt", "education", "law_policy", "liv_eco_inc", "ext_cap_build", "sus_use", "other")
out_type = c("eco_liv_std", "mat_liv_std", "health", "education", "soc_rel", "sec_saf", "gov", "sub_well", "culture", "free_choice", "other")
iobt_counts = matrix(nrow=11, ncol=10)
rownames(iobt_counts) <- out_type
colnames(iobt_counts) <- int_groups_type
iobm_counts = matrix(nrow=11, ncol=10)
rownames(iobm_counts) <- out_type
colnames(iobm_counts) <- int_groups_type
iobf_counts = matrix(nrow=11, ncol=10)
rownames(iobf_counts) <- out_type
colnames(iobf_counts) <- int_groups_type
iobfo_counts = matrix(nrow=11, ncol=10)
rownames(iobfo_counts) <- out_type
colnames(iobfo_counts) <- int_groups_type

terrestrial <- c("T_TSTMBF", "T_TSTDBF", "T_TSTCF", "T_TBMF", "T_TCF", "T_BFT", "T_TSTGSS", "T_TGSS", "T_FGS", "T_MGS", "T_T", "T_MFWS", "T_DXS", "T_M")
forests <- c("T_TSTMBF", "T_TSTDBF", "T_TSTCF", "T_TBMF", "T_TCF", "T_BFT", "T_MFWS")
freshwater <- c("FW_LL", "FW_SL", "FW_LR", "FW_LRH", "FW_LRD", "FW_SR", "FW_XB")
marine <- c("M_P", "M_TSS", "M_TU", "M_TRU", "M_TRC", "M_TSTSS")

#Calculate biome coutns for each linkage in blank matrix
for (i in int_groups_type){
  for (j in out_type){
    subset <- distinct(filter(int_out_b_final, Outcome == j, int_group == i))
    ter <- distinct(filter(subset, Biome. %in% terrestrial))
    mar <- distinct(filter(subset, Biome. %in% marine))
    fre <- distinct(filter(subset, Biome. %in% freshwater))
    fst <- distinct(filter(subset, Biome. %in% forests))
    t_count <- n_distinct(ter$aid)
    m_count <- n_distinct(mar$aid)
    f_count <- n_distinct(fre$aid)
    fo_count <- n_distinct(fst$aid)
    iobt_counts[j,i] <- as.numeric(as.vector(t_count))
    iobm_counts[j,i] <- as.numeric(as.vector(m_count))
    iobf_counts[j,i] <- as.numeric(as.vector(f_count))
    iobfo_counts[j,i] <- as.numeric(as.vector(fo_count))
  }
}

#Relabel rows and columns
int_group_labels = c("Area protection", "Land/Water Management", "Resource management/protection", "Species Management", "Education", "Law & Policy", "Livelihood, Economic & Other Incentives", "External Capacity Building", "Sustainable use", "Other")
out_labels = c("Economic living standards", "Material living standards", "Health", "Education", "Social relations", "Security & safety", "Governance & empowerment", "Subjective well-being", "Culture & Spiritual", "Freedom of choice/action", "Other")
rownames(iobt_counts) <- out_labels
colnames(iobt_counts) <- int_group_labels
rownames(iobm_counts) <- out_labels
colnames(iobm_counts) <- int_group_labels
rownames(iobf_counts) <- out_labels
colnames(iobf_counts) <- int_group_labels
rownames(iobfo_counts) <- out_labels
colnames(iobfo_counts) <- int_group_labels

#Define color palette for heatmap
palette_f <- colorRampPalette(c("#ffffd4", "#fed98e", "#fe9929", "#d95f0e", "#993404")) (n=299)
palette_m <- colorRampPalette(c("#f1eef6", "#bdc9e1", "#74a9cf", "#2b8cbe", "#045a8d")) (n=299)
palette_t <- colorRampPalette(c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")) (n=299)

#Write heatmap and legend to PDF
pdf(file="Interventions_Outcomes_T_biomes_Heatmap.pdf", width=11, height=8.5)
heatmap.2(iobt_counts, Colv=NA, dendrogram="none", col=palette_t, cellnote=iobt_counts, notecol="black", notecex=1.0, trace="none", cexRow=1.5, cexCol=1.5, key=TRUE, Rowv=NA)
dev.off()
pdf(file="Interventions_Outcomes_Forests_biomes_Heatmap.pdf", width=11, height=8.5)
heatmap.2(iobfo_counts, Colv=NA, dendrogram="none", col=palette_t, cellnote=iobfo_counts, notecol="black", notecex=1.0, trace="none", cexRow=1.5, cexCol=1.5, key=TRUE, Rowv=NA)
dev.off()
pdf(file="Interventions_Outcomes_M_biomes_Heatmap.pdf", width=11, height=8.5)
heatmap.2(iobm_counts, Colv=NA, dendrogram="none", col=palette_m, cellnote=iobm_counts, notecol="black", notecex=1.0, trace="none", cexRow=1.5, cexCol=1.5, key=TRUE, Rowv=NA)
dev.off()
pdf(file="Interventions_Outcomes_F_biomes_Heatmap.pdf", width=11, height=8.5)
heatmap.2(iobf_counts, Colv=NA, dendrogram="none", col=palette_f, cellnote=iobf_counts, notecol="black", notecex=1.0, trace="none", cexRow=1.5, cexCol=1.5, key=TRUE, Rowv=NA)
dev.off()

##Heatmap of incidences between intervention + biomes
#Load required packages
library(gplots)
library(RColorBrewer)
#Create new dataframe with intervention and biome data
int_biome <- left_join(data.interv, data.biomes, by = "aid")
int_biome <- select(int_biome,aid,Int_type,Biome.)
#Create blank data matrix with labeled rows and columns
int_type = c("area_protect", "area_mgmt", "res_mgmt", "sp_control", "restoration", "sp_mgmt", "sp_recov", "sp_reint", "ex_situ", "form_ed", "training", "aware_comm", "legis", "pol_reg", "priv_codes", "compl_enfor", "liv_alt", "sub", "market", "non_mon", "inst_civ_dev", "part_dev", "cons_fin", "sus_use", "other")
biome_type = c("T_TSTMBF", "T_TSTDBF", "T_TSTCF", "T_TBMF", "T_TCF", "T_BFT", "T_TSTGSS", "T_TGSS", "T_FGS", "T_MGS", "T_T", "T_MFWS", "T_DXS", "T_M", "FW_LL", "FW_SL", "FW_LR", "FW_LRH", "FW_LRD", "FW_SR", "FW_XB", "M_P", "M_TSS", "M_TU", "M_TRU", "M_TRC", "M_TSTSS")
ib_counts = matrix(nrow=27, ncol=25)
rownames(ib_counts) <- biome_type
colnames(ib_counts) <- int_type
#Calculate number of unique studies for each linkage cell between intervention and outcome
#Calculate in for loop and write to blank matrix
for (i in int_type){
  for (j in biome_type){
    subset <- filter(int_biome, Biome. == j, Int_type == i)
    ib_counts[j,i] <- n_distinct(subset$aid)
  }
}
#Relabel rows and columns
int_labels = c("Area protection", "Area management", "Resource management/protection", "Species control", "Restoration", "Species management", "Species recovery", "Species reintroduction", "Ex-situ conservation", "Formal education", "Training", "Awareness & Communications", "Legislation", "Policies & Regulations", "Private sector standards and codes", "Compliance & enforcement", "Enterprises & livelihood alternatives", "Substitution", "Market-based forces", "Non-monetary values", "Institutional & civil society development", "Alliance & partnership development", "Conservation finance", "Sustainable use", "Other")
biome_labels = c("Tropical/sub-tropical moist broadleaf forests", "Tropical/sub-tropical dry broadleaf forests", "Tropical/sub-tropical coniferous forests", "Temperate broadleaf and mixed forests", "Temperate coniferous forests", "Boreal forests & taiga", "Tropical/sub-tropical grasslands, savannas & shrublands", "Temperate grasslands, savannas & shrublands", "Flooded grasslands and savannas", "Montane grasslands & shrublands", "Tundra", "Mediterranean forests, woodlands & scrubs", "Deserts & xeric shrublands", "Mangroves", "Large lakes", "Small lakes", "Large rivers", "Large river headwaters", "Large river deltas", "Small rivers", "Xeric basins", "Marine-polar", "Marine-temperate shelfs and seas", "Marine-temperate upwelling", "Marine-tropical upwelling", "Marine-tropical corals", "Marine-tropical shelfs and seas")
rownames(ib_counts) <- biome_labels
colnames(ib_counts) <- int_labels
#Define color palette for heatmap
palette <- colorRampPalette(c("white", "paleturquoise1", "paleturquoise3", "turquoise2", "turquoise4", "darkslategray4", "darkslategray")) (n=299)
#Write heatmap and legend to PDF
pdf(file="Interventions_Biomes_Heatmap.pdf", width=11, height=8.5)
heatmap.2(ib_counts, Colv=NA, dendrogram="none", col=palette, cellnote=ib_counts, notecol="black", notecex=1.0, trace="none", cexRow=1.5, cexCol=1.5, key=TRUE, Rowv=NA)
dev.off()

##Heatmap of incidences between intervention + HWB w/ conceptual model
#Load required packages
library(gplots)
library(RColorBrewer)
#Create new dataframe with intervention and outcome data
concept <- select(data.pathways,aid,Concept_mod)
int_out_c <- left_join(data.interv, data.outcome,by = "aid")
int_out_c <- distinct(int_out_c)
int_out_c <- left_join(int_out_c,concept,by="aid")
int_out_c <- distinct(int_out_c)
int_out_c <- select(int_out_c,aid,Int_type,Outcome,Concept_mod)
#Create blank data matrix with labeled rows and columns
int_type = c("area_protect", "area_mgmt", "res_mgmt", "sp_control", "restoration", "sp_mgmt", "sp_recov", "sp_reint", "ex_situ", "form_ed", "training", "aware_comm", "legis", "pol_reg", "priv_codes", "compl_enfor", "liv_alt", "sub", "market", "non_mon", "inst_civ_dev", "part_dev", "cons_fin", "sus_use", "other")
out_type = c("env", "mat_liv_std", "eco_liv_std", "health", "education", "soc_rel", "sec_saf", "gov", "sub_well", "culture", "free_choice", "other")
ioc_counts = matrix(nrow=12, ncol=25)
rownames(ioc_counts) <- out_type
colnames(ioc_counts) <- int_type
#Calculate number of unique studies for each linkage cell between intervention and outcome
#Calculate in for loop and write to blank matrix
for (i in int_type){
  for (j in out_type){
    subset <- filter(int_out_c, Outcome == j, Int_type == i)
    subset_c <- filter(int_out_c, Outcome == j, Int_type == i, Concept_mod == 1)
    percent <- (n_distinct(subset_c$aid) / n_distinct(subset$aid))*100
    ioc_counts[j,i] <- percent
  }
}
#Relabel rows and columns
int_labels = c("Area protection", "Area management", "Resource management/protection", "Species control", "Restoration", "Species management", "Species recovery", "Species reintroduction", "Ex-situ conservation", "Formal education", "Training", "Awareness & Communications", "Legislation", "Policies & Regulations", "Private sector standards and codes", "Compliance & enforcement", "Enterprises & livelihood alternatives", "Substitution", "Market-based forces", "Non-monetary values", "Institutional & civil society development", "Alliance & partnership development", "Conservation finance", "Sustainable use", "Other")
out_labels = c("Environmental", "Material living standards", "Economic living standards", "Health", "Education", "Social relations", "Security & safety", "Governance & empowerment", "Subjective well-being", "Culture & Spiritual", "Freedom of choice/action", "Other")
rownames(ioc_counts) <- out_labels
colnames(ioc_counts) <- int_labels
ioc_counts <- round(ioc_counts, digits=1)
#Define color palette for heatmap
palette <- colorRampPalette(c("white", "paleturquoise1", "paleturquoise3", "turquoise2", "turquoise4", "darkslategray4", "darkslategray")) (n=299)
#Write heatmap and legend to PDF
pdf(file="Interventions_Outcomes_wConcept_Heatmap.pdf", width=11, height=8.5)
heatmap.2(ioc_counts, Colv=NA, dendrogram="none", col=palette, cellnote=ioc_counts, notecol="black", notecex=1.0, trace="none", cexRow=1.5, cexCol=1.5, key=TRUE, Rowv=NA)
dev.off()

##Conceptual models
concept <- distinct(select(data.pathways,aid,Concept_mod))
mod <- count(concept,Concept_mod)
model <- as.numeric(as.vector(mod[2,2]))
none <- as.numeric(as.vector(mod[1,2]))
unk <- as.numeric(as.vector(mod[3,2]))
slices <- c(model,unk,none)
lbls <- c("Conceptual model", "Unknown", "None")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls,"(",pct,sep="")
lbls <- paste(lbls,"%)",sep="")
pdf(file="Conceptual_models.pdf",width=11, height=8.5)
pie(slices,labels=lbls,main="Studies employing conceptual models")
dev.off()

##Common models
models <- select(data.pathways,aid,Concept_mod,Concept_mod_name)
models <- filter(models,Concept_mod == 1)
models <- distinct(models)
model_types <- count(models,Concept_mod_name)
model_types <- arrange(model_types,desc(n))
total <- as.numeric(sum(model_types$n))
model_types <- mutate(model_types, percent=n/total*100)
model_types$percent <- round(model_types$percent, digits=1)
write.csv(model_types,"Types of conceptual models.csv")
##Equity
equity <- distinct(select(data.outcome,aid,Equity))
counts <- count(equity,Equity == 1)
yes <- as.numeric(counts[2,2])
no <- as.numeric(counts[1,2])
slices <- c(yes,no)
lbls <- c("Yes", "No")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, "(",pct,sep="")
lbls <- paste(lbls,"%)",sep="")
pdf(file="Equity.pdf",width=11, height=8.5)
pie(slices,labels=lbls,main="Studies considering some element of equity")
dev.off()

##Elements of equity
equity_elm <- select(data.outcome,aid,Equity,Equity_cat)
separate(equity_elm,Equity_cat,sep=",")

##Study objectives
study_obj <- select(data.study,aid,Study_obj.env,Study_obj.soc,Study_obj.econ)
study_obj <- distinct(study_obj)
colnames(study_obj) <- c("aid","env","soc","econ")

##Average number of outcomes measured per study
subset <- select(data.outcome,aid,Outcome)
subset <- distinct(subset)
avgs <- count(subset,aid)
avg_outcome <- mean(avgs$n)
st_dev <- sd(avgs$n)
