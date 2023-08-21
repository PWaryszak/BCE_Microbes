#RAW DATA Cleaning:=======

#Load Libraries and Notes:
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(RColorBrewer)
library(lme4)
library(vegan)

#Pawel continues to clean taxa names in "Taxa_DATA_ALL_EXTRACTED.xlsx" file:
#Link3GDRIVE: https://docs.google.com/spreadsheets/d/1yICgLcTXEYdZul7Hto4U4CA0xgr34vNO/edit?usp=drive_link&ouid=100881671914675049849&rtpof=true&sd=true

taxa <- read_excel("Taxa_DATA_ALL_EXTRACTED.xlsx", sheet = "TaxaDATA_CLEAN") #Get dataset cleaned (21-Aug-2023) on Google Drive here:
df <- taxa %>% mutate_if(is.character, as.factor) #convert all columns from character to factor for further manipulations
names(df)
#[1] "Covidence #"                        
#[2] "Study ID (orignal, pink=duplicated)"
#[3] "Study_ID (no space)"                
#[4] "StudyID_Cov"                        
#[5] "Microbial richness"                 
#[6] "Microbial taxa reported/assessed"   
#[7] "Microbial taxa extracted"           
#[8] "Microbial_taxa_extracted_clean"  
# From BlueBug01" to BlueBug65" - this column "Microbial_taxa_extracted_clean" split by comma into single taxa names

#LOAD Phyla dataset:
phyla1 <- read_excel("Taxa_DATA_ALL_EXTRACTED.xlsx", sheet = "PhylaFromReview") #Get clean CSM
phyla2 <- read_excel("Ludwig2021_PHYLA_Dataset_LTP_06_2022.xlsx", sheet = "Table S1 number of taxa in LTP",skip=2) #Get phyla from paper by Ludwig 2021:
P2 <- phyla2[,1] #PHyla from Ludwig2021 had no hits in our data. I had to combine it with P1. For example Proteobacteria
colnames(P2)[1]<-"Phyla"
P1 <- phyla1[,1] #P1 data from our literature review.
P1P2 <- rbind (P1,P2)#Hybrid phyla list, combining Ludwig2021 and our own dataset.


taxa_long <- gather(df, key = "BlueBug", value = "SeekPhyla", BlueBug01:BlueBug65, na.rm=T) #Turning 64 BluBug columns into "SeekPhyla" column
View(taxa_long)
dim(taxa_long) #2887   10


taxa2 <- taxa_long %>%
  mutate(P = if_else(SeekPhyla %in% P1P2 $ Phyla, SeekPhyla, "NOPE"))##Fill new "P" with Phyla if present in "SeekPhyla" column:
dim(taxa2)


taxa2spread <- taxa2 %>% group_by(P) %>%
  summarise(n = n())
  
View(taxa2spread)
head(taxa2spread) 
# P                  n
# Acidobacteria     88

yes<- taxa2spread %>% filter(P!="NOPE")
yes_sum <- sum(yes$n)

no <- taxa2spread %>% filter(P=="NOPE")
no_sum <- sum(no$n)
NOPE <- no_sum/ (yes_sum + no_sum) *100
round(NOPE,0) #60% of NO PHYLA info


################
#Stopped here on 21-Aug-2023
####################
#Merge treat and community matrix by StudyID and run NMDS






#NMDS====
#Get species matric (Phyla as columns)
taxa_nmds <- taxa2 %>% 
  group_by(StudyID_Cov, P) %>%
  summarise(n = n()) %>%
  spread(P,n, fill=0)

#Get ecosystem as grouping factor:
eco <- read_excel("DataAnalysis_BlueBugs2022.xlsx", sheet = "clean_data") #Get  origin dataset  to get ecosystem data

community_matrix <- taxa_nmds[ ,2:65] #Get the community matrix of Phyla
View(community_matrix)

#Get
treat <- as.data.frame(eco$Ecosystem) #Get the ecosystem as grouping variable.

#Run NMDS:
example_NMDS <- metaMDS(community_matrix,k=2,trymax=10)

#Plot NMDS:
ordiplot(example_NMDS)
ordihull(example_NMDS,groups=treat,draw="polygon",col="grey90",label=F)
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",col=c(rep("green",5),rep("blue",5)),
   air=0.01,cex=1.25)
