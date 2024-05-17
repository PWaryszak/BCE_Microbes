#RAW DATA:=======
#Load Libraries and Notes:
if (!require(ggpubr)) install.packages('ggpubr')
if (!require(scales)) install.packages('scales')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(readxl)) install.packages('readxl')

library(tidyverse)
library (ggpubr)
library(readxl)
library(scales)



#GET TAXA from MASTER DATA:================
MASTER_DATA<- read_excel("DataAnalysis_BlueBugs2022_MASTERFILE.xlsx", sheet = "clean_data") #Get  origin dataset  to get ecosystem data
# From BlueBug01" to BlueBug63" - this column "Microbial_taxa_extracted_clean" split by comma into single taxa names
#TSD = taxonomic species description. TSD if present (to be skipped), "Normal" to keep for taxa analysis


#Turn wide taxa part of data  into taxa_long for matching taxa records with phyla next:
df <- MASTER_DATA %>%
      select(StudyID_Cov,Covidence_ID_Number,Microbial_taxa_extracted_clean_split: BlueBug63 ) %>%
      mutate_if(is.character, as.factor)  #convert all columns from character to factor for further data wrangling 

nrow(df)# 653 papers total including TSD(Taxonomic Species Description) papers


#turn wide to long format of taxa to do a global search of phyla:
taxa_long <- gather(df, key = "BlueBug", value = "SeekPhyla", Microbial_taxa_extracted_clean_split:BlueBug63, na.rm=T) %>% #Turning single-species BluBug columns into one "SeekPhyla" column
             group_by(StudyID_Cov, Covidence_ID_Number,SeekPhyla) %>% #way to remove duplicated Phyla records
             summarise(SeekPhylaCount = n())

  
names(taxa_long)#"StudyID_Cov" "BlueBug"     "SeekPhyla"  
dim(taxa_long) #2372    4



#LOAD Phyla dataset:=========
phyla1 <- read_excel("DataAnalysis_BlueBugs2022_MASTERFILE.xlsx", sheet = "PhylaFromReview") #Get clean CSM
names(phyla1)#"Phyla" "Group" "Comment"
P1 <- as.data.frame( phyla1[,1:2])  #P1 data from our literature review contains 71 phylas.
P1

phyla2 <- read_excel("Ludwig2021_PHYLA_Dataset_LTP_06_2022.xlsx", sheet = "Table S1 number of taxa in LTP",skip=2) #Get phyla from paper by Ludwig 2021:
P2 <- as.data.frame(phyla2[1:39,1] )  #Total of 39 Phylas from Ludwig2021 had no hits in our data. I combined it with phyla present in our reveiw. For example Proteobacteria not present in Ludwig2021
colnames(P2)[ 1]<-"Phyla" #Rename column to match P1 for rbind function
P2$Group <- "Bacteria"

P1P2 <- rbind (P1,P2)# combining phyla lists from Ludwig2021 and our own phyla-dataset from the review
names(P1P2)#"Phyla" "Group"
dim(P1P2)# 110   2

P1P2_clean <- P1P2  %>%
      filter ( Phyla != "Deinococcus-Thermus") %>% # Merge that phyla into Deinococcota that already exists in the dataset
      distinct(Phyla) #remove duplicated Phyla

dim(P1P2_clean)#93  1



taxa2 <- taxa_long %>%   #Use P1P2_clean phyla list to loop over SeekPhyla Column for the Phyla hits
  mutate(Phyla_Hits = if_else(SeekPhyla %in% P1P2_clean $ Phyla, SeekPhyla, "NONE")) #Fill Phyla_Hits with phyla if hit.


#Create Group_Hits with  Groups: bacteria, fungi, archaea if hit in taxa2
taxa3 <- taxa2 %>%
  mutate(Group_Hits = if_else(Phyla_Hits %in% P1P2$Phyla, P1P2$Group[match(Phyla_Hits, P1P2$Phyla)], "NONE"))

#Get phyla to match functions in microbial_functions_Blue bugs.xlsx":
names(taxa3)
phyla2match <- taxa3 %>% 
  select(Phyla_Hits,Group_Hits) %>%
  mutate(bacteria_fungi_archaea = Group_Hits )
View(phyla2match)

#archaea
archaea2match <- read_excel("microbial_functions_Blue bugs.xlsx", sheet = "archaea") 
archaea_merged <- left_join(archaea2match, phyla2match, by = "Covidence_ID_Number")
View(archaea_merged)
write.csv(archaea_merged, file = "archaea_merged.csv", row.names = F)

#bacteria:
bacteria2match <- read_excel("microbial_functions_Blue bugs.xlsx", sheet = "bacteria")
bacteria_merged <- left_join(bacteria2match, phyla2match, by = "Covidence_ID_Number")
View(bacteria_merged)
write.csv(bacteria_merged, file = "bacteria_merged.csv", row.names = F)

#fungi:
fungi2match <- read_excel("microbial_functions_Blue bugs.xlsx", sheet = "fungi")
head(fungi2match)
fungi_merged <- left_join(fungi2match, phyla2match, by = "Covidence_ID_Number")
View(fungi_merged)
write.csv(fungi_merged, file = "fungi_merged.csv", row.names = F)


