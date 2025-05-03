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
      filter(TSD != "TSD") %>% #TSD RECORDS ARE REMOVED (Taxonomic Species Description)
      select(StudyID_Cov,Microbial_taxa_extracted_clean_split: BlueBug63 ) %>%
      mutate_if(is.character, as.factor)  #convert all columns from character to factor for further data wrangling 

nrow(df)#490 papers total after removing TSD papers


#turn wide to long format of taxa to do a global search of phyla:
taxa_long <- gather(df, key = "BlueBug", value = "SeekPhyla", Microbial_taxa_extracted_clean_split:BlueBug63, na.rm=T) %>% #Turning single-species BluBug columns into one "SeekPhyla" column
             group_by(StudyID_Cov, SeekPhyla) %>% #way to remove duplicated Phyla records
             summarise(SeekPhylaCount = n())

  
names(taxa_long)#"StudyID_Cov" "BlueBug"     "SeekPhyla"  
dim(taxa_long) #2046    3



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


ecosytems <-  MASTER_DATA[ , c("StudyID_Cov", "Ecosystem")] #Get ecosystem type from the MASTER_DATA


taxa3_ecosystem <- left_join( taxa3,ecosytems, by = "StudyID_Cov") %>%  #merge ecosystem type with taxa3
   filter(Ecosystem == "saltmarsh" | Ecosystem == "mangrove" | Ecosystem == "seagrass" ) %>%  #Get these 3 only
   filter(Phyla_Hits != "NONE" ) %>%
   filter(Phyla_Hits != "NA")

unique(taxa3_ecosystem$Ecosystem)# "saltmarsh" "mangrove"  "seagrass" 
unique(taxa3_ecosystem$Phyla_Hits)#67 Phyla
Total_Papers <- length(unique(taxa3_ecosystem$StudyID_Cov))
Total_Papers  #480 = our 100% Numbers behind the stack bar plot below:

names(taxa3_ecosystem)#"StudyID_Cov" "BlueBug"     "SeekPhyla"   "Phyla_Hits"  "Group_Hits"  "Ecosystem"  
#write.csv(taxa3_ecosystem, file = "PhylaHits_PerPaper.csv", row.names = F)


t<- taxa3_ecosystem %>%
  group_by(Group_Hits, Phyla_Hits, Ecosystem ) %>%
  summarise(Abundance = n(),
            Frequency = round(Abundance/ Total_Papers * 100,1)) #Compute % occurrence of Phyla

View(t)
#write.csv(t, "Phyla_StackBarNumber.csv", row.names = F)

#RESPOND TO REVIWER ON:
#Quantitative community structure: Instead of focusing on presence/absence,
#how do relative abundances of key microbial taxa vary across BCEs?





#Get Group_Hits Ecosystem Totals:
t2 <- t %>% group_by(Group_Hits,Ecosystem) %>%
  summarise(Total = sum(Abundance))

t2
#Group_Hits Ecosystem Total
#1 Archaea    mangrove     69
#2 Archaea    saltmarsh    30
#3 Archaea    seagrass      9
#4 Bacteria   mangrove    591
#5 Bacteria   saltmarsh   341
#6 Bacteria   seagrass     79
#7 Fungi      mangrove     57
#8 Fungi      saltmarsh    27
#9 Fungi      seagrass      3



#Get unique phyla within these 3 groups:
g <- taxa3_ecosystem %>%
  group_by(Group_Hits, Phyla_Hits) %>%
  summarise(Total_Hits = n(), Frequency = round(Total_Hits/ nrow(taxa3_ecosystem)*100,1)) %>%
  group_by(Group_Hits) %>%
  summarise(Total_Per_Group = sum(Total_Hits),n_unique = n())

g #Unique phyla within these 3 groups:
#1 Archaea            11
#2 Bacteria           41
#3 Fungi              14


#Archaea Relative Abundance PLOT=================
taxa3_ecosystem$Ecosystem2 <- ifelse(taxa3_ecosystem$Ecosystem == "mangrove", "Mangrove",
  ifelse(taxa3_ecosystem$Ecosystem == "saltmarsh", "Saltmarsh", "Seagrass"))

# Create the ggplot of Archaea:
a <- taxa3_ecosystem[taxa3_ecosystem$Group_Hits == "Archaea", ]

# Step 1: Calculate relative abundance
a_rel <- a %>%
  group_by(Ecosystem2, Phyla_Hits) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Ecosystem2) %>%
  mutate(relative_abundance = count / sum(count) * 100)

View(a_rel) #to be merged with other b_rel and F_rel later on for analysis

# Step 2: Plot using geom_bar(stat = "identity")
a_plot <- ggplot(data = a_rel, aes(x = Ecosystem2, y = relative_abundance, fill = Phyla_Hits)) + 
  geom_bar(stat = "identity", col = "black") +
  scale_y_continuous(limits = c(0, 100)) +  # now it's percentage
  scale_fill_manual(values = c(
    '#f7fcfd','#e5f5f9','#ccece6','#99d8c9','#66c2a4','#41ae76','#238b45','#006d2c','#00441b',
    '#756bb1','#9966FF','#bcbddc')) +
  theme_bw() +
  ggtitle("Archaea") +
  labs(fill = "Phyla:", x = "", y = "Relative abundance (%)") +
  theme(
    axis.text.x = element_text(size = 26, colour = "black"),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    legend.position =  "top",   #c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 16),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 26),
    legend.box.background = element_rect(),
    legend.margin = margin(6,6,6,6)
  )

a_plot


#Bacteria Relative Abundance PLOT (Top 12)=================
#Top 11 taxa are listed + all others are combined together as "Other"

b <- taxa3_ecosystem[ taxa3_ecosystem$Group_Hits == "Bacteria" , ]# Create the ggplot of Bacteria only

library(dplyr)
library(ggplot2)

# Step 1: Calculate relative abundance
b_rel <- b %>%
  group_by(Ecosystem2, Phyla_Hits) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Ecosystem2) %>%
  mutate(relative_abundance = count / sum(count) * 100)

# Step 2: Identify top 11 phyla by total relative abundance
top_11_phyla <- b_rel %>%
  group_by(Phyla_Hits) %>%
  summarise(total_abundance = sum(relative_abundance), .groups = "drop") %>%
  arrange(desc(total_abundance)) %>%
  slice_head(n = 11) %>%
  pull(Phyla_Hits)

# Step 3: Replace non-top phyla with "Other"
b_rel_grouped <- b_rel %>%
  mutate(Phyla_Hits = ifelse(Phyla_Hits %in% top_11_phyla, Phyla_Hits, "Other")) %>%
  group_by(Ecosystem2, Phyla_Hits) %>%
  summarise(relative_abundance = sum(relative_abundance), .groups = "drop")

# Optional: set factor levels so "Other" is last in legend
b_rel_grouped$Phyla_Hits <- factor(b_rel_grouped$Phyla_Hits, levels = c(sort(top_11_phyla), "Other"))

# Step 4: Plot
b_plot <- ggplot(data = b_rel_grouped, aes(x = Ecosystem2, y = relative_abundance, fill = Phyla_Hits)) + 
  geom_bar(stat = "identity", col = "black") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c(
    '#f7f4f9','#e7e1ef','#d4b9da','#c994c7','#df65b0','#e7298a','#ce1256','#980043','#67001f',
    '#fff7f3','#fde0dd','#cccccc'  # The last color is for "Other"
  )) +
  theme_bw() +
  ggtitle("Bacteria") +
  labs(fill = "Phyla:", x = "", y = "Relative abundance (%)") +
  guides(fill = guide_legend(ncol = 4)) +
  theme(
    axis.text.x = element_text(size = 26, colour = "black"),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    legend.position =  "top",   #c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 16),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 26),
    legend.box.background = element_rect(),
    legend.margin = margin(6,6,6,6)
  )


b_plot




#Fungi Relative Abundance PLOT (Top 12)=================
#Top 11 taxa are listed and all others are combined together as "Other"

f <- taxa3_ecosystem[ taxa3_ecosystem$Group_Hits == "Fungi" , ]# Create the ggplot of Bacteria only

library(dplyr)
library(ggplot2)

# Step 1: Calculate relative abundance
f_rel <- f %>%
  group_by(Ecosystem2, Phyla_Hits) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Ecosystem2) %>%
  mutate(relative_abundance = count / sum(count) * 100)

# Step 2: Identify top 11 phyla
top_11_phyla <- f_rel %>%
  group_by(Phyla_Hits) %>%
  summarise(total_abundance = sum(relative_abundance), .groups = "drop") %>%
  arrange(desc(total_abundance)) %>%
  slice_head(n = 11) %>%
  pull(Phyla_Hits)

# Step 3: Group remaining phyla into "Other"
f_rel_grouped <- f_rel %>%
  mutate(Phyla_Hits = ifelse(Phyla_Hits %in% top_11_phyla, Phyla_Hits, "Other")) %>%
  group_by(Ecosystem2, Phyla_Hits) %>%
  summarise(relative_abundance = sum(relative_abundance), .groups = "drop")

# Step 4: Set factor levels so "Other" is last
f_rel_grouped$Phyla_Hits <- factor(f_rel_grouped$Phyla_Hits, levels = c(sort(top_11_phyla), "Other"))

# Step 5: Plot
f_plot <- ggplot(data = f_rel_grouped, aes(x = Ecosystem2, y = relative_abundance, fill = Phyla_Hits)) + 
  geom_bar(stat = "identity", col = "black") +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c(
    '#f7fbff','#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#08519c',
    '#08306b','#f1eef6','#bdc9e1','#cccccc'  # Last color is for "Other"
  )) +
  theme_bw() +
  ggtitle("Fungi") +
  labs(fill = "Phyla: ", x = "", y = "Relative abundance (%)") +
  guides(fill = guide_legend(ncol = 4)) +  # 4 columns of 3 entries
  theme(
    axis.text.x = element_text(size = 26, colour = "black"),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 26),
    legend.position = "top",
    legend.justification = "center",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 16),
    legend.key.size = unit(0.8, "line"),
    legend.box.background = element_rect(),
    legend.margin = margin(6,6,6,6),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 26)
  )
f_plot

#COMBINE 3 PLOTS (A,B,F)===========
#3 Columns
p_plots_horizon  <- ggarrange(a_plot, b_plot, f_plot, ncol=3)
p_plots_horizon
ggsave(p_plots_horizon, filename = "FIG_PhylaStackBar_ncol3_V5.jpg", width = 60, height = 18, units = "cm", dpi = 600)

#1 Column:
p_plots_horizon  <- ggarrange(a_plot, b_plot, f_plot, ncol=1)
p_plots_horizon
ggsave(p_plots_horizon, filename = "FIG_PhylaStackBar_ncol1.jpg", height = 60, width = 26, units = "cm", dpi = 600)
