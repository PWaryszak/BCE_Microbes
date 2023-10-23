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
      filter(TSD != "TSD") %>% #TSD RECORDS ARE skippe
      select(StudyID_Cov,Microbial_taxa_extracted_clean_split: BlueBug63 ) %>%
      mutate_if(is.character, as.factor) #convert all columns from character to factor for further manipulations

  
taxa_long <- gather(df, key = "BlueBug", value = "SeekPhyla", Microbial_taxa_extracted_clean_split:BlueBug63, na.rm=T) #Turning single-species BluBug columns into one "SeekPhyla" column
names(taxa_long)#"StudyID_Cov" "BlueBug"     "SeekPhyla"  
dim(taxa_long) #31552     3



#LOAD Phyla dataset:=========
phyla1 <- read_excel("DataAnalysis_BlueBugs2022_MASTERFILE.xlsx", sheet = "PhylaFromReview") #Get clean CSM
names(phyla1)#"Phyla" "Group" "Comment"
P1 <- as.data.frame( phyla1[,1:2])  #P1 data from our literature review contains 62 phylas.
P1

phyla2 <- read_excel("Ludwig2021_PHYLA_Dataset_LTP_06_2022.xlsx", sheet = "Table S1 number of taxa in LTP",skip=2) #Get phyla from paper by Ludwig 2021:
P2 <- as.data.frame(phyla2[1:39,1] )  #Total of 39 Phylas from Ludwig2021 had no hits in our data. I combined it with phyla present in our reveiw. For example Proteobacteria not present in Ludwig2021
colnames(P2)[ 1]<-"Phyla" #Rename column to match P1 for rbind function
P2$Group <- "Bacteria"

P1P2 <- rbind (P1,P2)# combining phyla lists from Ludwig2021 and our own phyla-dataset from the review
names(P1P2)#"Phyla" "Group"


taxa2 <- taxa_long %>%   #Use P1P2 phyla list to loop over SeekPhyla Column for the Phyla hits
  mutate(Phyla_Hits = if_else(SeekPhyla %in% P1P2 $ Phyla, SeekPhyla, "NONE")) #Fill Phyla_Hits with phyla if hit.

taxa3 <- taxa2 %>%
  mutate(Group_Hits = if_else(Phyla_Hits %in% P1P2$Phyla, P1P2$Group[match(Phyla_Hits, P1P2$Phyla)], "NONE"))#Fill Group_Hits with  Group (bacteria, fungi, archaea) if hit.

ecosytems <-  MASTER_DATA[ , c("StudyID_Cov", "Ecosystem")] #Get ecosystem type from the MASTER_DATA

taxa3_ecosystem <- left_join( taxa3,ecosytems, by = "StudyID_Cov") %>%  #merge ecosystem type with taxa3
   filter(Ecosystem == "saltmarsh" | Ecosystem == "mangrove" | Ecosystem == "seagrass" ) %>%  #Get these 3 only
   filter(Phyla_Hits != "NONE")

unique(taxa3_ecosystem$Ecosystem)# "saltmarsh" "mangrove"  "seagrass" 
names(taxa3_ecosystem)#"StudyID_Cov" "BlueBug"     "SeekPhyla"   "Phyla_Hits"  "Group_Hits"  "Ecosystem"  
View(taxa3_ecosystem)




#Get Numbers behind the stack bar plot below:
t<- taxa3_ecosystem %>%
  group_by(Group_Hits, Phyla_Hits, Ecosystem ) %>%
  summarise(Abundance = n(),
            Frequency = round(Abundance/ nrow(taxa3_ecosystem)*100,1))

View(t)
#write.csv(t, "Phyla_StackBarNumber.csv", row.names = F)



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


#PLOTS=================
# Define the Archaea data
a <- taxa3_ecosystem[taxa3_ecosystem$Group_Hits == "Archaea", ]

# Create the ggplot of Archaea
a_plot <- ggplot(data = a, aes(x = Ecosystem, fill = Phyla_Hits)) + 
  geom_bar(col = "black") +
    scale_y_continuous(limits = c(0,60))+
  scale_fill_manual(values = c(
    '#f7fcfd','#e5f5f9','#ccece6','#99d8c9','#66c2a4','#41ae76','#238b45','#006d2c','#00441b',
    '#756bb1','#bcbddc')) +
  theme_bw() +
  ggtitle("Archaea") +
  labs(fill = "Phyla:", x = "", y = "Number of occurances") +
  theme(
    axis.text.x = element_text(size = 16, colour = "black"),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    legend.position = c(0.95, 0.95),  # Position the legend at (0.6, 0.9)
    legend.justification = c(1, 1),  # Align the legend to the top-right
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 16),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18),
    legend.box.background = element_rect(),  # Add a black box around the legend
    legend.margin = margin(6,6,6,6))  # Remove margin around the legend box

a_plot



b <- taxa3_ecosystem[ taxa3_ecosystem$Group_Hits == "Bacteria" , ]# Create the ggplot of Bacteria only
b_plot <- ggplot( data = b ,aes(x = Ecosystem, fill = Phyla_Hits)) + 
  geom_bar(col="black") +
  scale_fill_manual(values = c('#f7f4f9','#e7e1ef','#d4b9da','#c994c7','#df65b0','#e7298a','#ce1256','#980043','#67001f', '#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177','#49006a',
 '#ffffe5','#fff7bc','#fee391','#fec44f','#fe9929','#ec7014','#cc4c02','#993404','#662506', '#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026',
 '#f7f7f7','#cccccc','#969696','#636363','#252525'))+
  theme_bw() +
  ggtitle("Bacteria")+
  labs(fill = "Phyla:", x = "", y = "") +
  guides(fill=guide_legend(ncol=2))+
  theme(
    axis.text.x = element_text(size = 16, colour = "black"),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    legend.position = c(0.98, 0.98),  # Position the legend at (0.6, 0.9)
    legend.justification = c(1, 1),  # Align the legend to the top-right
    #legend.direction='horizontal',
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 10),
    legend.key.size = unit(0.8,"line"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18),
    legend.box.background = element_rect(),  # Add a black box around the legend
    legend.margin = margin(6,6,6,6))  # Remove margin around the legend box

b_plot

f <- taxa3_ecosystem[ taxa3_ecosystem$Group_Hits == "Fungi" , ]# Create the ggplot of Bacteria only
f_plot <- ggplot( data = f ,aes(x = Ecosystem, fill = Phyla_Hits)) + 
  geom_bar(col="black") +
  scale_fill_manual(values = c('#f7fbff','#deebf7','#c6dbef','#9ecae1','#6baed6','#4292c6','#2171b5','#08519c','#08306b','#f1eef6','#bdc9e1','#74a9cf','#2b8cbe','#045a8d'))+
  theme_bw() +
  scale_y_continuous(limits = c(0,60))+
  ggtitle("Fungi")+
  labs(fill ="Phyla: ", x="", y="")+
  theme(
    axis.text.x = element_text(size = 16, colour = "black"),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    legend.position = c(0.95, 0.95),  # Position the legend at (0.6, 0.9)
    legend.justification = c(1, 1),  # Align the legend to the top-right
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 16),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18),
    legend.box.background = element_rect(),  # Add a black box around the legend
    legend.margin = margin(6,6,6,6))  # Remove margin around the legend box

f_plot

#COMBINE 3 PLOTS (A,B,F)===========

p_plots_horizon  <- ggarrange(a_plot, b_plot, f_plot, ncol=3)
p_plots_horizon

ggsave(p_plots_horizon, filename = "FIG_PhylaStackBar3.jpg", width = 60, height = 18, units = "cm", dpi = 600)
