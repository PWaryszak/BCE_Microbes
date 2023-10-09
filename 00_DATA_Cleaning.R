#RAW DATA Cleaning:=======
#Load Libraries and Notes:
library(tidyverse)
library(readxl)
library(vegan)

#Pawel continues to clean taxa names in "Taxa_DATA_ALL_EXTRACTED.xlsx" file:
#Link3GDRIVE: https://docs.google.com/spreadsheets/d/1yICgLcTXEYdZul7Hto4U4CA0xgr34vNO/edit?usp=drive_link&ouid=100881671914675049849&rtpof=true&sd=true

#LOAD TAXA data
taxa <- read_excel("Taxa_DATA_ALL_EXTRACTED.xlsx", sheet = "TaxaDATA_CLEAN") #Get dataset cleaned (21-Aug-2023) on Google Drive here:
df <- taxa %>% mutate_if(is.character, as.factor) #convert all columns from character to factor for further manipulations
names(df)
#[1] "Covidence #"                         
#[2] "StudyIn"                             
# [3] "StudyID"                             
# [4] "Study ID (orignal, pink=duplicated)" 
# [5] "Study_ID (no space)"                 
#[6] "StudyID_Cov"                         
# [7] "Microbial richness"                  
# [8] "Microbial taxa reported/assessed"    
# [9] "Microbial taxa extracted"            
# [10] "Microbial_taxa_extracted_clean"      
# [11] "Microbial_taxa_extracted_clean_split" - this is a first column of text-to-column split of the "Microbial_taxa_extracted_clean"    
# From BlueBug01" to BlueBug63" - this column "Microbial_taxa_extracted_clean" split by comma into single taxa names


#Missing TAXA records:========
df_NoPhylaRecords <- filter (df ,is.na(Microbial_taxa_extracted_clean_split) == TRUE) %>%
                     select(StudyID_Cov)
print(df_NoPhylaRecords, n=90)

sum(is.na(df$Microbial_taxa_extracted_clean_split)) # 83

#Get Records with Phyla in them for Christina:===================
BlueBugs <-select(df, Microbial_taxa_extracted_clean_split:BlueBug63)
BlueBugs <- as.data.frame(BlueBugs)

BlueBugs[(BlueBugs=="NA")] <-0
BlueBugs $ Richness <- specnumber(BlueBugs)
names(BlueBugs)


#Turn wide taxa into taxa_long for matching taxa records with phyla next:
taxa_long <- gather(df, key = "BlueBug", value = "SeekPhyla", Microbial_taxa_extracted_clean_split:BlueBug63, na.rm=T) #Turning single-species BluBug columns into one "SeekPhyla" column
names(taxa_long)
dim(taxa_long) #2804   12


#LOAD Phyla dataset:
phyla1 <- read_excel("Taxa_DATA_ALL_EXTRACTED.xlsx", sheet = "PhylaFromReview") #Get clean CSM
names(phyla1)#"Phyla" "Group" "Comment"
P1 <- as.data.frame( phyla1[,1:2])  #P1 data from our literature review contains 55 phylas.
P1

phyla2 <- read_excel("Ludwig2021_PHYLA_Dataset_LTP_06_2022.xlsx", sheet = "Table S1 number of taxa in LTP",skip=2) #Get phyla from paper by Ludwig 2021:
P2 <- as.data.frame(phyla2[1:39,1] )  #Total of 39 Phylas from Ludwig2021 had no hits in our data. I combined it with phyla present in our reveiw. For example Proteobacteria not present in Ludwig2021
colnames(P2)[ 1]<-"Phyla" #Rename column to match P1 for rbind function
P2$Group <- "Bacteria"

P1P2 <- rbind (P1,P2)# combining phyla lists from Ludwig2021 and our own phyla-dataset from the review
P1P2


View(taxa_long)
names(taxa2)
taxa2 <- taxa_long %>%   #Use P1P@ phyla list to loop over SeekPhyla Column for the Phyla hits
  mutate(Phyla_Hits = if_else(SeekPhyla %in% P1P2 $ Phyla, SeekPhyla, "NONE")) #Fill Phyla_Hits with phyla if hit.

taxa3 <- taxa2 %>%
  mutate(Group_Hits = if_else(Phyla_Hits %in% P1P2$Phyla, P1P2$Group[match(Phyla_Hits, P1P2$Phyla)], "NONE"))#Fill Group_Hits with  Group (bacteria, fungi, archaea) if hit.

View(taxa3)

#CREATE TABLE S1 PHYLA_HITS in WORD using "taxa2spread":=========
library(officer)
library(flextable)

#Frequencies of phyla hits:
dim(taxa2)#2804  rows
taxa_row_total <- as.numeric(nrow(taxa2))

taxa2spread <- taxa2 %>% group_by(Phyla_Hits) %>%
  summarise(n = n(),
           Freq_percent = round( (n / taxa_row_total * 100), 2)) %>%
  select(Phyla_Hits, Freq_percent) %>%
    arrange(desc(Freq_percent))

taxa2spread


ft <- flextable(data = taxa2spread) %>% theme_zebra %>% autofit # Create flextable object
ft# See flextable in RStudio viewer
tmp <- tempfile(fileext = ".docx") # Create a temp file
read_docx() %>%   body_add_flextable(ft) %>%   print(target = tmp) # Create a docx file:
browseURL(tmp) # open word document. SAVED as PhylaFrequencyTable.docx Table S1 and send an update to CB on 09oct2023


#For Stacked Barplot CB's:==============
names(DATA)


############################
#FINISHED HERE on 09 Oct 2023 Get Phyla/Freq/Group/Ecosystem table and drawa a stacked barplot==========
#####################

#Mangroves
taxa2spread <- taxa2 %>% 
  
  #filter mangroves
  #Gather the Phyla to long
  #Martch with group
  # group_by(Phyla)
  #summarise(n = n(),
  #Freq_percent = round( (n / taxa_row_total * 100), 2)) %>%
  
    #Group Phyla Freq
#   A    Guard     14
#WEB: https://www.statology.org/stacked-barplot-in-r/


  group_by(Phyla_Hits) %>%
  summarise(n = n(),
           Freq_percent = round( (n / taxa_row_total * 100), 2)) %>%
  select(Phyla_Hits, Freq_percent) %>%
    arrange(desc(Freq_percent))

taxa2spread



taxa2stack <- DATA %>% group_by() %>%
  summarise(n = n(),
           Freq_percent = round( (n / taxa_row_total * 100), 2)) %>%
  select(Phyla_Hits, Freq_percent) %>%
    arrange(desc(Freq_percent))

taxa2spread


#Sum-up Phyla-hits==========
yes<- taxa2spread %>% filter(Phyla_Hits!="NONE")
yes_sum <- sum(yes$n)

no <- taxa2spread %>% filter(Phyla_Hits=="NONE")
no_sum <- sum(no$n)

NOPE <- no_sum/ (yes_sum + no_sum) *100
round(NOPE,1) #50% of NO PHYLA info



#CLEAN VEG data off singletons========

#Get species matrix (Phyla as columns)
taxa_nmds <- taxa3 %>% 
  group_by(StudyID_Cov, Phyla_Hits) %>%
  summarise(n = n()) %>%
  spread(Phyla_Hits,n, fill=0)

View(taxa_nmds)#646  27

#Get ecosystem as grouping factor:
eco <- read_excel("DataAnalysis_BlueBugs2022_MASTERFILE.xlsx", sheet = "clean_data") %>% #Get  origin dataset  to get ecosystem data
                 filter(Ecosystem == "mangrove" | Ecosystem == "saltmarsh" | Ecosystem == "seagrass") #filter out the couple of records with mixed records: mix, soil, NA values 
               
dim(eco)#646  27

#CLEAN THE ROWS:
taxa_nmds$row_sum <- rowSums(taxa_nmds[ , 2:71], na.rm=TRUE)#columns with phyla from Acidobacteria to Zygomycota)

clean_rows <- taxa_nmds %>%  filter(row_sum >= 1) %>%# Filter out rows and columns with sums smaller than 1
               select(-row_sum)

dim(taxa_nmds) #586  73
dim(clean_rows) # 586  72

#CLEAN THE COLUMNS:
column_sums <- colSums(taxa_nmds[, -1]) # Calculate phyla column sums
column_sums_data <- data.frame(StudyID_Cov = "Sum", taxa_nmds [1, -1] * 0 + column_sums)#  Create a data frame with the sum row

# Combine the column_sums_data with the original dataset taxa_nmds
sums <- rbind(taxa_nmds, column_sums_data)
dim(taxa_nmds) # 586  73
dim(clean_rows) #586  72

#ID Singleton columns:
occur.cols <- apply(taxa_nmds[,-1],2,sum)#sum species occurrences in each column
table(occur.cols) #no zero abundances

#remove 0-sum  columns if any:
good.matrix <- taxa_nmds[,-1][ , ! occur.cols <= 0  ] #removing all 0-sum  columns
names(good.matrix) #341  66 = there were no zero columns

clean_columns <- good.matrix %>% select(-row_sum) #Remove that Row_sum column before NMDS

StudyID_Cov_community_matrix <- cbind(clean_rows[,"StudyID_Cov"], clean_columns)

DATA<- left_join(StudyID_Cov_community_matrix, eco, by = "StudyID_Cov")#Join phyla matrix with the Master file "eco"

View(DATA)

#Get ecosystems
treat <- as.data.frame(DATA$Ecosystem)#Get the ecosystem as grouping variable.
colnames(treat)[1] <- "region"

#NMDS, LESS OUTLIERS:=============
community_matrix <- clean_columns
veg.env <- cbind(community_matrix, treat)

MDS <- metaMDS(community_matrix, k=2, trymax=10, distance = "bray")
MDS$stress # Stress = 0.0008396378

coordinates<-as.data.frame(MDS$points[,1:2])#site scores or:
veg.nmds<-as.data.frame(cbind(coordinates, veg.env))
names(veg.nmds)

region_data <- filter(veg.nmds, region == "mangrove" |region == "saltmarsh"|region=="seagrass") %>%
  filter( MDS1 < 5) #one big outlier in MDS
  
dim(veg.nmds); dim(region_data)            

#Draw a hull:
grp.a <- region_data[region_data$region == "mangrove", ][chull(region_data[region_data$region =="mangrove", c("MDS1", "MDS2")]), ]
grp.b <- region_data[region_data$region == "saltmarsh", ][chull(region_data[region_data$region =="saltmarsh", c("MDS1", "MDS2")]), ]
grp.c <- region_data[region_data$region == "seagrass", ][chull(region_data[region_data$region =="seagrass", c("MDS1", "MDS2")]), ]

hull.data <- rbind(grp.a, grp.b,grp.c)  #combine grp.a and grp.b

reg_plot <- ggplot() + 
  geom_polygon(data=hull.data,aes(x=MDS1,y=MDS2,fill=region, group=region),alpha=0.30) + # add the convex hulls
  geom_point(data=region_data ,aes(x=MDS1,y=MDS2,shape=region,colour=region),size=4)+ # add the point markers
  #scale_colour_manual(values=c("mangrove" = "red", "seagrass" = "blue", saltmarsh = "brown")) +
  labs(x = "NMDS 1",y="NMDS 2", colour = "", shape = "")+
  guides(fill = FALSE, size = FALSE)+
  #coord_equal() +
  theme_bw() +
  theme(#axis.text.x = element_blank(),  # remove x-axis text
    #axis.text.y = element_blank(), # remove y-axis text
    #axis.ticks = element_blank(),  # remove axis ticks
    #axis.title.x = element_text(size=18), # remove x-axis labels
    axis.title.y = element_text(size=18),
    axis.title.x = element_text(size=18),
    legend.position = "top",
    legend.text = element_text(size=18),
    legend.title  = element_text(size=18),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  #remove major-grid labels
    panel.grid.minor = element_blank(),  #remove minor-grid labels
    plot.background = element_blank())  

reg_plot

#ggsave(reg_plot,bg = "white",width = 12, height = 7, file = "NMDS_PLOT_EcossytemsPhyla_50PercentOfDATA.png")




#NMDS ALL DATA:=============
community_matrix <- clean_columns
veg.env <- cbind(community_matrix, treat) #Combininig ecosystem with phyla matrix

names(veg.env)

MDS <- metaMDS(community_matrix,k=2,trymax=10, distance = "bray")
MDS$stress # Stress = 0.0005720224

coordinates<-as.data.frame(MDS$points[,1:2])#site scores or:
veg.nmds<-as.data.frame(cbind(coordinates, veg.env))
region_data <- filter(veg.nmds, region == "mangrove" |region == "saltmarsh"|region=="seagrass") 

#Draw a hull:
grp.a <- region_data[region_data$region == "mangrove", ][chull(region_data[region_data$region =="mangrove", c("MDS1", "MDS2")]), ]
grp.b <- region_data[region_data$region == "saltmarsh", ][chull(region_data[region_data$region =="saltmarsh", c("MDS1", "MDS2")]), ]
grp.c <- region_data[region_data$region == "seagrass", ][chull(region_data[region_data$region =="seagrass", c("MDS1", "MDS2")]), ]

hull.data <- rbind(grp.a, grp.b,grp.c)  #combine grp.a and grp.b

reg_plot2 <- ggplot() + 
  geom_polygon(data=hull.data,aes(x=MDS1,y=MDS2,fill=region, group=region),alpha=0.30) + # add the convex hulls
  geom_point(data=region_data ,aes(x=MDS1,y=MDS2,shape=region,colour=region),size=4)+ # add the point markers
  #scale_colour_manual(values=c("mangrove" = "red", "seagrass" = "blue", saltmarsh = "brown")) +
  labs(x = "NMDS 1",y="NMDS 2", colour = "", shape = "")+
  guides(fill = FALSE, size = FALSE)+
  #coord_equal() +
  theme_bw() +
  theme(#axis.text.x = element_blank(),  # remove x-axis text
    #axis.text.y = element_blank(), # remove y-axis text
    #axis.ticks = element_blank(),  # remove axis ticks
    #axis.title.x = element_text(size=18), # remove x-axis labels
    axis.title.y = element_text(size=18),
    axis.title.x = element_text(size=18),
    legend.position = "top",
    legend.text = element_text(size=18),
    legend.title  = element_text(size=18),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  #remove major-grid labels
    panel.grid.minor = element_blank(),  #remove minor-grid labels
    plot.background = element_blank())  

reg_plot2

#ggsave(reg_plot2,bg = "white",width = 12, height = 7, file = "NMDS_OUTLIERS_PLOT_EcossytemsPhyla_40PercentOfDATA.png")


################
#Stopped here on 28-Aug-2023, Talk to CB what steps next to take.
####################







