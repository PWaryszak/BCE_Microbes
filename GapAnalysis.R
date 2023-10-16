#RAW DATA Cleaning:=======
#Load Libraries and Notes:

if (!require(stringr))install.packages('stringr')
library(stringr)
if (!require(tidyverse))install.packages('tidyverse')
library(tidyverse)
if (!require(readxl))install.packages('readxl')
library(readxl)

#BCL  data:============
df_bcl <- read_excel("DataAnalysis_BlueBugs2022_MASTERFILE.xlsx", sheet = "clean_data") %>%
  select(StudyID_Cov, Country_clean, Ecosystem)#

dim(df_bcl)#656   3

df_bcl2 <-df_bcl %>% separate(Country_clean,into = c("C1","C2"),sep = ",", remove = TRUE) 
View(df_bcl2)

two_country_records <-   df_bcl2 [!is.na(df_bcl2$C2),] #keep the C2 only
two_country_records
two_country_records <- two_country_records %>% select(-C1) %>%
                       rename(Country_Name = C2) #Rename C2 to match with C1 dataframe and Bertram2021
two_country_records

one_country_records <- df_bcl2 %>% select( -C2) %>% 
                       rename(Country_Name = C1) #Rename C1 to match with C2 dataframe and Bertram2021
one_country_records

df_bcl3 <- rbind(one_country_records,two_country_records)
head(df_bcl3)

df_bcl3_ByCountry <- df_bcl3 %>% group_by(Country_Name,Ecosystem) %>%
                     summarise(n =n()) %>%
                    filter(Ecosystem == "seagrass"| Ecosystem == "saltmarsh" | Ecosystem == "mangrove" )%>%
                    group_by(Ecosystem) %>%
                    summarise(n_countries =n()) %>% #Number of countries with records on BCE (Blue Carbon Ecosystems)
                    mutate(Presence = "Review")
df_bcl3_ByCountry




#GLOBAL DATA:========
df_bertram<- read_excel("DataAnalysis_BlueBugs2022_MASTERFILE.xlsx", sheet = "Bertram2021_BCE_Country_DATA") #
names(df_bertram)

df_bertram2 <- df_bertram %>% select(Country_Name,seagrass_km2,saltmarsh_km2,mangroves_km2) %>%
                filter(seagrass_km2 >0 |saltmarsh_km2 >0 | mangroves_km2 >0) %>%
                rename(seagrass=seagrass_km2,saltmarsh=saltmarsh_km2, mangrove=mangroves_km2 )#rename to match df_bcl3_ByCountry
df_bertram2            


df_bertram3_ByCountry <- df_bertram2 %>% gather(key = "Ecosystem", "km2",  seagrass:mangrove) %>%
                 filter(km2 >0) %>% #Remove countries with zero km2 of Ecosystem
                 group_by(Ecosystem) %>%
                 summarise(n_countries =n()) %>% #Number of countries with records on BCE (Blue Carbon Ecosystems)
                 mutate(Presence = "Global")
df_bertram3_ByCountry


df_bertram3_ByArea <- df_bertram2 %>% gather(key = "Ecosystem", "km2",  seagrass:mangrove) %>%
                 filter(km2 >0) %>% #Remove countries with zero km2 of Ecosystem
                 group_by(Ecosystem) %>%
                 summarise(Area_km2 =sum(km2)) %>% #Number of countries with records on BCE (Blue Carbon Ecosystems)
                 mutate(Presence = "Global")
df_bertram3_ByArea


#MERGE BCL + GLOBAL DATA for plotting and gap analysis:
gap <- rbind(df_bcl3_ByCountry, df_bertram3_ByCountry)
gap$Ecosystem <- str_to_title(gap$Ecosystem) #capitalising first letter on Ecosystme names
gap$Ecosystem <- factor(gap$Ecosystem, levels = c("Mangrove","Seagrass","Saltmarsh"))
gap
write.csv(gap, file = "GapAnalysisNumbers.csv", row.names = F)


#PLOT1 (BCE by country number)=========
PD<-position_dodge(0.9)

ggplot(gap, aes(x=Presence, y= n_countries, fill= Ecosystem , width=.75)) +
    geom_bar(position=PD, stat="identity",colour="black")+
    facet_grid(.~Ecosystem) +
    scale_fill_manual(values=c("Mangrove" = "#238443", "Seagrass" = "yellow", "Saltmarsh" = "#dd3497")) +
  labs(x = "Presence",y="Number of countries")+
  guides(fill = FALSE, size = FALSE)+
  #coord_equal() +
  theme_bw() +
  theme(axis.text.x = element_text(size=16, colour = "black"),
    axis.text.y =  element_text(size=16, colour = "black"),
    axis.title.x = element_text(size=18, colour = "black"), 
    axis.title.y = element_text(size=18, colour = "black"),
    strip.text = element_text(size=18, colour = "black"),
    legend.position = "none",
    legend.text = element_text(size=18),
    legend.title  = element_text(size=18),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  #remove major-grid labels
    plot.background = element_blank())  

#ggsave(bg = "white",width = 12, height = 7, file = "GAP_ANALYSIS_PLOT.png")


#PLOT2 (2y-axis , publication+area by ecosystem)========
#publications data from the log spreadsheet:
categories <- c("mangrove", "saltmarsh", "seagrass")
number_of_pubs <- c(436, 167, 42)  #nyumber of publications by ecosystem
values_secondary <- c(38, 25, 16) #Number of countries by ecosystem
publication_data <- data.frame(Category = categories, number_of_pubs = number_of_pubs, Value_Secondary = values_secondary)
publication_data

#Create a data for plotting:
df <- cbind(df_bertram3_ByArea, publication_data)
df2 <- df %>% select(Ecosystem, Area_km2,number_of_pubs )
df2
df2$Area_km2a <- df2$Area_km2/1000 #To fit on the plot of 2 axes

# Create a bar plot using ggplot2 and geom_bar
ggplot(df2, aes(x = Ecosystem)) +
  geom_bar(aes(y = number_of_pubs), stat = "identity", fill = c("#238443", "#dd3497", "yellow")) +
  geom_point(aes(y = Area_km2a), color = "black", size = 5, shape = 21, fill = "red", stroke = 1) +  # Add points for the secondary y-axis
  labs(title = "") +
  scale_y_continuous(
    name = "Number of publications",
    sec.axis = sec_axis(~./20, name = "Area  (x1000 km2)"))+
  scale_fill_manual(values=c("mangrove" = "#238443", "seagrass" = "yellow", saltmarsh = "#dd3497")) +
  theme_bw() +
  theme(axis.text.x = element_text(size=16, colour = "black"),
    axis.text.y =  element_text(size=16, colour = "black"),
    axis.title.x = element_text(size=18, colour = "black"), 
    axis.title.y = element_text(size=18, colour = "black"),
    strip.text = element_text(size=18, colour = "black"),
    legend.position = "none",
    legend.text = element_text(size=18),
    legend.title  = element_text(size=18),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  #remove major-grid labels
    plot.background = element_blank())  

#ggsave(bg = "white",width = 12, height = 7, file = "GAP_ANALYSIS_PLOT2.png")
