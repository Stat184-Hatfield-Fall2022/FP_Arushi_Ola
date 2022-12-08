library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggmap)
library(rayshader)
library(rgl)



##Bringing in the newyork tree data sets for every 10 years 

nytrees_1995 <- read_csv("Desktop/1995_Street_Tree_Census.csv")
nytrees_2005 <- read_csv("Downloads/2005_Street_Tree_Census.csv")
nytrees_2015 <- read_csv("Downloads/2015_Street_Tree_Census_-_Tree_Data.csv")
nytrees_species <- read_csv("Downloads/archive/new_york_tree_species.csv")


#Data wrangling

nytrees_1995 <- nytrees_1995 %>%
  select(Spc_Common,Condition,Borough,Longitude,Latitude,Zip_New)

nytrees_2005 <- nytrees_2005 %>%
  select(spc_common,status,boroname,latitude,longitude,zipcode)

nytrees_2015 <- nytrees_2015 %>%
  select(spc_common,health,problems,borough,latitude,longitude,postcode)


## creating map Visualization
##using ggmaps 

register_google(key = "AIzaSyC7mdyVg4lKT5RGVW9GDIZB5x9apdKO9zY", write = TRUE)

newyork.map <- get_map(location = 'New York', maptype= "hybrid",source='google',zoom=12)

nytrees_1995_map <- ggmap(newyork.map) + 
  geom_point(data=nytrees_1995, aes(x=Longitude,y=Latitude,color= "green"),size=.05)+
  theme(axis.ticks = element_blank(), axis.text = element_blank())

nytrees_2005_map <- ggmap(newyork.map) + 
  geom_point(data = nytrees_2005, aes(x=longitude,y=latitude,colour = "green"),size=.05)+
  theme(axis.ticks = element_blank(), axis.text = element_blank())
        
nytrees_2015_map <-ggmap(newyork.map) + 
  geom_point(data = nytrees_2015, aes(x=longitude,y=latitude,colour = "green"),size=.05)+
  theme(axis.ticks = element_blank(), axis.text = element_blank())
      
 
##color filled by the condition of the tree
##Labeling 

#1995
nytrees_1995 %>% filter(Condition %in% c("Good", "Excellent", "Poor", "Dead"))
nytrees_1995_map = ggmap(newyork.map) + geom_point(data = nytrees_2005, aes(x=longitude,y=latitude,colour = status),size=.05)+
  scale_color_manual(
    values = c(Dead = "#282626",
               Excellent = "#16DE0C",
               Good = "#3D6C08",
               Poor = "#E50E0E")
  ) +
  theme_minimal()

#2005
nytrees_2005_map = ggmap(newyork.map) + 
  geom_point(data = nytrees_2005, aes(x=longitude,y=latitude,colour = status),size=.05)+
  scale_color_manual(
    values = c(Dead = "#282626",
               Excellent = "#16DE0C",
               Good = "#3D6C08",
               Poor = "#E50E0E")
  ) +
  theme_minimal()

#2015
nytrees_2015_map = ggmap(newyork.map) + 
  geom_point(data = nytrees_2015, aes(x=longitude,y=latitude,colour = health),size=.05)+
scale_color_manual(
  values = c(Fair = "#3D6C08",
             Good = "#16DE0C",
             Poor = "#E50E0E")  
  ) +
  theme_minimal()
  
  
##Borough has the most tree

sort(summary(as.factor(nytrees_1995$Borough), decreasing=TRUE)[1:5])
sort(summary(as.factor(nytrees_2005$boroname), decreasing=TRUE)[1:5])
sort(summary(as.factor(nytrees_2015$borough), decreasing=TRUE)[1:5])


#most popolar species
mostPopular_1995 <- sort(summary(as.factor(nytrees_1995$Spc_Common), decreasing=TRUE)[1:5])
mostPopular_2005 <- sort(summary(as.factor(nytrees_2005$spc_common), decreasing=TRUE)[1:5])
mostPopular_2015 <- sort(summary(as.factor(nytrees_2015$spc_common), decreasing=TRUE)[1:5])
list(mostPopular_1995)
list(mostPopular_2005)
list(mostPopular_2015)

#3d grapph

#Visualization using Bar plot on the most common species per borough
##needs to be fixed!!!
species_common <- ggplot(nytrees_2015) +
  aes(x = spc_common) +
  geom_bar(fill = "#112446") +
  theme_minimal() +
  facet_wrap(vars(borough)) 


...

#Using Facet Visualization on the Health/Condition of the tree per borough

Borough_condition_1995 <- nytrees_1995 %>%
  filter(Condition %in% c("Good", "Excellent", "Poor", "Dead")) %>%
  ggplot() +
  aes(x = Borough, fill = Borough) +
  geom_bar() +
  scale_fill_manual(
    values = c(Bronx = "#814198",
               Brooklyn = "#0370B7",
               Manhattan = "#E59C0C",
               Queens = "#E01D1D",
               `Staten Island` = "#4B935B")
  ) +
  labs(
    x = "Borough",
    y = "Total num of Tree",
    title = "1995 Trees Condition per Borough",
    fill = "Boroughs"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold")
  ) +
  facet_wrap(vars(Condition))


Borough_condition_2005 <- ggplot(nytrees_2005) +
  aes(x = boroname, fill = boroname) +
  geom_bar() +
  scale_fill_manual(
    values = c(Bronx = "#814198",
               Brooklyn = "#0370B7",
               Manhattan = "#E59C0C",
               Queens = "#E01D1D",
               `Staten Island` = "#4B935B")
  ) +
  labs(
    x = "Borough",
    y = "Total num of Tree",
    title = "2005 Trees Condition per Borough"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold")
  ) +
  facet_wrap(vars(status))


Borough_condition_2015 <- nytrees_2015 %>%
  filter(health %in% c("Good", "Fair", "Poor")) %>%
  ggplot() +
  aes(x = borough, fill = borough) +
  geom_bar() +
  scale_fill_manual(
    values = c(Bronx = "#814198",
               Brooklyn = "#0370B7",
               Manhattan = "#E59C0C",
               Queens = "#E01D1D",
               `Staten Island` = "#4B935B")
  ) +
  labs(
    x = "Borough",
    y = "Total num of Tree",
    title = "2005 Trees Condition per Borough"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold")
  )+
  facet_wrap(vars(health))

### shows the top 5 species per condition

#1995
Excellent_rate_1995<- nytrees_1995 %>%
  select(Spc_Common,Condition)  %>%
  filter(Condition %in% "Excellent")

Good_rate_1995<- nytrees_1995 %>%
  select(Spc_Common,Condition)  %>%
  filter(Condition %in% "Good")

Poor_rate_1995<- nytrees_1995 %>%
  select(Spc_Common,Condition)  %>%
  filter(Condition %in% "Poor")

Dead_rate_1995<- nytrees_1995 %>%
  select(Spc_Common,Condition)  %>%
  filter(Condition %in% "Dead")

sort(summary(as.factor(Excellent_rate_1995$Spc_Common), decreasing=TRUE)[1:5])
sort(summary(as.factor(Good_rate_1995$Spc_Common), decreasing=TRUE)[1:5])
sort(summary(as.factor(Poor_rate_1995$Spc_Common), decreasing=TRUE)[1:5])
sort(summary(as.factor(Dead_rate_1995$Spc_Common), decreasing=TRUE)[1:5])

#2005
Excellent_rate_2005<- nytrees_2005 %>%
  select(spc_common,status)  %>%
  filter(status %in% "Excellent")

Good_rate_2005<- nytrees_2005 %>%
  select(spc_common,status)  %>%
  filter(status %in% "Good")

Poor_rate_2005<- nytrees_2005 %>%
  select(spc_common,status)  %>%
  filter(status %in% "Poor")

Dead_rate_2005<- nytrees_2005 %>%
  select(spc_common,status)  %>%
  filter(status %in% "Dead")

sort(summary(as.factor(Excellent_rate_2005$spc_common), decreasing=TRUE)[1:5])
sort(summary(as.factor(Good_rate_2005$spc_common), decreasing=TRUE)[1:5])
sort(summary(as.factor(Poor_rate_2005$spc_common), decreasing=TRUE)[1:5])
sort(summary(as.factor(Dead_rate_2005$spc_common), decreasing=TRUE)[1:5])

#2015 
Good_rate_2015<- nytrees_2015 %>%
  select(spc_common,health)  %>%
  filter(health %in% "Good")

Fair_rate_2015<- nytrees_2015 %>%
  select(spc_common,health)  %>%
  filter(health %in% "Fair")

Poor_rate_2015<- nytrees_2015 %>%
  select(spc_common,health)  %>%
  filter(health %in% "Poor")

sort(summary(as.factor(Good_rate_2015$spc_common), decreasing=TRUE)[1:5])
sort(summary(as.factor(Fair_rate_2015$spc_common), decreasing=TRUE)[1:5])
sort(summary(as.factor(Poor_rate_2015$spc_common), decreasing=TRUE)[1:5])




esquisse::esquisser(data = nytrees_2015, viewer = "browser")



#Seeing the top 5 Excellent,Good,Poor, and Dead species 

esquisse::esquisser(data = nytrees_2015, viewer = "browser")

