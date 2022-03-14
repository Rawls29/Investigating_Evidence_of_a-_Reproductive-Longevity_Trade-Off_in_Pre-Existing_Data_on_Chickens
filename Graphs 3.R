rm(list=ls())
setwd('C:/Users/samra/Documents/My Documents/Uni/Imperial/Winter Project/R')
load(file = "chicken.Rda")

library(ggplot2)

#Av Longevity Scatterplot
#Subsetting data to keep only the results with an entry for:
#1. Average egg production
#2. Average longevity
#3. Purpose
chicken_longevity <- subset(chicken,
                            !is.na(chicken$Egg_Production_Av) &
                              !is.na(chicken$Longevity_Av) &
                              !is.na(chicken$Purpose_2))
#Z-scaling data
chicken_longevity$z.Egg_Production_Av <- scale(chicken_longevity$Egg_Production_Av)
#Creating a new column in the dataframe and setting the contents as z-scaled average egg production
chicken_longevity$z.Longevity_Av <- scale(chicken_longevity$Longevity_Av)
#Creating a new column in the dataframe and setting the contents as z-scaled average longevity

chicken_longevity$Estimates <- NA
#Creating a new column in the dataframe and setting the contents as NA
chicken_longevity$Breed_2 <- NA
#Creating a new column in the dataframe and setting the contents as NA

#Manually setting how many estimates of each breed there are
chicken_longevity$Estimates[which(chicken_longevity$Breed=="American Game Fowl"|
                                    chicken_longevity$Breed=="Andalusian"|
                                    chicken_longevity$Breed=="Ayam Cemani"|
                                    chicken_longevity$Breed=="Buckeye"|
                                    chicken_longevity$Breed=="Buttercup"|
                                    chicken_longevity$Breed=="Catalana"|
                                    chicken_longevity$Breed=="Chantecler"|
                                    chicken_longevity$Breed=="Cornish"|
                                    chicken_longevity$Breed=="Crevecoeur"|
                                    chicken_longevity$Breed=="Golden Comet"|
                                    chicken_longevity$Breed=="Hamburg"|
                                    chicken_longevity$Breed=="Holland"|
                                    chicken_longevity$Breed=="Houdan"|
                                    chicken_longevity$Breed=="Java"|
                                    chicken_longevity$Breed=="Jersey Giant"|
                                    chicken_longevity$Breed=="Kraienkopp"|
                                    chicken_longevity$Breed=="La Fleche"|
                                    chicken_longevity$Breed=="Lakenvelder"|
                                    chicken_longevity$Breed=="Lamona"|
                                    chicken_longevity$Breed=="Langshan"|
                                    chicken_longevity$Breed=="Legbar"|
                                    chicken_longevity$Breed=="Malay"|
                                    chicken_longevity$Breed=="Minorca"|
                                    chicken_longevity$Breed=="Modern Game"|
                                    chicken_longevity$Breed=="Naked Neck"|
                                    chicken_longevity$Breed=="New Hampshire"|
                                    chicken_longevity$Breed=="Onagadori"|
                                    chicken_longevity$Breed=="Orloff"|
                                    chicken_longevity$Breed=="Redcap"|
                                    chicken_longevity$Breed=="Rhode Island White"|
                                    chicken_longevity$Breed=="Rosecomb"|
                                    chicken_longevity$Breed=="Sapphire Gem"|
                                    chicken_longevity$Breed=="Sebright"|
                                    chicken_longevity$Breed=="Silkie"|
                                    chicken_longevity$Breed=="Sultan"|
                                    chicken_longevity$Breed=="Sumatra"|
                                    chicken_longevity$Breed=="Swedish Flower Hen"|
                                    chicken_longevity$Breed=="White-faced Black Spanish"|
                                    chicken_longevity$Breed=="Yokohama")]<-1
chicken_longevity$Estimates[which(chicken_longevity$Breed=="Ameraucana"|
                                    chicken_longevity$Breed=="Ancona"|
                                    chicken_longevity$Breed=="Araucana"|
                                    chicken_longevity$Breed=="Australorp"|
                                    chicken_longevity$Breed=="Barnevelder"|
                                    chicken_longevity$Breed=="Delaware"|
                                    chicken_longevity$Breed=="Dominique"|
                                    chicken_longevity$Breed=="Dorking"|
                                    chicken_longevity$Breed=="Easter Egger"|
                                    chicken_longevity$Breed=="Faverolles"|
                                    chicken_longevity$Breed=="Leghorn"|
                                    chicken_longevity$Breed=="Marans"|
                                    chicken_longevity$Breed=="Old English Game"|
                                    chicken_longevity$Breed=="Plymouth Rock"|
                                    chicken_longevity$Breed=="Polish"|
                                    chicken_longevity$Breed=="Serama"|
                                    chicken_longevity$Breed=="Sussex"|
                                    chicken_longevity$Breed=="Welsummer")]<-2
chicken_longevity$Estimates[which(chicken_longevity$Breed=="Brahma"|
                                    chicken_longevity$Breed=="Cochin"|
                                    chicken_longevity$Breed=="ISA Brown"|
                                    chicken_longevity$Breed=="Rhode Island Red")]<-3
chicken_longevity$Estimates[which(chicken_longevity$Breed=="Orpington")]<-4
chicken_longevity$Estimates[which(chicken_longevity$Breed=="Wyandotte")]<-5

#A for loop setting the entry in the new "Breed_2" column to
#"Other" if a breed only has one etimate and to
#the entry of the "Breed" column if a breed has any other number of estimates
for(i in 1:nrow(chicken_longevity)){
  ifelse(chicken_longevity$Estimates[i]==1, 
         chicken_longevity$Breed_2[i]<-"Other",
         chicken_longevity$Breed_2[i]<-chicken_longevity$Breed[i])
}

#Subsetting the dataset to just the breeds not labelled "Other" in the Breed_2 column
chicken_long_breed <- subset(chicken_longevity, 
                             chicken_longevity$Breed_2!="Other")
#Subsetting the dataset to just the breeds labelled "Other" in the Breed_2 column
chicken_long_other <- subset(chicken_longevity,
                             chicken_longevity$Breed_2=="Other")

#Plotting the two data subsets and colouring the breed subset by breed
#So that breeds with more than one estimate are labelled
plot1 <- ggplot(data= chicken_long_other, aes(y=z.Egg_Production_Av,
                                              x=z.Longevity_Av))+
  geom_point(position = "jitter", size=3)+
  labs(y="z-Scaled Average Annual Egg Production",
       x="z-Scaled Average Total Lifespan")+
  theme_classic()+
  theme(text=element_text(size=16))+
  geom_point(data= chicken_long_breed, aes(y=z.Egg_Production_Av,
                                           x=z.Longevity_Av,
                                           colour=Breed),
             position = "jitter", size=3)
plot(plot1)

#Max Longevity Scatterplot
#Subsetting data to keep only the results with an entry for:
#1. Average egg production
#2. Maximum longevity
#3. Purpose
chicken_max_longevity <- subset(chicken, 
                                !is.na(chicken$Egg_Production_Av) & 
                                  !is.na(chicken$Longevity_Max) &
                                  !is.na(chicken$Purpose_2))
#Z-scaling data
chicken_max_longevity$z.Egg_Production_Av <- scale(chicken_max_longevity$Egg_Production_Av)
#Creating a new column in the dataframe and setting the contents as z-scaled average egg production
chicken_max_longevity$z.Longevity_Max <- scale(chicken_max_longevity$Longevity_Max)
#Creating a new column in the dataframe and setting the contents as z-scaled maximum longevity

chicken_max_longevity$Estimates <- NA
#Creating a new column in the dataframe and setting the contents as NA
chicken_max_longevity$Breed_2 <- NA
#Creating a new column in the dataframe and setting the contents as NA

#Manually setting how many estimates of each breed there are
chicken_max_longevity$Estimates[which(chicken_max_longevity$Breed=="Ameraucana"|
                                        chicken_max_longevity$Breed=="American Game Fowl"|
                                        chicken_max_longevity$Breed=="Andalusian"|
                                        chicken_max_longevity$Breed=="Araucana"|
                                        chicken_max_longevity$Breed=="Ayam Cemani"|
                                        chicken_max_longevity$Breed=="Barnevelder"|
                                        chicken_max_longevity$Breed=="Chantecler"|
                                        chicken_max_longevity$Breed=="Cornish"|
                                        chicken_max_longevity$Breed=="Delaware"|
                                        chicken_max_longevity$Breed=="Dominique"|
                                        chicken_max_longevity$Breed=="Dorking"|
                                        chicken_max_longevity$Breed=="Java"|
                                        chicken_max_longevity$Breed=="Kraienkopp"|
                                        chicken_max_longevity$Breed=="Lakenvelder"|
                                        chicken_max_longevity$Breed=="Leghorn"|
                                        chicken_max_longevity$Breed=="Marans"|
                                        chicken_max_longevity$Breed=="Minorca"|
                                        chicken_max_longevity$Breed=="Modern Game"|
                                        chicken_max_longevity$Breed=="Naked Neck"|
                                        chicken_max_longevity$Breed=="New Hampshire"|
                                        chicken_max_longevity$Breed=="Old English Game"|
                                        chicken_max_longevity$Breed=="Onagadori"|
                                        chicken_max_longevity$Breed=="Orloff"|
                                        chicken_max_longevity$Breed=="Orpington"|
                                        chicken_max_longevity$Breed=="Plymouth Rock"|
                                        chicken_max_longevity$Breed=="Redcap"|
                                        chicken_max_longevity$Breed=="Rhode Island White"|
                                        chicken_max_longevity$Breed=="Sapphire Gem"|
                                        chicken_max_longevity$Breed=="Silkie"|
                                        chicken_max_longevity$Breed=="Sultan"|
                                        chicken_max_longevity$Breed=="Sumatra"|
                                        chicken_max_longevity$Breed=="Sussex"|
                                        chicken_max_longevity$Breed=="Swedish Flower Hen"|
                                        chicken_max_longevity$Breed=="Yokohama")]<- 1
chicken_max_longevity$Estimates[which(chicken_max_longevity$Breed=="Brahma"|
                                        chicken_max_longevity$Breed=="Cochin"|
                                        chicken_max_longevity$Breed=="Easter Egger"|
                                        chicken_max_longevity$Breed=="Faverolles"|
                                        chicken_max_longevity$Breed=="ISA Brown"|
                                        chicken_max_longevity$Breed=="Welsummer"|
                                        chicken_max_longevity$Breed=="Wyandotte")]<- 2

#A for loop setting the entry in the new "Breed_2" column to
#"Other" if a breed only has one etimate and to
#the entry of the "Breed" column if a breed has any other number of estimates
for(i in 1:nrow(chicken_max_longevity)){
  ifelse(chicken_max_longevity$Estimates[i]==1, chicken_max_longevity$Breed_2[i]<-"Other",
         chicken_max_longevity$Breed_2[i]<-chicken_max_longevity$Breed[i])
}

#Subsetting the dataset to just the breeds not labelled "Other" in the Breed_2 column
chicken_max_long_breed <- subset(chicken_max_longevity, 
                                 chicken_max_longevity$Breed_2!="Other")
#Subsetting the dataset to just the breeds labelled "Other" in the Breed_2 column
chicken_max_long_other <- subset(chicken_max_longevity,
                                 chicken_max_longevity$Breed_2=="Other")

#Plotting the two data subsets and colouring the breed subset by breed
#So that breeds with more than one estimate are labelled
plot2 <- ggplot(data= chicken_max_long_other, aes(y=z.Egg_Production_Av,
                                                  x=z.Longevity_Max))+
  geom_point(position = "jitter", size=3)+
  labs(y="z-Scaled Average Annual Egg Production",
       x="z-Scaled Maximum Total Lifespan")+
  theme_classic()+
  theme(text=element_text(size=16))+
  geom_point(data= chicken_max_long_breed, aes(y=z.Egg_Production_Av,
                                               x=z.Longevity_Max,
                                               colour=Breed),
             position = "jitter", size=3)+
  scale_color_brewer(palette = "Paired")
plot(plot2)

#Productive Lifespan
#Subsetting data to keep only the results with an entry for:
#1. Average egg production
#2. Average productive lifspan
#3. Purpose
chicken_prod_life <- subset(chicken, 
                            !is.na(chicken$Egg_Production_Av) & 
                              !is.na(chicken$Productive_Lifespan_Av)&
                              !is.na(chicken$Source_Reliability)&
                              !is.na(chicken$Purpose_2))
#Z-scaling data
chicken_prod_life$z.Egg_Production_Av <- scale(chicken_prod_life$Egg_Production_Av)
#Creating a new column in the dataframe and setting the contents as z-scaled average egg production
chicken_prod_life$z.Productive_Lifespan_Av <- scale(chicken_prod_life$Productive_Lifespan_Av) 


chicken_prod_life$Estimates <- NA
#Creating a new column in the dataframe and setting the contents as NA
chicken_prod_life$Breed_2 <- NA
#Creating a new column in the dataframe and setting the contents as z-scaled average longevity

#Manually setting how many estimates of each breed there are
chicken_prod_life$Estimates[which(chicken_prod_life$Breed=="Ancona"|
                                    chicken_prod_life$Breed=="Babcock Brown"|
                                    chicken_prod_life$Breed=="Bovans Black"|
                                    chicken_prod_life$Breed=="Bovans Brown"|
                                    chicken_prod_life$Breed=="Dekalb Brown"|
                                    chicken_prod_life$Breed=="Dekalb White"|
                                    chicken_prod_life$Breed=="Denizli"|
                                    chicken_prod_life$Breed=="Ga Choi"|
                                    chicken_prod_life$Breed=="Ga Ri"|
                                    chicken_prod_life$Breed=="Ga Tre"|
                                    chicken_prod_life$Breed=="Gaok"|
                                    chicken_prod_life$Breed=="Gerze"|
                                    chicken_prod_life$Breed=="Hisex White"|
                                    chicken_prod_life$Breed=="Hy-Line Brown"|
                                    chicken_prod_life$Breed=="Hy-Line Pink"|
                                    chicken_prod_life$Breed=="Hy-Line Silver Brown"|
                                    chicken_prod_life$Breed=="Hy-Line Sonia"|
                                    chicken_prod_life$Breed=="INRA ev21"|
                                    chicken_prod_life$Breed=="INRA Jouy 850"|
                                    chicken_prod_life$Breed=="ISA White"|
                                    chicken_prod_life$Breed=="Jeokgalsaek Jaerae-jong"|
                                    chicken_prod_life$Breed=="Lohmann Sandy"|
                                    chicken_prod_life$Breed=="Lohmann Silver"|
                                    chicken_prod_life$Breed=="Lohmann Tradition"|
                                    chicken_prod_life$Breed=="Maatiaiskana"|
                                    chicken_prod_life$Breed=="Mallorquina"|
                                    chicken_prod_life$Breed=="Menorquina"|
                                    chicken_prod_life$Breed=="Mongolian Local Hen"|
                                    chicken_prod_life$Breed=="New Hampshire"|
                                    chicken_prod_life$Breed=="Penedesenca"|
                                    chicken_prod_life$Breed=="Pita Pinta"|
                                    chicken_prod_life$Breed=="Polbar"|
                                    chicken_prod_life$Breed=="Shaver Black"|
                                    chicken_prod_life$Breed=="Shaver Brown"|
                                    chicken_prod_life$Breed=="Shaver White")] <- 1
chicken_prod_life$Estimates[which(chicken_prod_life$Breed=="Altsteirer"|
                                    chicken_prod_life$Breed=="Babcock White"|
                                    chicken_prod_life$Breed=="Bovans White"|
                                    chicken_prod_life$Breed=="Cornish"|
                                    chicken_prod_life$Breed=="Hisex Brown"|
                                    chicken_prod_life$Breed=="Plymouth Rock")] <- 2
chicken_prod_life$Estimates[which(chicken_prod_life$Breed=="ISA Brown"|
                                    chicken_prod_life$Breed=="Rhode Island Red")] <- 3
chicken_prod_life$Estimates[which(chicken_prod_life$Breed=="Lohmann Brown")] <- 4
chicken_prod_life$Estimates[which(chicken_prod_life$Breed=="Leghorn")] <- 5

#A for loop setting the entry in the new "Breed_2" column to
#"Other" if a breed only has one etimate and to
#the entry of the "Breed" column if a breed has any other number of estimates
for(i in 1:nrow(chicken_prod_life)){
  ifelse(chicken_prod_life$Estimates[i]==1, chicken_prod_life$Breed_2[i]<-"Other",
         chicken_prod_life$Breed_2[i]<-chicken_prod_life$Breed[i])
}

#Subsetting the dataset to just the breeds not labelled "Other" in the Breed_2 column
chicken_prod_breed <- subset(chicken_prod_life, 
                             chicken_prod_life$Breed_2!="Other")
#Subsetting the dataset to just the breeds labelled "Other" in the Breed_2 column
chicken_prod_other <- subset(chicken_prod_life,
                             chicken_prod_life$Breed_2=="Other")

#Plotting the two data subsets and colouring the breed subset by breed
#So that breeds with more than one estimate are labelled
plot3 <- ggplot()+
  geom_point(data= chicken_prod_other, aes(y=z.Egg_Production_Av,
                                           x=z.Productive_Lifespan_Av),
             position="jitter", size=3)+
  labs(y="z-Scaled Average Annual Egg Production",
       x="z-Scaled Average Productive Lifespan")+
  theme_classic()+
  theme(text=element_text(size=16))+
  geom_point(data= chicken_prod_breed, aes(y=z.Egg_Production_Av,
                                           x=z.Productive_Lifespan_Av,
                                           colour=Breed),
             position = "jitter", size=3)+
  scale_color_brewer(palette = "Paired")
plot(plot3)
