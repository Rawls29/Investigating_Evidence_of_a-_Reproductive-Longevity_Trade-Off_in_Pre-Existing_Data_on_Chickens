rm(list=ls())
setwd('C:/Users/samra/Documents/My Documents/Uni/Imperial/Winter Project/R')
load(file = "chicken.Rda")

library(ggplot2)

#Av Longevity Scatterplot
#Subsetting data
chicken_longevity <- subset(chicken,
                            !is.na(chicken$Egg_Production_Av) &
                              !is.na(chicken$Longevity_Av) &
                              !is.na(chicken$Purpose_2))
#Z-scaling data
chicken_longevity$z.Egg_Production_Av <- scale(chicken_longevity$Egg_Production_Av)
chicken_longevity$z.Longevity_Av <- scale(chicken_longevity$Longevity_Av)
#Plotting scatterplot
ggplot(data= chicken_longevity, aes(y=z.Egg_Production_Av,
                                            x=z.Longevity_Av))+
  geom_point()+
  labs(y="z-Scaled Average Annual Egg Production",
       x="z-Scaled Average Total Lifespan")

#Max Longevity Scatterplot
#Subsetting data
chicken_max_longevity <- subset(chicken, 
                                !is.na(chicken$Egg_Production_Av) & 
                                  !is.na(chicken$Longevity_Max) &
                                  !is.na(chicken$Purpose_2))
#Z-scaling data
chicken_max_longevity$z.Egg_Production_Av <- scale(chicken_max_longevity$Egg_Production_Av)
chicken_max_longevity$z.Longevity_Max <- scale(chicken_max_longevity$Longevity_Max)
#Plotting scatterplot
ggplot(data= chicken_max_longevity, aes(y=z.Egg_Production_Av,
                                      x=z.Longevity_Max))+
  geom_point()+
  labs(y="z-Scaled Average Annual Egg Production",
       x="z-Scaled Maximum Total Lifespan")

#Productive Lifespan
#Subsetting data
chicken_prod_life <- subset(chicken, 
                            !is.na(chicken$Egg_Production_Av) & 
                              !is.na(chicken$Productive_Lifespan_Av)&
                              !is.na(chicken$Source_Reliability)&
                              !is.na(chicken$Purpose_2))
#Z-scaling data
chicken_prod_life$z.Egg_Production_Av <- scale(chicken_prod_life$Egg_Production_Av)
chicken_prod_life$z.Productive_Lifespan_Av <- scale(chicken_prod_life$Productive_Lifespan_Av)                                                
#Plotting scatterplot
ggplot(data= chicken_prod_life, aes(y=z.Egg_Production_Av,
                                            x=z.Productive_Lifespan_Av))+
  geom_point()+
  labs(y="z-Scaled Average Annual Egg Production",
       x="z-Scaled Average Productive Lifespan")
