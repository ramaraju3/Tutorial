FoodPoison <- read.csv("Food_Poisoning.csv", as.is=TRUE, header=TRUE)
FoodPoison
head(FoodPoison)
new.dfcolnames(FoodPoison)


National_cuisine_type <- c("American","British","Chinese","Continental_European",
                           "Indian","Italian","Sandwich_bar","Seafood","Other_Cuisines","Not_known")
S_Enteritidis_non_PT <- c(0,17,76,4,4,12,5,0,18,0)
S_Enteritidis_PT <- c(5,16,48,8,12,20,3,0,15,3)
S_Typhimurium <- c(0,1,7,0,10,5,2,0,4,0)
Other_Salmonella_spp <- c(0,5,2,1,6,1,2,0,2,2)
Bacillus_spp <- c(0,0,15,1,22,1,0,0,5,6)
Campylobacter_spp <- c(0,11,4,3,7,3,0,1,9,1)
Clostridium_perfringens <- c(3,6,3,2,11,2,0,0,8,3)
Scombrotoxin <- c(0,2,0,7,0,4,5,3,6,0)
VTEC <- c(0,4,1,0,5,0,0,0,1,1)
Foodborne_viruses <- c(3,11,4,4,2,2,5,8,9,2)
Other_pathogen_or_toxin <- c(4,12,8,2,11,13,5,7,10,8)
Mixed_pathogens <- c(0,0,2,0,1,0,1,0,0,0)
Not_known <- c(5,3,5,3,14,6,1,12,8,4)



sample2 <- cbind(National_cuisine_type,S_Enteritidis_non_PT,S_Enteritidis_PT,S_Typhimurium,
                Other_Salmonella_spp,Bacillus_spp,Campylobacter_spp,Clostridium_perfringens,Scombrotoxin,VTEC,
                Foodborne_viruses,Other_pathogen_or_toxin,Mixed_pathogens,Not_known)

FoodPoisonDF<-as.data.frame(sample2)

library(reshape2)
new.df<-melt(FoodPoisonDF, id.vars="National_cuisine_type", variable.name="Bacteria", value.name="count")
new.df$count <- as.numeric(new.df$count)

library(grid)
hw <- theme_gray() + theme(
  strip.background=element_rect(fill=rgb(.9,.95,1),
                                colour=gray(.5), size=.2),
  panel.border=element_rect(fill=FALSE,colour=gray(.5)),
  axis.text=element_text(colour="black"),
  axis.ticks=element_blank(),
  axis.ticks.margin=unit(-0.05,"cm"),
  panel.grid.minor = element_blank()
)

library(ggplot2)
ggplot(new.df, aes(y=Bacteria,x=count, color = factor(Bacteria), size=.5))+ 
  geom_point()+
  facet_grid(. ~ National_cuisine_type)+
  labs(list(title = "Restaurant-associated foodborne outbreaks reported in England and Wales from 1992 to 2009 \n 
            A comparison between Food types & Bacteria causing poisoning", x = "Number of incidents reported",
            y = "Types of Bacteria"))+hw

ggplot(new.df, aes(y=Bacteria,x=count, color = factor(Bacteria), size=.5))+ 
  geom_point()+
  facet_grid(. ~ National_cuisine_type, scales="free_x")+hw+
  labs(list(title = "Restaurant-associated foodborne outbreaks reported in England and Wales from 1992 to 2009 \n 
            A comparison between Food types & Bacteria causing poisoning", x = "Number of incidents reported",
            y = "Types of Bacteria"))

ggplot(new.df, aes(y=Bacteria,x=count, color = factor(Bacteria), size=.5))+ 
  geom_point()+
  facet_grid(. ~ National_cuisine_type, scales="free_x", space="free_x")+hw+
  labs(list(title = "Restaurant-associated foodborne outbreaks reported in England and Wales from 1992 to 2009 \n 
            A comparison between Food types & Bacteria causing poisoning", x = "Number of incidents reported",
            y = "Types of Bacteria"))


ggplot(new.df,aes(x=National_cuisine_type,y=count,fill=Bacteria))+
  geom_bar(stat="identity", position="dodge",colour="black")+
  theme_bw()+labs(list(title = "Restaurant-associated foodborne outbreaks reported in England and Wales from 1992 to 2009 \n 
            A comparison between Food types & Bacteria causing poisoning", x = "Types of cuisines",
                       y = "Number of reported outbreaks"))


sample<-FoodPoison[,c(16:28)];

#record virus names
virus_names <- names(sample);

#decide colors 
colors <- rainbow(12);

#Decide american values
american <- sample[1,]
american_values <- as.numeric(american[1,])

#Do the same for all the others
british <- sample[2,]
british_values <- as.numeric(british[1,])

chinese <- sample[3,]
chinese_values <- as.numeric(chinese[1,])

continental_european <- sample[4,]
continental_european_values <- as.numeric(continental_european[1,])

indian <- sample[5,]
indian_values <- as.numeric(indian[1,])

italian <- sample[6,]
italian_values <- as.numeric(italian[1,])

sandwich_bar <- sample[7,]
sandwich_bar_values <- as.numeric(sandwich_bar[1,])

seafood <- sample[8,]
seafood_values <- as.numeric(seafood[1,])

other_cuisines <- sample[9,]
other_cuisines_values <- as.numeric(other_cuisines[1,])

not_known <- sample[10,]
not_known_values <- as.numeric(not_known[1,])

barplot_data <- data.frame(American = american_values, British = british_values, 
                           Chinese = chinese_values, Continental = continental_european_values, 
                           Indian = indian_values, Italian = italian_values, 
                           Sandwich_Bar = sandwich_bar_values, Seafood = seafood_values, 
                           Other = other_cuisines_values, Not_Known = not_known_values);

barplot(as.matrix(barplot_data), main = "Bacteria vs Cuisines", beside = T, col= colors, ylim = c(0,80));

par(xpd = T);

legend(30, 75, virus_names, cex = 0.7,colors, ncol =2, bty="n")
