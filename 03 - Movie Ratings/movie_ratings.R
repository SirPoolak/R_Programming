
#select "Data_Movie_Ratings.csv" file:
movies <- read.csv(file.choose())


colnames(movies) <- c("Film", "Genre", "CriticRating", "AudienceRating", "BudgetMillions", "Year")
movies$Year <- factor(movies$Year)



head(movies)
str(movies)
summary(movies)


library(ggplot2)

#-----------------------------------POINTS
#Critic Rating vs Audience Rating
ggplot(movies) +
  geom_point(aes(x=CriticRating, y=AudienceRating, colour=Genre, size=BudgetMillions, alpha=I(0.6)))

#Statistical Transformations
ggplot(data = movies, aes(x=CriticRating, y=AudienceRating, colour=Genre)) +
  geom_point(aes(size=BudgetMillions), alpha=I(0.4)) +
  geom_smooth(fill = NA) +
  xlab("Critic Rating") +
  ylab("Audience Rating") +
  ggtitle("Critic Rating vs Audience Rating") +
  theme(axis.title.x = element_text(colour = "DarkGreen", size = 30,),
        axis.title.y = element_text(colour = "Red", size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 20),
        
        plot.title = element_text(colour = "DarkBlue", size = 40))



#----------------------------------HISTOGRAM
#Budget
ggplot(movies) +
  geom_histogram(aes(x = BudgetMillions, fill = Genre), binwidth = 10, colour = "Black") + 
  xlab("Budget Axis") +
  ylab("Number of Movies") +
  ggtitle("Movie Budget Distribution") +
  theme(axis.title.x = element_text(colour = "DarkGreen", size = 30,),
        axis.title.y = element_text(colour = "Red", size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 20),
        legend.position = c(1, 1),
        legend.justification = c(1,1),
        
        plot.title = element_text(colour = "DarkBlue", size = 40))



#Audience Rating
ggplot(movies) +
  geom_histogram(aes(x = AudienceRating), binwidth = 10, fill = "white", colour = "Blue") + 
  xlab("Audience Axis") +
  ylab("Number of Movies") +
  ggtitle("Audience Rating") +
  theme(axis.title.x = element_text(colour = "DarkGreen", size = 30,),
        axis.title.y = element_text(colour = "Red", size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        
        plot.title = element_text(colour = "DarkBlue", size = 40,))


#Critic Rating
ggplot(movies) +
  geom_histogram(aes(x = CriticRating), binwidth = 10, fill = "white", colour = "Blue") +
  xlab("Critic Axis") +
  ylab("Number of Movies") +
  ggtitle("Critic Rating") +
  theme(axis.title.x = element_text(colour = "DarkGreen", size = 30,),
        axis.title.y = element_text(colour = "Red", size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        
        plot.title = element_text(colour = "DarkBlue", size = 40,))



ggplot(data = movies, aes(x = BudgetMillions)) +
  geom_histogram(aes(fill = Genre), binwidth = 10, colour = "Black") +
  facet_grid(Genre~., scales = "free")



#----------------------------------DENSITY
ggplot(movies) +
  geom_density(aes(x = BudgetMillions, fill = Genre),
               position = "stack")



#----------------------------------BOXPLOT
#Audience Rating
ggplot(data = movies,aes(x = Genre, y = AudienceRating, colour = Genre)) + 
  geom_jitter() +
  geom_boxplot(size = 1.2, alpha = 0.5) +
  xlab("") +
  ylab("Audience Rating") +
  ggtitle("Audience Rating Box-plot by Genre") +
  theme(axis.title.x = element_text(colour = "DarkGreen", size = 30,),
        axis.title.y = element_text(colour = "Red", size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 20),
        
        plot.title = element_text(colour = "DarkBlue", size = 40))


#Critic Rating
ggplot(data = movies,aes(x = Genre, y = CriticRating, colour = Genre)) + 
  geom_jitter() +
  geom_boxplot(size = 1.2, alpha = 0.5)




#----------------------------------SCATTERPLOTS
ggplot(data = movies, aes(x = CriticRating, y = AudienceRating, colour = Genre)) +
  geom_point(size = 2) + 
  facet_grid(Genre~.)

ggplot(data = movies, aes(x = CriticRating, y = AudienceRating, colour = Genre)) +
  geom_point(size = 2) + 
  facet_grid(.~Year)

ggplot(data = movies, aes(x = CriticRating, y = AudienceRating, colour = Genre)) +
  geom_point(size = 2) + 
  facet_grid(Genre~Year)

ggplot(data = movies, aes(x = CriticRating, y = AudienceRating, colour = Genre)) +
  geom_point(aes(size = BudgetMillions)) +
  geom_smooth() +
  facet_grid(Genre~Year) +
  coord_cartesian(ylim = c(0, 100)) +
  geom_smooth(fill = NA) +
  xlab("Critic Rating") +
  ylab("Audience Rating") +
  ggtitle("Correlation between audience and critic, evolved throughout the years by genre.") +
  theme(axis.title.x = element_text(colour = "DarkGreen", size = 20,),
        axis.title.y = element_text(colour = "Red", size = 20),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 15),
        
        plot.title = element_text(colour = "DarkBlue", size = 30))

