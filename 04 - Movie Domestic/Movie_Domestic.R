#read "Data Movie.csv" file:
movies <- read.csv(file.choose())


colnames(movies) <- c("Day", "Director", "Genre", "Title", "Release", "Studio", "Adjusted.Gross", 
                      "Budget", "Gross", "IMDb", "LensRating", "Overseas", "OverseasPercent", "Profit", 
                      "ProfitPercent", "Runtime", "US", "GrossPercentUS")

selecGenre <- movies$Genre %in% c("action", "adventure", "animation", "comedy", "drama") 
selecGenre

SelectStudio  <- movies$Studio %in% c("Buena Vista Studios", "Fox", "Paramount Pictures", "Sony", "Universal", "WB")
SelectStudio

selectMovies <- movies[selecGenre & SelectStudio,]
selectMovies

head(SelectStudio)
str(SelectStudio)
summary(SelectStudio)


library(ggplot2)



ggplot(data = selectMovies, aes(x = Genre, y = GrossPercentUS)) +
  geom_jitter(aes(size = Budget, colour = Studio)) +
  geom_boxplot(alpha = 0.6, outlier.colour = NA) + 
  xlab("Genre") + 
  ylab("Gross % US") +
  ggtitle("Domestic Gross % by Genre") +
  labs(size = "Budget $M", colour = "Studios") +
  theme(axis.title.x = element_text(colour = "DarkBlue", size = 20),
        axis.title.y = element_text(colour = "DarkBlue", size = 20),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        plot.title = element_text(hjust = 0.5, size = 25, ),)

