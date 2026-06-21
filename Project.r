library(dplyr)
library(ggplot2)
library(plotly)
library(knitr)
library(maps)
library(countrycode)
library(patchwork)
library(reshape2)

setwd(getwd())
getwd()

movies <- read.csv("/home/cyrill/Downloads/R_Project/movies.csv", fill = TRUE, header = TRUE) |>
  filter(!is.na(date), !is.na(rating), minute <= 240, date < 2024)

studios <- read.csv("/home/cyrill/Downloads/R_Project/studios.csv", fill = TRUE, header = TRUE)

releases <- read.csv("/home/cyrill/Downloads/R_Project/releases.csv", fill = TRUE, header = TRUE) |>
  filter(country == "USA") |>
  filter(type == "Theatrical" | type == "Digital") |>
  select(-date)|>
  distinct()|>
  rename(age_rating = rating)

genres <- read.csv("/home/cyrill/Downloads/R_Project/genres.csv", fill = TRUE, header = TRUE) 

directors <- read.csv("/home/cyrill/Downloads/R_Project/directors.csv", fill = TRUE, header = TRUE)

countries <- read.csv("/home/cyrill/Downloads/R_Project/countries.csv", fill = TRUE, header = TRUE)


ggplot(movies) + aes(x = date) + geom_bar()

movies_releases <- inner_join(movies, releases, by = "id")
head(movies_releases)

movies_releases_grouped <- movies_releases |>
  group_by(name, date) |>
  filter(date > 2012)|>
  filter(any(type == "Digital") && all(type != "Theatrical")) |>
  ungroup()|>
  inner_join(genres, by = "id")|>
  group_by(date, genre)|>
  summarise(n = n())
head(movies_releases_grouped, 20)
p <- ggplot(movies_releases_grouped) + aes(x = date, y = n, group = genre) + geom_line(aes(color = genre))+ geom_point(size = 1, aes(color = genre))
ggplotly(p)

# 
# movies_releases_grouped <- movies_releases |>
#   group_by(name, date) |>
#   filter(date > 2000)|>
#   filter(any(type == "Digital") && all(type != "Theatrical")) |>
#   ungroup()|>
#   group_by(date)|>
#   summarise(n_movies = n())
# head(movies_releases_grouped, 20)
# p <- ggplot(movies_releases_grouped) + aes(x = date, y = n_movies) + geom_line()+ geom_point(size = 1)
# ggplotly(p)



# movies|>
#   group_by(date) |>
#   summarize(n = n())|>
#   ggplot() +
#   aes(x = date, y = n) +
#   geom_line()


m <- movies|>
  filter(date > 2000)|>
  group_by(date) |>
  summarize(n = n())

m <- movies|>
  filter(date > 2012)|>
  group_by(date) |>
  summarize(n = n()) |>
  inner_join(movies_releases_grouped, by = "date")

head(movies_genres_grouped)
head(m)

k <- m |>
  filter(genre == "Comedy")

#####
movies_genres_grouped <- inner_join(movies, genres, by = "id") |>
  filter(date > 2000) |>
  group_by(date, genre) |>
  summarise(n = n())

movies_releases_grouped <- inner_join(movies, releases, by = "id") |>
  group_by(name, date) |>
  filter(date > 2000)|>
  filter(any(type == "Digital") && all(type != "Theatrical")) |>
  ungroup()|>
  group_by(date)|>
  summarise(n_movies = n())

m <- inner_join(movies, releases, by = "id") |>
  group_by(name, date) |>
  filter(date > 2000)|>
  filter(any(type == "Digital") && all(type != "Theatrical")) |>
  ungroup()|>
  group_by(date)|>
  summarise(n_movies = n())|>
  inner_join(inner_join(movies, genres, by = "id") |>
               filter(date > 2000) |>
               group_by(date, genre) |>
               summarise(n = n()), by = "date")


movies_year <- movies|>
  filter(date > 2000) |>
  group_by(date) |>
  summarise(all = n())

head(m)




bullshit <- inner_join(movies, releases, by = "id") |>
  group_by(name, date) |>
  filter(date > 2000)|>
  filter(any(type == "Digital") && all(type != "Theatrical")) |>
  ungroup()|>
  group_by(date)|>
  summarise(n_movies = n())|>
  inner_join(
    inner_join(movies_genres_grouped, movies_year, by = "date")|>
  mutate(percent = n/all)
  ) |>
  mutate(digital_percent = n_movies/all)

head(bullshit)

bullshit|>
  ggplot() +
  aes(x = digital_percent, y = percent, color = genre) +
  geom_point()

head(movies_genres_grouped)
correlation <- list() 
counter <-  0 
unique(m$genre)

for (i in unique(m$genre))
{
  counter <- counter + 1
  x = as.numeric(m[m$genre == i,]$n)
  y = as.numeric(m[m$genre == i,]$n_movies)
  
  correlation[[counter]] <- cor(x,y)
}

correlation

m|>filter(genre == "TV Movie")|>ggplot() + aes(x = n_movies, y = n, group = date, color = genre) + geom_point() + facet_wrap(vars(genre), nrow = 5) + guides(color = FALSE)


ggplot(m) + aes(x = n, y = n_movies, group = date, color = genre) + geom_point() + facet_wrap(vars(genre), nrow = 5, scales = "free_x") + guides(color = FALSE) + labs(x = "Digital only movies", y = "All movies")
ggplot(m) + aes(x = n_movies, y = n, group = date, color = genre) + geom_point()

head(m)
m |>
  filter(genre == "Crime")|>
  ggplot() + aes(x = n.x, y = n.y) + geom_point()
head(m)
head(movies_releases_grouped)



ggplot() + geom_line(data = movies_releases_grouped, aes(x = date, y = n_movies)) +
  geom_line(data = m, aes(x = date, y = n, color = "red"))

ggplot() + geom_line(data = movies_releases_grouped, aes(x = date, y = n_movies)) +
  geom_line(data = m, aes(x = date, y = n.y, color = "red"))
  
ggplo


ggplot(movies_genres_grouped) + aes(x = date, y = n, group = genre) + geom_line(aes(color = genre)) + facet_wrap(vars(genre), nrow = 5) + guides(color = FALSE)


movies_genres <- inner_join(movies, genres, by = "id") |>
  filter(date > 2000)

movies_genres|>
  ggplot() + aes(x = date, y = rating, group = genre) + geom_point(aes(color = genre))+ facet_wrap(vars(genre), nrow = 5) + guides(color = "none")


head(movies_genres_grouped, 10)
ggplot(movies_genres_grouped) + aes(x = date, y = n, group = genre) + geom_line(aes(color = genre)) + facet_wrap(vars(genre), nrow = 5) + guides(color = "none") + geom_smooth(method = "lm", weight = 0.1, alpha = 0.1, aes(color = genre))

##### Shit for real this time
movies|>
  ggplot() + aes(x = date, y = minute) + 
  geom_boxplot(aes(group = date), outlier.alpha = 0.05, outlier.shape = 1) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  scale_x_continuous(breaks = seq(1872, 2024, by = 4), expand = c(0,1))

ggplot(movies) + aes(x = minute, y = rating) + geom_point(alpha = 0.1)

head(directors)

inner_join(movies, directors, by = "id") |>
  ggplot() + 
  aes(x = )

movie_project <- read.csv("Movie_Data_File.csv", fill = TRUE, header = TRUE)|>
  select(Film_title, Runtime, Likes)|>
  rename(name = Film_title)

summary(movie_project)

head(movie_project)
write.csv(shit, file = "movies.csv", row.names = FALSE)

shit <- inner_join(temp, movie_project, by = c("name",  "minute" ="Runtime")) |>
  filter(!is.na(date), !is.na(rating), minute <= 240, date < 2024) |>
  distinct(id, .keep_all = TRUE)

head(movies)
head(shit)

summary(shit)

temp <- read.csv("movies.csv", fill = TRUE, header = TRUE)
head(temp)



shit|>
  ggplot() + aes(x = date, y = minute) + 
  geom_boxplot(aes(group = date), outlier.alpha = 0.05, outlier.shape = 1) +
  labs(
    title = "Lenght of the movie throughout years",
    x = "Year",
    y = "Lenght",
  ) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  scale_x_continuous(breaks = seq(1872, 2024, by = 4), expand = c(0,1))


#####
top10studios_rating <- inner_join(shit, studios, by = "id") |>
  group_by(studio)|>
  summarise(n = n(), mean_rating = mean(rating))|>
  filter(n > 4)|>
  arrange(desc(mean_rating))


head(top10studios_rating, 40)


movies_studios <- inner_join(movies, studios, by = "id") |>
  filter(studio %in% pull(top10studios, studio)) |>
  group_by(date, studio)|>
  summarise(mean_rating = mean(rating))

head(movies_studios, 10)

top10studios <- studios|>
  group_by(studio)|>
  summarise(n_movies = n())|>
  arrange(desc(n_movies))|>
  slice_head(n = 10)
head(top10studios, 10)


top10studios_rating <- inner_join(shit, studios, by = "id") |>
  group_by(studio)|>
  summarise(n = n(), mean_rating = mean(rating))|>
  filter(n > 4)|>
  arrange(desc(mean_rating))|>
  slice_head(n = 10)


head(top10studios_rating, 10)

movies_studios <- inner_join(movies, studios, by = "id") |>
  filter(studio %in% pull(top10studios_rating, studio)) |>
  group_by(date, studio)|>
  summarise(mean_rating = mean(rating))


ggplot(movies_studios) + aes(x = date, y = mean_rating) + geom_line(aes(color = studio)) + facet_wrap(vars(studio), nrow = 5) + 
  guides(color = "none")



ggplot(movies_studios) + aes(x = date, y = mean_rating) + geom_line(aes(color = studio)) + facet_wrap(vars(studio), nrow = 5) + 
  geom_smooth(method = "lm", linewidth = 0.5) +
  guides(color = "none")

movies_studios <- inner_join(movies, studios, by = "id") |>
  group_by(date, studio)|>
  summarise(mean = mean(rating), n = n())|>
  ungroup()|>
  group_by(studio)|>
  summarise(n_movies = n()) |>
  filter(n_movies > 3) |>
  arrange(desc(n_movies))|>
  slice_head(n = 10)
head(movies_studios, 10)

inner_join(movies, releases, by = "id") |>
  group_by(name, date) |>
  filter(date >= 2010)|>
  filter(any(type == "Digital") && all(type != "Theatrical")) |>
  ungroup()|>
  group_by(date)|>
  inner_join(genres, by = "id") |>
  filter(date >= 2012)|>
  group_by(date, genre)|>
  summarise(n = n()) |>
  ggplot() + 
  aes(x = date, y = n, group = genre) + 
  geom_line(aes(color = genre)) + 
  geom_point(size = 1, aes(color = genre)) +
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +
  labs(
    title = "Growth of the genres after 2012",
    x = "Year",
    y = "Number of films",
    color = "Genre"
  )

 
ggplot(dat_world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white") + 
  coord_fixed(ratio = 1.3)


head(countries)
head(genres)
head(dat_for_map)
dat_for_map <- inner_join(genres, countries, by = "id") |>
  mutate(country = countrycode::countrycode(
    sourcevar = country, origin = 'country.name', destination = 'country.name')
  ) |>
  group_by(genre, country)|>
  summarise(n = n())|>
  ungroup()|>
  group_by(country)|>
  slice(which.max(n))
dat_world_map <- map_data("world") |> 
  mutate(region = countrycode::countrycode(
    sourcevar = region, origin = 'country.name', destination = 'country.name')
  )
p <- dplyr::full_join(x = dat_for_map, y = dat_world_map, 
                      by = c("country" = "region"),
                      multiple = "all", relationship = "many-to-many") |> 
  ggplot(mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(mapping = aes(fill = genre)) +  
  coord_fixed(ratio = 1.3) +
  labs(fill = "Genre")
ggplotly(p)


movies|>
  ggplot() + aes(x = rating, y = Watches) + geom_point() + geom_smooth(method = "lm")

movies |>
  mutate(percent = Likes/Watches) |>
  ggplot() + aes(x = rating, y = percent) + geom_point()

movies |>
  mutate(percent = Total_ratings/Watches) |>
  ggplot() + aes(x = rating, y = percent) + geom_point()

movies|>
  ggplot() + aes(x = rating, y = Likes) + geom_point() + geom_smooth()

movies|>
  ggplot() + aes(x = rating, y = Total_ratings) + geom_point() + geom_smooth()

p <- movies|>
  ggplot() + aes(x = rating, y = Total_ratings) + geom_point()

m <- movies |>
  mutate(percent = Total_ratings/Watches)
pearson_test_result <- cor(m$rating, m$percent)
spearman_test_result <- cor(m$rating, m$percent, method = "spearman")
print(pearson_test_result)
print(spearman_test_result)

ggplotly(p)

letterboxd <- read.csv(file = "Movie_Data_File.csv", header = TRUE) |>
  select(Film_title, Average_rating, Watches,Likes,R_0.5,R_1,R_1.5,R_2,R_2.5,R_3,R_3.5,R_4,R_4.5,R_5,Total_ratings, Original_language) |>
  filter(!is.na(Average_rating)) |>
  rename(title = Film_title)|>
  mutate(R_0.5 = R_0.5 / Total_ratings, R_1 = R_1 / Total_ratings, R_1.5 = R_1.5 / Total_ratings, R_2 = R_2 / Total_ratings, 
         R_2.5 = R_2.5 / Total_ratings, R_3 = R_3 / Total_ratings, R_3.5 = R_3.5 / Total_ratings, R_4 = R_4 / Total_ratings, 
         R_4.5 = R_4.5 / Total_ratings, R_5 = R_5 / Total_ratings, Likes = Likes / Watches, Total_ratings = Total_ratings / Watches)
summary(letterboxd)
head(letterboxd)

letterboxd |>
  ggplot() + aes(Likes, R_3) + geom_point()



correlation <- list() 
counter <-  0 


c1 <- cor(letterboxd[,5:14], letterboxd[,4], method = "spearman")
c2 <- cor(letterboxd[,5:14], letterboxd[,15], method = "spearman")

rownames(c1) <- NULL
rownames(c2) <- NULL

table <- data.frame(
  Rating = c("R_0.5","R_1","R_1.5","R_2","R_2.5","R_3","R_3.5","R_4","R_4.5","R_5"),
  Likes = c1[,1],
  Total_Rating = c2[,1])

table
kable(table, align="l")



unique(m)

letterboxd|>
  melt(id.vars = "Likes", measure.vars =  c("R_0.5","R_1","R_1.5","R_2","R_2.5","R_3","R_3.5","R_4","R_4.5","R_5"))|>
  ggplot(aes(Likes, value)) +
  geom_point() +
  facet_grid(variable ~ ., shrink = FALSE)

plots <- list() 
counter <-  0 

parse(text = "R_0.5")

ggplot(letterboxd) + aes(x = .data[["Likes"]], y = .data[["R_0.5"]]) + geom_point()

for (i in c("R_0.5","R_1","R_1.5","R_2","R_2.5","R_3","R_3.5","R_4","R_4.5","R_5"))
{
  counter <- counter + 1
  
  
  plots[[counter]] <- ggplot(letterboxd) + aes(Likes, .data[[i]]) + geom_point(alpha = 0.1)
}
wrap_plots(plots, ncol = 3)

plots



movies|>
  ggplot() + 
  aes(x = minute, y = after_stat(density)) + 
  geom_histogram(binwidth = 4) +
  geom_density(color = "red")


letterboxd |> 
  count(Original_language) |>
  top_n(n = 5) |>
  ggplot(aes(x = "", y = n, fill = Original_language)) +
  geom_col() +
  coord_polar("y") +
  labs(title = "Graph") +
  theme_void()
letterboxd

letterboxd |>
  select(Original_language) |>
  group_by(Original_language) |>
  count(Original_language)|>
  arrange(desc(n))|>
  group_by(Original_language = factor(c(Original_language[1:6], rep("Other", n() - 6)),
                            levels = c(Original_language[1:6], "Other"))) |>
  tally(n) 

