setwd("~/Università - work_in_progress/LABORATORIO E GESTIONE DI IMPRESA/Business case P&G")

## PACKAGES
packages <- c("DataExplorer","plotly","ggplot2","corrplot","pander","caret","readxl",
              "sf","mapview","miscTools","dplyr","caret","rpart","rpart.plot",
              "pROC", "car")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))

## DATASET and DATA PREPROCESSING

# load dataset
dataset <- read_excel("SALES - FEMALE GROOMING.xlsx")

# data preprocessing
colnames(dataset) <- c("codicePDV","precipLuglio","tempmaxLuglio","numHotels",
                       "numCamping","numBedandBreakfast","TotalAccomodations",
                       "Latitude","Longitude","Regione","ResidentiTotali",
                       "ResidentiUomini","ResidentiDonne","StatoCivile",
                       "AltroStatoCivile","Divorziati","Eta5","Eta5-9",
                       "Eta10-14","Eta15-19","Eta20-24","Eta25-29","Eta30-34",
                       "Eta35-39","Eta40-44", "Eta45-49","Eta50-54","Eta55-59",
                       "Eta60-64","Eta65-69","Eta70-74","Eta74","Laurea",
                       "Diploma","Media","Elementare","Occupati","Disoccupati",
                       "Studenti","TotaleFamiglie","ComponentiFamiglie",
                       "Reddito","Provincia","DistanzaMare","ValueSales")
dataset$Regione <- as.factor(dataset$Regione)
dataset$Provincia <- as.factor(dataset$Provincia)
levels(dataset$Provincia) <- c("AG", "AL", "AN", "AR", "AT", "AV", "BA",
                               "BL", "BG", "BO", "BR", "CA", "CB", "CE",
                               "CT", "CH", "CO", "CS", "FI", "FR", "GE",
                               "GO", "AQ", "LT", "MT", "NA", "NO", "PD",
                               "PA", "PG", "PN", "BZ", "TN", "RM", "TO",
                               "AO")

# structure dataset
str(dataset)

# check if there are any missing values
pander(introduce(dataset))
plot_missing(dataset)

## DATA EXPLORATION

# summary of all attributes
pander(summary(dataset))

# stores location
location <- st_as_sf(dataset, coords = c("Longitude","Latitude"), crs = 4326)
mapview(location)

# barplot of all categorical attributes
plot_bar(dataset[1:240,], ggtheme = theme_bw())

# histogram of all numerical attributes
plot_histogram(dataset[1:240,-c(1,8,9)], ncol = 6L, ggtheme = theme_bw())

# correlation matrix (senza codicePDV, Lat, Long, Regione e Provincia)
dataset_num <- dataset[1:240,-c(1,8,9,10,43)]
corrplot(cor(dataset_num), method = "number", diag = FALSE)
corrplot(cor(dataset_num[,c(1,2,4,39,40)]), method = "number", diag = FALSE)

# scatterplots
s1 <- ggplot(dataset[1:240,], aes(x = precipLuglio, y = ValueSales)) + 
  geom_point() + 
  stat_smooth(method = "lm", col = "#C42126", size = 1)

s2 <- ggplot(dataset[1:240,], aes(x = tempmaxLuglio, y = ValueSales)) + 
  geom_point() + 
  stat_smooth(method = "lm", col = "#C42126", size = 1) 

s3 <- ggplot(dataset[1:240,], aes(x = numCamping, y = ValueSales)) + 
  geom_point() + 
  stat_smooth(method = "lm", col = "#C42126", size = 1)
  
s4 <- ggplot(dataset[1:240,], aes(x = DistanzaMare, y = ValueSales)) + 
  geom_point() + 
  stat_smooth(method = "lm", col = "#C42126", size = 1)

subplot(s1, s2, s3, s4, nrows = 2, titleX = TRUE, titleY = TRUE)

## LINEAR REGRESSION and PREDICTION OF MISSING VALUE SALES

# fit_1
fit_1 <- lm(ValueSales ~ ., 
            data = dataset[1:240,])
summary(fit_1)

# fit_2
fit_2 <- lm(ValueSales ~ tempmaxLuglio + DistanzaMare + numCamping, 
            data = dataset[1:240,])
summary(fit_2)
coef(fit_2)

# check fit_2
vif(fit_2) # check collinearity
predict(fit_2, data.frame(tempmaxLuglio = dataset$tempmaxLuglio[1],
                          DistanzaMare = dataset$DistanzaMare[1],
                          numCamping = dataset$numCamping[1]))

plot(fit_2)
plot(fit_2, which=2, col=c("red"))  # Q-Q Plot

# prediction of missing ValueSales
dataset$ValueSales[241:480] = 
  predict(fit_2, data.frame(tempmaxLuglio = dataset$tempmaxLuglio[241:480],
                            DistanzaMare = dataset$DistanzaMare[241:480],
                            numCamping = dataset$numCamping[241:480]))

# check the complete dataset
pander(introduce(dataset))
plot_missing(dataset)

# save
write.csv2(dataset, file = "dataset_completo.csv")

## CLASSIFICATION TREE
set.seed(123)
dataset$y <- ifelse(dataset$ValueSales > mean(dataset$ValueSales), "Yes", "No")
pander(round(prop.table(table(dataset$y))*100,2))
index <- createDataPartition(dataset$y,p=0.75,list=FALSE) 
train = dataset[index,]
test = dataset[-index,]
tree <- rpart(y ~ . -ValueSales, train)
rpart.plot(tree)
tree.predict <- predict(tree, test, type = "class")
confusionMatrix(tree.predict, as.factor(test$y), positive = "Yes")

# potential pdv --> DistanzaMare < 37
C1 <- filter(dataset, DistanzaMare < 37)
location_C1 <- st_as_sf(C1, coords = c("Longitude","Latitude"), crs = 4326)
plot_histogram(C1[,-c(1,8,9)], ncol = 6L, ggtheme = theme_bw())

# potential pdv --> DistanzaMare >= 37,
# Provincia != {Agrigento, Arezzo, Asti, Avellino, Bari, Belluno, Bergamo, Bologna,
# Campobasso, Como, Frosinone, L'Aquila, Matera, Novara, Pordenone, Bolzano,
# Torino, Valle d'Aosta}, numCamping >=7
p <- levels(dataset$Provincia)
C2 <- filter(dataset, DistanzaMare >= 37,
             Provincia != p[1], Provincia != p[4],
             Provincia != p[5], Provincia != p[6],
             Provincia != p[7], Provincia != p[8],
             Provincia != p[9], Provincia != p[10],
             Provincia != p[13], Provincia != p[17],
             Provincia != p[20], Provincia != p[23],
             Provincia != p[25], Provincia != p[27],
             Provincia != p[31], Provincia != p[32],
             Provincia != p[35], Provincia != p[36],
             numCamping >= 7)
location_C2 <- st_as_sf(C2, coords = c("Longitude","Latitude"), crs = 4326)

# potential pdv --> DistanzaMare >= 37,
# Provincia != {Agrigento, Arezzo, Asti, Avellino, Bari, Belluno, Bergamo, Bologna,
# Campobasso, Como, Frosinone, L'Aquila, Matera, Novara, Pordenone, Bolzano,
# Torino, Valle d'Aosta}, numCamping < 7, precipLuglio < 39
C3 <- filter(dataset, DistanzaMare >= 37,
             Provincia != p[1], Provincia != p[4],
             Provincia != p[5], Provincia != p[6],
             Provincia != p[7], Provincia != p[8],
             Provincia != p[9], Provincia != p[10],
             Provincia != p[13], Provincia != p[17],
             Provincia != p[20], Provincia != p[23],
             Provincia != p[25], Provincia != p[27],
             Provincia != p[31], Provincia != p[32],
             Provincia != p[35], Provincia != p[36],
             numCamping < 7, 
             precipLuglio < 39)
location_C3 <- st_as_sf(C3, coords = c("Longitude","Latitude"), crs = 4326)

mapview(location_C1, col.regions = "blue") + 
  mapview(location_C2, col.regions = "red") + 
  mapview(location_C3, col.regions = "green")