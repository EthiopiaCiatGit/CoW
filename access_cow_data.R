#CoW data Explanatory data analysis

#install & load packages
remotes::install_github("iqss/dataverse-client-r")
packages_required <- c("dataverse", "dplyr", "stringr", "ggplot2", "ggpubr", "tibble",
                       "reshape", "sp", "terra", "readxl")
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))

#Read the data from dataverse
data <- 
  get_dataframe_by_name(
  filename   = "Fertilizer data.tab",
  dataset    = "doi:10.7910/DVN/M8FCSL",
  server     = "dataverse.harvard.edu",
  original   = TRUE,
   .f         = readxl::read_xlsx) %>% as.data.frame()

#view the structure of data
colnames(data)
str(data)
summary(data)

# Let us create treatment and location identifiers, Location is going to serve 
# as trial ID in this case
data <- data[order(data$n, data$p, data$k), ]
data$treatment <- paste(data$n, data$p, data$k, sep="_")
train_data$location <- as.factor(paste(data$x, data$y, sep="_"))

#Check the spatial coordinates and how much the two datasets overlap
plot(data$x, data$y)
data <- droplevels(data[data$x > 10, ])
data$location <- as.factor(paste(data$x, data$y, sep="_")) 
plot(train_data$x, train_data$y)

length(unique(data$location))
length(unique(train_data$location))
length(unique(train_data[train_data$location %in% data$location,]))

#Understand data structure, treatment, year, location, .
data_locYear <- unique(data[, c("year", "location")])
data_locYear <- data_locYear[order(data_locYear$year),]
hist(data_locYear$year)
table(data_locYear$year)
locyears <- as.data.frame(table(data_locYear$year))
locyears <- locyears[order(locyears$Freq), ]

#treatments, replicates, locations and trials
trtLoc <- as.data.frame(table(data$treatment, data$location))
names(trtLoc) <- c("treatment", "location", "Freq")
trtLoc <- trtLoc[trtLoc$Freq > 0, ]
head(trtLoc, 25)
tl <- data[data$location == "36.98_10.66", ]
table(tl$treatment)

ggplot(tl, aes(treatment, grain_yield))+
  geom_point()+ 
  theme_bw()

tl1 <- data[data$location == "36.4_7.3", ]
ggplot(tl1, aes(treatment, grain_yield, col=factor(year)))+
  geom_point()+
  facet_wrap(~year)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust = 1), legend.position = 'none')

table(tl1$treatment, tl1$year)

#How many treatments are tested across different years?
data_TY <- unique(data[, c("year","treatment")])
treatCount <- as.data.frame(table(data_TY$treatment))
quantile(treatCount$Freq, probs = seq(0, 1, 0.1))

length(treatCount$Var1)
length(treatCount[treatCount$Freq == 1, "Var1"])
length(treatCount[treatCount$Freq >= 3, "Var1"])

Treat_3Years <- unique(data[data$treatment %in% treatCount[treatCount$Freq >= 3, "Var1"],c("location", "treatment", "year") ])
treatlocyear <- as.data.frame(table(Treat_3Years$treatment,Treat_3Years$location))
treatlocyear <- treatlocyear[treatlocyear$Freq>1, ]
unique(treatlocyear[treatlocyear$Freq > 1, ]$Var1)

#Trials with only one yield data point at a location.
oneTrt <- as.data.frame(table(data$location))
oneValue <- oneTrt[oneTrt$Freq == 1, ] 

#trials have only one data point.
#Among these 14, a treatment with no other match in the data and appearing in only one location with 1 rep should be dropped ??
  
  oneValue <- merge(unique(data[,c("location", "treatment", "year")]),oneValue, by.x="location", by.y="Var1")
oneValue <- merge(unique(data[,c("location", "treatment", "grain_yield")]),oneValue, by=c("location", "treatment"))
dd1 <- data[data$treatment == oneValue$treatment[1] & data$year == oneValue$year[1], ]
dd2 <- data[data$treatment == oneValue$treatment[2] & data$year == oneValue$year[2], ]
dd3 <- data[data$treatment == oneValue$treatment[3] & data$year == oneValue$year[3], ]
dd4 <- data[data$treatment == oneValue$treatment[4] & data$year == oneValue$year[4], ]
dd5 <- data[data$treatment == oneValue$treatment[5] & data$year == oneValue$year[5], ]
dd6 <- data[data$treatment == oneValue$treatment[6] & data$year == oneValue$year[6], ]
dd7 <- data[data$treatment == oneValue$treatment[7] & data$year == oneValue$year[7], ]
dd8 <- data[data$treatment == oneValue$treatment[8] & data$year == oneValue$year[8], ]
dd9 <- data[data$treatment == oneValue$treatment[9] & data$year == oneValue$year[9], ]
dd10 <- data[data$treatment == oneValue$treatment[10] & data$year == oneValue$year[10], ]
dd11 <- data[data$treatment == oneValue$treatment[11] & data$year == oneValue$year[11], ]
dd12 <- data[data$treatment == oneValue$treatment[12] & data$year == oneValue$year[12], ]
dd13 <- data[data$treatment == oneValue$treatment[13] & data$year == oneValue$year[13], ]
dd14 <- data[data$treatment == oneValue$treatment[14] & data$year == oneValue$year[14], ]


dd1$index <- ifelse(dd1$location %in% oneValue$location, "1", "0")
dd2$index <- ifelse(dd2$location %in% oneValue$location, "1", "0")
dd3$index <- ifelse(dd3$location %in% oneValue$location, "1", "0")
dd4$index <- ifelse(dd4$location %in% oneValue$location, "1", "0")
dd5$index <- ifelse(dd5$location %in% oneValue$location, "1", "0")
dd6$index <- ifelse(dd6$location %in% oneValue$location, "1", "0")
dd7$index <- ifelse(dd7$location %in% oneValue$location, "1", "0")
dd8$index <- ifelse(dd8$location %in% oneValue$location, "1", "0")
dd9$index <- ifelse(dd9$location %in% oneValue$location, "1", "0")
dd10$index <- ifelse(dd10$location %in% oneValue$location, "1", "0")
dd11$index <- ifelse(dd11$location %in% oneValue$location, "1", "0")
dd12$index <- ifelse(dd12$location %in% oneValue$location, "1", "0")
dd13$index <- ifelse(dd13$location %in% oneValue$location, "1", "0")
dd14$index <- ifelse(dd14$location %in% oneValue$location, "1", "0")
suppressWarnings(ggplot(dd1, aes(location, grain_yield, col=index, size=index)) +
                   geom_point()+
                   theme_bw()+
                   theme(axis.text.x = element_text(angle=45,hjust = 1)))

suppressWarnings(ggplot(dd3, aes(location, grain_yield, col=index, size=index)) +
                   geom_point()+
                   theme_bw()+
                   theme(axis.text.x = element_text(angle=45,hjust = 1)))
dd3B <- data[data$location %in% dd3$location & data$year %in% dd3$year, ]
dd3B$index <- ifelse(dd3B$location %in% oneValue$location, "1", "0")
suppressWarnings(ggplot(dd3B, aes(location, grain_yield, col=treatment, size=index)) +
                   geom_point()+
                   theme_bw()+
                   theme(axis.text.x = element_text(angle=45,hjust = 1)))

suppressWarnings(ggplot(dd4, aes(location, grain_yield, col=index, size=index)) +
                   geom_point()+
                   theme_bw()+
                   theme(axis.text.x = element_text(angle=45,hjust = 1)))

dd4B <- data[data$location %in% dd4$location & data$year %in% dd4$year & data$x > 38, ]
dd4B$index <- ifelse(dd4B$location %in% oneValue$location, "1", "0")
suppressWarnings(ggplot(dd4B, aes(location, grain_yield, col=treatment, size=index)) +
                   geom_point()+
                   theme_bw()+
                   theme(axis.text.x = element_text(angle=45,hjust = 1)))
suppressWarnings(ggplot(dd8, aes(location, grain_yield, col=index, size=index)) +
                   geom_point()+
                   theme_bw()+
                   theme(axis.text.x = element_text(angle=45,hjust = 1)))

dd8B <- data[data$location %in% dd8$location & data$year %in% dd8$year, ]
dd8B$index <- ifelse(dd8B$location %in% oneValue$location, "1", "0")
suppressWarnings(ggplot(dd8B, aes(location, grain_yield, col=treatment, size=index)) +
                   geom_point()+
                   theme_bw()+
                   theme(axis.text.x = element_text(angle=45,hjust = 1)))

suppressWarnings(ggplot(dd9, aes(location, grain_yield, col=index, size=index)) +
                   geom_point()+
                   theme_bw()+
                   theme(axis.text.x = element_text(angle=45,hjust = 1)))

dd9B <- data[data$location %in% dd9$location & data$year %in% dd9$year, ]
dd9B$index <- ifelse(dd9B$location %in% oneValue$location, "1", "0")
suppressWarnings(ggplot(dd9B, aes(location, grain_yield, col=treatment, size=index)) +
                   geom_point()+
                   theme_bw()+
                   theme(axis.text.x = element_text(angle=45,hjust = 1)))

data[data$location== "39.275639_7.338042",]
dd9$trial <- "dd9"
dd10$trial <- "dd10"
dd12$trial <- "dd12"
dd13$trial <- "dd13"
dd14$trial <- "dd14"

dd9_14 <- rbind(dd9, dd10, dd12, dd13, dd14)
suppressWarnings(ggplot(dd9_14, aes(location, grain_yield, col=treatment , size=index)) +
                   geom_point()+
                   facet_wrap(~trial)+
                   theme_bw()+
                   theme(axis.text.x = element_text(angle=90,hjust = 1)))
suppressWarnings(ggplot(dd9_14, aes(location, grain_yield, col=treatment , size=index)) +
                   geom_point()+
                   theme_bw()+
                   theme(axis.text.x = element_text(angle=90,hjust = 1)))
suppressWarnings(ggplot(dd11, aes(location, grain_yield, col=index, size=index)) +
                   geom_point()+
                   theme_bw()+
                   theme(axis.text.x = element_text(angle=45,hjust = 1)))

dd11B <- data[data$location %in% dd11$location & data$year %in% dd11$year, ]
dd11B$index <- ifelse(dd11B$location %in% oneValue$location, "1", "0")
suppressWarnings(ggplot(dd11B, aes(location, grain_yield, col=treatment, size=index)) +
                   geom_point()+
                   theme_bw()+
                   theme(axis.text.x = element_text(angle=45,hjust = 1)))

tyl <- data[, c("treatment", "year", "location")]
tyl$ty <- paste(tyl$year, tyl$location, sep="_")

tylist <- c()
for(ty in unique(tyl$ty)){
  tyld <- tyl[tyl$ty == ty, ]
  if(nrow(tyld) == length(unique(tyld$treatment))){
    tylist <- c(tylist, as.character(unique(tyld$location)))
  }
}
length(tylist)

ggplot(data, aes(treatment, grain_yield))+
  geom_point()+
  theme_bw()

data$colsize <- factor(ifelse(data$grain_yield > 7000, "red", "green"))
ggplot(data, aes(x =x, y=y, col=colsize, size=colsize))+
  geom_point()
yieldAbove7000 <- droplevels(data[data$grain_yield > 7000, ])
quantile(yieldAbove7000$n, probs =seq(0,1,0.1))
quantile(data$n, probs =seq(0,1,0.1))
quantile(yieldAbove7000$p, probs =seq(0,1,0.1))
quantile(data$p, probs =seq(0,1,0.1))
quantile(yieldAbove7000$k, probs =seq(0,1,0.1))
quantile(data$k, probs =seq(0,1,0.1))

#We just need to check systematically for trials like the following
tl2<- data[data$location == "37.021061_10.533216", ]
ggplot(tl2, aes(treatment, grain_yield))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust = 1))