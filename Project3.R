library(tidyverse)
# url where the data is located
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"

# download the file
download.file(url, destfile = "lax_to_jfk.tar.gz")

# untar the file so we can get the csv only
# if you run this on your local machine, then can remove tar = "internal" 
untar("lax_to_jfk.tar.gz", tar = "internal")

# read_csv only 
sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv",
                        col_types = cols('DivDistance' = col_number(), 
                                         'DivArrDelay' = col_number()))

#************ Analysing Individual feature patterns using Visualization ********
# Boxplot
ggplot(data = sub_airline, mapping = aes(x = Reporting_Airline, y = ArrDelay)) +
  geom_boxplot(fill = "bisque",color = "black", alpha = 0.3) +
  geom_jitter(aes(color = 'blue'), alpha=0.2) +
  labs(x = "Airline") +
  ggtitle("Arrival Delays by Airline") +
  guides(color = FALSE) +
  theme_minimal() +
  coord_cartesian(ylim = quantile(sub_airline$ArrDelay, c(0, 0.99)))
# Load Alaska data, deleting rows that have missing departure delay or arrival delay data
alaska_flights <- sub_airline %>%
  filter(Reporting_Airline == "AS") %>%
  filter(!is.na(DepDelay) & !is.na(ArrDelay)) %>%
  filter(DepDelay < 40)

ggplot(data = alaska_flights, mapping = aes(x = DepDelay, y = ArrDelay)) +
  geom_point() +
  ggtitle("Alaska Flight Depature Delays vs Arrival Delays")
# list the data types for each column
str(sub_airline)
## **** Question 1) what is the data type of the column ArrDelayminutes ?
str(sub_airline$ArrDelayMinutes)
class(sub_airline$ArrDelayMinutes)
## ***** Question 2) Find the corelation between the following colums: DepDelayminutes and ArrDelayminutes
cor(sub_airline$DepDelayMinutes, sub_airline$ArrDelayMinutes)
# *********** Continous numercical variables ************
## Positive linear relationship 
# DepDelayMinutes as potential predictor variable of ArrDelayMinutes
ggplot(data = sub_airline, mapping = aes(x = DepDelayMinutes, y = ArrDelayMinutes)) +
  geom_point() + 
  geom_smooth(method = "lm", na.rm = TRUE)
cor(sub_airline$DepDelayMinutes, sub_airline$ArrDelayMinutes)
## Weak linear relationship
ggplot(data = sub_airline, mapping = aes(x = WeatherDelay, y = ArrDelayMinutes)) +
  geom_point() + 
  geom_smooth(method = "lm", na.rm = TRUE)
cor(sub_airline$WeatherDelay, sub_airline$ArrDelayMinutes, use = "complete.obs")
## Question 3a) Find the relation between x = 'CarrierDelay', y = 'ArrDelayMinutes'
cor(sub_airline$CarrierDelay,sub_airline$ArrDelayMinutes, use = "complete.obs")
# Quwstion 3b) Given the correlation, do you expect a linear relationship
ggplot(data = sub_airline, mapping = aes(x = CarrierDelay, y = ArrDelayMinutes)) +
  geom_point() + 
  geom_smooth
## ************** Descriptive Statistics ********************
summary_airline_delays <- sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(count = n(), 
            mean = mean(ArrDelayMinutes, na.rm = TRUE),
            std_dev = sd(ArrDelayMinutes, na.rm = TRUE), 
            min = min(ArrDelayMinutes, na.rm = TRUE), 
            median = median(ArrDelayMinutes, na.rm=TRUE),
            iqr = IQR(ArrDelayMinutes, na.rm = TRUE), 
            max = max(ArrDelayMinutes, na.rm = TRUE))

summary_airline_delays
# to identify the type of each column we apply sapply to eac column using typeof
sapply(sub_airline, typeof)
## Value counts, we can summarize categorical data using count function
sub_airline %>%
  count(Reporting_Airline)
## Basics of Grouping
avg_delays <- sub_airline %>%
  group_by(Reporting_Airline, DayOfWeek) %>%
  summarize(mean_delays = mean(ArrDelayMinutes), .groups = 'keep')

head(avg_delays)
## Arranging the values to decending order
# sort the dataframe in R using multiple variables with Dplyr
sorted <- avg_delays %>% 
  arrange(desc(mean_delays))

head(sorted)
# to make it easier to understand, we can transform thi table into a HeatMap
# The color is still hard to see and identify,  let's change the color
avg_delays %>% 
  ggplot(aes(x = Reporting_Airline, 
             y = DayOfWeek, 
             fill = mean_delays)) +
  # set the tile's borders to be white with size 0.2
  geom_tile(color = "white", size = 0.2) +
  # define gradient color scales
  scale_fill_gradient(low = "yellow", high = "red")
# sophisticated HeatMap
# This visualization will use lubridate package
library(lubridate)
# Let's take a simple average across Reporting_Airline and DayOfWeek
avg_delays <- sub_airline %>%
  group_by(Reporting_Airline, DayOfWeek) %>%
  summarize(mean_delays = mean(ArrDelayMinutes), .groups = 'keep') %>%
  # create a new variable "bins" from mean_delays
  # make the first range -0.1 to 0.1 to include zero values
  mutate(bins = cut(mean_delays,breaks = c(-0.1,0.1,10,20,30,50, max(mean_delays)),
                    labels = c("0","0-10","10-20","20-30","30-50",">50"))) %>%
  mutate(bins = factor(as.character(bins),levels = rev(levels(bins))))


ggplot(avg_delays, aes(x = Reporting_Airline, 
                       y = lubridate::wday(DayOfWeek, label = TRUE), 
                       fill = bins)) +
  geom_tile(colour = "white", size = 0.2) +
  geom_text(aes(label = round(mean_delays, 3))) +
  guides(fill = guide_legend(title = "Delays Time Scale"))+
  labs(x = "Reporting Airline",y = "Day of Week",title = "Average Arrival Delays")+
  # Define color palette for the scale
  scale_fill_manual(values = c("#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4"))
## Question 4) Use the groupby and summarize function to find the avg "arrdelaymin" of each flight in the reporting airline
# grouping results
sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(mean_delays = mean(ArrDelayMinutes))   
# ****************** Correlation and Causation ****************
sub_airline %>% 
  select(DepDelayMinutes, ArrDelayMinutes) %>% 
  cor(method = "pearson")
sub_airline %>% 
  cor.test(~DepDelayMinutes + ArrDelayMinutes, data = .)
# Corelations between multiple variables
correlation <- sub_airline %>%
  select(ArrDelayMinutes, DepDelayMinutes, 
         CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay) %>% 
  cor(use = "pairwise.complete.obs", method = "pearson")

correlation
# Downloading the corrplot library first.
install.packages("corrplot")
library(corrplot)

numerics_airline <- sub_airline %>%
  select(ArrDelayMinutes, DepDelayMinutes, CarrierDelay,
         WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay)

airlines_cor <- cor(numerics_airline, method = "pearson", use='pairwise.complete.obs')

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(airlines_cor, method = "color", col = col(200),  
         type = "upper", order = "hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 45, #Text label color and rotation
)
# ********* ANOVA (Analysis of Variance) *****************
summary_airline_delays <- sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(Average_Delays = mean(ArrDelayMinutes, na.rm = TRUE))

summary_airline_delays %>%  
  ggplot(aes(x = Reporting_Airline, y = Average_Delays)) + 
  geom_bar(stat = "identity") +
  ggtitle("Average Arrival Delays by Airline")
aa_as_subset <- sub_airline %>%
  select(ArrDelay, Reporting_Airline) %>%
  filter(Reporting_Airline == 'AA' | Reporting_Airline == 'AS')

ad_aov <- aov(ArrDelay ~ Reporting_Airline, data = aa_as_subset)
summary(ad_aov)
