#Assignment 2: Time series analysis

#Defining the general colors  
fill_color = '#111111'
decoration_color = '#cccccc'
main1_color = '#FE7C01'
main2_color = '#0183FE'
main3_color = '#e51acf'
main4_color = '#3cc344'
main5_color = '#fbf71d'

#Setting a dark theme option with black background
theme_set(dark_theme_gray()+ theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  plot.title = element_text(size = 14, hjust = 0, color = decoration_color),
  axis.ticks = element_blank(),
  axis.title = element_text(size = 10, hjust = 0.5, color = decoration_color),
  legend.title = element_blank(),
  panel.background =element_rect(fill = fill_color),
  strip.background =element_rect(fill = fill_color), 
  plot.background = element_rect(fill = fill_color),
  legend.background = element_rect(fill = fill_color)
))

#loading data set
#the data set contains hourly usage of two-stored house in 2016-2020 located in Houston, Texas.
#source: kaggle.com
Power_usage <- read.csv("https://github.com/alintu/Assignments/blob/master/Visual%20Analytics/data/power_usage_2016_to_2020.csv?raw=true", header=T, sep=',')

#Check the data 
names(Power_usage)
head(Power_usage, n=10)
str(Power_usage)
summary(Power_usage)

#convert date column to timestamp
Power_usage$StartDate <-as.POSIXct(Power_usage$StartDate, tz = "UTC", format="%Y-%m-%d %H:%M:%OS")

#display class of the variable
class(Power_usage$StartDate)

#Line chart for all categories over time
ggplot(Power_usage, aes(StartDate, Value..kWh.)) +
  geom_line(aes(colour=main1_color), show.legend = FALSE) +
  labs(title = "Residential power usage of two-stored house in 2016-2020")

#Line chart for all categories over time coloured by the group variable (notes)
ggplot(Power_usage, aes(StartDate, Value..kWh., colour = notes)) +
  geom_line(show.legend = TRUE) +
  geom_smooth(se=FALSE, colour= decoration_color) +
  labs(title = "Residential power usage of two-stored house in 2016-2020")

#small multiple chart by group variable 
ggplot(Power_usage, aes(StartDate, Value..kWh.)) +
  geom_line(colour=main3_color) + 
  facet_wrap(~notes, ncol=2,  strip.position = "bottom") +
  labs(title = "Residential power usage of two-stored house in 2016-2020")

#small multiple chart by group variable with trend line
ggplot(Power_usage, aes(StartDate, Value..kWh.)) +
  geom_line(colour=main2_color) + 
  facet_wrap(~notes, ncol=2) +
  geom_smooth(se=FALSE, colour= main5_color, size=0.5) +
  labs(title = "Residential power usage of two-stored house in 2016-2020")

#select only weekday data (one group)
weekday <- dplyr::filter(Power_usage, notes ==  "weekday")

#line chart for all categories (days)
ggplot(weekday, aes(StartDate, Value..kWh.))+
  geom_line(colour=main4_color, show.legend = FALSE) +
  geom_smooth(se=FALSE, colour= main5_color) +
  labs(title = "Residential power usage during weekdays in 2016-2020")

#small multiple chart by weekday
ggplot(weekday, aes(StartDate, Value..kWh.))+
  geom_line(colour=main4_color, show.legend = FALSE) + 
  facet_wrap(~day_of_week, ncol=2, strip.position = "bottom") +
  geom_smooth(se=FALSE, colour= main5_color) +
  labs(title = "Residential power usage during weekdays in 2016-2020")

#add all data in the background, highlighting particular weekday 
ggplot() +
  geom_line(data = transform(weekday, day_of_week = NULL), aes(StartDate, Value..kWh.), alpha = 0.5, lwd = 0.1, colour = "white") +
  geom_line(data = weekday, aes (StartDate, Value..kWh., group = day_of_week), lwd = 0.3, show.legend = FALSE, colour = main2_color) +
  geom_smooth(data=weekday, aes(StartDate, Value..kWh., group = 1), lwd = 1, method = 'loess', span = 0.1, se = TRUE, color = main5_color) + 
  facet_wrap(~ day_of_week, ncol=2, strip.position = "bottom") +
  theme(strip.background = element_blank(), strip.placement = "outside") +
  theme(axis.text.x = element_blank()) +
  labs(title = "Power usage by weekday including trendline, showing all data in the back") 

