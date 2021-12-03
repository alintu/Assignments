# Assignment 1

#install.packages
library(ggplot2)
library(ggridges)
library(ggdark)
library(ggExtra)
library(ggbeeswarm)

#Defining the general colors  
fill_color = '#111111'
decoration_color = '#cccccc'
main1_color = '#FE7C01'
main2_color = '#0183FE'
main3_color = '#e51acf'
main4_color = '#3cc344'
main5_color = '#fbf71d'
myColors <- c("#FE7C01", "#0183FE", "#F8FF00", "#8EFD02", "#EE00FF", "#EC1332", "#03FC96",  "#FFFFFF", "#800D9E", "#1D46E2")

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

#Data loading
Happy_world <- read.csv("https://gist.githubusercontent.com/sandravizz/8b7cf476e4d07331eee00f1fa0249e12/raw/65395ac49ea7154a43f47f455ec26e270c0fb4a3/World%2520Happiness%2520Report")

#Data checking
names(Happy_world)
head(Happy_world, n=10)
str(Happy_world)
summary(Happy_world)
view(Happy_world)

#DISTRIBUTION

#Simple Healt/Life expectancy distribution
ggplot(Happy_world, aes(Health..Life.Expectancy.)) + 
  geom_freqpoly(colour = main2_color, binwidth = 0.1) 
 
#Simple Freedom distribution
ggplot(Happy_world, aes(Freedom)) + 
  geom_freqpoly(colour = main1_color, binwidth = 0.1) 

#Histograms

#Freedom histogram for different Regions 
ggplot(Happy_world, aes(Freedom, fill = Region)) +
  geom_histogram(position = "dodge", binwidth = 0.3) + 
  scale_fill_manual(values=myColors)

#Histogram and frequency polygon for Health/Life expectancy 
ggplot(Happy_world, aes(Health..Life.Expectancy., colour = Region)) + 
  geom_histogram(colour=decoration_color, fill = decoration_color, alpha = 0.2, size =0, binwidth = 0.1) +
  geom_freqpoly(binwidth = 0.1)+ 
  scale_colour_manual(values=myColors)

 #Multiple: Health..Life.Expectancy. histogram for different Regions
ggplot(Happy_world, aes(Health..Life.Expectancy.)) + 
  geom_histogram(binwidth = 0.02, fill = main2_color) + 
  facet_wrap(. ~ Region)

#Small multiple histogram for Freedom by Regions
ggplot(Happy_world, aes(Freedom, fill = Region)) + 
  geom_histogram(binwidth = 0.1) + 
  scale_fill_manual(values=myColors) +
  facet_wrap(. ~ Region)

#Boxplots

# Boxplot for Freedom by Region  
ggplot(Happy_world, aes(Region, Freedom)) +
  geom_boxplot(colour=main1_color,
               outlier.color=decoration_color)

# Boxplot for Health..Life.Expectancy by Region  
ggplot(Happy_world, aes(Region, Health..Life.Expectancy.)) +
  geom_boxplot(colour=main5_color,
               outlier.color=decoration_color)

#Density charts

#Multiple density charts by Regions for Health/Life Expectancy
ggplot(Happy_world, aes(Health..Life.Expectancy., stat(density), fill = Region)) +
  geom_density(color= NA) +
  scale_fill_manual(values=myColors) +
  facet_wrap(. ~ Region)

#for Freedom
ggplot(Happy_world, aes(Freedom, stat(density), fill = Region)) +
  geom_density(color= NA) +
  scale_fill_manual(values=myColors) +
  facet_wrap(. ~ Region)

#Multiple density charts
#Health/Life expectancy
ggplot(Happy_world, aes(Health..Life.Expectancy., group = Region, fill = Region)) +
  geom_density(adjust=1.5 , color= NA, fill=main2_color, alpha =0.1) 

#Freedom
ggplot(Happy_world, aes(Freedom, group = Region, fill = Region)) +
  geom_density(adjust=1.5 , color= NA, fill=main1_color, alpha =0.1) 

#
#RELATIONSHIP ANALYSIS

?stat_smooth

#Scatter plots with visual encoding
ggplot(Happy_world, aes(Health..Life.Expectancy., Freedom, colour=Freedom)) +
  geom_point(size=1)+
  scale_colour_gradient(low = main2_color, high = main1_color)+
  facet_wrap( ~ Region, ncol=3)

#Scatter plots with trend lines 
ggplot(Happy_world, aes(Health..Life.Expectancy., Freedom)) +
  geom_point(color=main3_color, size=0.8, alpha=0.8)+
  facet_wrap( ~ Region, ncol=3, scales = "free") +
  stat_smooth(color=decoration_color, size=0.5)

#Dot-dash scatter plots 
ggplot(Happy_world, aes(Health..Life.Expectancy., Freedom)) + 
  geom_point(size=0.9, alpha=0.7, color=main5_color) + 
  geom_rug(size=0.4, alpha=0.9, color=main5_color)+
  facet_wrap( ~ Region, ncol=3) + 
  xlab("") + 
  ylab("")

#Beeswarm 
#jitter plot
ggplot(Happy_world, aes(Health..Life.Expectancy., Freedom)) + 
  geom_jitter(size=0.5, color=main3_color)+
  facet_wrap( ~ Region, ncol=3)

#Beewswarm
ggplot(Happy_world, aes(Health..Life.Expectancy., Freedom, colour=Region)) + 
  geom_quasirandom(groupOnX=TRUE) +
  scale_colour_manual(values=myColors)

#Aggregation through hexagonal binning 
ggplot(Happy_world, aes(Health..Life.Expectancy., Freedom))+ 
  geom_hex(bins=15, alpha = 0.6)+
  xlim(0, 1)+
  scale_fill_gradient(low=main2_color, high=main5_color)+
  facet_wrap( ~ Region, ncol=3) 

#Aggregation through hexagonal binning - log. scaling 
ggplot(Happy_world, aes(Health..Life.Expectancy., Freedom))  + 
  geom_hex(bins=15, alpha = 0.6) +
  scale_x_log10(breaks = round(as.vector(quantile(diamonds$carat)), digits = 1))+
  scale_y_log10(breaks = round(as.vector(quantile(diamonds$price)), digits = 1))+
  scale_fill_gradient(low=main2_color, high=main5_color)+
  facet_wrap( ~ Region, ncol=3)

#Heatmaps
ggplot(Happy_world, aes(Health..Life.Expectancy., Freedom)) +
  geom_bin2d(bins = 20) +
  scale_fill_gradient(low=main4_color, high=main1_color)+
  facet_wrap( ~ Region, ncol=3)
