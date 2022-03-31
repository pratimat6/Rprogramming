#Pratima Tiwari
#NP000411


#Report from .csv file:
report<-read.csv("hourly_weather.csv")
print(report) 

#Display data contained in .csv file
summary(report)

#Import_Library:
library(dplyr)
library(ggplot2)
library(ggpubr)

#Analysis Example_1 : -
##In this example,Month vs Temperature at the origin is analysed between x and y.
#Visualization and Exploration


display1<- ggplot(report, mapping = aes(x =month, y = temp,
          color = origin )) +
        theme(panel.background = element_rect(fill = "cornsilk",
        colour = "red", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
        colour = "brown"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
        colour = "brown")
        )+ geom_point(alpha = 0.15)+
        scale_x_continuous(limits = c(1,12), breaks = seq(1,12,1))+
        stat_smooth(method = "loess",formula = y~x) +
        labs(title = 'Plot month Vs Temperature ',
             x = 'month ', y = 'Temperature ( ÂºF )')

print(display1)


#Analysis Example_2 : -
##In this example, an analysis is done between x and y  for Temperature and its frequency.
#Visualization

display2<-hist(report$temp,
     main = "Frequency of Temperature Histogram",
     xlab = "Temperature ( ºF )",ylab = "Frequency ( No.)",
     las = 1,
     col = c("red", "black"))
print(display2)


#Analysis Example_3 : -
##In this example, an analysis is done between x and y  for Temperature vs Dew point.
#Visualization, Exploration and Manipulation

display3 <- ggplot(report %>%
                     filter(report$temp>35), mapping = aes(x = factor(temp),
          y =dewp )) + geom_boxplot(col="black",fill="white")  +
  labs(title = ' Temperature VS Dew point Boxplot',
       x = 'Temperature ( ºF )', y = 'Dewpoint ( ºF )')
print(display3)


#Analysis Example_4 : -
##In this example, an analysis is done between x and y  for Month vs Wind Speed at the origin.
#Visualization, Exploration and Manipulation

display4<- (
  report %>%
    filter(report$wind_speed<30) %>% # To filter the data
    ggplot(mapping = aes(x = factor(month), y = wind_speed)) + 
    geom_boxplot(col="blue",fill="white") + 
    labs(title = ' Box Plot Month Vs Wind Speed',x = 'Month', y = 'Wind Speed ( mph )'))
print(display4)


#Analysis Example_5 : -
##In this example, an analysis is done between x and y  for Wind Speed vs Wind Guest at the origin.
#Visualization and Exploration

line<-stat_smooth(method = "loess",formula = y~x)

##visualization 
display5<- ggplot(report, mapping = aes(x = wind_speed, 
            y = wind_gust, color = origin)) +
            theme(panel.background = element_rect(fill = "lightpink",
            colour = "blueviolet", size = 0.5, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
            colour = "lightblue"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
            colour = "lightblue")
            )+ xlim(10,35) + ylim(10,45)+
            geom_point(alpha = 0.15)+ line + 
            labs(title = 'Plot Wind Speed Vs Wind Gust ',
              x = 'Wind Speed (mph) ', y = 'Wind Gust (mph)')

print(display5)


#Analysis Example_6 : -
##In this example, an analysis is done between x and y  for Hour vs Wind Direction at the origin.
#Visualization and Exploration

display6<- ggplot(report, mapping = aes(x = hour, y = wind_dir,
                  color = origin)) + geom_boxplot() +
            labs(title = 'Wind Direction Change in Each Hour',
                 x = 'Hour', y = 'Wind Direction ( degree )')
print(display6)



#Analysis Example_7 : -
##In this example,Visibility and its frequency at the different origin is analyzed between x and y.
#visualization and exploration

air_visib<- facet_wrap(~origin)

display7<-ggplot(data = report, mapping = aes(x = visib, na.rm = TRUE)) + stat_bin(bins = 40)+ 
  geom_histogram() + labs(title = 'Histogram of Visibility at Different Origin',
                          x = 'Visibility') + air_visib
print(display7)




#Analysis Example_8 : -
##In this example,Month and its Pressure is analysed between x and y.
#visualization and Exploration

display8<-plot(report$month,report$pressure, col = "pink", pch = 2, 
               main = "  Plot Month Vs Pressure", xlab = "Month", 
               ylab = "Pressure ( millibars )", las = 0)
print(display8)



#Analysis Example_9 : -
##In this example,humidity and its frequency with respect to month is examined between x and y.
#visualization and Exploration

day_plot<-facet_wrap(~day)


display9<-ggplot(report, mapping = aes(x = humid)) + 
  geom_histogram(col="green",fill="burlywood1") + labs(title = 'Histogram of Humidity',
                                                      x = 'Humidity')
print(display9+day_plot)




#Analysis Example_10 : -
##In this example, At the origin, an analysis is performed between x and y for month vs humidity at origin.
#visualization and Exploration

display10 <- ggplot(report) +
  aes(x = month, y = humid, colour = origin) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(origin))
print(display10) 




#Analysis Example_11 : -
##In this example, an analysis is carried out between Wind Direction and its frequency between x and y.
#visualization and Exploration

display11<- ggplot(report, mapping = aes(x =wind_dir)) + 
  geom_histogram(col="purple",fill="chartreuse")+
  theme(panel.background = element_rect(fill = "azure2",
        colour = "red", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
        colour = "blue"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
        colour = "blue")
        )+ labs(title = 'Histogram of wind direction', 
         x = 'Wind Direction ( degrees )',y = 'Frequency ( Nos.)')
print(display11)




#Analysis Example_12 : -
##In this example, an analysis is done between x and y  for Hour vs Temperature.
#visualization and Exploration

display12<-ggplot(report,aes(x = hour,y = temp)) + 
            geom_point(aes(colour = temp)) +
            scale_colour_gradient2(low = "red", mid = "black" , high = "purple",
            midpoint = 62) + geom_smooth(color = "red",size = 1) +
            scale_y_continuous(limits = c(10,100), breaks = seq(10,100,10)) +
            ggtitle ("Daily average temperature") +
            xlab("Hour") +  ylab ("Temperature ( TºF )")
print(display12)





#Analysis Example_13 : -
##In this example,Wind Direction and its Wind Speed at the origin is analyzed between x and y.
#visualization and Exploration

display13<- ggplot(report, mapping = aes(x = wind_dir, y = wind_speed,
            color = origin )) +
            theme(panel.background = element_rect(fill = "aliceblue",
            colour = "yellow", size = 0.5, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
            colour = "red"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
            colour = "red")
            )+ geom_point(alpha = 0.15)+
            stat_smooth(method = "loess",formula = y~x) + 
            scale_y_continuous(limits = c(5,35), breaks = seq(5,35,5))+ 
            scale_x_continuous(limits = c(0,359), breaks = seq(0,359,10))+
            labs(title = 'Plot Wind Diection VS Wind Speed ',
            x = 'Wind Direction ( degree ) ', y = 'Wind Speed ( mph )')

print(display13)




#Analysis Example_14 : -
##In this example,Humidity and its Precipitation is analysed between x and y. 
#visualization and Exploration

display14<-plot(report$humid,report$precip, col = "blue", pch = 8, 
              main = " Plot Against Humidity and Precipitation", xlab = "Humidity", 
              ylab = "Precipitation ( inches )", las = 1)
print(display14)




#Extra Features: 


#Analysis Example_15 : -
##In this example, an analysis is done between x and y  for Humidity vs Temperature.
#visualization and Exploration

library(ggpubr)

# Scatter plot colored by groups ("Species")
display15 <- ggscatter(report, x = "humid", y = "temp",
                color = "origin", palette = "jco",
                size = 3, alpha = 0.6)+
              labs(title = 'Temperature Vs Humidity ',
              x = 'Humidity ', y = 'Temperature ( TºF )')+border()                                         
# Marginal density plot of x (top panel) and y (right panel)
xplot <- ggdensity(report, "humid", fill = "origin",
                   palette = "jco")
yplot <- ggdensity(report, "temp", fill = "origin", 
                   palette = "jco")+
  rotate()
# Cleaning the plots
yplot <- yplot + clean_theme() 
xplot <- xplot + clean_theme()
# Arranging the plot
ggarrange(xplot, NULL, display15, yplot, 
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(1, 2),
          common.legend = TRUE)
print(display15)

#Analysis Example_16 : -
##In this example, an analysis is done between x and y  for Hour vs Pressure.
#visualization and Exploration

display16<-ggplot(data = report, mapping = aes(x = factor(hour),
                                 y = humid, na.rm = TRUE)) + geom_violin() + labs(title="Hourly Change in Humidity",
                                  x="Hour", y="Pressure ( millibars )")

print(display16)


