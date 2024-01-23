library(readxl)

X490_Richness <- read_excel("input/490_Richness.xlsx")

View(X490_Richness)

#set up our data frame(not necessary)
Richness <- X490_Richness$`Pooled Richness`

Park<- X490_Richness$`Park`

Plot <- X490_Richness$`Plot Type`

data <- data.frame(Plot, Richness, Park)

data

#Reorder groups
data$Park <- factor(data$Park, levels = c("VER", "SDM", "SEN", "SES", "DMG", "CLF"))
data$Plot <- factor(data$Plot, levels = c("Floral", "Shrubby", "Microforest"))

#this model includes both 
modl <- aov(Richness ~ Plot + Park, data = data)

summary(modl)

modl
# We have our p-value for richness

#Shapiro-Wilk normality test
shapiro.test(modl$residuals)

#Levene's test of homoscedacity
library(car)

leveneTest(Richness ~ Plot, data=data)
leveneTest(Richness ~ Park, data=data)

#Outliers
boxplot(Richness ~ Plot, data = data)
boxplot(Richness ~ Park, data = data)

# Now on to activity

activity <- read_excel("input/490_Activity.xlsx")
activity

#Reorganize our data into easier names to follow
Park <- activity$Park

Park

Plot <- activity$`Plot Type`

Plot

Flight <- activity$Flight

Flight

Xse <- activity$`Plot Use`

Xse

#Now to create a data frame
bhav <- data.frame(Park, Plot, Flight, Xse)
bhav

#Reorder groups
bhav$Park <- factor(bhav$Park, levels = c("VER", "SDM", "SEN", "SES", "DMG", "CLF"))

bhav$Plot <- factor(bhav$Plot, levels = c("Floral", "Shrubby", "Microforest"))

#Testing the activity of flight within parks and plots
wings <- aov(Flight ~ Plot + Park, data = bhav)

summary(wings)

#If residuals needed run this line
wings

#Shapiro-Wilk normality test
shapiro.test(wings$residuals)

#Levene's test of homoscedacity
library(car)

leveneTest(Flight ~ Plot, data=data)
leveneTest(Flight ~ Park, data=data)

#Outliers
boxplot(Flight ~ Plot, data = data)
boxplot(Flight ~ Park, data = data)

#Testing the activity of plot usage within the parks and plots
Plant <- aov(Xse ~ Plot + Park, data= bhav)

summary(Plant)

#If residuals needed run this line
Plant

#Shapiro-Wilk normality test
shapiro.test(Plant$residuals)
#FailedTest

#Kruskal-wallis test since shapiro failed
kruskal.test(Xse ~ Plot, data = data)
kruskal.test(Xse ~ Park, data = data)


#Levene's test of homoscedacity
library(car)

leveneTest(Xse ~ Plot, data=data)
leveneTest(Xse ~ Park, data=data)

#Outliers
boxplot(Xse ~ Plot, data = data)
boxplot(Xse ~ Park, data = data)


#Boxplots on stuff

# Plot vs Richness
richxplot <- 
  boxplot (Richness~Plot,
           data = data,
           main = "",
           xlab = "Design",
           ylab = "Richness",
           col =  "lightblue1",
           border = "cadetblue4") +
  stripchart(Richness~Plot,
             vertical = TRUE,
             data = bhav,
             method = "jitter",
             add = TRUE,
             pch = 20,
             col = "grey6")
richxplot

# Park vs Richness
richxpark <- 
  boxplot (Richness~Park,
           data = data,
           main = "",
           xlab = "Park",
           ylab = "Richness",
           col =  "lightblue1",
           border = "cadetblue4") +
  stripchart(Richness~Park,
             vertical = TRUE,
             data = bhav,
             method = "jitter",
             add = TRUE,
             pch = 20,
             col = "grey6")
richxpark

#Plot vs Flight
flightxplot <- 
         boxplot (Flight~Plot,
         data = bhav,
         main = "",
         xlab = "Design",
         ylab = "Instances of Flight",
         col =  "lightblue1",
         border = "cadetblue4") +
  stripchart(Flight~Plot,
             vertical = TRUE,
             data = bhav,
             method = "jitter",
             add = TRUE,
             pch = 20,
             col = "grey6")
flightxplot

#ParkvsFlight
flightxpark <- 
  boxplot (Flight~Park,
           data = bhav,
           main = "",
           xlab = "Park",
           ylab = "Instances of Flight",
           col =  "lightblue1",
           border = "cadetblue4") +
  stripchart(Flight~Park,
             vertical = TRUE,
             data = bhav,
             method = "jitter",
             add = TRUE,
             pch = 20,
             col = "grey6")
flightxplot

#Use = Xse
#ParkvsUse
parkxusage <- 
  boxplot (Xse~Park,
           data = bhav,
           main = "",
           xlab = "Park",
           ylab = "Instances of Plot Use",
           col =  "lightblue1",
           border = "cadetblue4") +
  stripchart(Xse~Park,
             vertical = TRUE,
             data = bhav,
             method = "jitter",
             add = TRUE,
             pch = 20,
             col = "grey6")
parkxusage

#PlotvsXse
plotxusage <- 
  boxplot (Xse~Plot,
           data = bhav,
           main = "",
           xlab = "Design",
           ylab = "Instances of Plot Use",
           col =  "lightblue1",
           border = "cadetblue4") +
  stripchart(Xse~Plot,
             vertical = TRUE,
             data = bhav,
             method = "jitter",
             add = TRUE,
             pch = 20,
             col = "grey6")
plotxusage

