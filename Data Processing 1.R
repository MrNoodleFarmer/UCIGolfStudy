library(usmap)
library(ggplot2)
library(shiny)
library(rmarkdown)
library(knitr)


rsconnect::setAccountInfo(name='yuanxicheng',
                          token='FF88EF16267341188B6038D65C624FA0',
                          secret='LPRZ68Nm0+ZK2by8l7vJxx2gx/lwjotNBxpOsYEs')
library(rsconnect)
rsconnect::deployApp("C:/Users/roy19/OneDrive/Documents/Golf_Research")


boys <- read.csv("Golf_Research/JGSBoys.csv", head= T)
girls <- read.csv("Golf_Research/JGSGirls.csv", head=T)
State_Pop <- read.csv("Golf_Research/State_Population.csv", head = T)
State_Income <- read.csv("Golf_Research/State_Med_Income.csv", head = T)
State_Courses <- read.csv("Golf_Research/State_Courses.csv", head = T)
State_Weather <- read.csv("Golf_Research/State_Weather.csv", head = T)

#Testing usmap and ggplot2 libraries 
  #plots map with no data
#plot_usmap(regions = "states") + 
 # labs(title = "U.S. States",
  #     subtitle = "This is a blank map of the United States.")+ 
  #theme(panel.background=element_blank())

#plot_usmap(data = countypov, values = "pct_pov_2014", include = c("CT", "ME", "MA", "NH", "VT"), color = "blue") + 
 # scale_fill_continuous(low = "white", high = "blue", name = "Poverty Percentage Estimates", label = scales::comma) + 
  #labs(title = "New England Region", subtitle = "Poverty Percentage Estimates for New England Counties in 2014") +
  #theme(legend.position = "right")

#dim(countypov)
#summary(countypov)
#names(countypov)
#head(countypov)

#state.data=data.frame(table(boys$St.Prov))
#names(state.data)=c("state", "Freq")
#plot_usmap(data = state.data, values = "Freq")

#plot_usmap(data = statepop, values = "pop_2015", color = "red") + 
#  scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
#  theme(legend.position = "right")


#state.pop = statepop
#state.pop




#Problems to ponder: 
#How to convert country, hometown, and state info into coordinates that can be marked on the map (hometown, State/Province, Country)
#Did I download everything correctly? How do we display number of people that are ___ within a certain area? For example, maybe a 
#color graph where different colored points display different grad. years, or an overall color for a state with the largest amount of players graduating in a specific year?
#Do we make multiple maps? Can we make a function where user can switch between maps?


#create variable to run through list and assign numbers to a certain state
 #maybe like a for loop through the entire df to see things like how many ppl are in each state

#Processing data for JGS boys
unique(boys$Ctry)

boysUS <- boys[(boys$Ctry=="US"),]
boysUS$St.Prov <- toupper(boysUS$St.Prov)
boysUS <- boysUS[!((boysUS$St.Prov=="--"|boysUS$St.Prov=="BC"|boysUS$St.Prov=="AB"|boysUS$St.Prov=="GU"|boysUS$St.Prov=="DC"|boysUS$St.Prov=="PR")),]
boysUS <- subset(boysUS, select = -c(1,2,7,9,10,11,12,13))
rownames(boysUS) <- 1:nrow(boysUS)

#boysUS
#unique(boysUS$Ctry)
#unique(boysUS$St.Prov)
#nrow(boysUS)
#nrow(boys)
#head(boysUS,10)

#Processing JGS Girls
girlsUS <- girls[(girls$Ctry=="US"),]
girlsUS$St.Prov <- toupper(girlsUS$St.Prov)
girlsUS <- girlsUS[!((girlsUS$St.Prov=="--"|girlsUS$St.Prov=="BC"|girlsUS$St.Prov=="AB"|girlsUS$St.Prov=="GU"|girlsUS$St.Prov=="DC"|girlsUS$St.Prov=="PR")),]
girlsUS <- subset(girlsUS, select = -c(1,2,7,9,10,11,12,13))
rownames(girlsUS) <- 1:nrow(girlsUS)

#head(girlsUS, 10)
#unique(girlsUS$Ctry)
#unique(girlsUS$St.Prov)
#nrow(girlsUS)
#nrow(girls)



#Visualizing Processed Data

#boys US map
boys_map <- data.frame(state = unique(boysUS$St.Prov))
boys_map[nrow(boys_map) + 1,] <- c("AK")
Bpopulation <- c()
for (i in boys_map$state) {
  temp_count = 0
  for (k in boysUS$St.Prov) {
    if (i == k) {
      temp_count = temp_count + 1
    }
  }
  Bpopulation <- c(Bpopulation, temp_count)
}
Bpopulation
boys_map$pop <- Bpopulation
boys_map <- boys_map[order(boys_map$state),]
rownames(boys_map) <- 1:nrow(boys_map)
head(boys_map, 10)

p1<- plot_usmap(regions = "states", data = boys_map, values = "pop", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Population Num", label = scales::comma)+
  labs(title = "JGS Boys in US", subtitle = "Population of Competitive Junior Golf Boys across the US") +
  theme(legend.position = "right")

  # Boys With standardized population
State_Pop_Temp <- State_Pop[-c(1:5),]
State_Pop_Temp$NAME <- state.abb[match(State_Pop_Temp$NAME,state.name)]
State_Pop_Temp <- State_Pop_Temp[order(State_Pop_Temp$NAME),]
rownames(State_Pop_Temp) <- 1:nrow(State_Pop_Temp)
pop_stand_boys <- boys_map
for (i in 1:50) {
  pop_stand_boys$pop[i] <- pop_stand_boys$pop[i]/State_Pop_Temp$POPESTIMATE2021[i]
}
plot_usmap(regions = "states", data = pop_stand_boys, values = "pop", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Population Num", label = scales::comma)+
  labs(title = "JGS Boys in US With Standardized Population", subtitle = "Population of Competitive Junior Golf Boys across the US, standardized by estimate population of people in each state.") +
  theme(legend.position = "right")


#girls US map
girls_map <- data.frame(state = unique(girlsUS$St.Prov))
girls_map[nrow(girls_map) + 1,] <- c("SD")
girls_map[nrow(girls_map) + 1,] <- c("AK")
girls_map[nrow(girls_map) + 1,] <- c("VT")
girls_map[nrow(girls_map) + 1,] <- c("ME")
Gpopulation <- c()
for (i in girls_map$state) {
  temp_count1 = 0
  for (k in girlsUS$St.Prov) {
    if (i == k) {
      temp_count1 = temp_count1 + 1
    }
  }
  Gpopulation <- c(Gpopulation, temp_count1)
}
girls_map$pop <- Gpopulation
girls_map <- girls_map[order(girls_map$state),]
rownames(girls_map) <- 1:nrow(girls_map)
head(girls_map, 10)

plot_usmap(regions = "states", data = girls_map, values = "pop", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Population Num", label = scales::comma)+
  labs(title = "JGS Girls in US", subtitle = "Population of Competitive Junior Golf Girls across the US") +
  theme(legend.position = "right")

  #Girls With Standardized Population
pop_stand_girls <- girls_map
for (i in 1:50) {
  pop_stand_girls$pop[i] <- pop_stand_girls$pop[i]/State_Pop_Temp$POPESTIMATE2021[i]
}
plot_usmap(regions = "states", data = pop_stand_girls, values = "pop", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Population Num", label = scales::comma)+
  labs(title = "JGS Girls in US With Standardized Population", subtitle = "Population of Competitive Junior Golf Girls across the US, standardized by estimate population of people in each state.") +
  theme(legend.position = "right")


#Combined Map 
boys_temp <- boys_map
girls_temp <- girls_map
for (i in 1:50) {
  boys_temp$pop[i] <- boys_temp$pop[i]+girls_temp$pop[i] 
}
junior_map <- boys_temp
plot_usmap(regions = "states", data = junior_map, values = "pop", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Population Num", label = scales::comma)+
  labs(title = "JGS Players in US", subtitle = "Population of Competitive Junior Players Across the US") +
  theme(legend.position = "right")

  #Combined With Standardized Population
pop_stand_junior <- junior_map
for (i in 1:50) {
  pop_stand_junior$pop[i] <- pop_stand_junior$pop[i]/State_Pop_Temp$POPESTIMATE2021[i]
}
plot_usmap(regions = "states", data = pop_stand_junior, values = "pop", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Population Num", label = scales::comma)+
  labs(title = "JGS Players in US With Standardized Population", subtitle = "Population of Competitive Junior Golf Players across the US, standardized by estimate population of people in each state.") +
  theme(legend.position = "right")


  # Quick conversion of State_Courses' state names using State_Income
colnames(State_Income)[1] <- "State"
State_Income <- State_Income[order(State_Income$State),]
State_Courses["State"] <- State_Income["State"]


#Map of Median Household Income by State
head(State_Income,5)
State_Income$State <- state.abb[match(State_Income$State,state.name)]
State_Income <- State_Income[order(State_Income$State),]
rownames(State_Income) <- 1:nrow(State_Income)
colnames(State_Income)[1] <- "state"

plot_usmap(regions = "states", data = State_Income, values = "HouseholdIncome", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Income Index", label = scales::comma)+
  labs(title = "Median Household Income", subtitle = "Census of the median reported household income per state") +
  theme(legend.position = "right")

#Total Players divided household income
pop_per_income <- pop_stand_junior
pop_per_income$pop <- pop_per_income$pop*(State_Income$HouseholdIncome)

plot_usmap(regions = "states", data = pop_per_income, values = "pop", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Index", label = scales::comma)+
  labs(title = "Players Per Median Household Income", subtitle = "Players in each state standardized by median income") +
  theme(legend.position = "right")

#Map of golf courses in each state
head(State_Courses, 5)
State_Courses$State <- state.abb[match(State_Courses$State,state.name)]
State_Courses <- State_Courses[order(State_Courses$State),]
rownames(State_Courses) <- 1:nrow(State_Courses)
colnames(State_Courses)[1] <- "state"
colnames(State_Courses)[2] <- "Courses"
State_Courses$Courses <- as.numeric(State_Courses$Courses)
State_Courses[is.na(State_Courses)] = 1044
plot_usmap(regions = "states", data = State_Courses, values = "Courses", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Golf Courses", label = scales::comma)+
  labs(title = "Golf Courses By State", subtitle = "Number of established golf courses across each state") +
  theme(legend.position = "right")


#Map of players per golf courses in each state
pop_per_course <- pop_stand_junior
for (i in 1:50) {
  pop_per_course$pop[i] <- (pop_per_course$pop[i]*State_Courses$Courses[i])/100000
}
plot_usmap(regions = "states", data = pop_per_course, values = "pop", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Index Num", label = scales::comma)+
  labs(title = "JGS Players in US in Relation to Courses", subtitle = "Population of Competitive Junior Golf Players across the US 
       divided by the total number of golf courses in each state") +
  theme(legend.position = "right")



  #For girls only
pop_per_course_girls <- pop_stand_girls
for (i in 1:50) {
  pop_per_course_girls$pop[i] <- (pop_per_course_girls$pop[i]*State_Courses$Courses[i])
}
plot_usmap(regions = "states", data = pop_per_course_girls, values = "pop", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Population Num", label = scales::comma)+
  labs(title = "JGS Girls in US in Relation to Courses", subtitle = "Population of Competitive Junior Golf Girls across the US 
       divided by the total number of golf courses in each state") +
  theme(legend.position = "right")

  #For Boys only
pop_per_course_boys <- pop_stand_boys
for (i in 1:50) {
  pop_per_course_boys$pop[i] <- (pop_per_course_boys$pop[i]*State_Courses$Courses[i])
}

plot_usmap(regions = "states", data = pop_per_course_boys, values = "pop", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Population Num", label = scales::comma)+
  labs(title = "JGS Boys in US in Relation to Courses", subtitle = "Population of Competitive Junior Golf Boys across the US 
       divided by the total number of golf courses in each state") +
  theme(legend.position = "right")

#Map of girls to boy ratio
bg_ratio <- girls_map
bg_ratio$pop <- bg_ratio$pop/boys_map$pop

plot_usmap(regions = "states", data = bg_ratio, values = "pop", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Population Num", label = scales::comma)+
  labs(title = "Girl to Boy Ratio Per State", subtitle = "ratio of Junior Girls to Boys in each state") +
  theme(legend.position = "right")

#Map of girls to all juniors ratio
gbtotal_ratio <- girls_map
gbtotal_ratio$pop <- gbtotal_ratio$pop/junior_map$pop
plot_usmap(regions = "states", data = gbtotal_ratio, values = "pop", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Population Num", label = scales::comma)+
  labs(title = "Girl to Boy Ratio Per State", subtitle = "ratio of Junior Girls to Boys in each state") +
  theme(legend.position = "right")


#Map of Avg Temp
State_Weather$state <- state.abb[match(State_Weather$state,state.name)]
plot_usmap(regions = "states", data = State_Weather, values = "AvgF", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Fahrenheit", label = scales::comma)+
  labs(title = "Avg Temp by State", subtitle = "The annual average temperature from 1971-2000 of each state by Fahrenheit") +
  theme(legend.position = "right")

  #Map of Precipitation
plot_usmap(regions = "states", data = State_Weather, values = "mm", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "mm", label = scales::comma)+
  labs(title = "Avg Precipitation by State", subtitle = "The annual precipitation from 1971-2000 of each state by millimeters") +
  theme(legend.position = "right")

  #Map of Percent Humidity
plot_usmap(regions = "states", data = State_Weather, values = "Afternoon", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Percent", label = scales::comma)+
  labs(title = "Annual Avg Relative Humidity by State", subtitle = "The annual percent humidity of each state from 1971-2000") +
  theme(legend.position = "right")

#Player vs Avg Temp
juniorVsTemp <- junior_map
juniorVsTemp$pop <- juniorVsTemp$pop/State_Weather$AvgF
plot_usmap(regions = "states", data = juniorVsTemp, values = "pop", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Index", label = scales::comma)+
  labs(title = "Player Population in Relation to Avg Temp", subtitle = "The annual average temperature from 1971-2000 of each state by Fahrenheit") +
  theme(legend.position = "right")


#Player vs Avg Precipitation
juniorVsPrecip <- pop_stand_junior
juniorVsPrecip$pop <- juniorVsPrecip$pop*State_Weather$mm
plot_usmap(regions = "states", data = juniorVsPrecip, values = "pop", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Index", label = scales::comma)+
  labs(title = "Player Population in Relation to Avg Temp", subtitle = "The annual average temperature from 1971-2000 of each state by Fahrenheit") +
  theme(legend.position = "right")

#Player vs Afternoon Humidity
juniorVsHum <- pop_stand_junior
juniorVsHum$pop <- juniorVsHum$pop*State_Weather$Afternoon
plot_usmap(regions = "states", data = juniorVsHum, values = "pop", color = "black")+
  scale_fill_continuous(low = "white", high = "red", name = "Index", label = scales::comma)+
  labs(title = "Player Population in Relation to Avg Temp", subtitle = "The annual average temperature from 1971-2000 of each state by Fahrenheit") +
  theme(legend.position = "right")




#Scatter plot Graphs

#Junior population vs State Populations
juniorVsStatePop<- data.frame(x = State_Pop_Temp$POPESTIMATE2021, y= junior_map$pop)
ggplot(juniorVsStatePop,aes(x = x,y = y)) +geom_point(colour='blue') + xlab("State Population") + ylab("Golfer Population")

#Boys and Girl (separated) population vs State Populations
boysVsStatePop <- data.frame(x = State_Pop_Temp$POPESTIMATE2021, y=boys_map$pop)
girlsVsStatePop <- data.frame(x = State_Pop_Temp$POPESTIMATE2021, y=girls_map$pop)
ggplot(boysVsStatePop,aes(x = x,y = y), group = measure, colour = measure) +geom_point(colour='blue') +geom_point(data=girlsVsStatePop,colour='red') + xlab("State Population") + ylab("Golfer Population") + ggtitle("Junior Population Vs State Population, Separated by Gender")

#boys and Girl (separated) pop vs Household Income by State
boysVsStateInc <- data.frame(x = State_Income$HouseholdIncome, y=boys_map$pop)
girlsVsStateInc <- data.frame(x = State_Income$HouseholdIncome, y=girls_map$pop)
ggplot(boysVsStateInc,aes(x = x,y = y)) +geom_point(colour='blue') +geom_point(data=girlsVsStateInc,colour='red') + xlab("State Income") + ylab("Golfer Population")

#boys and Girl (separated) pop vs Golf Courses
boysVsCourses <- data.frame(x = State_Courses$Courses, y=boys_map$pop)
girlsVsCourses <- data.frame(x = State_Courses$Courses, y=girls_map$pop)
ggplot(boysVsCourses,aes(x = x,y = y)) +geom_point(colour='blue') +geom_point(data=girlsVsCourses,colour='red') + xlab("Golf Courses") + ylab("Golfer Population")

#boys and Girl (separated) pop vs State Temperature 
boysVsTemp <- data.frame(x = State_Weather$AvgF, y=boys_map$pop)
girlsVsTemp <- data.frame(x = State_Weather$AvgF, y=girls_map$pop)
ggplot(boysVsTemp,aes(x = x,y = y)) +geom_point(colour='blue') +geom_point(data=girlsVsTemp,colour='red') + xlab("Avg Temperature Fahrenheit") + ylab("Golfer Population")

#boys and Girl (separated) pop vs State Precipitation
boysVsPrecip <- data.frame(x = State_Weather$mm, y=boys_map$pop)
girlsVsPrecip <- data.frame(x = State_Weather$mm, y=girls_map$pop)
ggplot(boysVsPrecip,aes(x = x,y = y)) +geom_point(colour='blue') +geom_point(data=girlsVsPrecip,colour='red') + xlab("Precipitation") + ylab("Golfer Population")

#boys and Girl (separated) pop vs State Humidity in Morning
boysVsMornHum <- data.frame(x = State_Weather$Morning, y=boys_map$pop)
girlsVsMornHum <- data.frame(x = State_Weather$Morning, y=girls_map$pop)
ggplot(boysVsMornHum,aes(x = x,y = y)) +geom_point(colour='blue') +geom_point(data=girlsVsMornHum,colour='red') + xlab("Morning Humidity") + ylab("Golfer Population")

#boys and Girl (separated) pop vs State Humidity in Afternoon
boysVsAfterHum <- data.frame(x = State_Weather$Afternoon, y=boys_map$pop)
girlsVsAfterHum <- data.frame(x = State_Weather$Afternoon, y=girls_map$pop)
ggplot(boysVsAfterHum,aes(x = x,y = y)) +geom_point(colour='blue') +geom_point(data=girlsVsAfterHum,colour='red') + xlab("Afternoon Humidity") + ylab("Golfer Population")

#fitting linear models
#boys & girls pop vs state pop
boysVsStatePopLm <- lm(y ~ x, data = boysVsStatePop)
girlsVsStatePopLm <- lm(y ~ x, data = girlsVsStatePop)
#boys & girls pop vs state income
boysVsStateIncLm <- lm(y ~ x, data = boysVsStateInc)
girlsVsStateIncLm <- lm(y ~ x, data = girlsVsStateInc)
#boys & girls pop vs state courses
boysVsCoursesLm <- lm(y ~ x, data = boysVsCourses)
girlsVsCoursesLm <- lm(y ~ x, data = girlsVsCourses)
#boys & girls pop vs state average temp
boysVsTempLm <- lm(y ~ x, data = boysVsTemp)
girlsVsTempLm <- lm(y ~ x, data = girlsVsTemp)
#boys & girls pop vs state precipitation
boysVsPrecipLm <- lm(y ~ x, data = boysVsPrecip)
girlsVsPrecipLm <- lm(y ~ x, data = girlsVsPrecip)
#boys & girls pop vs state humidity AM
boysVsMornHumLm <- lm(y ~ x, data = boysVsMornHum)
girlsVsMornHumLm <- lm(y ~ x, data = girlsVsMornHum)
#boys & girls pop vs state humidity PM
boysVsAfterHumLm <- lm(y ~ x, data = boysVsAfterHum)
girlsVsAfterHumLm <- lm(y ~ x, data = girlsVsAfterHum)

#visualizing linear models
par(mfrow=c(2,2))

summary(boysVsStatePopLm)
plot(boysVsStatePopLm)

summary(girlsVsStatePopLm)
plot(girlsVsStatePopLm)

summary(boysVsStateIncLm)
plot(boysVsStateIncLm)

summary(girlsVsStateIncLm)
plot(girlsVsStateIncLm)

summary(boysVsCoursesLm)
plot(boysVsCoursesLm)

summary(girlsVsCoursesLm)
plot(girlsVsCoursesLm)

summary(boysVsTempLm)
plot(boysVsTempLm)

summary(girlsVsTempLm)
plot(girlsVsTempLm)

summary(boysVsPrecipLm)
plot(boysVsPrecipLm)

summary(girlsVsPrecipLm)
plot(girlsVsPrecipLm)

summary(boysVsMornHumLm)
plot(boysVsMornHumLm)

summary(girlsVsMornHumLm)
plot(girlsVsMornHumLm)

summary(boysVsAfterHumLm)
plot(boysVsAfterHumLm)

summary(girlsVsAfterHumLm)
plot(girlsVsAfterHumLm)

par(mfrow=c(1,1))

#Plotting linear regression line
plot(y ~ x, data = boysVsStatePop, ylab = "Golfer Population", xlab = "State Population")
  abline(boysVsStatePopLm)





