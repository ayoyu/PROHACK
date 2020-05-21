library(ggplot2)
library(dplyr)



#set working directory where you have your train/test data & stuff
setwd("/home/ouadie/PythonWorkspace/proHack/")
dir()

rawData = read.csv("train.csv")
summary(rawData)
names(rawData)


############ Target Variable distribution
gy = ggplot(data = rawData, aes(x=y))
gy+geom_histogram(bins=100, fill='brown', alpha=0.8)
gy +  geom_histogram(bins=100,aes( y=..density..), colour="black", fill="wheat")+geom_density(alpha=.5, fill='brown2') 

############# Missing values
missingValsPerc = apply(rawData, MARGIN = 2, FUN = function(x){return(round((sum(is.na(x))/length(x))*100,2))})
missingValsPerc = as.data.frame(missingValsPerc, row.names = names(missingValsPerc))
missingValsPerc
missingValsPerc['galaxy',]  


#########  existence.expectancy.at.birth Vs existence.expectancy.index        ######################## 

g_expectancy = ggplot(data = rawData, aes(x=existence.expectancy.at.birth,y=existence.expectancy.index, color=galactic.year))
g_expectancy + geom_point()


" 1 - We see that the relationship has a linear component, y=ax+b, we can find b through a y-ax"

" 2 - Life.expectancy.at.birth..male..galactic.years.  AND Life.expectancy.at.birth..female..galactic.years.
have over 65% missing values, they can be filled using the existence.expectancy.at.birth and maybe another gender related feature"


cor(rawData$existence.expectancy.index, rawData$existence.expectancy.at.birth, use = "complete.obs")

######### Gross.income.per.capita Vs existence.expectancy.at.birth ##############

g_income = ggplot(data = rawData, aes(x=log(Gross.income.per.capita)**2,y=Income.Index, color=galactic.year))
g_income = ggplot(data = rawData, aes(x=(log(Gross.income.per.capita)-log(10000))/(log(7500)-log(10000)),y=Income.Index, color=galactic.year))


min(rawData$Gross.income.per.capita, na.rm = T)
sum(rawData$Gross.income.per.capita<=0, na.rm = TRUE) #aberrant value


#imputing aberrant value with the smallest positive value
rawData$Gross.income.per.capita[is.na(rawData$Gross.income.per.capita)] = rawData$Gross.income.per.capita[order(rawData$Gross.income.per.capita)[2]]
hist(log(rawData$Gross.income.per.capita+1), breaks = 100)

" 1 - Logarithmic relationship, check this : https://bizfluent.com/how-4894211-calculate-human-development-index.html"


########### Intergalactic.Development.Index..IDI. Vs. Education.Index   ###############

g_dev = ggplot(data = rawData, aes(x=Intergalactic.Development.Index..IDI.,y=Education.Index, color=existence.expectancy.index))
g_dev + geom_point()

" 1 - Linear relationship, besides a potential relationship between dev metrics and expectancy metrics, check bellow : "
g_dev_expectancy = ggplot(data = rawData, aes(x=existence.expectancy.index**2,y=Education.Index, color=galactic.year))
g_dev_expectancy + geom_point()

" 2 - Indeed, the relationship seems quadratic : dev Vs expectancy"

###########  Intergalactic.Development.Index..IDI...Rank

hist(rawData$Intergalactic.Development.Index..IDI...Rank, breaks = 100)
length(unique(rawData$Intergalactic.Development.Index..IDI...Rank))
devRankSubset = rawData %>% select(c("galaxy","galactic.year","Intergalactic.Development.Index..IDI...Rank")) %>% group_by(galactic.year) %>%  mutate(number_ranks = length(unique(Intergalactic.Development.Index..IDI...Rank)),  
                                                                                                                                                      number_galaxies = length(unique(galaxy)),  
                                                                                                                                                      max_rank = max(Intergalactic.Development.Index..IDI...Rank, na.rm = T),
                                                                                                                                                      min_rank = min(Intergalactic.Development.Index..IDI...Rank, na.rm = T))
devRankSubset = devRankSubset  %>% group_by(galactic.year) %>%  mutate(rank_range = max_rank-min_rank)

min(devRankSubset$number_galaxies) #at least we have 87 galaxy record for each year
max(devRankSubset$number_galaxies) #at most we have 181 galaxy record for each year (we can never find all the galaxies in a single year)


#number of galaxies per year : starting arround 100700, the number of galaxies goes down from 180 to 87 (maybe some are used in test with different names?)
g_number_galaxies = ggplot(data = devRankSubset, aes(x= galactic.year, y = number_galaxies))
g_number_galaxies+geom_line()
#evolution of ranks range
g_number_galaxies = ggplot(data = devRankSubset, aes(x= galactic.year, y = rank_range))
g_number_galaxies+geom_line()
"Something happens in the year 1010000 : the intergalactic rank margins (range) grow bigger, which means that the gap 
gets bigger by time, some grow exponentially and some stay still (or improve slowly)"

summary(devRankSubset$rank_range)


#############Population..total..millions.   Vs.     Population..urban....      Vs.       Mortality.rate..under.five..per.1.000.live.births. 


g_population = ggplot(data = rawData, aes(x= Population..total..millions., y = Population..urban...., color = Population..total..millions.))
g_population+geom_point()

" 1 - seems like we have two clusters of galaxies based on population's size"
min(rawData$Population..total..millions., na.rm = T)  # -160.4362 : aberrant value, looks like it's on purpose, missing values are given this  -160.4362

g_population_morta = ggplot(data = rawData, aes(x= log(Mortality.rate..under.five..per.1.000.live.births.+1), y = Population..urban...., color = Population..urban....))
g_population_morta+geom_point()

" 2 - obviously, galaxies with more urban population have less mortality rates"


################# Population.using.at.least.basic.drinking.water.services....       VS.    Population.using.at.least.basic.sanitation.services.... 

g_basic_services = ggplot(data = rawData, aes(x= Population.using.at.least.basic.drinking.water.services...., y = Population.using.at.least.basic.sanitation.services....))
g_basic_services+geom_point(color='brown2', alpha=0.8, size=2)

" 1 - almost linear, make sense"



#Investigating time effect on target y

#' 0 : No missing values for both variables
missingValsPerc['galactic.year',]  
missingValsPerc['y',]  

#' 1 : RAW plot
g_y_time = ggplot(data = rawData, aes(x=galactic.year,y=y))
g_y_time+geom_point(color='brown')


#' 2 : Aggregated per year plot : we have 26 distinct years
rawDataTime = rawData %>% select(c("galactic.year","y")) %>%  group_by(galactic.year)%>%mutate(mean_Y=mean(y)) %>% select(c("galactic.year","mean_Y")) %>% distinct()

g_y_time = ggplot(data = rawDataTime, aes(x=galactic.year,y=mean_Y))
g_y_time+geom_line(color='wheat', lwd=2)+geom_point(color='brown')  #WTF


#' 3 : Aggregated per galaxy plots : we have 181 galaxy, let's check some 10 sized batches

uniqueGalaxies = unique(rawData$galaxy)

subsetGalaxies = uniqueGalaxies[1:10]
rawDataSubset = rawData %>% select(c("galaxy","galactic.year","y")) %>% filter(galaxy %in% subsetGalaxies)
dim(rawDataSubset)
g_y_time = ggplot(data = rawDataSubset, aes(x=galactic.year,y=y, color=galaxy))
print(g_y_time+geom_line(aes(color=galaxy)))

subsetGalaxies = uniqueGalaxies[110:120]
rawDataSubset = rawData %>% select(c("galaxy","galactic.year","y")) %>% filter(galaxy %in% subsetGalaxies)
dim(rawDataSubset)
g_y_time = ggplot(data = rawDataSubset, aes(x=galactic.year,y=y, color=galaxy))
print(g_y_time+geom_line(aes(color=galaxy)))

"Something happens in the year 1010000 : many galaxies change behavior at that time (go up or down, flex point)"
"let's see the time distribution in test & train sets"


rawTest = read.csv("test.csv")
dim(rawTest)
g_time_test = ggplot(data = rawTest, aes(x=galactic.year))
g_time_test+geom_histogram(bins = 100)

subsetTrainTime = rawData %>% select("galaxy","galactic.year") %>% mutate(set="Train Set")
subsetTestTime = rawTest %>% select("galaxy","galactic.year") %>% mutate(set="Test Set")
joinedSubset = rbind(subsetTrainTime, subsetTestTime)

g_joined_time_data = ggplot(data = joinedSubset, aes(x=galactic.year, fill=set))
g_joined_time_data + geom_histogram(bins=40,aes(fill=set))









