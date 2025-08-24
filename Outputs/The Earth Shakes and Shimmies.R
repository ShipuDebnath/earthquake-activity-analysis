library(tidyverse)
library(tmap)
library(tmaptools)
library(forcats)
library(wordcloud)
library(sf)
library(lsr)
library(tidycensus)
EQ = all_month[, c(1:6, 14)]
EQ$Date = substr(EQ$time, 1, 10)
EQ$Date = as.Date(EQ$Date, format = "%Y-%m-%d")
EQ$time <- NULL
EQ$magType = tolower(EQ$magType)
EQ$magType = as.factor(EQ$magType)
locl = str_split(EQ$place, ",", simplify = TRUE)
locl = as.data.frame(locl)
View(locl)
locl$V2 = trimws(locl$V2)
locl$V2[locl$V2 == "Aleutian Islands"] <- "Alaska"
locl$V2[locl$V2 == "CA"] <- "California"
EQ$location = locl$V2
EQ$place <- NULL
EQ$location = as.factor(EQ$location)
View(EQ)
summary(EQ)
EQ = EQ[EQ$mag > 0, ]
A = ggplot(EQ,aes(x = mag)) + geom_histogram(col="#af33ff", fill = "#af33ff")
A
A + scale_x_continuous(breaks = seq(from = 0, to = 7, by = 0.5)) + labs(title = "Earthquake Magnitudes for the past 30 days") + labs(x="magnitude", y="Number") + theme_light()
D30 = aggregate(mag ~ Date, data = EQ, mean)
B = ggplot(data = D30, aes(x = Date, y = mag)) + geom_line() + geom_point()
B
B + labs(title = "Average Earthquake Activity over the past 30 days") + labs(y = "magnitude") + theme_light()
PMag = aggregate(mag ~ location, data = EQ, mean)
GT47 = PMag[PMag$mag > 4.7, ]
c = ggplot(data = GT47, aes(x = fct_reorder(location, mag), y = mag)) + geom_bar(stat="identity", color="#c3831a", fill="#c3831a")
c
c + labs(title="Locations with the most Intense Earthquake Activity \n over the past 30 days") + labs(x = "locations", y = "magnitude") + coord_flip() + theme_light()
LocFerq = data.frame(table(EQ$location))
View(LocFerq)
wordcloud(LocFerq$Var1, LocFerq$Freq, scale = c(3, 1), min.freq = 5)
CNA = EQ[EQ$location %in% c("California", "Nevada", "Alaska"), ]
ggplot(CNA, aes(x = location, y = mag)) + geom_jitter(alpha = 0.10) + labs(x = "locations", y = "magnitude") + theme_light()
CN = EQ[EQ$location %in% c("California", "Nevada"), ]
CNsf = st_as_sf(CN, coords = c("longitude", "latitude"))
CNsf = rename(CNsf, magnitude = mag)
data(state_laea)
state_laea <- st_as_sf(state_laea)
plot(state_laea)
statesCN = state_laea[state_laea$GEOID %in% c("06", "32"),]
plot(st_geometry(statesCN))
tm_shape(statesCN) + tm_polygons(col = "#f2fbd2") + tm_shape(CNsf) + tm_bubbles(size = 0.1, col = "magnitude") + tm_layout(main.title = "Earthquakes in California and Nevada over the past 30 days", main.title.size = 1, legend.outside = TRUE)
t.test(CN$mag ~ factor(CN$location))
cohensD(CN$mag ~ factor(CN$location))

CAK = EQ[EQ$location %in% c("California", "Alaska"), ]
CAKsf = st_as_sf(CAK, coords = c("longitude", "latitude"))
CAKsf = rename(CAKsf, magnitude = mag)
data(state_laea)
state_laea <- st_as_sf(state_laea)
plot(state_laea)
statesCAK = state_laea[state_laea$GEOID %in% c("06", "32"),]
plot(st_geometry(statesCAK))
tm_shape(statesCAK) + tm_polygons(col = "#f2fbd2") + tm_shape(CAKsf) + tm_bubbles(size = 0.1, col = "magnitude") + tm_layout(main.title = "Earthquakes in California and Alaska over the past 30 days", main.title.size = 1, legend.outside = TRUE)
t.test(CAK$mag ~ factor(CAK$location))
cohensD(CAK$mag ~ factor(CAK$location))

