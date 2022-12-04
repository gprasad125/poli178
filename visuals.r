# pkgs to run
require("dplyr")
require("ggplot2")
require("plyr")
require("scales")
require("sf")
require("rnaturalearth")
require("rnaturalearthdata")

# read in dataset
china = read.csv("data/chinaGlobalInvestmentTracker.csv")

############################################################

# Visualization 1: Heatmap of Chinese presence around the world

# group data by Country and calculate for each country how many times it appears
presence = china %>% group_by(Country) %>% dplyr::summarise(Frequency=n())

# replace terms to fit for rnaturalearth data 
original = c("USA", "Russian Federation", "Britain", "UAE", "Trinidad-Tobago")
replace = c("United States of America", "Russia", "United Kingdom", "United Arab Emirates", "Trinidad-Tobago")
presence$Country = mapvalues(presence$Country, from = original, to = replace)

# merge table to get geometric coordinates for plotting 
world <- ne_countries(scale = "medium", returnclass = "sf")
presence.world = merge(world, presence, by.x = "admin", by.y = "Country", all = TRUE)

# plot in ggplot + geom_sf() 
heatmap = ggplot(presence.world) + geom_sf(aes(fill = Frequency)) + scale_fill_gradient(low = "#cadbfd", high = "#052a75", na.value = "red")

############################################################

# Visualization 2: Bar plot of most prominent Sectors by Region

# group data by Region and Sector and calculate for each group how many times it appears
sector_by_region = china %>% group_by(Region, Sector) %>% dplyr::summarise(Frequency=n())

# order data by decreasing value of frequency 
sector_by_region = sector_by_region[order(sector_by_region$Frequency, decreasing = TRUE), ]

# replace region names to fit plot better 
original = unique(china$Region)
replace = c("NA", "US", "EA", "EU", "WA", "SA", "AUS", "NAF", "SAF")
sector_by_region$Region = mapvalues(sector_by_region$Region, from = original, to = replace)

# plot in ggplot + geom_bar()
barplot_sbr = ggplot(sector_by_region, aes(fill = Sector, x = Region, y = Frequency)) + geom_bar(position = "dodge", stat = "identity")

############################################################

# Visualization 3: Box plot showing distribution of Money by Sector

# subset data to just important columns 
money_and_sector = china[c("Quantity.in.Millions", "Sector")]

# plot in ggplot + geom_boxplot()
boxplot_mas = ggplot(money_and_sector, aes(x = Sector, y = Quantity.in.Millions)) + geom_boxplot() + ylim(0, 20100)

############################################################

# Visualization 4: Heatmap of Amount Spent around the world

# group data by Country and calculate for each country how much money was spent there 
spent = china %>% group_by(Country) %>% dplyr::summarise(Spent=sum(Quantity.in.Millions))

# replace terms to fit for rnaturalearth data 
original = c("USA", "Russian Federation", "Britain", "UAE", "Trinidad-Tobago")
replace = c("United States of America", "Russia", "United Kingdom", "United Arab Emirates", "Trinidad-Tobago")
spent$Country = mapvalues(presence$Country, from = original, to = replace)

# merge table to get geometric coordinates for plotting
spent.world = merge(world, spent, by.x = "admin", by.y = "Country", all = TRUE)

# plot in ggplot + geom_sf()
heatmap_spent = ggplot(spent.world) + geom_sf(aes(fill=Spent)) + scale_fill_gradient(low = "#0084ff", high = "#110071", na.value = "red")
