#  How GDP and Social Inequality Affect Netflix's Growth? #####

# The main objective is to present graphics in R.
# Therefore, I will not spend too much time in cleaning and organizing the data
# (that is why I choose a simple dataset).

# Thesis: For that I will compare the number of subscriptions in Netflix with
# the country's wage, to prove that as the salary condition increases,
# the number of subscribers also increases.

# Links for the data:
# IMDB data:
  # https://datasets.imdbws.com/
# Netflix Data:
  # https://www.comparitech.com/blog/vpn-privacy/countries-netflix-cost/
# GDP:
  # https://data.worldbank.org/indicator/
# Wage Inequality Dataset:
  # https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/LM4OWF
# TOP 10 Netflix shows:
  # https://top10.netflix.com/
# Netflix subscribers and revenue by country:
  # https://www.comparitech.com/tv-streaming/netflix-subscribers/
# ISO Country Codes - Global:
  # https://www.kaggle.com/datasets/andradaolteanu/iso-country-codes-global



###################### PART 01 ######################

# Working directory
# Run this script from the repository root (where this .R file lives).
# In RStudio you can do: Session > Set Working Directory > To Source File Location.
# Paths below are relative and use forward slashes so they work on every OS.

# Necessary packages
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(plotly)



# Importing the data
netflix_data_dec_2021 <- read.csv("Datasets/raw/dados_netflix_Dec_2021.csv")

netflix_subscriptions_jul_2021 <- read.csv("Datasets/raw/as_net_2021.csv")

wage_inequality_data <- read.csv("Datasets/raw/dados_desig_soc_harvard.csv")

world_bank_data <- read.csv("Datasets/raw/dados_world_bank.csv", header = FALSE)

top_10_shows_netflix <- read_excel("Datasets/raw/top10.xlsx")

wikipedia_iso_country_codes <- read.csv("Datasets/raw/iso-country-codes.csv")

# data.tsv is the IMDB title.basics file (~800MB, not committed).
# Download title.basics.tsv.gz from https://datasets.imdbws.com/,
# extract it, and save it as Datasets/raw/data.tsv before running stages 3-7.
IMDB_Data <- read_tsv("Datasets/raw/data.tsv")

## See the data

# View(netflix_data_dec_2021)
# View(netflix_subscriptions_jul_2021)
# View(wage_inequality_data)
# View(world_bank_data)
# View(top_10_shows_netflix)
# View(wikipedia_iso_country_codes)
# View(IMDB_Data)

# Mapping & Cleaning

# Analyzing each dataset
# I have used this commands bellow to analyze the quality of the data 
# and to understand the datasets

# dim(x)
# colnames(x)
# rownames(x)
# head(x)
# str(x)
# summary(x)

########### PART 02 ###########

### Cleaning and Preparing the First Combined Dataset ###

# Create a column with the difference data for the bar chart 
# (standard plan - basic plan)
netflix_data_dec_2021$basic_standard_diff <- (
  netflix_data_dec_2021$Cost.Per.Month...Standard.... 
  - netflix_data_dec_2021$Cost.Per.Month...Basic....)

# Create a column with the data difference for the bar chart 
# (premium plan - standard plan)
netflix_data_dec_2021$premium_standard_diff <- (
  netflix_data_dec_2021$Cost.Per.Month...Premium.... 
  - netflix_data_dec_2021$Cost.Per.Month...Standard....)


# Combine previous data with GDP data
names(world_bank_data)[
  names(world_bank_data) == 'V1'] <- 'Country'

netflix_data_GDP <- merge(
  netflix_data_dec_2021, 
  world_bank_data, 
  by = "Country")

# Extract the GDP column.
# IMPORTANT: in the raw World Bank file the year header row maps V64 -> 2019
# and V65 -> 2020. This pipeline keeps V64, so the value is 2019 GDP, not 2020.
# The column is therefore labeled "2019 GDP (World Bank)" to match the data.
# To use true 2020 GDP instead, keep V65 below and relabel accordingly
# (note 2020 carries the COVID drop, e.g. Brazil 2019 = 1.878e12, 2020 = 1.445e12).
netflix_data_GDP2019 <- netflix_data_GDP[-c(11:72, 74, 75)]

names(netflix_data_GDP2019)[names(netflix_data_GDP2019) == 'V64'] <-
  "2019 GDP (World Bank)"

# Cleanup of wage inequality dataframe
wage_inequality_data <- wage_inequality_data[, c(1:3)]

wage_inequality_data_year <- 
  wage_inequality_data %>% 
  group_by(country) %>% 
  summarise(max = max(year, na.rm = TRUE))

# Combine the dataframes
wage_inequality_data <- merge(
  wage_inequality_data, 
  wage_inequality_data_year, 
  by.x = c("country", "year"), 
  by.y = c("country", "max"))

netflix_data_GDP_wage2020 <- merge(
  netflix_data_GDP2019,
  wage_inequality_data,
  by.x=c("Country"),
  by.y=c("country"))

# Clear the billing and subscription dataset 
# and combine it with the previous dataframe
netflix_subscriptions <- netflix_subscriptions_jul_2021[,c(1, 23,24)]

complete <- merge(
  netflix_data_GDP_wage2020, 
  netflix_subscriptions, 
  by=c("Country"))

# Merge the countrycode into the choropleth map
countrycode <- wikipedia_iso_country_codes[,c(1, 3)]

complete <- merge(
  complete, 
  countrycode, 
  by.x=c("Country"), 
  by.y=c("English.short.name.lower.case"))


# Save the dataframe produced so far
write.csv(complete, "Datasets/clean/complete.csv", row.names = FALSE)


### Cleaning and Preparing the Second Combined Dataset ###

# Clear and filter the IMDB dataframe
genre <- IMDB_Data[,-c(1, 4:8)]
names(genre)[names(genre) == 'primaryTitle'] <- 'show_title'

# Associate the genre with the Top 10 shows
topgenre <- merge(
  top_10_shows_netflix, 
  genre, 
  by = "show_title")
# View(topgenre)

# Clear the previous dataframe to keep only 1 entry for each top 10
topgenre <- topgenre[
  (topgenre$category == "Films" & topgenre$titleType == "movie") |
    (topgenre$category == "TV" & topgenre$titleType == "tvSeries"), ] 

topgenre <- distinct(
  topgenre, 
  show_title, 
  week, 
  country_name, 
  category, 
  titleType,
  cumulative_weeks_in_top_10, 
  .keep_all= TRUE)
# View(topgenre)

# Keep only movie genre information by country
topgenrecountry <- topgenre[,-c(1, 3:9)]
#View(topgenrecountry)

# Dataframe pivot
topgenrecountry <- separate(
  topgenrecountry, 
  c("genres") , 
  c("genre1", "genre2", "genre3"), 
  sep = ",")

topgenrecountry <- pivot_longer(
  topgenrecountry, 
  c("genre1", "genre2", "genre3"), 
  names_to = "genre123", 
  values_to = "genres")
# View(topgenrecountry)

# Count the number of genres
genrecount <- count(topgenrecountry, country_name, genres)
genrecount <- na.omit(genrecount)
genrecount <-subset(genrecount, genres!="\\N")
genrecount$n <- as.numeric(genrecount$n)
#View(genrecount)

# Save to disk
write.csv(genrecount, "Datasets/clean/genrecount.csv", row.names = FALSE) 

### Cleaning and Preparing the Third Combined Dataset ###

# Rename the previous dataframe
sunburst <- rename(genrecount, label = country_name)

# Remove the dashes
sunburst$genres = sub("-", " ", sunburst$genres)

# Set the name
sunburst$parent = c("total  - ")
sunburst$parent <- paste(sunburst$parent, sunburst$genres)
sunburst$id = c(" - ")
sunburst$id <- paste(sunburst$parent, sunburst$id)
sunburst$id <- paste(sunburst$id, sunburst$label)
sunburst$n <- as.numeric(sunburst$n)
#View(sunburst)

# Aggregate
added <- aggregate(sunburst$n, list(sunburst$genres), FUN=sum)
added <- rename(added, label = Group.1)
added <- rename(added, n = x)
added$n <- as.numeric(added$n)
added$genres <- c(NA)
added$parent <- c("total")
added$id <- c(" - ")
added$id <- paste(added$parent, added$id)
added$id <- paste(added$id, added$label)
#View(added)

# calculate sum
total = sum(added$n)

# Combine everything into the final dataframe.
# Drop the genres helper column first so the frame has 4 columns
# (label, n, parent, id), then prepend the root row with named columns so
# the root keeps a real label ("total") instead of landing as NA.
sunburst <- rbind(added, sunburst)
sunburst <- sunburst[,-c(3)]
root_row <- data.frame(
  label = "total",
  n = total,
  parent = NA,
  id = "total")
sunburst <- rbind(root_row, sunburst)
sunburst$n <- as.numeric(sunburst$n)
#View(sunburst)

# Save to disk
write.csv(sunburst, "Datasets/clean/sunburst.csv", row.names = FALSE)


### Cleaning and Preparing the Fourth Combined Dataset ###

# Let's work with top 10 to avoid performance issues in graphics.
# sunburst has 29 header rows here: 1 grand-total row plus 28 genre-aggregate
# rows (Action through Western). Drop all 29 so only country-level rows remain.
# The previous -c(1:28) left the 29th header row (Western, n=453) in the data,
# which corrupted top10sunburst.csv and double-counted 453 in the grand total.
top10sunburst <- subset(sunburst, parent != "total" & label != "total")
top10sunburst$n <- as.numeric(top10sunburst$n)
#View(top10sunburst)

# Top 10 genres by country
top10sunburst <- top10sunburst %>% 
  group_by(label) %>%
  top_n(10,n)
#View(top10sunburst)

# Recalculate the totals, adjust and match the dataframe
top10add <- aggregate(
  top10sunburst$n, 
  list(top10sunburst$parent), 
  FUN = sum)
top10add <- rename(top10add, id = Group.1)
top10add <- rename(top10add, n = x)

top10add$label = sub("total  -  ", "", top10add$id)
top10add$parent = c("total")
top10add$n <- as.numeric(top10add$n)

total = sum(top10add$n)
top10sunburst <- rbind(top10add, top10sunburst)
# Prepend the root row with named columns (4 columns: id, n, label, parent).
# The old rbind passed a 5-element vector into this 4-column frame, which
# misaligned the columns and left the root node without a label in the output.
top10root <- data.frame(
  id = "total",
  n = total,
  label = "total",
  parent = NA)
top10sunburst <- rbind(top10root, top10sunburst)
top10sunburst$n <- as.numeric(top10sunburst$n)
#View(top10sunburst)

# Save to disk
write.csv(top10sunburst, "Datasets/clean/top10sunburst.csv", row.names = FALSE)


### Cleaning and Preparing the Fifth Combined Dataset ###

# Filter the previous dataframe and create a new one
in_total <- sunburst[-c(1),]
in_total$parent = sub("total  -  ", "", in_total$parent)
in_total$parent = sub("total", NA, in_total$parent)
in_total$id = sub("total  -  ", "", in_total$id)
#View(in_total)

# Salva em disco
write.csv(in_total, "Datasets/clean/in_total.csv", row.names = FALSE)


### Sixth Combined Dataset Cleanup and Preparation ### 

# Filter the previous dataframe and create a new one
countrytree <- in_total[-c(1:28),]
# rename maps label -> parents and parent -> labels. Use the exact new names
# below instead of relying on data.frame $ partial matching (countrytree$parent
# / $label), which silently breaks on tibbles or with warnPartialMatchDollar.
countrytree <- rename(countrytree, parents = label)
countrytree <- rename(countrytree, labels = parent)
countrytree$id = c(" - ")
countrytree$id <- paste(countrytree$labels, countrytree$id)
countrytree$id <- paste(countrytree$id, countrytree$parents)
countries <- aggregate(countrytree$n, list(countrytree$parents), FUN = sum)
countries <- rename(countries, labels = Group.1)
countries <- rename(countries, n = x)
countries$n <- as.numeric(countries$n)
countries$id <- countries$labels
countries$parents <- c(NA)
countrytree <- rbind(countrytree, countries)
#View(countrytree)

# Save to disk
write.csv(countrytree, "Datasets/clean/countrytree.csv", row.names = FALSE)

######################################################

# Load the first clean dataset
complete <- read.csv("Datasets/clean/complete.csv")

# Set the data type of some columns
complete$X..of.Subscribers.Q4.2021..Estimate. <- 
  as.numeric(
    gsub(
      ",", 
      "", 
      complete$X..of.Subscribers.Q4.2021..Estimate.))

complete$Q4.2021.Revenue....Estimate. <- 
  as.numeric(
    gsub(
      ",", 
      "", 
      complete$Q4.2021.Revenue....Estimate.))

## Chanhging the columns names
names(complete)[
  names(complete) == 'Q4.2021.Revenue....Estimate.'] <- 
  'Netflix.Q42021.Revenue'

names(complete)[
  names(complete) == 'X..of.Subscribers.Q4.2021..Estimate.'] <- 
  'Netflix.Subscriptions.Q42021'

names(complete)[
  names(complete) == 'Total.Library.Size'] <- 
  'Total.Catalog.Size'

names(complete)[
  names(complete) == 'Cost.Per.Month...Basic....'] <- 
  'Basic.Subscription.Price'

names(complete)[
  names(complete) == 'Cost.Per.Month...Standard....'] <- 
  'Standard.Subscription.Price'

names(complete)[
  names(complete) == 'Cost.Per.Month...Premium....'] <- 
  'Premium.Subscription.Price'

# Create dataframes by filtering outliers
complete_scat_out <- 
  filter(
    complete, 
    Country != "United States") 
#Because the volume of signatures and views are overwhelmingly high

complete_bar <- 
  filter(
    complete, 
    Country != "Switzerland") #Because social inequality is very low

complete_bar_out <- 
  filter(complete_bar, 
         Country != "South Africa") #Because social inequality is very high

# Load datasets 2, 3 and 6
genre <- read.csv("Datasets/clean/genrecount.csv")
tree <- read.csv("Datasets/clean/sunburst.csv")
countries <- read.csv("Datasets/clean/countrytree.csv")

# Filter the list of countries by removing NA values
country_list <- filter(countries, is.na(parents))



####### Visualization #######

# Scatter Plot

## We are going to analyze this mainly variables:
## Netflix.Q42021.Revenue, Netflix.Subscriptions.Q42021,
## Total.Catalog.Size, Basic.Subscription.Price, 
## Standard.Subscription.Price, Premium.Subscription.Price

## without outliers

## Netflix.Q42021.Revenue
fig <- plot_ly(
  data = complete_scat_out, 
  x = ~X2019.GDP..World.Bank.,
  y = ~Netflix.Q42021.Revenue,
  type = "scatter", 
  mode = "markers", 
  color = ~Country,
  text = ~Country,
               marker = list(size = 10,
                             line = list(color = 'rgba(152, 0, 0, .8)',
                                         width = 2)))

fig <- fig %>% 
  layout(title = 'Netflix Q4-2021 Revenue',
            yaxis = list(title = 'Netflix Revenue'),
            xaxis = list(title = 'GDP (USD)'))

fig


## Netflix.Subscriptions.Q42021
fig <- plot_ly(
  data = complete_scat_out, 
  x = ~X2019.GDP..World.Bank.,
  y = ~Netflix.Subscriptions.Q42021,
  type = "scatter", 
  mode = "markers", 
  color = ~Country,
  text = ~Country,
  marker = list(size = 10,
                line = list(color = 'rgba(152, 0, 0, .8)',
                            width = 2)))

fig <- fig %>% 
  layout(title = 'Netflix Subscriptions Q42021',
              yaxis = list(title = 'Subscription'),
              xaxis = list(title = 'GDP (USD)'))

fig

## Total.Catalog.Size
fig <- plot_ly(
  data = complete_scat_out, 
  x = ~X2019.GDP..World.Bank.,
  y = ~Total.Catalog.Size,
  type = "scatter", 
  mode = "markers", 
  color = ~Country,
  text = ~Country,
  marker = list(size = 10,
                line = list(color = 'rgba(152, 0, 0, .8)',
                            width = 2)))

fig <- fig %>% 
  layout(title = 'Total Netflix Catalog Size',
              yaxis = list(title = 'Catalog Size'),
              xaxis = list(title = 'GDP (USD)'))

fig

## Basic.Subscription.Price
fig <- plot_ly(
  data = complete_scat_out, 
  x = ~X2019.GDP..World.Bank.,
  y = ~Basic.Subscription.Price,
  type = "scatter", 
  mode = "markers", 
  color = ~Country,
  text = ~Country,
  marker = list(size = 10,
                line = list(color = 'rgba(152, 0, 0, .8)',
                            width = 2)))

fig <- fig %>% 
  layout(title = 'Basic Netflix Subscription Price',
              yaxis = list(title = 'Basic Subscription Price'),
              xaxis = list(title = 'GDP (USD)'))

fig

## Standard.Subscription.Price
fig <- plot_ly(
  data = complete_scat_out, 
  x = ~X2019.GDP..World.Bank.,
  y = ~Standard.Subscription.Price,
  type = "scatter", 
  mode = "markers", 
  color = ~Country,
  text = ~Country,
  marker = list(size = 10,
                line = list(color = 'rgba(152, 0, 0, .8)',
                            width = 2)))

fig <- fig %>% 
  layout(
    title = 'Standard Netflix Subscription Price',
    yaxis = list(title = 'Standard Subscription Price'),
    xaxis = list(title = 'GDP (USD)'))

fig

## Premium.Subscription.Price
fig <- plot_ly(
  data = complete_scat_out, 
  x = ~X2019.GDP..World.Bank.,
  y = ~Premium.Subscription.Price,
  type = "scatter", 
  color = ~Country,
  mode = "markers", 
  text = ~Country,
  marker = list(size = 10,
                line = list(color = 'rgba(152, 0, 0, .8)',
                            width = 2)))

fig <- fig %>% 
  layout(
    title = 'Premium Netflix Subscription Price',
    yaxis = list(title = 'Premium Subscription Price'),
    xaxis = list(title = 'GDP (USD)'))

fig


## Billing x Subscriptions
fig <- plot_ly(
  data = complete_scat_out, 
  x = ~Netflix.Subscriptions.Q42021, 
  y = ~Netflix.Q42021.Revenue,
  type = "scatter", 
  mode = "markers", 
  color = ~Country,
  text = ~Country,
  marker = list(size = 10,
                line = list(color = 'rgba(152, 0, 0, .8)',
                            width = 2)))

fig <- fig %>% 
  layout(
    title = 'Billing x Subscriptions',
    yaxis = list(title = 'Netflix Revenue in Q4-2021'),
    xaxis = list(title = 'Netflix Subscribers in Q4-2021'))

fig


## Map Billing (Revenue)

l <- list(color = toRGB("grey"), width = 0.5)
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Miller')
)

fig <- plot_geo(complete_scat_out)
fig <- fig %>% 
  add_trace(
    z = complete_scat_out$Netflix.Q42021.Revenue, 
    color = complete_scat_out$Netflix.Q42021.Revenue, 
    colorscale = 'Purples', 
    text = ~Country, 
    locations = ~Alpha.3.code, 
    marker = list(line = l))

fig <- fig %>%
  colorbar(title = 'Scale')

# Pass geo = g so the Miller projection and frame settings actually apply.
fig <- fig %>%
  layout(title = 'Netflix Global Map in Q4-2021 (Billing)', geo = g)

fig


## Map Subscription

l <- list(color = toRGB("grey"), width = 0.5)
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Miller')
)

fig <- plot_geo(complete_scat_out)
fig <- fig %>% 
  add_trace(
    z = complete_scat_out$Netflix.Subscriptions.Q42021, 
    color = complete_scat_out$Netflix.Subscriptions.Q42021, 
                         colorscale = 'Purples', 
                         text = ~Country, 
                         locations = ~Alpha.3.code, 
                         marker = list(line = l))

fig <- fig %>%
  colorbar(title = 'Scale')

# Pass geo = g so the Miller projection and frame settings actually apply.
fig <- fig %>%
  layout(title = 'Netflix Global Map in Q4-2021 (Subscription)', geo = g)

fig




## Disclaimer: a good part of this project was largely done in the 
## Data Science Academy, Big Data Analytics with R and 
## Microsoft Azure Machine Learning course 
## (part of the Data Scientist training)
