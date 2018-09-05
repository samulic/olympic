## Questo file contiene alcuni steps di preprocessing come per esempio
# - eliminazione di "Art competition" dagli sport
# - shift dell'anno delle olimpiadi invernali di due, cosi' da avere il medesimo anno per estate/inverno
# - eliminazione dell'attributo "note" relativo all'eventuale descrizione del comitato olimpico/nazione
# - eliminazione dell'attributo "games" ricavabile concatenando "Year" e "Season"
# - eliminazione dell'attributo "team" perche' spesso identico a "Country" e se non identico, comunque appartenente a "Country"
# - eliminazione dell'attributo 'name' perche' non utile se non in fase esplorativa (chi vince le medaglie?), inoltre abbiamo ID
# - settaggio dell'attrobuto 'year' come fattore ordinato
## Viene inoltre integrato il dataset con il dato relativo alla zona geografica di appartenenza di ogni 
## comitato olimpico (NOC), piu' precisamente viene aggiunta una colonna di "sottozona geografica" con 17 livelli

library(tidyverse)

data <- read_csv("input/athlete_events.csv",
                 col_types = cols(
                   ID = col_character(),
                   Name = col_character(),
                   Sex = col_factor(levels = c("M","F")),
                   Age =  col_integer(),
                   Height = col_double(),
                   Weight = col_double(),
                   Team = col_character(),
                   NOC = col_character(),
                   Games = col_character(),
                   Year = col_integer(),
                   Season = col_factor(levels = c("Summer","Winter")),
                   City = col_character(),
                   Sport = col_character(),
                   Event = col_character(),
                   Medal = col_factor(levels = c("Gold","Silver","Bronze"))
                 )
)

noc <- read_csv("input/noc_nazioni.csv",
                col_types = cols(
                  NOC = col_character(),
                  region = col_character()
                ))

data <- data %>% 
  left_join(noc, by = "NOC") %>%
  filter(!is.na(region))

str(data)
# Escludiamo "art competitions" dal dataset (3576 obs ~> 1.3%)
data <- data %>% filter(Sport != "Art Competitions")
# Recode year of Winter Games after 1992 to match the next Summer Games
# Thus, "Year" now applies to the Olympiad in which each Olympics occurred 
original <- c(1994,1998,2002,2006,2010,2014)
new <- c(1996,2000,2004,2008,2012,2016)
for (i in 1:length(original)) {
  data$Year <- gsub(original[i], new[i], data$Year)
}
data$Year <- as.integer(data$Year)

#############load('input/data.rdata')
# this file contains every National Olympic Commitee with its ISO country code
countries <- read.csv("input/nazioni_iso.csv", stringsAsFactors = TRUE, na.strings="")
# this file contains every country's location details (i.e. continent, subregion) identified by its ISO code
iso <- read.csv("input/nazioni_details.csv", stringsAsFactors = TRUE, na.strings="")
# get the list of unique NOC available in the dataset
comitati <- as.tibble(list(NOC = as.factor(data$NOC))) %>% unique() %>% arrange(NOC)

# Map unknown NOC to known NOC so that we can join them and retrive ISO code
# Some NOC are peculiar, so we need to manually identify the ones which are not in the list of NOC
# Since we are only interested in retriving the location details (i.e. subregion (Western Asia))
# we generalize small/obsolete NOC to bigger ones, for example, FRG and GDR which refer to West/East Germany
# we set them to GER, without loss of information regarding the location details.
comitati$NOC <- plyr::mapvalues(comitati$NOC, 
                                from=c("AHO", "ANZ", "BOH", "CRT", "EUN", "FRG", "GDR", "IOA", "LIB", "MAL", "NBO", "NFL",
                                       "RHO", "SAA", "SCG", "TCH", "UAR", "URS", "VNM", "WIF", "YAR", "YMD", "YUG"), 
                                to = c("ANT", "AUS", "CZE", "GRE", "UKR", "GER", "GER", "IOA", "LBN", "MAS", "MAS", "CAN",
                                       "ZIM", "GER", "MNE", "CZE", "UAE", "RUS", "VIE", "JAM", "YEM", "YEM", "SRB"))

comitati_location <- left_join(comitati, countries, by = c("NOC" = "IOC"))
comitati_location <- filter(comitati_location, !is.na(NOC))
head(comitati_location, 10)
# Let's now join on ISO to add a location
comitati_iso <- left_join(comitati_location, iso, by = c("ISO" = "alpha.3"))
comitati_iso <- select(comitati_iso, NOC, Country, ISO, region, sub.region)

distinct_noc_full <- comitati_iso %>% select(NOC, ISO, region, sub.region) %>% unique()

# Since we have modified the NOC names, we have to join on Team
#dd <- left_join(df, full, by = "Team")
# Ok we have a problem, some team names are the same for different NOCs --> additional non-existing rows on join
# solution: easy, use the same mapping as above for NOC on the original DF and join on NOC
# we are not afraid of losing information about these NOCs because they all are exception, most of them
# referring to countries that do not exist anymore (think about YUGoslavia for example )
data$NOC <- plyr::mapvalues(data$NOC, 
                          from=c("AHO", "ANZ", "BOH", "CRT", "EUN", "FRG", "GDR", "IOA", "LIB", "MAL", "NBO", "NFL",
                                 "RHO", "SAA", "SCG", "TCH", "UAR", "URS", "VNM", "WIF", "YAR", "YMD", "YUG"), 
                          to = c("ANT", "AUS", "CZE", "GRE", "UKR", "GER", "GER", "IOA", "LBN", "MAS", "MAS", "CAN",
                                 "ZIM", "GER", "MNE", "CZE", "UAE", "RUS", "VIE", "JAM", "YEM", "YEM", "SRB"))

final <- left_join(data, distinct_noc_full, by = "NOC")

status <- funModeling::df_status(final)
final$ISO <- NULL
library(data.table)
setnames(final, "region.x", "Country")
setnames(final, "notes", "Notes")
setnames(final, "region.y", "Continent")
setnames(final, "sub.region", "Sub.region")
colnames(final[,16:19])
# Change order of columns for further analysis
final <- final[, c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 9, 12, 13, 14, 15, 16, 18, 19, 17)]
data <- final
str(data)
filter(data, !is.na(Notes)) %>% select(Team, Notes) %>% unique()
# Remove Notes since they are not useful
data$Notes <- NULL
# Remove "Games" since it is perfectly reproducible from Year+Season
data$Games <- NULL
# Locate teams which have a name different than the country of origin
teams_different <- filter(data, Team != Country) %>% select(Team, Country, Year) %>% unique()
data$Team <- NULL
# Qui sotto vengono riportati i valori sia di NOC sia di Country che non hanno una relazione biunivoca
# Ovvero, se c'e' A -> B, non puo' esistere A -> C ne' C -> B, in linea di massima, pero' non e' cosi (e ci sta anche).
noc_country <- cbind(data$NOC, data$Country)
doppi_country.idx <- unique(noc_country)[,2] %>% duplicated()
doppi_noc.idx <- unique(noc_country)[,1] %>% duplicated()
a <- unique(noc_country)[doppi_country.idx,]
b <- unique(noc_country)[doppi_noc.idx,]
doppioni <- rbind(a, b)
filter(data[,c("NOC", "Country")], NOC %in% doppioni[,1] | Country %in% doppioni[,2]) %>% unique()
#TODO still not finished, how to deal with these? we want to only keep one of the two columns, preferably Country
# Yeah, we believe we won't lose too much information if we only keep Country...
noc_country_different <- filter(data[,c("NOC", "Country")], NOC %in% doppioni[,1] | Country %in% doppioni[,2]) %>% unique()
data$NOC <- NULL
# Order Year
data$Year <- as.ordered(data$Year)
# Not interested in the names, we keep ID
data$Name <- NULL
save(data, teams_different, noc_country_different, file = "input/complete.RData")
