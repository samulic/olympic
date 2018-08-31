load('input/data.rdata')
# this file contains every National Olympic Commitee with its ISO country code
countries <- read.csv("input/nazioni.csv", stringsAsFactors = TRUE, na.strings="")
# this file contains every country's location details (i.e. continent, subregion) identified by its ISO code
iso <- read.csv("input/countries.csv", stringsAsFactors = TRUE, na.strings="")
# get the list of unique NOC available in our dataset
comitati <- as.tibble(list(NOC = as.factor(df$NOC))) %>% unique() %>% arrange(NOC)

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

df$NOC <- plyr::mapvalues(df$NOC, 
                          from=c("AHO", "ANZ", "BOH", "CRT", "EUN", "FRG", "GDR", "IOA", "LIB", "MAL", "NBO", "NFL",
                                 "RHO", "SAA", "SCG", "TCH", "UAR", "URS", "VNM", "WIF", "YAR", "YMD", "YUG"), 
                          to = c("ANT", "AUS", "CZE", "GRE", "UKR", "GER", "GER", "IOA", "LBN", "MAS", "MAS", "CAN",
                                 "ZIM", "GER", "MNE", "CZE", "UAE", "RUS", "VIE", "JAM", "YEM", "YEM", "SRB"))

final <- left_join(df, distinct_noc_full, by = "NOC")

status <- df_status(final)
final$ISO <- NULL
require(data.table)
setnames(final, "region.x", "Country")
setnames(final, "notes", "Notes")
setnames(final, "region.y", "Continent")
setnames(final, "sub.region", "Sub.region")
colnames(final[,16:19])
final <- final[, c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 9, 12, 13, 14, 15, 16, 18, 19, 17)]
data <- final

save(data, file = "input/complete.RData")
