#### Churches Geolocation Final Dataset ####

rm(list=ls())
cat('\014')
gc()

our_packages<- c("readxl","ggplot2", "sf", "tmap", "raster","units", "e1071", 
                 "googleway", "rgeos", "spatstat", "maptools", "rgdal", 
                 "spdep", "maps", "RColorBrewer", "readr","dplyr", "tidyr", "ggmap",
                 "maps", "fs", "readtext", "data.table", "stringr", "openai", "fuzzyjoin",
                 "httr", "geomander", "tinytiger", "tidycensus", "lubridate", "googledrive", 
                 "tokenizers", "quanteda", "caret", "MLmetrics")


for (i in our_packages) {
  if (!require(i, character.only = TRUE)) {
    print(paste(i, "needs to be installed!"))
    install.packages(i, dependencies = TRUE)
    library(i, character.only = TRUE)
  } else {
    print(paste0(i, " [", packageVersion(i), "] is already installed and loaded!"))
  }
}

# load the main dataset 
October302024_SermonsInfo <- readRDS("~/Dropbox/Sermons 2.0/Main Dataset/October302024_SermonsInfo.rds")
df <- October302024_SermonsInfo 

# find the unique churches 
length(unique(df$UniqueID))
cat("There are", length(unique(df$UniqueID)), "churches in the final sample of transcribed sermons")

# search for address/geolocation within the folders 
main_folder <- "/Users/nelamrchkovska/Library/CloudStorage/Dropbox/Sermons/States"
shp_files <- list.files(path = main_folder, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
shp_files <- shp_files[-559] # remving the us county shp file 
shp_data_list <- lapply(shp_files, st_read)

### Wrangling data so I can rbind all of the congregations ###

# Remove the specified column from elements 74 to 117
column_to_remove <- "nr_cnty"

for (i in 74:117) {
  if (column_to_remove %in% colnames(shp_data_list[[i]])) {
    shp_data_list[[i]] <- shp_data_list[[i]][ , !(colnames(shp_data_list[[i]]) %in% column_to_remove)]
  }
}

# Names and default values for the new columns
new_columns <- c("county", "cnty_ln", "GEOID")
default_values <- list(NA, NA, NA)  # Set default values as needed

# Add the new columns to elements 14 through 29
for (i in 14:29) {
  for (j in seq_along(new_columns)) {
    shp_data_list[[i]][[new_columns[j]]] <- default_values[[j]]
  }
}

# Use the first element as the reference for column names and order
reference_columns <- colnames(shp_data_list[[1]])

# Ensure all elements have the same columns as the first element
for (i in 1:length(shp_data_list)) {
  missing_columns <- setdiff(reference_columns, colnames(shp_data_list[[i]]))
  shp_data_list[[i]][, missing_columns] <- NA
  
  shp_data_list[[i]] <- shp_data_list[[i]][ , reference_columns, drop = FALSE]
}

all_shp_data <- do.call(rbind, shp_data_list)

#### SAVING ALL CONGREGATIONS INFO ####
output_shp_file <-  "/Users/nelamrchkovska/Library/CloudStorage/Dropbox/Sermons 2.0/Main Dataset/October302024_AllCongregationsInfo.shp"
#st_write(all_shp_data, output_shp_file, delete_dsn = TRUE) 


# let's see if the unique churches in my main dataset can be matched with this whole dataset 
youtube_congregations <- unique(df$UniqueID)

all_shp_data <- all_shp_data %>%
  distinct(placeid, .keep_all = TRUE)

all_shp_data <- all_shp_data %>%
  mutate(youtube_congregation=ifelse(placeid %in% youtube_congregations, 1, 0))
table(all_shp_data$youtube_congregation) #3777

youtube_congregations <- all_shp_data[all_shp_data$youtube_congregation==1,]
youtube_congregations <- youtube_congregations %>%
  select(-"youtube_congregation") %>%
  rename("UniqueID"="placeid")

#### SAVING YOUTUBE CONGREGATIONS INFO ####
output_shp_file <-  "/Users/nelamrchkovska/Library/CloudStorage/Dropbox/Sermons 2.0/Main Dataset/October302024_YouTubeCongregationsInfo.shp"
#st_write(youtube_congregations, output_shp_file, delete_dsn = TRUE) 

#### Adding Demographics and Voting Data to Congregations ####

#load spatial block-level data 

block <- st_read("/Users/nelamrchkovska/Library/CloudStorage/Dropbox/Sermons/Counties/Block-level geographic data/nhgis0006_shapefile_tl2021_us_blck_grp_2021/US_blck_grp_2021.shp")

names(block)
block <- block[block$STATEFP=="04" | block$STATEFP=="06" | block$STATEFP=="08" | block$STATEFP=="16" | block$STATEFP=="17" | 
                 block$STATEFP=="22" | block$STATEFP=="29" | block$STATEFP=="35" | block$STATEFP=="36" | block$STATEFP=="37" | 
                 block$STATEFP=="39" | block$STATEFP=="41" | block$STATEFP=="42" | block$STATEFP=="48" | block$STATEFP=="49" | 
                 block$STATEFP=="50" |  block$STATEFP=="53" |  block$STATEFP=="54",]

length(unique(block$STATEFP)) #18 states

# matching the crs of block data and youtube congregations
youtube_congregations_sf <- sf::st_transform(youtube_congregations, crs = st_crs(block))

# create a 10 mile radius around each congregation
buffer_radius <- 10 * 1609.34

points_sf_buffered <- st_buffer(youtube_congregations_sf, dist = buffer_radius)

# intersect with block data to see which blocks are within the congregations 10 miles radius 

intersection_result <- st_intersection(points_sf_buffered, block)
names(intersection_result)
length(unique(intersection_result$UniqueID)) 

# load precint voting data 

# Directory and shapefile loading
main_directory <- "/Users/nelamrchkovska/Library/CloudStorage/Dropbox/Sermons/Counties/Precinct-level voting data/"
folders <- list.dirs(main_directory, full.names = TRUE, recursive = FALSE)
folders <- folders[-7]

load_shapefiles <- function(folders) {
  shapefiles <- list()
  for (folder in folders) {
    shp_files <- list.files(folder, pattern = "\\.shp$", full.names = TRUE)
    for (shp_file in shp_files) {
      data_name <- gsub(".shp", "", basename(shp_file))
      shapefiles[[data_name]] <- st_read(shp_file)
    }
  }
  return(shapefiles)
}

shapefiles <- load_shapefiles(folders)

# Processing each state
states <- list(
  wa = shapefiles$wa_2020,
  az = shapefiles$az_2020,
  co = shapefiles$co_2020,
  ca = shapefiles$ca_2020,
  il = shapefiles$il_2020,
  la = shapefiles$la_2020,
  oh = shapefiles$oh_2020,
  tx = shapefiles$tx_2020,
  vt = shapefiles$vt_2020,
  id = shapefiles$ca_2020,
  mo = shapefiles$mo_2020,
  nc = shapefiles$nc_2020,
  nm = shapefiles$nm_2020,
  ny = shapefiles$ny_2020,
  or = shapefiles$or_2020,
  pa = shapefiles$pa_2020,
  ut = shapefiles$ut_2020,
  wv = shapefiles$wv_2020
)

states <- lapply(states, st_make_valid)

process_voting_data <- function(voting_data, points_sf_buffered, id) {
  voting_data <- voting_data %>%
    mutate(across(contains("G20PRE"), ~ as.numeric(parse_number(as.character(.))), .names = "numeric_{col}"))
  
  voting_df <- as.data.frame(voting_data)
  voting_df$pres_votes_tot <- rowSums(voting_df %>% select(starts_with("numeric_G20PRE")), na.rm = TRUE)
  
  voting_data$pres_votes_tot <- voting_df$pres_votes_tot
  
  voting_data <- voting_data %>%
    mutate(rep_vote = numeric_G20PRERTRU / pres_votes_tot,
           dem_vote = numeric_G20PREDBID / pres_votes_tot) %>%
    select(geometry, pres_votes_tot, rep_vote, dem_vote)
  
  points_sf_buffered <- st_transform(points_sf_buffered, crs = st_crs(voting_data))
  sf_use_s2(FALSE)
  intersection_result <- st_intersection(points_sf_buffered, voting_data)
  
  community_voting <- intersection_result %>%
    as.data.frame() %>%
    group_by(UniqueID) %>%
    summarise(comm_dem = mean(dem_vote, na.rm = TRUE),
              comm_rep = mean(rep_vote, na.rm = TRUE),
              comm_pres = mean(pres_votes_tot, na.rm = TRUE))
  
  assign(paste0("community_voting_", id), community_voting, envir = .GlobalEnv)
  return(community_voting)
}

results <- lapply(names(states), function(state_id) {
  process_voting_data(states[[state_id]], points_sf_buffered, state_id)
})

voting_data <- do.call(rbind, results)
voting_data <- voting_data %>%
  distinct(UniqueID, .keep_all = TRUE)


# merge youtube_congregations with voting_data 

test <- left_join(youtube_congregations, voting_data, by="UniqueID")
test <- test %>%
  distinct(UniqueID, .keep_all = TRUE)
test <- test[!is.na(test$address),]
nrow(test[is.na(test$comm_rep),]) #139 
test_na <- test[is.na(test$comm_rep),]

# checking what is up with idaho 
id_voting <- id_2020 %>%
  select(contains("G20PRE"), "geometry")
id_voting <- id_voting %>%
  rowwise() %>%
  mutate(pres_votes_tot = sum(c_across(contains("G20PRE")), na.rm = TRUE))

id_voting <- id_voting %>%
  mutate(rep_vote=G20PRERTRU/pres_votes_tot,
         dem_vote=G20PREDBID/pres_votes_tot)
id_voting <- st_make_valid(id_voting)

id_voting <- id_voting %>%
  select("geometry", "pres_votes_tot", "rep_vote", "dem_vote")
names(id_voting)
class(id_voting)

str(id_voting)
class(id_voting)
st_crs(id_voting)==st_crs(points_sf_buffered)
points_sf_buffered <- st_transform(points_sf_buffered, crs = st_crs(id_voting))
sf_use_s2(FALSE)
intersection_result_voting <- st_intersection(points_sf_buffered, id_voting)

names(intersection_result_voting)
length(unique(intersection_result_voting$UniqueID)) #264 congregations 

intersection_result_voting <- as.data.frame(intersection_result_voting)
names(intersection_result_voting)

community_voting_id <- intersection_result_voting %>%
  group_by(UniqueID) %>%
  summarise(comm_dem=mean(dem_vote, na.rm=TRUE),
            comm_rep=mean(rep_vote, na.rm=TRUE),
            comm_pres=mean(pres_votes_tot, na.rm=TRUE))


join <- inner_join(test_na, community_voting_id, by="UniqueID")
names(join) <- gsub("\\.x$", "", names(join))
join$comm_dem <- join$comm_dem.y
join$comm_rep <- join$comm_rep.y
join$comm_pres <- join$comm_pres.y
join <- join %>%
  select(-c("comm_dem.y", "comm_rep.y", "comm_pres.y"))
voting_congs <- rbind(test, join)
voting_congs <- voting_congs %>%
  distinct(UniqueID, .keep_all = TRUE)
class(voting_congs)

# now merging census data 

# load census data 

#census <- read_csv("~/Dropbox/Sermons/Counties/Census block-level data/nhgis0005_ds254_20215_blck_grp.csv")
census <- read_csv("~/Dropbox/Sermons/Counties/Census block-level data/nhgis0009_csv/nhgis0009_ds262_20225_blck_grp.csv")
states <- c("Arizona", "California", "Colorado", "Idaho", "Illinois", "Louisiana", "Missouri", "New Mexico", "New York",
            "North Carolina", "Ohio", "Oregon", "Pennsylvania", "Texas", "Utah", "Vermont", "Washington", "West Virginia")
census <- census[census$STATE %in% states,]

names(census) #GISJOIN and GEOID
census_sub <- census %>%
  select("GISJOIN", "STUSAB", "STATE","STATEA", "COUNTY", "COUNTYA", "TRACTA", "BLKGRPA", "GEO_ID", "TL_GEO_ID",
         "AQM4E001", "AQM4E002", "AQM4E026", "AQNGE001", "AQNGE002", "AQNGE003", "AQPKE001", "AQPKE019", "AQPKE020", 
         "AQPKE021", "AQPKE022", "AQPKE023", "AQPKE024","AQPKE025",  "AQP6E001", "AQRAE001","AQNGE004", "AQNGE005",
        "AQNGE006","AQNGE007")

census_sub <- census_sub %>%
  rename("total_gender"="AQM4E001", 
         "male"="AQM4E002",
         "female"="AQM4E026",
         #"total_pop" ="AQNGE001",
         "total_race"="AQNGE001",
         "white"="AQNGE002",
         "black"="AQNGE003",
         "native"="AQNGE004",
         "asian"="AQNGE005",
         "pacific"="AQNGE006",
         "other_race"="AQNGE007",
         "total_educ"="AQPKE001",
         "college_below1year"="AQPKE019",
         "college_nodegree"="AQPKE020",
         "associate"="AQPKE021",
         "bachelor"="AQPKE022",
         "master"="AQPKE023",
         "professional"="AQPKE024",
         "doctorate"="AQPKE025",
         "median_income"="AQP6E001",
         "capita_income"="AQRAE001"
  )

census_sub <- census_sub %>%
  mutate(male_avg=male/total_gender,
         female_avg=female/total_gender, 
         educ_avg=(associate+bachelor+master+professional+doctorate)/total_educ,
         white_avg=white/total_race, 
         black_avg=black/total_race,
         native_avg=native/total_race,
         pacific_avg=pacific/total_race,
         asian_avg=asian/total_race,
         other_race_avg=other_race/total_race)

names(census_sub)
points_sf_buffered <- st_buffer(youtube_congregations_sf, dist = buffer_radius)

# intersect with block data to see which blocks are within the congregations 10 miles radius 

intersection_result <- st_intersection(points_sf_buffered, block)
names(intersection_result)
length(unique(intersection_result$UniqueID)) 

merged_census <- left_join(intersection_result, census_sub, by = "GISJOIN")
unique(merged_census$state)

community_demos <- merged_census %>%
  group_by(UniqueID) %>%
  summarise(comm_male=mean(male_avg, na.rm=TRUE),
            comm_female=mean(female_avg, na.rm=TRUE),
            comm_income_median=mean(median_income, na.rm=TRUE),
            comm_income_cap=mean(capita_income, na.rm=TRUE),
            comm_black=mean(black_avg, na.rm=TRUE),
            comm_white=mean(white_avg, na.rm=TRUE),
            comm_asian=mean(asian_avg, na.rm=TRUE),
            comm_other_races=mean(other_race_avg, na.rm=TRUE),
            comm_pacific=mean(pacific_avg, na.rm=TRUE),
            comm_native=mean(native_avg, na.rm=TRUE),
            comm_educ=mean(educ_avg, na.rm=TRUE)) 


community_demos <- st_transform(community_demos, st_crs(voting_congs))
census_voting <- st_join(voting_congs, community_demos, by="UniqueID")
census_voting <- census_voting %>%
  distinct(UniqueID.x, .keep_all = TRUE)
names(census_voting) <- gsub("\\.x$", "", names(census_voting))

#### Getting Denominations from ARDA ####

arda <- read_excel("/Users/nelamrchkovska/Library/CloudStorage/Dropbox/Sermons/ARDA Data/PENN7748841B.xlsx")

arda <- rename(arda, "state"="SECONDARY STATE")
arda <- arda %>%
  mutate(state = case_when(
    state == "NC" ~ "North Carolina",
    state == "TX" ~ "Texas",
    state == "CA" ~ "California",
    state == "AZ" ~ "Arizona",
    state == "CO" ~ "Colorado",
    state == "IL" ~ "Illinois",
    state == "LA" ~ "Louisiana",
    state == "NY" ~ "New York",
    state == "OH" ~ "Ohio",
    state == "WV" ~ "West Virginia",
    state == "PA" ~ "Pennsylvania",
    state == "VT" ~ "Vermont",
    state == "WA" ~ "Washington",
    state == "ID" ~ "Idaho",
    state == "MO" ~ "Missouri",
    state == "NM" ~ "New Mexico",
    state == "OR" ~ "Oregon",
    state == "UT" ~ "Utah",
    state == "WV" ~ "West Virginia",
    TRUE ~ state
  ))

# renaming important variables for merging 
arda <- arda[arda$state %in% states,]
arda <- rename(arda, "street"="SECONDARY ADDRESS")
arda <- rename(arda, "phn_nmb"="TELEPHONE")
arda <- rename(arda, "city"="SECONDARY CITY")
arda <- rename(arda, "zipcode"="SECONDARY ZIP CODE")
arda <- rename(arda, "name"="COMPANY NAME")
arda$phn_nmb <- gsub("[-() ]", "", arda$phn_nmb)
head(arda$state)
head(arda$street)
arda$street <- gsub(" ", "", arda$street)
head(arda$zipcode)
head(arda$phn_nmb)
head(arda$name)
head(arda$street)

congs_final <- census_voting

head(congs_final$city)
congs_final$name1 <- toupper(congs_final$name)
congs_final$name1 <- gsub(" ", "", congs_final$name1)
congs_final$city <- str_extract(congs_final$address, "(?<=,\\s)[^,]+(?=,\\s[A-Z]{2}\\s\\d{5})")
congs_final$city <- toupper(congs_final$city)
congs_final$city <- gsub(" ", "", congs_final$city)
congs_final <- congs_final[!congs_final$city=="NA",]
congs_final$state <- gsub(" ", "", congs_final$state)
congs_final$cityname <- paste(congs_final$state, congs_final$city, congs_final$name1, sep="_")
congs_final$street <- str_extract(congs_final$address, "^[^,]+")
congs_final$street <- toupper(congs_final$street)
congs_final$street <- gsub(" ", "", congs_final$street)
congs_final$citystreet <- paste(congs_final$city, congs_final$street, sep="_")


df_test <- congs_final %>%
  group_by(state) %>%
  filter(duplicated(citystreet) == FALSE) %>%
  filter(!duplicated(citystreet, fromLast = TRUE)) %>%
  ungroup()


head(arda$city)
arda$name <- toupper(arda$name)
arda$name <- gsub(" ", "", arda$name)
arda$city <- gsub(" ", "", arda$city)
arda$state <- gsub(" ", "", arda$state)
arda$cityname <- paste(arda$state, arda$city, arda$name, sep="_")
arda$citystreet <- paste(arda$city, arda$street, sep="_")

arda_test <- arda %>%
  group_by(state) %>%
  filter(duplicated(citystreet) == FALSE) %>%
  filter(!duplicated(citystreet, fromLast = TRUE)) %>%
  ungroup()

class(df_test)
df_test <- as.data.frame(df_test)

merge1 <- left_join(df_test, arda_test, by="citystreet")
merge1 <- merge1[!is.na(merge1$`DENOMINATION DESCRIPTION`),]

#### FIRST ROUND MATCHING (straight matching with street name) RESULT ###
cat("Matched:", nrow(merge1), "\n", 
    "Out of unique", nrow(df_test), "church observations", "\n",
    "which is", round((nrow(merge1)) / nrow(df_test) * 100, 2))

# left congs to match 
cong_left1 <- anti_join(df_test, merge1, by="citystreet") 
cat(nrow(cong_left1), "left to match")


#### SECOND ROUND MATCHING (straight matching with cityname) ###
merge2 <- left_join(cong_left1, arda_test, by="cityname")
merge2 <- merge2[!is.na(merge2$`DENOMINATION DESCRIPTION`),]
cat("Matched:", nrow(merge2), "\n", 
    "Out of unique", nrow(cong_left1), "church observations", "\n",
    "which is", round((nrow(merge2)) / nrow(cong_left1) * 100, 2))

# left congs to match 
cong_left2 <- anti_join(cong_left1, merge2, by="cityname") 
cat(nrow(cong_left2), "left to match")

merge1_fuzzy_jw1 <- stringdist_inner_join(cong_left2, arda_test, by=c("cityname"="cityname"), 
                                          method="jw", max_dist=0.1, distance_col='dist')

result1 <- merge1_fuzzy_jw1 %>%
  group_by(state.x, city.x) %>%
  filter(dist == min(dist))

result1_check <- result1 %>%
  select("cityname.x", "cityname.y", "dist")

names(merge1)
merge1$citystreet.y <- NA
names(merge2)
merge2$cityname.y <- NA
new_order <- c(1:54, 56, 55)
merge2 <- merge2[, new_order]
#merge2 <- merge2 %>%
  #select(-c("citystreet.y.1"))
names(result1)
result1 <- result1 %>%
  select(-"dist")

# renaming variables to merge observations that are in all matched datasets 

setdiff(names(merge1), names(merge2))  
setdiff(names(result1), names(merge1)) 

all.equal(names(merge1), names(merge2))
#merge1 <- rename(merge1, "citystreet.x"="citystreet")
merge2 <- rename(merge2, "cityname.x"="cityname")
all.equal(names(merge1), names(result1))

# rbinding all observations together 
cong_matched <- rbind(merge1, merge2, result1) #2065

names(cong_matched)
cong_matched <- cong_matched %>%
  select(1:26, "GROUP DESCRIPTION", "DENOMINATION DESCRIPTION")
cong_matched <- cong_matched %>%
  rename("group"= "GROUP DESCRIPTION", 
         "denomination" = "DENOMINATION DESCRIPTION")
cong_matched_tojoin <- cong_matched %>%
  select(UniqueID, group, denomination)

congregations <- left_join(census_voting, cong_matched_tojoin, by="UniqueID")
congregations <- congregations %>%
  distinct(UniqueID, .keep_all = TRUE) %>%
  select(-"UniqueID.y")

#### ARDA religious traditions by hand ####

reltrad.df <- read_csv("~/Dropbox/Sermons 2.0/Racial Justice/Special_Issue_Sociological_Focus/arda_reltrad.csv")
reltrad.df <- reltrad.df %>%
  mutate(denom = str_to_upper(Group))


# Cleaning denom by hand
reltrad.df <- reltrad.df %>%
  mutate(denom_clean = denom) %>%
  mutate(denom_clean = if_else(denom_clean=="AMERICAN ASSOCIATION OF LUTHERAN CHURCHES", "AMERICAN ASSOCIATION OF LUTHERAN CHURCH", denom_clean),
         denom_clean = if_else(denom_clean=="AMERICAN BAPTIST CHURCHES IN THE U.S.A.", "AMERICAN BAPTIST CHURCHES/USA", denom_clean),
         denom_clean = if_else(denom_clean=="ANGLICAN CHURCH IN NORTH AMERICA", "ANGLICAN EPISCOPAL OF NORTH AMERICA", denom_clean),
         denom_clean = if_else(denom_clean=="APOSTOLIC CHRISTIAN CHURCH (NAZARENE)", "APOSTOLIC CHRISTIAN (NAZAREAN)", denom_clean),
         denom_clean = if_else(denom_clean=="APOSTOLIC FAITH (KANSAS)", "APOSTOLIC FAITH CHURCH", denom_clean),
         denom_clean = if_else(denom_clean=="ARMENIAN APOSTOLIC CHURCH (CATHOLICOSATE OF CILICIA)", "ARMENIAN APOSTOLIC CHURCH", denom_clean),
         denom_clean = if_else(denom_clean=="ASSEMBLIES OF GOD INTERNATIONAL FELLOWSHIP", "ASSEMBLIES OF GOD", denom_clean),
         denom_clean = if_else(denom_clean=="BAPTIST BIBLE FELLOWSHIP INTERNATIONAL", "BAPTIST BIBLE FELLOWSHIP", denom_clean),
         denom_clean = if_else(denom_clean=="BRETHREN IN CHRIST CHURCH", "BRETHREN IN CHRIST", denom_clean),
         denom_clean = if_else(denom_clean=="CHRISTIAN CHURCHES AND CHURCHES OF CHRIST", "CHRISTIAN & CHURCHES OF CHRIST", denom_clean),
         denom_clean = if_else(denom_clean=="CHRISTIAN AND MISSIONARY ALLIANCE", "CHRISTIAN & MISSIONARY ALLIANCE", denom_clean),
         denom_clean = if_else(denom_clean=="CHRISTIAN REFORMED CHURCH IN NORTH AMERICA", "CHRISTIAN REFORMED CHURCH", denom_clean),
         denom_clean = if_else(denom_clean=="CHURCH OF GOD (ANDERSON, INDIANA)", "CHURCH OF GOD (ANDERSON, IN)", denom_clean),
         denom_clean = if_else(denom_clean=="CHURCH OF GOD (CLEVELAND, TENNESSEE)", "CHURCH OF GOD (CLEVELAND, TN)", denom_clean),
         denom_clean = if_else(denom_clean=="CHURCHES OF GOD, GENERAL CONFERENCE", "CHURCH OF GOD GENERAL CONFERENCE", denom_clean),
         denom_clean = if_else(denom_clean=="CHURCH OF JESUS CHRIST OF LATTER-DAY SAINTS", "CHURCH OF JESUS CHRIST-LATTER DAY SAINTS", denom_clean),
         denom_clean = if_else(denom_clean=="CHURCH OF THE LUTHERAN BRETHREN OF AMERICA", "CHURCH OF THE LUTHERAN BRETHREN", denom_clean),
         denom_clean = if_else(denom_clean=="CONSERVATIVE BAPTIST ASSOCIATION OF AMERICA (CBAMERICA)", "CONSERVATIVE BAPTIST ASSOCIATION", denom_clean),
         denom_clean = if_else(denom_clean=="CONSERVATIVE CONGREGATIONAL CHRISTIAN CONFERENCE", "CONSERVATIVE CONGREGATION CHRISTIAN CONFERENCE", denom_clean),
         denom_clean = if_else(denom_clean=="EVANGELICAL FREE CHURCH OF AMERICA", "EVANGELICAL FREE CHURCH", denom_clean),
         denom_clean = if_else(denom_clean=="EVANGELICAL PRESBYTERIAN CHURCH (1981)", "EVANGELICAL PRESBYTERIAN CHURCH", denom_clean),
         denom_clean = if_else(denom_clean=="FELLOWSHIP OF GRACE BRETHREN CHURCHES", "FELLOWSHIP OF GRACE BRETHREN", denom_clean),
         denom_clean = if_else(denom_clean=="FULL GOSPEL FELLOWSHIP OF CHURCHES AND MINISTERS INTERNATIONAL", "FULL GOSPEL FELLOWSHIP", denom_clean),
         denom_clean = if_else(denom_clean=="GENERAL ASSOCIATION OF REGULAR BAPTIST CHURCHES", "GENERAL ASSOC. OF REGULAR BAPTISTS", denom_clean),
         denom_clean = if_else(denom_clean=="GREEK ORTHODOX ARCHDIOCESE OF AMERICA", "GREEK ORTHODOX CHURCH", denom_clean),
         denom_clean = if_else(denom_clean=="PRESBYTERIAN CHURCH (U.S.A.)", "PRESBYTERIAN CHURCH (USA)", denom_clean),
         denom_clean = if_else(denom_clean=="OPEN BIBLE CHURCHES", "OPEN BIBLE STANDARD CHURCHES", denom_clean),
         denom_clean = if_else(denom_clean=="NATIONAL BAPTIST CONVENTION, USA, INC.", "NATIONAL BAPTIST CONVENTION-USA", denom_clean),
         denom_clean = if_else(denom_clean=="NATIONAL BAPTIST CONVENTION OF AMERICA INTERNATIONAL, INC.", "NATIONAL BAPTIST CONVENTION OF AMERICA", denom_clean),
         denom_clean = if_else(denom_clean=="PRIMITIVE METHODIST CHURCH IN THE U.S.A.", "PRIMITIVE METHODIST CHURCH", denom_clean),
         denom_clean = if_else(denom_clean=="SEVENTH-DAY ADVENTIST CHURCH", "SEVENTH DAY ADVENTISTS", denom_clean),
         denom_clean = if_else(denom_clean=="PROTESTANT REFORMED CHURCHES IN AMERICA", "PROTESTANT REFORMED CHURCHES", denom_clean),
         denom_clean = if_else(denom_clean=="LUTHERAN CHURCH-MISSOURI SYNOD (LCMS)", "LUTHERAN CHURCH - MISSOURI SYNOD", denom_clean),
         denom_clean = if_else(denom_clean=="MENNONITE CHURCH, USA", "MENNONITE CHURCH USA", denom_clean),
         denom_clean = if_else(denom_clean=="NON-DENOMINATIONAL CHRISTIAN CHURCHES", "NON-DENOMINATIONAL CHURCHES", denom_clean),
         denom_clean = if_else(denom_clean=="INDEPENDENT FUNDAMENTAL CHURCHES OF AMERICA", "INDEPENDENT FUNDAMENTAL CHURCHES", denom_clean),
         denom_clean = if_else(denom_clean=="WISCONSIN EVANGELICAL LUTHERAN SYNOD", "WISCONSIN EVANGELICAL LUTHERAN", denom_clean),
         denom_clean = if_else(denom_clean=="UNITED PENTECOSTAL CHURCH INTERNATIONAL", "UNITED PENTECOSTAL CHURCH", denom_clean),
         denom_clean = if_else(denom_clean=="CATHOLIC CHURCH", "ROMAN CATHOLIC CHURCH", denom_clean),
         denom_clean = if_else(denom_clean=="FREE METHODIST CHURCH – USA", "FREE METHODIST CHURCH OF NORTH AMERICA", denom_clean),
         denom_clean = if_else(denom_clean=="INTERNATIONAL COUNCIL OF COMMUNITY CHURCHES", "INTERNATIONAL COUNCIL COMMUNITY CHURCHES", denom_clean),
         denom_clean = if_else(denom_clean=="MISSIONARY CHURCH, THE", "MISSIONARY CHURCH", denom_clean))



congregations_test <- congregations %>%
  mutate(denom = denomination,
         denom_upper = toupper(denomination)) %>%
  select(denom, denom_upper) %>%
  group_by(denom) %>%
  slice(1) %>%
  ungroup()

denoms.reltrad <- congregations_test %>%
  left_join(reltrad.df, join_by(denom==denom_clean), relationship = "one-to-one")


denoms.reltrad <- denoms.reltrad %>%
  mutate(reltrad = Tradition) %>%
  mutate(reltrad = if_else(denom=="ANGLICAN - OTHER", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="BAPTIST - OTHER", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="BRETHREN - OTHER", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="CHRISTIAN - OTHER", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="CHURCH OF GOD (ORIGINAL)", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="CHURCH OF GOD - OTHER", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="CHURCH ON THE ROCK", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="BAPTIST BIBLE FELLOWSHIP", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="BAPTIST GENERAL CONFERENCE", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="BIBLE PRESBYTERIAN CHURCH", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="CHURCHES OF CHRIST (NON-INSTRUMENTAL)", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="CONGREGATIONAL CHRISTIAN CHURCHES", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="COOPERATIVE BAPTIST FELLOWSHIP", "Mainline Protestant", reltrad),
         reltrad = if_else(denom=="EVANGELICAL - OTHER", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="EVANGELICAL CHRISTIAN CHURCH", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="FREE WILL BAPTISTS", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="FULL GOSPEL FELLOWSHIP", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="INDEPENDENT BAPTIST CHURCHES", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="INDEPENDENT BIBLE CHURCHES", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="INTERNATIONAL FOURSQUARE GOSPEL", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="JEWISH - OTHER", "Jewish", reltrad),
         reltrad = if_else(denom=="LUTHERAN - OTHER", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="METHODIST - OTHER", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="METROPOLITAN COMMUNITY CHURCHES", "Mainline Protestant", reltrad),
         reltrad = if_else(denom=="NON CLASSIFIED AFFILIATION", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="PENTECOSTAL - OTHER", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="PRESBYTERIAN/REFORMED - OTHER", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="SEVENTH DAY CHURCH OF GOD", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="UNITED/UNION CHURCHES", "Evangelical Protestant", reltrad),
         reltrad = if_else(denom=="UNITY SCHOOL OF CHRISTIANITY", "Other Christians", reltrad))

denoms.reltrad <- denoms.reltrad %>%
  mutate(reltrad = if_else(reltrad=="Jewish" | reltrad=="Latter-day Saints" | reltrad=="Orthodox" | reltrad=="Other Christians", "Other", reltrad)) %>%
  mutate(reltrad_factor = as.factor(reltrad)) %>%
  select(denom,denom_upper,reltrad_factor)

congregations_test <- as.data.frame(congregations_test)
class(congregations_test)
sermons.merged <- congregations_test %>%
  left_join(denoms.reltrad, join_by(denom_upper==denom_upper), relationship = "many-to-one")
names(sermons.merged)
sermons.merged <- rename(sermons.merged, "denomination"="denom.x")
sermons.merged <- sermons.merged %>%
  select("denomination", "reltrad_factor")
names(congregations)

congregations <- left_join(congregations, sermons.merged)
class(congregations)
congregations <- congregations %>%
  mutate(
    reltrad_factor = as.character(reltrad_factor),
    reltrad_factor = case_when(
      denomination == "CHURCH OF GOD (HOLINESS)" ~ "Evangelical Protestant",
      denomination == "APOSTOLIC CHURCHES - OTHER" ~ "Other",
      denomination == "COMMUNITY OF CHRIST (RLDS)" ~ "Other",
      denomination == "UKRANIAN ORTHODOX CHURCH" ~ "Other",
      denomination == "CHRISTIAN SCIENCE CHURCHES" ~ "Other",
      denomination == "SEVENTH DAY BAPTIST GEN. CONFERENCE" ~ "Evangelical Protestant",
      denomination == "CHURCHES OF GOD (FINDLAY, OH)" ~ "Evangelical Protestant",
      denomination == "WORD CHURCHES" ~ "Evangelical Protestant",
      denomination == "UNITED BRETHREN IN CHRIST" ~ "Evangelical Protestant",
      denomination == "MORAVIAN CHURCH IN AMERICA" ~ "Mainline Protestant",
      TRUE ~ reltrad_factor  # Keeps the original value if none of the conditions are met
    )
  )

# check for missing observations 
na_counts_vars <- sapply(congregations, function(x) sum(is.na(x)))
print("NA counts by variable:")
print(na_counts_vars)

write_rds(congregations, "/Users/nelamrchkovska/Library/CloudStorage/Dropbox/Sermons 2.0/Main Dataset/October312024_CongregationsCommunityInfo.rds")


output_path <- "/Users/nelamrchkovska/Library/CloudStorage/Dropbox/Sermons 2.0/Main Dataset/October312024_CongregationsCommunityInfo.shp"
st_write(congregations, output_path)


# subsetting congregations that need denomination coded 

denominations_needed <- October312024_CongregationsCommunityInfo[is.na(October312024_CongregationsCommunityInfo$denomination),]
class(denominations_needed)
denominations_needed <- denominations_needed %>%
  select("UniqueID", "name", "address", "website", "phn_nmb",
         "denomination")
denominations_needed <- as.data.frame(denominations_needed) %>%
  select(-c(geometry))
write_csv(denominations_needed, "/Users/nelamrchkovska/Library/CloudStorage/Dropbox/Sermons 2.0/Main Dataset/November152024_denominations_missing.csv")






