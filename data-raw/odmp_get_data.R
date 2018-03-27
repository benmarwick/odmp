#----------------------------------------------------------------------------------
library(tidyverse)
library(glue)
library(rvest)

years <- 1791:2018 # 228 years (or URLs to scrape)
odmp_urls <-
  glue('https://www.odmp.org/search/year?year={years}')

# get bio details

# get URLs to each bio
odmp_page_urls <-
  # scrape each page for each year, get URL to more details
  map(odmp_urls,
      ~read_html(.x) %>%
        html_nodes(".officer-short-details a") %>%
        html_attr('href')) %>%
  # convert to character vector
  unlist()

saveRDS(odmp_page_urls, "data-raw/odmp_page_urls.rds")

# get web pages from the bio page of each officer
# this is pretty slow...
odmp_page_bio_page  <-
  # scrape each page for each year, get URL to more details
  map(odmp_page_urls,
      ~read_html(.x))

saveRDS(odmp_page_bio_page, "data-raw/odmp_page_bio_page.rds")
# now from these pages, we can extract some info:

# 1. get the bio details
odmp_page_bio_page_bio <-
  map(odmp_page_bio_page,
      ~html_nodes(.x, ".officer-bio") %>%
        html_text()  )

# 2. get the names
odmp_page_bio_page_name <-
  map(odmp_page_bio_page,
      ~html_nodes(.x, ".officer-incident-description h3") %>%
        html_text() ) %>%
  unlist()

# 3. get the EOW (date), location and name with rank
odmp_page_bio_page_eow <-
  map(odmp_page_bio_page,
      ~html_nodes(.x, ".officer-eow") %>%
        html_text() ) %>%
  unlist()

# 4. get agency
odmp_page_bio_page_agency <-
  map(odmp_page_bio_page,
      ~html_nodes(.x, ".officer-agency") %>%
        html_text() ) %>%
  unlist()

# 5. get rank
odmp_page_bio_page_name_rank <-
  map(odmp_page_bio_page,
      ~html_nodes(.x, ".officer-short-details strong ") %>%
        html_text() ) %>%
  unlist()

# tidy this into cols

# convert to data frame for each officer
variables <- c("other5", "Age" , "Tour" , "Badge", "Veteran", "Incident details", "Cause", "Incident Date", "Weapon", "Offender", "Location", "other1", "other2", "other3", "other4")

odmp_page_bio_df <-
  odmp_page_bio_page_bio %>%
  # convert to character vector
  unlist() %>%
  # convert to data frame
  enframe() %>%
  # split 'value' column by line breaks into new cols
  separate(value, into = variables, sep = "\n")


# add the names, EOW, location, remove the dogs...
library(lubridate)
odmp_page_bio_df_names_eow_date <-
  odmp_page_bio_df %>%
  mutate(name = odmp_page_bio_page_name) %>%
  mutate(name_and_rank = odmp_page_bio_page_name_rank) %>%
  mutate(eow = str_replace(odmp_page_bio_page_eow, "End of Watch ", "")) %>%
  mutate(posix_date = as.Date(str_trim(eow), format= "%A, %B %e, %Y"),
         year = year(posix_date),
         month = month(posix_date),
         day = day(posix_date))  %>%
  filter(!str_detect(Age, "Breed"))

# The bio pages do not all have the same structure, so some of the
# fields are staggered across the wrong columns. We need to scan all
# columns for the values for each variable of interest. Fortunately,
# most of the interesting values have a prefix we can use to find them

# regex for each variable:
# Age
# Tour
# Badge
# Incident Details
# Cause
# Weapon
# Offender
# Location

# Find these wherever they are!
find_in_any_cell <-
  function(dfr, var){
    var <- tolower(var)
    # create dataframe to store output
    out <- as_data_frame(matrix(NA,
                                nrow = nrow(dfr),
                                ncol = ncol(dfr)))

    # search each column for rows that match the input
    for(i in 1:ncol(dfr)){
      dfr_this_col <- str_trim(tolower(pull(dfr[,i])))
      out[,i] <- ifelse(str_detect(dfr_this_col,
                                   glue('{var}.*')),
                        dfr_this_col, NA)
    }
    # drop cols that are all NA
    out <- Filter(function(x)!all(is.na(x)), out)
    return(out)

  }


# keywords we want to find in the df, this will make a list of
# one df for each keyword
keywords <- c("Age",
              "Tour",
              "Badge",
              "Incident Details",
              "Cause",
              "Weapon",
              "Offender",
              "lat")
dfs_with_keywords <- map(
  keywords,
  ~ find_in_any_cell(odmp_page_bio_df_names_eow_date, .x) %>%
    unite(col = "x") %>%
    mutate(x = str_replace_all(x, "_|NA", ""))
)
# bind them into a dataframe
keywords_df <- bind_cols(dfs_with_keywords)
names(keywords_df) <- keywords

# remove redundant text in the data
keywords_df_clean <-
  keywords_df %>%
  mutate_all(funs(str_replace_all(.,
                                  collapse(c(tolower(keywords),
                                             "not available"),
                                           "|"),
                                  ""))) %>%
  mutate_all(str_trim) %>%
  mutate(coords = str_replace_all(lat, '[[:alpha:]]|\\{|\\}|\\:|\\"', "")) %>%
  separate(coords, into = c("lat", "long"), ",") %>%
  mutate_at(vars(lat, long), as.numeric) %>%
  # get the names, dates, etc.  back on there
  mutate(name = odmp_page_bio_df_names_eow_date$name,
         name_and_rank = odmp_page_bio_df_names_eow_date$name_and_rank,
         eow = odmp_page_bio_df_names_eow_date$eow,
         posix_date = odmp_page_bio_df_names_eow_date$posix_date,
         year = odmp_page_bio_df_names_eow_date$year,
         month = odmp_page_bio_df_names_eow_date$month,
         day = odmp_page_bio_df_names_eow_date$day) %>%
  separate(name, into = c('first_name',
                          'other_name1',
                          'other_name2',
                          'other_name3'),
           sep = " ",
           remove = FALSE) %>%
  # years are limited for this fn, so
  mutate(year_rounded = ifelse(year < 1880, 1880,
                               ifelse(year > 2012, 2012,
                                      year)))


# get the gender of each officer, this isn't provided by the site,
# so we'll infer it from social security data
library(gender)

keywords_df_clean_gender <-
  keywords_df_clean %>%
  select(first_name,
         year)  %>%
  # years are limited for this fn, so
  mutate(year = ifelse(year < 1880, 1880,
                       ifelse(year > 2012, 2012,
                              year))) %>%
  gender_df(name_col = "first_name",
            year_col = "year") %>%
  mutate(first_name = name)

# join onto full table, since the gender fn only returns unique names
keywords_df_clean_gender_all_names <-
  keywords_df_clean %>%
  left_join(keywords_df_clean_gender,
            by = c('first_name' = 'first_name',
                   'year_rounded' ='year_min'))

odmp_age_gender_names_locations <-
  keywords_df_clean_gender_all_names %>%
  select(-`Incident Details`,
         -first_name,
         -other_name1,
         -other_name2,
         -other_name3,
         -year_rounded,
         -proportion_male,
         -proportion_female,
         -year_max) %>%
  mutate(rank = str_replace_all(name_and_rank, name.x, "")) %>%
  mutate(name = name.x) %>%
  select(-name.x,
         -name.y) %>%
  # age is a bit messy, get only the numbers
  mutate(Age = as.numeric(str_extract(Age, "\\d*")))

# get the state for each location

library(sp)
library(rgdal)
library(maps)
library(maptools)
latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))

  # Convert pointsDF to a SpatialPoints object
  pointsSP <- SpatialPoints(pointsDF,
                            proj4string=CRS("+proj=longlat +datum=WGS84"))

  # Use 'over' to get _indices_ of the Polygons object containing each point
  indices <- over(pointsSP, states_sp)

  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}


# get states from lat-longs, many rows with NAs are excluded

latlong_to_state <-
odmp_age_gender_names_locations %>%
  filter(!is.na(long),
         !is.na(lat)) %>%
  mutate(State = latlong2state(data.frame(x = long,
                                          y = lat)))

# join States back on to main data
odmp_age_gender_names_locations <-
  odmp_age_gender_names_locations %>%
  left_join(latlong_to_state)


write_csv(odmp_age_gender_names_locations,
          "data-raw/odmp_profile_page_data_1791_2018.csv")

odmp_1791_2018 <- odmp_age_gender_names_locations

usethis::use_data(odmp_1791_2018, overwrite = TRUE)


# end of data collection and cleaning ---


# check the locations


# quick look at a map
ggplot(odmp_age_gender_names_locations,
       aes(long, lat)) +
  geom_point() +
  coord_map()

library(mapview)
library(sf)

# make a spatial features object for mapping
keywords_df_clean_sf <-
  odmp_age_gender_names_locations %>%
  filter(!is.na(lat)) %>%
  filter(!is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"))

library(leaflet)

m <- leaflet(keywords_df_clean_sf) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(clusterOptions = markerClusterOptions(),
             popup = paste("Name:", keywords_df_clean_sf$name_and_rank, "<br>",
                           "Age:", keywords_df_clean_sf$Age, "<br>",
                           "Gender:", keywords_df_clean_sf$gender, "<br>",
                           "EOW:", keywords_df_clean_sf$eow, "<br>",
                           "Tour:", keywords_df_clean_sf$Tour, "<br>",
                           "Cause:", keywords_df_clean_sf$Cause, "<br>",
                           "Offender:", keywords_df_clean_sf$Offender))
m  # Print the map





#-------------------------------------------------------------------------
# some plots..

library(tidyverse)
theme_set(theme_minimal(base_size = 13))
odmp <- odmp_1791_2018

# how many per year?
n_per_year <-
  ggplot(odmp,
         aes(year)) +
  geom_bar() +
  geom_text(data = odmp %>% count(year, sort = TRUE) %>% slice(1),
            aes(year,
                n + 10,
                label = paste0("Maximum in ", year),
                hjust = 0)) +
  geom_vline(data = odmp %>% count(year, sort = TRUE) %>% slice(1),
                       aes(xintercept = year),
             colour = "red") +
  ylab("Police deaths per year")

# what ranks?
n_ranks <-
  odmp %>%
  group_by(rank) %>%
  tally(sort = TRUE) %>%
  filter(!is.na(rank)) %>%
  slice(1:10) %>%
  ggplot(aes(reorder(rank, n),
             n)) +
  geom_col()  +
  coord_flip() +
  xlab("")

# what cause?
n_cause <-
  odmp %>%
  group_by(Cause) %>%
  tally(sort = TRUE) %>%
  filter(!is.na(Cause)) %>%
  slice(1:20) %>%
  ggplot(aes(reorder(Cause, n),
             n)) +
  geom_col()  +
  coord_flip() +
  xlab("")

# where are the most and least?
library(statebins)
library(viridis)
n_where <-
  odmp %>%
  group_by(State) %>%
  tally() %>%
  mutate(State = tools::toTitleCase(State)) %>%
  filter(!is.na(State), State != "NA") %>%
  ggplot(aes(state=State,
             fill=n)) +
  geom_statebins(radius = unit(0.5, "npc")) +
  coord_equal() +
  scale_fill_viridis() +
  xlab(paste0("Total number of Police deaths by state, ",
              min(odmp$year, na.rm = TRUE),
              " - ",
              max(odmp$year, na.rm = TRUE))) +
  theme_statebins()

# proportion of deaths that are heart attacks over time
prop_heart_attack <-
  odmp %>%
  mutate(Cause = if_else(Cause == 'heart attack',
                         Cause,
                         'other')) %>%
  group_by(year,
           Cause) %>%
  tally() %>%
  mutate(prop = prop.table(n)) %>%
  filter(Cause == 'heart attack') %>%
  ggplot(aes(year,
             prop,
             group = 1)) +
  geom_point(size = 2) +
  ylab("Proportion of Police deaths\neach year due to heart attack")

# devtools::install_github("thomasp85/patchwork")
library(patchwork)
p_full <-
  ( n_ranks | n_cause | prop_heart_attack) /
  ( n_per_year | n_where )

wrap_elements(p_full) + ggtitle('Police Deaths in the US',
                                subtitle = "Data from https://www.odmp.org, code at https://github.como/benmarwick/odmp")





#----------------------------------------------------------------
#----------------------------------------------------------------
# An earlier method of scraping, not so good


variables <- c("empty", "Officer", "Location", "EOW", "Cause")

library(rvest)
odmp <-
  # scrape each page for each year
  map(odmp_urls,
      ~read_html(.x) %>%
        html_nodes(".officer-short-details") %>%
        html_text() ) %>%
  # convert to character vector
  unlist() %>%
  # convert to data frame
  enframe() %>%
  # split 'value' column by line breaks into new cols
  separate(value, into = variables, sep = "\n") %>%
  # remove unnecessary text
  mutate(EOW = str_replace_all(EOW, "EOW:", ""),
         Cause = str_replace_all(Cause, "Cause:", ""))

# separate a few variables out

remove_me <-  ",|Police Department|Department of Natural Resources|Fish|Game|Sheriff's Office|Office of Public Safety|Constable's Office|Department of Corrections|County Sheriff's Department|Sheriff's Police Department|Department of Correctional Services|Department of Corrections|Department of Police Services|Transportation Authority Police|Division of Police|Department of Wildlife and Fisheries|Highway Patrol|County Sheriff's Office|Parish Sheriff's Office|Parish Sheriff's Department|Department of Public Safety|School District Police Services|Department of Correction|Division of School Safety|State Police|Texas Highway Patrol|Texas Highway Patrol,|Department of Criminal Justice|Police Bureau|Metropolitan Police Department| Metropolitan Police Department, MO"

library(lubridate)
odmp_tidy <-
  odmp %>%
  mutate(State = str_extract(Location, "[A-Z]{2}$"),
         City = str_replace(Location, "[A-Z]{2}$", ""),
         City = str_replace(City, remove_me, ""),
         City = str_replace(City, " \\,|[[:punct:]]", ""),
         posix_date = as.Date(str_trim(EOW), format= "%A, %B %e, %Y"),
         year = year(posix_date),
         month = month(posix_date),
         day = day(posix_date)) %>%
  select(-empty) %>%
  mutate_all(funs(str_trim))

# get the rank of the officer

ranks <- c("officer|police|patrolman|deputy|sheriff|sergeant|detective|agent|policeman|constable|special|lieutenant|patrol|k9|corporal|captain|correctional|u.s.|inspector|private|town|warden|investigator|night|guard|corrections|assistant|reserve|senior|undersheriff|watchman|ranger|conservation|railroadkeeper|boatman|marshal|keeper|city marshal|jailer|posseman|collector|chief of|turnkey|deputized civilian|collector|roundsman|roundsman|principal|third|fourth|posse member|provost|custom house|high|paramedic|elect|acting chief|juvenile detention|juvenile security|man|trooper|federal|auxiliary|first|motorcycle|correction|traffic|prison|narcotics|probation|parole|prohibition|detention|superintendent|county|probationary|security|mounted|immigration|acting|supervisor|staff|operative|merchant|commissioner|criminal|dispatcher|dispensary|colonel|supervisory|technical|narcotic|game|park|class|employee|pilot|major|village|coast|director|district|field|public|revenue|motor|cadet")

odmp_tidy$Rank <-
  odmp_tidy$Officer%>%
  str_trim() %>%
  tolower() %>%
  str_split( boundary("word")) %>%
  map(., ~str_extract(.x, ranks)) %>%
  map(., ~paste0(.x, collapse = " ") %>%
        str_replace_all("NA", "") %>%
        str_trim()) %>%
  unlist()

# save
write_csv(odmp_tidy, "odmp_data_1791_2018.csv")
# read in
odmp_tidy <- read_csv("odmp_data_1791_2018.csv")

#---------------------------------------------

# quick look at a map
ggplot(odmp_age_gender_names_locations,
       aes(long, lat)) +
  geom_point() +
  coord_map()

library(mapview)
library(sf)

# make a spatial features object for mapping
keywords_df_clean_sf <-
  odmp_age_gender_names_locations %>%
  filter(!is.na(lat)) %>%
  filter(!is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"))

library(leaflet)

m <- leaflet(keywords_df_clean_sf) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(clusterOptions = markerClusterOptions(),
             popup = paste("Name:", keywords_df_clean_sf$name_and_rank, "<br>",
                           "Age:", keywords_df_clean_sf$Age, "<br>",
                           "Gender:", keywords_df_clean_sf$gender, "<br>",
                           "EOW:", keywords_df_clean_sf$eow, "<br>",
                           "Tour:", keywords_df_clean_sf$Tour, "<br>",
                           "Cause:", keywords_df_clean_sf$Cause, "<br>",
                           "Offender:", keywords_df_clean_sf$Offender))
m  # Print the map





#-------------------------------------------------------------------------
# some plots..

library(tidyverse)
theme_set(theme_minimal(base_size = 13))
odmp <- read_csv("odmp_data_1791_2018.csv")

# how many per year?
n_per_year <-
  ggplot(odmp,
         aes(year)) +
  geom_bar() +
  geom_text(data = odmp %>% count(year, sort = TRUE) %>% slice(1),
            aes(year,
                n + 10,
                label = paste0("Maximum in ", year))) +
  ylab("Police deaths per year")

# what ranks?
n_ranks <-
  odmp %>%
  group_by(Rank) %>%
  tally(sort = TRUE) %>%
  filter(!is.na(Rank)) %>%
  slice(1:10) %>%
  ggplot(aes(reorder(Rank, n),
             n)) +
  geom_col()  +
  coord_flip() +
  xlab("")

# what cause?
n_cause <-
  odmp %>%
  group_by(Cause) %>%
  tally(sort = TRUE) %>%
  filter(!is.na(Cause)) %>%
  slice(1:20) %>%
  ggplot(aes(reorder(Cause, n),
             n)) +
  geom_col()  +
  coord_flip() +
  xlab("")

# where are the most and least?
library(statebins)
library(viridis)
n_where <-
  odmp %>%
  group_by(State) %>%
  tally() %>%
  ggplot(aes(state=State,
             fill=n)) +
  geom_statebins(radius = unit(0.5, "npc")) +
  coord_equal() +
  scale_fill_viridis() +
  xlab(paste0("Total number of Police deaths, ",
              min(odmp$year, na.rm = TRUE),
              " - ",
              max(odmp$year, na.rm = TRUE))) +
  theme_statebins()

# proportion of deaths that are heart attacks over time
prop_heart_attack <-
  odmp %>%
  mutate(Cause = if_else(Cause == 'Heart attack',
                         Cause,
                         'other')) %>%
  group_by(year,
           Cause) %>%
  tally() %>%
  mutate(prop = prop.table(n)) %>%
  filter(Cause == 'Heart attack') %>%
  ggplot(aes(year,
             prop,
             group = 1)) +
  geom_point(size = 2) +
  ylab("Proportion of Police deaths\neach year due to heart attack")

# devtools::install_github("thomasp85/patchwork")
library(patchwork)
p_full <-
  ( n_ranks | n_cause | prop_heart_attack) /
  ( n_per_year | n_where )

wrap_elements(p_full) + ggtitle('Police Deaths in the US',
                                subtitle = "Data from https://www.odmp.org, code at https://gist.github.com/benmarwick/caae664abf667012531659cf05055673")

#------