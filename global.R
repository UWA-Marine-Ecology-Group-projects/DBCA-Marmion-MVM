### Libraries
library(gargle)
library(googlesheets4)
library(htmltools)
library(leafem)
library(leaflet)
library(leaflet.extras) # NEW
library(leafgl)
library(mongolite)
library(plyr)
library(purrr)
library(raster)
library(rgdal)
library(rgeos)
library(rrapply)
library(sf)
library(shiny)
library(shinyalert)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinydashboardPlus) # NEW
library(shinydisconnect)
library(shinyjs)
library(shinysurveys)
library(shinyFeedback)
library(devtools)
# devtools::install_github("szelepke/shinyRadioMatrix")
library(shinyRadioMatrix)
library(shinyTree)
library(shinyWidgets)
library(shinyvalidate) # NEW
library(stringr)
library(tidyr)
library(tibble)
library(rrapply)
library(stringi)
library(ggplot2)
library(dplyr) # load last to stop issues with plyr 

gs4_auth(cache = "secrets", email = TRUE)

# Mongo database ----
load("secrets/host.rda")
load("secrets/username.rda")
load("secrets/password.rda")

options(mongodb = list(
  "host" = host,
  "username" = username,
  "password" = password
))

databaseName <- "marmion"

## Leaflet spinner ----
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

## Read in activity list (downloaded from Googledrive) ----
activities <- read.csv("data/marmion_activitylist - activity-list.csv", na.strings=c("","NA"))

## Read in response scales ----
response.scales <- read.csv("data/Activity list - Response scales.csv", na.strings=c("","NA"))

## Read in questions (only using this in the matrix style so far) ----
questions <- read.csv("data/marmion_activitylist - questions.csv", na.strings=c("","NA")) %>%
  dplyr::select(section, question, type, response.items.for.matrix.only, scale.name, question.number)%>%
  mutate(question.number = as.character(question.number))

matrix.data <- questions %>%
  filter(type%in%c("matrix")) %>%
  dplyr::select(question.number,response.items.for.matrix.only)

# Group responses into a list for matrix
matrix.questions <- rrapply(cbind(matrix.data), how = "unmelt")

# Create a list of inputs for each activity selected ----
activity.list <- activities %>%
  dplyr::filter(!Category %in% c("Local knowledge")) %>%
  dplyr::mutate(Category = stringr::str_replace_all(.$Category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(Sub.category = stringr::str_replace_all(.$Sub.category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_","__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(Activity = stringr::str_replace_all(.$Activity, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  tidyr::replace_na(list(c(Category = "NA"))) %>%
  dplyr::mutate(input.suffix = paste(Category, Sub.category, Activity, sep = "__")) %>%
  dplyr::mutate(input.suffix = tolower(input.suffix)) %>%
  dplyr::mutate(days = paste("days__activity",input.suffix, sep = "__")) %>%
  dplyr::mutate(time = paste("time__activity",input.suffix, sep = "__")) %>%
  dplyr::mutate(description = paste("description__activity",input.suffix, sep = "__"))

days.inputs <- (unique(activity.list$days))
time.inputs <- (unique(activity.list$time))
description.inputs <- (unique(activity.list$description))

activity.input.list <- c(days.inputs, time.inputs, description.inputs)

# Create a list of inputs for each values selected ----
values.list <- activities %>%
  dplyr::filter(Category %in% c("Local knowledge")) %>%
  dplyr::mutate(Category = stringr::str_replace_all(.$Category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(Sub.category = stringr::str_replace_all(.$Sub.category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(input.suffix = paste(Category, Sub.category, sep = "__")) %>%
  dplyr::mutate(input.suffix = tolower(input.suffix)) %>%
  dplyr::mutate(description = paste("description__values",input.suffix, sep = "__"))
  
values.input.list <- (unique(values.list$description))

# Create a list of inputs for each pressure selected ----
pressures.list <- activities %>%
  dplyr::filter(Category %in% c("Pressures and threats")) %>%
  dplyr::mutate(Category = stringr::str_replace_all(.$Category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(Sub.category = stringr::str_replace_all(.$Sub.category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(Activity = stringr::str_replace_all(.$Activity, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(input.suffix = paste(Category, Sub.category, Activity, sep = "__")) %>%
  dplyr::mutate(input.suffix = tolower(input.suffix)) %>%
  dplyr::mutate(description = paste("description__pressures",input.suffix, sep = "__"))

pressures.input.list <- (unique(pressures.list$description))

# Create a list of the subcategories for the accordion ----
activity.acc <- activities %>%
  dplyr::mutate(nice.title = paste(Category, Sub.category,Activity, sep = " - "),
                nice.cat = Category,
                nice.sub = Sub.category,
                nice.act = Activity) %>%
  dplyr::filter(!Category %in% c("Local knowledge", "Other", "Pressures and threats")) %>%
  dplyr::mutate(Category = stringr::str_replace_all(.$Category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(Sub.category = stringr::str_replace_all(.$Sub.category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(Activity = stringr::str_replace_all(.$Activity, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(checkbox = paste("checkbox_", Category, "__", Sub.category, sep = "")) %>%
  glimpse()

other.acc <- activities %>%
  dplyr::mutate(nice.title = paste(Category, Sub.category, sep = " - "),
                nice.cat = Category,
                nice.act = Sub.category) %>%
  dplyr::filter(Category %in% c("Other")) %>%
  dplyr::mutate(Category = stringr::str_replace_all(.$Category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(Sub.category = stringr::str_replace_all(.$Sub.category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(checkbox = paste("checkbox_", Category, sep = "")) %>%
  glimpse()

values.acc <- activities %>%
  dplyr::mutate(nice.title = paste(Category, Sub.category, sep = " - "),
                nice.cat = Category,
                nice.act = Sub.category) %>%
  dplyr::filter(Category %in% c("Local knowledge")) %>%
  dplyr::mutate(Category = stringr::str_replace_all(.$Category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(Sub.category = stringr::str_replace_all(.$Sub.category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(checkbox = paste("checkbox_", Category, sep = "")) %>%
  glimpse()

pressures.acc <- activities %>%
  dplyr::mutate(nice.title = paste(Category, Sub.category, sep = " - "),
                nice.cat = Category,
                nice.sub = Sub.category,
                nice.act = Activity) %>%
  dplyr::filter(Category %in% c("Pressures and threats")) %>%
  dplyr::mutate(Category = stringr::str_replace_all(.$Category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(Sub.category = stringr::str_replace_all(.$Sub.category, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(Activity = stringr::str_replace_all(.$Activity, c("," = "", "[^[:alnum:]]" = "_", "___" = "_", "__" = "_", "\\_$" = ""))) %>%
  dplyr::mutate(checkbox = paste("checkbox_", Category, "__", Sub.category, sep = "")) %>%
  glimpse()

# grid.2km <- readOGR(dsn="spatial/Marmion_commonwealth grids_0.016 degrees_2 km.shp", layer="Marmion_commonwealth grids_0.016 degrees_2 km")
# save(grid.2km, file = "spatial/grid.2km.rda")

# grid.2km <- readOGR(dsn="spatial/Marmion_state grids_0.016 degrees_2 km.shp", layer="Marmion_state grids_0.016 degrees_2 km")
# save(grid.2km, file = "spatial/grid.2km.rda")

# grid.1km <- readOGR(dsn="spatial/Marmion_state grids_0.008 degrees_1 km.shp", layer="Marmion_state grids_0.008 degrees_1 km")
# save(grid.1km, file = "spatial/grid.1km.rda")

# bathy <- readOGR(dsn="spatial/bathy_cropped_single parts.shp", layer="bathy_cropped_single parts")
# save(bathy, file = "spatial/bathy.rda")



bathy <- readOGR(dsn="spatial/bathy_cropped_single parts_state only.shp", layer="bathy_cropped_single parts_state only")
save(bathy, file = "spatial/bathy.rda")

placenames <- st_read(dsn = "spatial/Marmion_PlaceNames.shp") %>%
  mutate(zoom.on = str_replace_all(.$zoom.on, "8", "10"))

labels <-  bind_cols(data.frame(st_coordinates(placenames[,1])), placenames) %>%
  filter(!X%in%c("NaN"))

# Zoom on at 12
# Zoom on at 10

on.10 <- labels %>%
  filter(zoom.on %in% c(10))

on.12 <- labels %>%
  filter(zoom.on %in% c(12))

# Read in spatial files ----
load("spatial/grid.1km.rda")
load("spatial/bathy.rda")

unique(bathy@data$LABEL)

bathy.pal <- colorFactor(palette = "Blues", 
                         levels = c("0-10m","10-20m","20-50m"))

bathy$LABEL <- factor(bathy$LABEL, 
                      levels = c("0-10m","10-20m","20-50m"))

# Grids for spatial questions ----
SpP <- SpatialPolygons(grid.1km@polygons)

SpP = SpatialPolygonsDataFrame(
  SpP,
  data = data.frame(ID = as.character(c(1:(
    length(SpP@polygons)
  ))),
  display = c(1:(
    length(SpP@polygons)
  ))),
  match.ID = FALSE
)

# Testing out navigation charts=
# r <- raster("spatial/cropped_raster.tif")

# rasterpal <- colorNumeric(c("#000000", "#666666", "#FFFFFF"), values(r),
#                     na.color = "transparent")

# m <- leaflet() %>%
#   addGeotiff(file = "spatial/cropped_raster.tif",
#              resolution = 250,
#              # pixelValuesToColorFn = myCustomJSFunc , 
#              colorOptions = colorOptions(
#                palette = c("#000000", "#666666", "#FFFFFF"),
#                na.color = "transparent"
#              )
#              )
# #   addRasterImage(x = r, colors = rasterpal)
# # 
# m

# which fields get saved ----
fieldsAll <- c("name", "email", "phone", "residence","postcode", "gender", "age", "origin", "traditionalowner", "generalcomment", "frequency", activity.input.list, values.input.list, pressures.input.list, "visited", "awaremarmionmarinepark", "fishinginsanctuary", "recreationinsanctuary")

# which fields are mandatory ----
fieldsMandatory <- c("name", "email", "phone", "residence", "gender", "age", "origin", "traditionalowner")

# CSS to use in the app ----
appCSS <-
  ".mandatory_star { color: red; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
  "

# FUNCTIONS ----
# Function to add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# Function to get a formatted string of the timestamp
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# Function to change the colour of polygons on leaflet map
change_color <- function(map, id_to_remove, data, colour, new_group){
  leafletProxy(map) %>%
    removeShape(id_to_remove) %>% # remove previous occurrence
    addPolygons(
      data = data,
      layerId = data$ID,
      group = new_group, # change group
      fillColor = colour,
      fillOpacity  = 0.5,
      weight = 1,
      color = "red",
      options = pathOptions(pane = "polygons"))
}

# Function to create a unique random ID to match datasets back up without using the users name ----
randomID <- function(n = 1000000) {
  a <- do.call(paste0, replicate(10, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%09d", sample(999999999, n, TRUE)), sample(LETTERS, n, TRUE))
}

saveData <- function(data, collection) {
  # Connect to the database
  db <- mongo(collection = collection,
              url = sprintf(
                "mongodb+srv://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName
              ),
              options = ssl_options(weak_cert_validation = TRUE))
  # Insert the data into the mongo collection as a data.frame
  # data <- as.data.frame(t(data))
  db$insert(data)
}

# Functions to create mouseover lat and lon for leaflet maps ----
clipboardDependency = function() {
  list(
    htmltools::htmlDependency(
      name = "clipboard",
      version = "0.0.1",
      src = system.file("htmlwidgets/lib/clipboard", package = "leafem"),
      script = "setClipboardText.js"
    )
  )
}

addmouselatlon <- function(map,
                                epsg = NULL,
                                proj4string = NULL,
                                native.crs = FALSE) {
  
  if (inherits(map, "mapview")) map <- mapview2leaflet(map)
  stopifnot(inherits(map, c("leaflet", "leaflet_proxy", "mapdeck")))
  
  if (native.crs) { 
    txt_detailed <- paste0("
                           ' x: ' + (e.latlng.lng).toFixed(5) +
                           ' | y: ' + (e.latlng.lat).toFixed(5) +
                           ' | epsg: ", epsg, " ' +
                           ' | proj4: ", proj4string, " ' +
                           ' | zoom: ' + map.getZoom() + ' '")
  } else {
    txt_detailed <- paste0("
                            'Latitude: ' + ((e.latlng.lat).toFixed(5)) + 
                            ' | Longitude: ' + (e.latlng.lng).toFixed(5) +
                           ' (Decimal Degrees)'")
  }
  
  
  txt_basic <- paste0("
                      'Latitude: ' + [0|(e.latlng.lat)] + 
                      '° ' + 
                      [0|((e.latlng.lat)<0?(e.latlng.lat)=-(e.latlng.lat):(e.latlng.lat))%1*60] +
                      ' ' + 
                      ((e.latlng.lat)*60%1*60).toFixed(3) +
                      ' S | ' +
                      
                      'Longitude: ' + [0|(e.latlng.lng)] + 
                      '° ' + 
                      [0|((e.latlng.lng)<0?(e.latlng.lng)=-(e.latlng.lng):(e.latlng.lng))%1*60] +
                      ' ' + 
                      ((e.latlng.lng)*60%1*60).toFixed(3) +
                      ' E (Degrees Minutes Seconds)'
                      
")
  
  map$dependencies = c(
    map$dependencies,
    clipboardDependency()
  )
  
  map <- htmlwidgets::onRender(
    map,
    paste0(
      "
      function(el, x, data) {
      // get the leaflet map
      var map = this; //HTMLWidgets.find('#' + el.id);
      // we need a new div element because we have to handle
      // the mouseover output separately
      // debugger;
      function addElement () {
      // generate new div Element
      var newDiv = $(document.createElement('div'));
      // append at end of leaflet htmlwidget container
      $(el).append(newDiv);
      //provide ID and style
      newDiv.addClass('lnlt');
      newDiv.css({
      'position': 'relative',
      'bottomleft':  '0px',
      'background-color': 'rgba(255, 255, 255, 0.7)',
      'box-shadow': '0 0 2px #bbb',
      'background-clip': 'padding-box',
      'margin': '0',
      'padding-left': '5px',
      'color': '#333',
      'font': '12px/1.5 \"Helvetica Neue\", Arial, Helvetica, sans-serif',
      'z-index': '700',
      });
      return newDiv;
      }
      // check for already existing lnlt class to not duplicate
      var lnlt = $(el).find('.lnlt');
      if(!lnlt.length) {
      lnlt = addElement();
      // grab the special div we generated in the beginning
      // and put the mousmove output there
      map.on('mousemove', function (e) {
      if (e.originalEvent.ctrlKey) {
      if (document.querySelector('.lnlt') === null) lnlt = addElement();
      lnlt.text(", txt_detailed, ");
      } else {
      if (document.querySelector('.lnlt') === null) lnlt = addElement();
      lnlt.text(", txt_basic, ");
      }
      });
      // remove the lnlt div when mouse leaves map
      map.on('mouseout', function (e) {
      var strip = document.querySelector('.lnlt');
      if( strip !==null) strip.remove();
      });
      };
      //$(el).keypress(67, function(e) {
      map.on('preclick', function(e) {
      if (e.originalEvent.ctrlKey) {
      if (document.querySelector('.lnlt') === null) lnlt = addElement();
      lnlt.text(", txt_basic, ");
      var txt = document.querySelector('.lnlt').textContent;
      console.log(txt);
      //txt.innerText.focus();
      //txt.select();
      setClipboardText('\"' + txt + '\"');
      }
      });
      }
      "
    )
  )
  map
}

# Function to validate email address ----
isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), 
        ignore.case=TRUE)
}

mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}
