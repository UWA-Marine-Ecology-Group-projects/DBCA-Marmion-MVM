function(request) {
  dashboardPage(
    dashboardHeader(titleWidth = "0px",
      tags$li(class = "dropdown",
              # ifelse('input.is_mobile_device == "TRUE"', 
              #        a(href="https://www.dbca.wa.gov.au/", target="_blank", 
              #          img(height = "40%", 
              #              src="dbca_logo_white_cropped.png")
              #        ),
              #        a(href="https://www.dbca.wa.gov.au/", target="_blank", 
              #          img(height = "50px", 
              #              src="dbca_logo_white.png")
              #        ))
              div(style="text-align: right; margin-bottom: 10px; margin-top: 10px; margin-right: 10px;", 
              a(href="https://www.dbca.wa.gov.au/", target="_blank",
                     img(width = "60%", 
                         src="dbca_logo_white_cropped.png") # 50% if not cropped
              ))
      )),
    dashboardSidebar(width = "0px"),
    dashboardBody(
      tags$head(includeHTML(("google-analytics.html"))),
      tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
      tags$head(tags$style(HTML(
        '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Source Sans Pro",sans-serif;
        padding: -0 10px;
        overflow: hidden;
        color: white;
      }
    '))),

      tags$head(HTML('<script type="text/javascript" src="https://webapiv2.navionics.com/dist/webapi/webapi.min.no-dep.js"></script>
<link rel="stylesheet" href="https://webapiv2.navionics.com/dist/webapi/webapi.min.css" >')),
      
      tags$body(HTML('<body data-root="https://webapiv2.navionics.com/dist/webapi/images" >')),

      tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass" margin-top: 10px;> <b>Marine Values Mapper </b> </span>\');
      })
     ')),
      
      shinyjs::useShinyjs(), # Have to put here in dashboard SEE https://cran.r-project.org/web/packages/shinyjs/vignettes/shinyjs-usage.html
      useShinyalert(), # To show shiny alerts
      shinyjs::inlineCSS(appCSS), # for mandatory star
      
      disconnectMessage(
        text = "An error occurred.",
        refresh = "",
        background = "#FFFFFF",
        colour = "#444444",
        refreshColour = "#337AB7",
        overlayColour = "#000000",
        overlayOpacity = 0.6,
        width = "full",
        top = "center",
        size = 22,
        css = ""
      ),
                fluidRow(
                 
                  tabBox(title = " ",
                         tags$head(tags$style(HTML('
 .nav-tabs-custom>.nav-tabs>li.active>a {
display: none;
 }
 
 .skin-blue .main-header .navbar .sidebar-toggle {
 display: none;
}

label {
  margin-left: 30px;
  margin-right: 30px;
}

.form-control {
  margin-left: 30px;
  margin-right: 30px;
}

.help-block {
  margin-left: 30px;
  margin-right: 30px;
}

h1 {
  margin-left: 30px;
  margin-right: 30px;
  margin-bottom: 10px;
  font-size: 22px;
}

h2 {
  margin-left: 30px;
  margin-right: 30px;
  margin-bottom: 10px;
  font-size: 18px;
}

h3 {
  margin-left: 60px;
  margin-right: 30px;
  margin-bottom: 10px;
  font-size: 16px;
}

.h4, h4 {
  font-size: 14px;
  margin-left: 30px;
  margin-right: 30px;
  margin-bottom: 15px;
}

.h5, h5 {
  font-size: 18px;
}

.h6, h6 {
  font-size: 16px;
}

.leaflet-container {
margin-left: 30px;
margin-right: 30px;
}

.box-group accordion shiny-bound-input {
margin-left: 30px;
margin-right: 30px;
}

p {
    margin: 0 0 10px;
    margin-left: 30px;
    margin-right: 30px;
}

.progress-group {
    margin-left: 30px;
    margin-right: 30px;
}

.bttn-unite.bttn-lg {
    padding: 10px 50px;
    margin-right: 30px;
    margin-left: 30px;
    margin-top: 10px;
}

.bttn-unite.bttn-md {
    padding: 10px 30px;
}

.skin-blue sidebar-mini{
display: none;
}

td {
    width: 100px;
}

.form-group shiny-radiomatrix-container dataTable-container shiny-bound-input {
    width: 90%;
    margin-left: 5%;
    margin-right: 5%;
}

.shiny-radiomatrix-row {
    width: 90%;
    margin-left: 5%;
    margin-right: 5%;
}

.shiny-radiomatrix {
    width: 90%;
    margin-left: 5%;
    margin-right: 5%;
}

.modal-dialog {
    border-radius: 20px;
}

.modal-body {
    border-radius: 20px;
}

.modal-content {
    border-radius: 20px;
}

.skin-blue sidebar-mini {
    padding-right: 0px;
}

html, body, .test_map_div {
    margin: 0;
    width: 100%;
    height: 100%;
}

'))),
                         id = "surveybox",
                         height = "100%", width = "100%",
                         tabPanel("Contact information", 
                                  
                                  h1("Please fill out your contact information:"),
                                  
                                  mobileDetect('isMobile'),
                                  
                                  h4(strong("Full name:"), labelMandatory("")),
                                  textInput(width = '94%', "name", NULL),
                                  
                                  h4(strong("Email:"), labelMandatory("")),
                                  textInput(width = '94%', "email", NULL),
                                  
                                  h4(strong("Phone number:"), labelMandatory("")),
                                  textInput(width = '94%', "phone", NULL),

                                  
                                  # DEMOGRAPHICS ----
                                  h2("Demographics:"),
                                  h4(strong("Usual place of residence:"), labelMandatory("")),
                                  radioButtons("residence", label = NULL,
                                               choices = c("Australia",
                                                           "Overseas"),
                                               selected = character(0)),
                                  
                                  conditionalPanel('input.residence == "Australia"',
                                                   h4(strong("What is your home postcode?"), labelMandatory("")),
                                                   textInput(width = '100%', "postcode", label = NULL)
                                  ),
                                  
                                  h4(strong("What is your gender?"), labelMandatory("")),
                                  radioButtons("gender", label = NULL,
                                               choices = c("Male",
                                                           "Female",
                                                           "Other"),
                                               selected = character(0)),
                                  
                                  h4(strong("To which age group do you belong?"), labelMandatory(""), "(parent assistance required for children under 13, parent permission required for children ages 14-17)"),
                                  radioButtons("age", label = NULL,
                                               choices = c(
                                                 "5-13",
                                                 "14-17",
                                                 "18-24",
                                                 "25-34",
                                                 "35-44",
                                                 "45-54",
                                                 "55-64",
                                                 "65-74",
                                                 "75+"),
                                               selected = character(0)),
                                  
                                  h4(strong("Are you of Aboriginal or Torres Strait Islander origin?"), labelMandatory("")),
                                  checkboxGroupInput("origin", label = NULL,
                                                     choices = c("No",
                                                                 "Yes, Aboriginal",
                                                                 "Yes, Torres Strait Islander"),
                                                     selected = character(0)),
                                  
                                  conditionalPanel('input.origin.includes("Yes, Aboriginal")',
                                                   h4(strong("Do you identify as a Traditional Owner of the region between Trigg and Two Rocks?"), labelMandatory("")),
                                                   radioButtons("traditionalowner", label = NULL,
                                                                choices = c("Yes",
                                                                            "No"),
                                                                selected = character(0))
                                  ),
                                  
                                  h4(strong("Roughly how often do you visit the coastal areas adjacent to Trigg, Ocean Reef, Yanchep and Two Rocks (e.g., visit a beach or go out on a boat)?"), labelMandatory("")),
                                  radioButtons("frequency", label = NULL,
                                               choices = c("Haven't ever visited",
                                                           "Once every 5+ years",
                                                           "Once every 3 to 5 years",
                                                           "Once every 1 to 2 years",
                                                           "Once a year",
                                                           "2 to 5 times a year",
                                                           "More than 5 times a year",
                                                           "On a weekly basis"),
                                               selected = character(0)),
                                  
                                  div(style="display:inline-block;width:100%;text-align: center;", 
                                  
                                  div(
                                    column(1,offset=10, actionBttn(
                                    inputId = "nextactivities",
                                    label = "Next",
                                    style = "unite",
                                    icon = icon("chevron-right"),
                                    color = "primary"
                                  )))
                         )),
                         tabPanel("Values mapping",
                                  h1("Values mapping"),
                                  h2("Which activities would you like to map?"),
                                  h3("Note: To maximise performance (e.g., in areas with slow internet connection) please consider only selecting up to 10 activities at a time. If you have more than 10 activities, you may repeat the survey to enter your remaining information"),
                                  # Activity 1 ----
                                  accordion(id = "id-accordiona1",
                                            accordionItem(
                                              id = "accordionact1",
                                              title =  unique(filter(activity.acc, cat_num %in% c("1"))$nice.cat),
                                              solidHeader = TRUE, status = "primary",
                                              do.call(accordion, c(list(id = "id-accordion1"), 
                                                                   lapply(seq_along(unique(filter(activity.acc, cat_num %in% c("1"))$Sub.category)), function(x){
                                                                     accordionItem(
                                                                       title = paste0(unique(filter(activity.acc, cat_num %in% c("1"))$nice.sub)[x]),
                                                                       checkboxGroupInput(paste0("checkbox_",
                                                                                                 unique(activity.acc$Category)[1], "__",
                                                                                                 unique(filter(activity.acc, cat_num %in% c("1"))$Sub.category)[x]),
                                                                                          label = NULL,
                                                                                          choices = (filter(activity.acc, cat_num == "1" & sub_num ==x)$nice.act),
                                                                                          selected = character(0))
                                                                     )
                                                                   })))
                                            )),
                                  
                                  # Activity 2 ----
                                  accordion(id = "id-accordiona2",
                                            accordionItem(
                                              id = "accordionact2",
                                              title =  unique(filter(activity.acc, cat_num %in% c("2"))$nice.cat),
                                              solidHeader = TRUE, status = "primary",
                                              do.call(accordion, c(list(id = "id-accordion2"), 
                                                                   lapply(seq_along(unique(filter(activity.acc, cat_num %in% c("2"))$Sub.category)), function(x){
                                                                     accordionItem(
                                                                       title = paste0(unique(filter(activity.acc, cat_num %in% c("2"))$nice.sub)[x]),
                                                                       checkboxGroupInput(paste0("checkbox_",
                                                                                                 unique(activity.acc$Category)[2], "__",
                                                                                                 unique(filter(activity.acc, cat_num %in% c("2"))$Sub.category)[x]),
                                                                                          label = NULL,
                                                                                          choices = (filter(activity.acc, cat_num == "2" & sub_num ==x)$nice.act),
                                                                                          selected = character(0))
                                                                     )
                                                                   })))
                                            )),
                                  
                                  # Activity 3 ----
                                  accordion(id = "id-accordiona3",
                                            accordionItem(
                                              id = "accordionact3",
                                              title =  unique(filter(activity.acc, cat_num %in% c("3"))$nice.cat),
                                              solidHeader = TRUE, status = "primary",
                                              do.call(accordion, c(list(id = "id-accordion3"), 
                                                                   lapply(seq_along(unique(filter(activity.acc, cat_num %in% c("3"))$Sub.category)), function(x){
                                                                     accordionItem(
                                                                       title = paste0(unique(filter(activity.acc, cat_num %in% c("3"))$nice.sub)[x]),
                                                                       checkboxGroupInput(paste0("checkbox_",
                                                                                                 unique(activity.acc$Category)[3], "__",
                                                                                                 unique(filter(activity.acc, cat_num %in% c("3"))$Sub.category)[x]),
                                                                                          label = NULL,
                                                                                          choices = (filter(activity.acc, cat_num == "3" & sub_num ==x)$nice.act),
                                                                                          selected = character(0))
                                                                     )
                                                                   })))
                                            )),
                                  
                                  # Activity 4 ----
                                  accordion(id = "id-accordiona4",
                                            accordionItem(
                                              id = "accordionact4",
                                              title =  unique(filter(activity.acc, cat_num %in% c("4"))$nice.cat),
                                              solidHeader = TRUE, status = "primary",
                                              do.call(accordion, c(list(id = "id-accordion4"), 
                                                                   lapply(seq_along(unique(filter(activity.acc, cat_num %in% c("4"))$Sub.category)), function(x){
                                                                     accordionItem(
                                                                       title = paste0(unique(filter(activity.acc, cat_num %in% c("4"))$nice.sub)[x]),
                                                                       checkboxGroupInput(paste0("checkbox_",
                                                                                                 unique(activity.acc$Category)[4], "__",
                                                                                                 unique(filter(activity.acc, cat_num %in% c("4"))$Sub.category)[x]),
                                                                                          label = NULL,
                                                                                          choices = (filter(activity.acc, cat_num == "4" & sub_num ==x)$nice.act),
                                                                                          selected = character(0))
                                                                     )
                                                                   })))
                                            )),
                                  
                                  # Activity 5 ----
                                  accordion(id = "id-accordiona5",
                                            accordionItem(
                                              id = "accordionact5",
                                              title =  unique(filter(activity.acc, cat_num %in% c("5"))$nice.cat),
                                              solidHeader = TRUE, status = "primary",
                                              do.call(accordion, c(list(id = "id-accordion5"), 
                                                                   lapply(seq_along(unique(filter(activity.acc, cat_num %in% c("5"))$Sub.category)), function(x){
                                                                     accordionItem(
                                                                       title = paste0(unique(filter(activity.acc, cat_num %in% c("5"))$nice.sub)[x]),
                                                                       checkboxGroupInput(paste0("checkbox_",
                                                                                                 unique(activity.acc$Category)[5], "__",
                                                                                                 unique(filter(activity.acc, cat_num %in% c("5"))$Sub.category)[x]),
                                                                                          label = NULL,
                                                                                          choices = (filter(activity.acc, cat_num == "5" & sub_num ==x)$nice.act),
                                                                                          selected = character(0))
                                                                     )
                                                                   })))
                                            )),
                                  
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  
                                  # Values ----
                                  h2("Which local knowledge topics would you like to map?"),
                                  
                                  accordion(id = "id-accordionv1",
                                            accordionItem(
                                              id = "accordionvalues",
                                              title = "Conservation/ecological information (you will be asked to map these)", #unique(values.acc$nice.cat),
                                              solidHeader = TRUE, status = "primary",
                                              checkboxGroupInput(paste0("checkbox_",
                                                                        unique(values.acc$Category)),
                                                                 label = NULL,
                                                                 choices = unique(values.acc$nice.act),
                                                                 selected = character(0))
                                            )),
                                  
                                  accordion(id = "id-accordionp1",
                                            accordionItem(
                                              id = "accordionpressures",
                                              title =  "Environmental pressures (you will be asked to map these)",
                                              solidHeader = TRUE, status = "primary",
                                              do.call(accordion, c(list(id = "id-accordionpressures1"),
                                                                   lapply(seq_along(unique(filter(pressures.acc, cat_num %in% c("8"))$Sub.category)), function(x){
                                                                     accordionItem(
                                                                       title = paste0(unique(filter(pressures.acc, cat_num %in% c("8"))$nice.sub)[x]),
                                                                       checkboxGroupInput(paste0("checkbox_Pressures_and_threats__",
                                                                                                 unique(filter(pressures.acc, cat_num %in% c("8"))$Sub.category)[x]),
                                                                                          label = NULL,
                                                                                          choices = (filter(pressures.acc, cat_num == "8" & sub_num ==x)$nice.act),
                                                                                          selected = character(0))
                                                                     )
                                                                   })))
                                            )),
                                  
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  
                                  # Other activities ----
                                  h2("Use an “other” option if you would like to map anything not listed above. You will be asked to describe the activity or knowledge on the next page."),
                                  accordion(id = "id-accordiono1",
                                            accordionItem(
                                              id = "accordionother",
                                              title = unique(other.acc$nice.cat),
                                              solidHeader = TRUE, status = "primary",
                                              checkboxGroupInput(paste0("checkbox_",
                                                                        unique(other.acc$Category)),
                                                                 label = NULL,
                                                                 choices = unique(other.acc$nice.act),
                                                                 selected = character(0))
                                            )),
                                  br(),
                                  br(),
                                  br(),
                                  div(style="display:inline-block;width:100%;text-align: center;", 
                                      
                                      div(
                                        column(1,offset=1, actionBttn(
                                    inputId = "backcontact",
                                    label = "Back",
                                    style = "unite",
                                    icon = icon("chevron-left"),
                                    color = "primary"
                                  )), 
                                  column(1,offset = 8, actionBttn(
                                    inputId = "nextspatial",
                                    label = "Next",
                                    style = "unite",
                                    icon = icon("chevron-right"),
                                    color = "primary"
                                  )))
                         )
                         ),
                         
                         tabPanel("Spatial questions",
                                  h1("Map your use and/or local knowledge"),
                                  h2("There are separate maps for each activity and knowledge topic you selected"),
                                  uiOutput("activity_plots"),
                                  uiOutput("values_plots"),
                                  uiOutput("pressures_plots"),
                                  div(style="display:inline-block;width:100%;text-align: center;", 
                                  div(
                                    column(1,offset = 10, actionBttn(
                                        inputId = "nextnonspatial",
                                        label = "Next",
                                        style = "unite",
                                        icon = icon("chevron-right"),
                                        color = "primary"
                                      ))))
                         ),
                         
                         tabPanel("Social values and benefits",
                                  h1("Social values and benefits"),
                                  br(),
                                  h2(
                                    "Please indicate on the scale below your level of agreement with each of the following statements. If you are unsure please leave it blank."),
                                  
                                  h3(em(
                                    "The current level of protection and management of marine areas between Trigg and Two Rocks is sufficient to guarantee conservation of marine ecosystems")),
                                  shinyRadioMatrix::radioMatrixInput(
                                    inputId = "rm9",
                                    rowIDsName = paste(" "),
                                    rowIDs = paste(" "),
                                    rowLLabels = c("Strongly disagree"),
                                    rowRLabels = c("Strongly agree"),
                                    labelsWidth = list("10%", "10%"),
                                    choices = c("1", "2", "3", "4", "5", "6", "7")
                                  ),
                                  br(),
                                  
                                  h3(em(
                                    "The marine areas between Trigg and Two Rocks provide", strong("me"), "with the following", strong("opportunities or benefits."), "Leave blank if you are unsure." )
                                  ),
                                  
                                  shinyRadioMatrix::radioMatrixInput(
                                    inputId = "rm10",
                                    rowIDsName = paste("                                                                                     "),
                                    rowIDs = c(unique(
                                      filter(matrix.data, question.number %in% c("10"))$response.items.for.matrix.only
                                    )),
                                    rowLLabels = c(rep(
                                      "Strongly disagree", length(unique(
                                        filter(matrix.data, question.number %in% c("10"))$response.items.for.matrix.only
                                      ))
                                    )),
                                    rowRLabels = c(rep("Strongly agree", length(
                                      unique(
                                        filter(matrix.data, question.number %in% c("10"))$response.items.for.matrix.only
                                      )
                                    ))),
                                    choices = c(unique(
                                      filter(response.scales, scale.name %in% c("agree"))$response.options
                                    )),
                                    labelsWidth = list('0%', '0%')
                                  ),
                                  
                                  br(),
                                  
                                  h3(em("The marine areas between Trigg and Two Rocks provide the following", strong("benefits to society."), "Leave blank if you are unsure." )
                                  ),
                                  shinyRadioMatrix::radioMatrixInput(
                                    inputId = "rm11",
                                    rowIDsName = paste("                                                                                     "),
                                    rowIDs = c(unique(
                                      filter(matrix.data, question.number %in% c("11"))$response.items.for.matrix.only
                                    )),
                                    rowLLabels = c(rep(
                                      "Strongly disagree", length(unique(
                                        filter(matrix.data, question.number %in% c("11"))$response.items.for.matrix.only
                                      ))
                                    )),
                                    rowRLabels = c(rep("Strongly agree", length(
                                      unique(
                                        filter(matrix.data, question.number %in% c("11"))$response.items.for.matrix.only
                                      )
                                    ))),
                                    choices = c(unique(
                                      filter(response.scales, scale.name %in% c("agree"))$response.options
                                    )),
                                    labelsWidth = list('0%', '0%')
                                  ),
                                  
                                  br(),
                                  div(style="margin-left: 30px;",
                                  h4(strong("Have you ever visited a marine park in Western Australia?"), labelMandatory("")),
                                  div(style="margin-left: 30px;",
                                  radioButtons(
                                    "visited",
                                    label = NULL,
                                    choices = c("Yes",
                                                "No",
                                                "Unsure"),
                                    selected = character(0)
                                  ))),
                                  br(),
                                  
                                  # awaremarmionmarinepark, fishinginsanctuary, recreationinsanctuary
                                  
                                  div(style="margin-left: 30px;",
                                      h4(strong("Were you aware that Marmion is a Marine Park?"), labelMandatory("")),
                                      div(style="margin-left: 30px;",
                                          radioButtons(
                                            "awaremarmionmarinepark",
                                            label = NULL,
                                            choices = c("Yes",
                                                        "No",
                                                        "Unsure"),
                                            selected = character(0)
                                          ))),
                                  br(),
                                  
                                  div(style="margin-left: 30px;",
                                      h4(strong("Is recreation fishing permitted in the sanctuary zones?"), labelMandatory("")),
                                      div(style="margin-left: 30px;",
                                          radioButtons(
                                            "fishinginsanctuary",
                                            label = NULL,
                                            choices = c("Yes",
                                                        "No",
                                                        "Unsure"),
                                            selected = character(0)
                                          ))),
                                  br(),
                                  
                                  div(style="margin-left: 30px;",
                                      h4(strong("Are boating, diving and snorkelling permitted in the sanctuary zones?"), labelMandatory("")),
                                      div(style="margin-left: 30px;",
                                          radioButtons(
                                            "recreationinsanctuary",
                                            label = NULL,
                                            choices = c("Yes",
                                                        "No",
                                                        "Unsure"),
                                            selected = character(0)
                                          ))),
                                  br(),
                                  
                                  shinyRadioMatrix::radioMatrixInput(
                                    inputId = "rm12",
                                    rowIDsName = " ",
                                    rowIDs = c(unique(
                                      filter(matrix.data, question.number %in% c("12"))$response.items.for.matrix.only
                                    )),
                                    rowLLabels = c(" "),
                                    choices = c(unique(
                                      filter(response.scales, scale.name %in% c("awareness"))$response.options
                                    )),
                                    labelsWidth = list("10%", "10%")
                                  ),
                                  
                                  br(),
                                  
                                  textAreaInput(
                                    "generalcomment",
                                    width = "94%",
                                    label = "Please provide any additional comments about values for the marine areas between Trigg and Two Rocks that have not already been covered in previous questions:",
                                    placeholder = NULL,
                                    height = "200px"
                                  ),
                                  
                                  div(style="display:inline-block;width:100%;text-align: center;", 
                                      div(column(1,offset=1, actionBttn(
                                        inputId = "backspatial",
                                        label = "Back",
                                        style = "unite",
                                        icon = icon("chevron-left"),
                                        color = "primary"
                                      )), 
                                      column(1,offset=8,uiOutput("submit"))
                                      )))),
                  progressBar(id = "progress", value=0, total=100, display_pct = FALSE, title = "Progress")
                )
        )
      )
      
    # )
  # )
}