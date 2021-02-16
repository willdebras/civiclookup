#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


js <- '
$(document).on("keyup", function(e) {
  if(e.keyCode == 13){
    Shiny.onInputChange("keyPressed", Math.random());
  }
});
'


library(shiny)
library(shinyMobile)
library(splitstackshape)
library(DT)
library(dplyr)
library(tidyr)
library(tuicalendr)
library(googlecivic)

calendar_info <- read.csv("Data/elec_dates_group.csv")

# Define UI for application that draws a histogram
ui <- f7Page(
    
    tags$head(
        
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/leaflet.css"),
        tags$script(js),
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.5/leaflet.js"),
        tags$script(src = "mapsearch.js"),
        tags$script(src = "https://www.mapquestapi.com/sdk/leaflet/v2.s/mq-map.js?key=hPrjJdxjgeUKecx7hGhLVEe4S7uGyTwa"),
        tags$script(src = "https://www.mapquestapi.com/sdk/leaflet/v2.s/mq-geocoding.js?key=hPrjJdxjgeUKecx7hGhLVEe4S7uGyTwa")
        
    ),
    
    options = list(theme = "ios",
                   color = "#673ab7",
                   dark = FALSE,
                   filled = FALSE
    ),
    # Application title
    title = HTML("Civic Lookup Tool"),


    # Sidebar with a slider input for number of bins 

        f7TabLayout(
            # panels are not mandatory. These are similar to sidebars
        
        panels = tagList(
            f7Panel(side = "left", theme = "light", effect = "cover"),
            f7Panel(side = "left", theme = "light", effect = "cover"),
        ),
        navbar = f7Navbar(
            title = div("Civic Information Lookup Tool", class = "navtitle"),
            # enable both panels
            left_panel = TRUE,
            right_panel = TRUE,
            transparent = TRUE
        ),
        # navbar = NULL,
        # f7Tabs is a special toolbar with included navigation
        f7Tabs(
            animated = TRUE,
            id = "tabs",
            style = "toolbar",
            f7Tab(
                tabName = "Poll location",
                icon = f7Icon("envelope"),
                active = TRUE,
 
                    h3("Polling location lookup", style = "margin-left:4vw; padding-top:3vh;"),
                    p("Type in your address to find information on your voting precinct and polling location.",  style = "margin-left:4vw; margin-right:2vw;"),
                
                div(f7Text(inputId = "addy", label = "Your address:", placeholder = "20 S Michigan, Chicago, IL"), style = "width:95%; padding-left:2.5%; "),
                #br(),
                div(uiOutput("location"), style = "margin-left:2vw; margin-right:2vw; text-align:center;"),
                #br(),
                #verbatimTextOutput("testmess"),
                div(id="map")
            ),
            f7Tab(
                tabName = "Your Rep",
                icon = f7Icon("briefcase"),
                active = TRUE,
                h3("Representative Lookup", style = "margin-left:4vw; padding-top:3vh;"),
                p("Type in your address to find name and contact information on all your political representatives.",  style = "margin-left:4vw; margin-right:2vw;"),
                
                div(f7Text(inputId = "addy2", label = "Your address:", placeholder = "20 S Michigan, Chicago, IL"), style = "width:95%; padding-left:2.5%; "),
                div(uiOutput("repinfo", style = "height:55vh; width:98vw;"), style = "text-align:center;")
            ),
            f7Tab(
                tabName = "Elections",
                icon = f7Icon("calendar"),
                active = TRUE,
                h3("Election Lookup", style = "margin-left:4vw; padding-top:3vh;"),
                p("Type in your address to find upcoming elections or view the calendar to see dates and deadlines in your state.",  style = "margin-left:4vw; margin-right:2vw;"),
                div(f7Text(inputId = "addy3", label = "Your address:", placeholder = "20 S Michigan, Chicago, IL"), style = "width:95%; padding-left:2.5%; "),
                calendarOutput("calendar", height = "44vh", width = "96vw")
                
            ),
            f7Tab(
                tabName = "About",
                icon = f7Icon("info_circle"),
                active = TRUE,
                h3("Election Lookup", style = "margin-left:4vw; padding-top:3vh;"),
                f7Card(style = "margin-left:4vw; margin-right:4vw;",
                       p("This Civic Information Lookup app was built to showcase the R package {googlecivic}. It features information gleaned from various functions in the package that wrap the Google Civic Information API.
                         This app can be used to pull voter information like polling locations, representative information, and upcoming election dates.", style = "padding-top:2vh;"),
                       p(HTML("To find source code for this open source application, head to <a href='https://github.com/willdebras/civiclookup'>the application github repo</a>. 
                              If you are interested in more information on the development of the {googlecivic} package, head to <a href='https://github.com/willdebras/googlecivic'>the {googlecivic} github repo</a>.")),
                       p(HTML("If you have questions, praise, or concerns about the application, feel free to contact the developers <a href='https://twitter.com/_willdebras'>on twitter</a>."), style = "padding-bottom:2vh;")
                       )
            )
    
        )

    
    )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  address <- reactiveVal(NULL)
  
  addressMQ <- reactiveVal(NULL)
  
  calendar_reac <- reactiveVal(NULL)
  
  state_query <- reactiveVal(NULL)
  
  

    # Tab 1 -- Polling location lookup
    
    observeEvent(input[["keyPressed"]],{
      
      #session$sendCustomMessage("mapInit", "true")
      
      
              address_query <- NULL
              state_query <- NULL
        
              try(
                address_query <- googlecivic::get_voterinfo(input$addy)
                )
      
              address(address_query)
      
      

                if(!is.null(address()$pollingLocations)) {
                  
                  address_compiled <- paste(address()$pollingLocations$address[2], address()$pollingLocations$address[3], address()$pollingLocations$address[4])
                  
                  
                  addressMQ(address_compiled)

                  session$sendCustomMessage("addressSanitized", addressMQ())
                  
                  
                  election_state <- address()$pollingLocations$address$state
                  
                  state_query(election_state)
                  
                  
                  calendar_filter <- calendar_info %>%
                    dplyr::filter(STATE == state_query()) %>%
                    dplyr::mutate(title = paste(STATE, TYPE),
                                  body = paste0(COUNT, "<br>", "<a href='https://ballotpedia.org/Elections_calendar'>", "Click here to find out more.", "</a>"),
                                  start = as.Date(DATE),
                                  end = as.Date(DATE),
                                  calendarId = ifelse(tolower(TYPE) == "ballot access", "filing deadline", "election date"),
                                  category = "allday"
                    )
                  
                  calendar_reac(calendar_filter)
                  
                  

                }
              
              
              

      
      
            
              try (
                
                output$location <- renderUI(
                  
                  if(exists("address_query")) {
                    
                    if(!is.null(address()$pollingLocations)) {
                    
                    h3(address()$pollingLocations$address[1],
                        br(),
                        address()$pollingLocations$address[2]
                        )
                    
                    }
                    
                    else {
                      
                      div("Could not find polling location for this address.")
                      
                    }
                    
                    
                  }
                  
                  else {
                    
                    div("Could not validate address. Check spelling, include county/city, state, and zipcode.")
                    
                  }
      
                )
                
              )  
      

        })
    
    
    reps_df <- reactiveVal(NULL)
    
    output$repsout <- renderDataTable({
      
      datatable(reps_df(),
                width = "100%",
                #extensions = 'Scroller', 
                
                escape = FALSE,
                rownames= FALSE,

                selection = "none",
                  options = list(
                    width = "100%",
                    pageLength = 50,
                    #scroller = TRUE,
                    scrollX = "90vw",
                    scrollY = "40vh",
                    ordering=FALSE,
                    
                    # columnDefs = list(list(width = '25%', targets = c(1, 2)),
                    #                   #list(width = '6%', targets = 3),
                    #                   list(width = '20%', targets = c(3,4))
                    #                   ),

                    dom = 't',
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().body()).css({'font-size':'80%',
                                                  'text-align':'left',
                                                  'word-wrap':'break-word',
                                                  'overflow-wrap':'break-word'});",
                      "$(this.api().table().header()).css({'font-size':'90%',
                                                  'text-align':'left',
                                                  'word-wrap':'break-word',
                                                  'overflow-wrap':'break-word'});",
                      "$('body').find('.dataTables_scrollBody').addClass('scrollbar');",
                      "}"),
                    
                    rowCallback = JS("function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
                                     "$('td:eq(4)', nRow).css({'word-break':'break-all'});",
                                     #"$('td:eq(5)', nRow).css({'word-break':'break-all'});",
                                     "}")
                    
                  )
                
                )
      
    })
    
    output$calendar <- renderCalendar({
      
      calendar(defaultView = "month", taskView = TRUE, scheduleView = c("time", "allday"), useNav = TRUE) %>% 
        set_calendars_props(id = "filing deadline", name = "Filing Deadline", color = "white", bgColor = "#800080") %>% 
        set_calendars_props(id = "election date", name = "Election Date", color = "white", bgColor = "#FFA500") %>%
        add_schedule_df(calendar_reac())
      
    })
    
    
    observeEvent(input[["keyPressed"]],{
      
      #session$sendCustomMessage("mapInit", "true")
      
      
      officials_df <- NULL
      
      try({
        reps_query <- googlecivic::get_rep_address(input$addy2)
        
        office <- reps_query$offices %>%
          tidyr::unnest() %>%
          `colnames<-`("office")
        
        
        officials_df <- cbind(office[-1,1], reps_query$officials)[,c("office", "name", "urls", "emails")] %>%
          dplyr::mutate(urls = ifelse(!emails=="NULL", paste0("<a href='", urls, "'>", "Link", "</a>"), "-"),
                        emails = ifelse(!emails=="NULL", paste0("<a href='mailto:", emails, "'>", "Mail", "</a>"), "-"))
      })
      
      reps_df(officials_df)
      
      
      try (
        
        output$repinfo <- renderUI(
          
          if(exists("officials_df")) {
            
            if(!is.null(reps_df())) {
              
              shinyMobile::f7Card(DTOutput("repsout"))
              
              
              
            }
            
            else {
              
              div("Could not find representative information for this address.")
              
            }
            
            
          }
          
          else {
            
            div("Could not validate address. Check spelling, include county/city, state, and zipcode.")
            
          }
          
        )
        
      )  
      
      
    })
    
  
    # observe({
    # 
    #   session$sendCustomMessage(type = "addressSanitized", message = addressMQ())
    # 
    # 
    # })
    # 
    # output$testmess <- renderText({
    #   
    #   addressMQ()
    #   
    # })
    
    
    observeEvent(input$addy, {
      
      shinyMobile::updateF7Text(session = session, 
                                inputId = "addy2", 
                                value = input$addy)
      
      shinyMobile::updateF7Text(session = session, 
                                inputId = "addy3", 
                                value = input$addy)
      
    })
    
    observeEvent(input$addy2, {
      
      shinyMobile::updateF7Text(session = session, 
                                inputId = "addy", 
                                value = input$addy2)
      
      shinyMobile::updateF7Text(session = session, 
                                inputId = "addy3", 
                                value = input$addy2)
      
    })
    
    observeEvent(input$addy3, {
      
      shinyMobile::updateF7Text(session = session, 
                                inputId = "addy", 
                                value = input$addy3)
      
      shinyMobile::updateF7Text(session = session, 
                                inputId = "addy2", 
                                value = input$addy3)
      
    })
    
    # observe({
    #   if (req(input$tabs) == "Your Rep") {
    #     
    #     
    #       if (input$addy2=="" & input$addy!="") {
    #         
    #         
    #         shinyMobile::updateF7Text(session = session,
    #                                   inputId = "addy2",
    #                                   value = input$addy)
    #         
    #       }
    #       
    #    }
    #   
    #   
    # })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
