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
 
                    h3("Polling location lookup", style = "margin-left:2vw;"),
                    p("Type in your address to find information on your voting precinct and polling location.",  style = "margin-left:2vw; margin-right:2vw;"),
                
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
                f7Card("other information")
            ),
            f7Tab(
                tabName = "Elections",
                icon = f7Icon("calendar"),
                active = TRUE,
                f7Card("upcoming elections")
            ),
            f7Tab(
                tabName = "About",
                icon = f7Icon("info_circle"),
                active = TRUE,
                f7Card("about the app")
            )
    
        )

    
    )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  address <- reactiveVal(NULL)
  
  addressMQ <- reactiveVal(NULL)
  
  

    # Tab 1 -- Polling location lookup
    
    observeEvent(input[["keyPressed"]],{
      
      #session$sendCustomMessage("mapInit", "true")
      
      
              address_query <- NULL
        
              try(
                
                address_query <- googlecivic::get_voterinfo(input$addy)
                
                
                )
      
              address(address_query)
      
      

                if(!is.null(address()$pollingLocations)) {
                  
                  address_compiled <- paste(address()$pollingLocations$address[2], address()$pollingLocations$address[3], address()$pollingLocations$address[4])
                  
                  
                  addressMQ(address_compiled)

                  session$sendCustomMessage("addressSanitized", addressMQ())

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

    
}

# Run the application 
shinyApp(ui = ui, server = server)
