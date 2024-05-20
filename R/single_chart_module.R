#' UI part Shiny module: single chart
#' 
#' @param id shiny module ID
#' @param i18n a shiny.i18n::Translator object that links different languages
#' 
#' @import shiny
#' @importFrom shinyWidgets airDatepickerInput
#' 

single_chart_ui <- function(id, i18n) {

  countries <- unique(Rstrology::cities$country)

  ns <- NS(id)
  
  fluidPage(
    includeCSS("./inst/www/custom.css"),
    sidebarPanel(
      h4(i18n$t("please_enter_data")),
      br(),
      selectizeInput(ns("country"), label=i18n$t("country"), choices=countries, selected=countries[1], multiple=FALSE),
      selectizeInput(ns("city"), label=i18n$t("city"), choices=cities$city[1], selected=cities$city[1], multiple=FALSE),
      actionButton(ns("more_cities"), label=i18n$t("more_cities")),
      br(),
      br(),
      br(),
      p(i18n$t("click_to_change")),
      shinyWidgets::airDatepickerInput(ns("date"), timepicker = TRUE, value=Sys.time())
#      actionButton(ns("draw"), label=i18n$t("show_chart"))
      

    ),
    mainPanel(
      
      
      fluidRow(
        column(width = 2,
               actionButton(ns("minus"), label="-")),
        column(width = 4, 
               selectizeInput(ns("number"), label="", choices=1:30, selected=1, multiple=FALSE)),
        column(width = 4, 
               selectizeInput(ns("unit"), label="",choices=c("Min","Hrs","Day","Mon","Yrs"), selected="Day", multiple=FALSE)),
        column(width = 2,
               actionButton(ns("add"), label="+"))
      ),
      
      plotOutput(ns("chart"), width="100%", height="600px"),
      textOutput(ns("Datetime"))
    )
  )
}

#' Server part shiny: single chart (update SelectInput)
#' 
#' @param id Shiny module ID
#' @param r6 R6 object to help with communication between modules (for translation etc.)
#' 
#' @import shiny
#' @import gargoyle
#' 

update_select_input_server <- function(id, r6){
  
  moduleServer(id, function(input, output, session){
    
    observeEvent(gargoyle::watch("change_language"), {

      r6$set_translation_language(r6$language)
      updateSelectInput(session, "country", label = r6$t("country"), choices = r6$t(unique(cities$country)))
      
    })
    
  })
}

#' Server part shiny: single chart
#' 
#' @param id Shiny module ID
#' @param r6 R6 object to help with communication between modules (for translation etc.)
#' 
#' @import shiny
#' @import shinyDatetimePickers
#' 

single_chart_server <- function(id, r6){
  
    moduleServer(id, function(input, output, session){
      
      observeEvent(input$country, { # input city ####
        
        all_cities <- reactive({ 
          cities$city [(cities$country %in% (unique(cities$country) [which(r6$t(unique(cities$country)) %in% input$country)]) )] 
        })
        
        big_cities <- reactive({
          cities$city [(cities$country %in% 
                          (unique(cities$country)[which(r6$t(unique(cities$country)) %in% input$country)])) & cities$big_city %in% TRUE]
        })
        
        if(input$more_cities){
          
          updateSelectizeInput(session, "city", choices=all_cities(), selected = all_cities()[1], server=TRUE)
        }
        
        else{
          
          updateSelectizeInput(session, "city", choices=big_cities(), server=TRUE, selected=big_cities()[1])
          
        }
      })
      
      timezone <- reactive({ cities$tz [which(cities$city %in% input[["city"]] )] })
      planet_position <- reactive({calculate_planet_position(date=input$date, timezone = timezone(), city = input$city)}) #%>%
#        bindEvent(input$draw, ignoreNULL=FALSE)
      
      new_date <- reactive({
        input$date + lubridate::days(input$number)
      }) %>% bindEvent(input$add)
      
    output[["chart"]] <- renderPlot({
      
      data <- planet_position()$planetary_position
      data <- data[!(row.names(data) %in% "true_node"),]
      draw_whole_sign_chart(data)
      
    }, width=800, height=600, res=72)
    
    output[["Datetime"]] <- renderText({
      
      if(is.null(input$add)) {
        as.character(input$date)
      } else {
        as.character(new_date())
      }
         
    })
    
    })
}
