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
      shinyWidgets::airDatepickerInput(ns("date"), timepicker = TRUE, value=Sys.time()),
      actionButton(ns("draw"), label=i18n$t("show_chart"))
      

    ),
    mainPanel(
      
      div(id="modify_time",
        actionButton(ns("minus"), label="<<"),
        selectizeInput(ns("value"), label="", choices=1:30, selected=1, multiple=FALSE),
        selectizeInput(ns("unit"), label="",choices=c("Min","Hrs","Day","Mon","Yrs"), selected="Day", multiple=FALSE),
        actionButton(ns("add"), label=">>")
      ),
      
      plotOutput(ns("chart"), width="100%", height="auto"),
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
      
     observeEvent(input$draw, {
        
        r6$horoscope_timezone <- cities$tz [which(cities$city %in% input[["city"]] )] 
        r6$horoscope_city <- cities$city [which(cities$city %in% input[["city"]] )]
        r6$horoscope_datetime <- input$date
        r6$update_chart()
        gargoyle::trigger("update_date")
        
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      observeEvent(input$add, {
        
        r6$horoscope_datetime  <- add_datetime(r6$horoscope_datetime, unit = input$unit, value = input$value)
        r6$update_chart()
        gargoyle::trigger("update_date")
        
      })
      
      observeEvent(input$minus, {
        
        r6$horoscope_datetime <- minus_datetime(r6$horoscope_datetime, unit = input$unit, value = input$value)
        r6$update_chart()
        gargoyle::trigger("update_date")
        
      })

      
      output[["chart"]] <- renderImage({
        
        gargoyle::watch("update_date")
        outfile <- tempfile(fileext=".jpg")
        jpeg(outfile, width=600,height=600, pointsize = 24,res=96,bg="white")
        print(r6$chart)
        dev.off()
        
        list(src=outfile)
      
      }, deleteFile = T)
    
      output[["Datetime"]] <- renderText({
        
        gargoyle::watch("update_date")
        as.character(r6$horoscope_datetime)
        
        })
    
    })
}
