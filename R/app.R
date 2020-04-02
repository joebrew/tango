
##################################################
# UI
##################################################
#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import shiny
#' @import ggplot2
#' @import shinyMobile
#' @import leaflet
#' @import RColorBrewer
#' @import sp
#' @import DT
#' @import dplyr
mobile_app_ui <- function(request) {
  options(scipen = '999')
  
  tagList(
    mobile_golem_add_external_resources(),
    
    f7Page(
      init = f7Init(
        skin = 'ios', #  c("ios", "md", "auto", "aurora"),
        theme = 'light', #c("dark", "light"),
        filled = TRUE
      ),
      title = "Tango",
      f7SingleLayout(
        navbar = f7Navbar(
          title = "Tango",
          hairline = TRUE,
          shadow = TRUE
        ),
        toolbar = f7Toolbar(
          position = "bottom",
          f7Link(label = "Carlos Chaccour", src = "https://www.isglobal.org/person?p_p_id=viewpersona_WAR_intranetportlet&p_p_lifecycle=0&p_p_col_id=column-1&p_p_col_count=1&_viewpersona_WAR_intranetportlet_struts_action=%2Fview%2FpersonaView&_viewpersona_WAR_intranetportlet_personaId=7304", external = TRUE),
          f7Link(label = "Joe Brew", src = "https://databrew.cc", external = TRUE)
          
        ),
        # main content
        f7Shadow(
          intensity = 10,
          hover = TRUE,
          f7Card(
            sliderInput('age_at_risk',
                        'Age of "risk"',
                        min = 0, max = 100,
                        value = c(80, 100)),
            # sliderInput('vulnerability_age',
            #             'Age of "vulnerability"',
            #             min = 0, max = 100,
            #             value = c(20, 50)),
            checkboxInput('overlay_deaths',
                     'Overlay observed deaths at CA level?',
                      value = FALSE),
            selectInput('show',
                        'Show on map:',
                        choices = c('Risk'#,
                                    # 'Vulnerability'
                                    )),
                                    # 'Product of receptivity and vulnerability (combined index)')),
            br(),
            f7Button('generate_map',
                         'Generate map and tables'),
            br(),
            helpText('Note: generating the map may take up to 30 seconds. Be patient'),
            br(),
            f7Button('clear_map',
                         'Clear map'),
            br(),
            leafletOutput('the_plot', height = '500px'),
            DT::dataTableOutput('the_table'),
            br(),
            sliderInput('cut_off',
                        'Cut-off threshold',
                        min = 0, max = 100, value = 10),
            DT::dataTableOutput('summary_table'),
            
            br(),
            f7Button('generate_threshold_map',
                     'Generate THRESHOLD map'),
            br(),
            helpText('Note: generating the map may take up to 30 seconds. Be patient'),
            leafletOutput('threshold_plot', height = '500px'),
            
            height = 500
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
mobile_golem_add_external_resources <- function(){
  # addResourcePath(
  #   'www', system.file('app/www', package = 'covid19')
  # )
  
  share <- list(
    title = "Tango",
    url = "http://joebrew.net",
    image = "http://www.databrew.cc/images/blog/covid2.png",
    description = "Joe Brew / Tango",
    twitter_user = "joethebrew"
  )
  
  tags$head(
    
    # Facebook OpenGraph tags
    tags$meta(property = "og:title", content = share$title),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:url", content = share$url),
    tags$meta(property = "og:image", content = share$image),
    tags$meta(property = "og:description", content = share$description),
    
    # Twitter summary cards
    tags$meta(name = "twitter:card", content = "summary"),
    tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
    tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
    tags$meta(name = "twitter:title", content = share$title),
    tags$meta(name = "twitter:description", content = share$description),
    tags$meta(name = "twitter:image", content = share$image),
    
    # golem::activate_js(),
    # golem::favicon(),
    # Add here all the external resources
    # Google analytics script
    # includeHTML(system.file('app/www/google-analytics-mini.html', package = 'covid19')),
    # includeScript(system.file('app/www/script.js', package = 'covid19')),
    # includeScript(system.file('app/www/mobile.js', package = 'covid19')),
    # includeScript('inst/app/www/script.js'),
    
    # includeScript('www/google-analytics.js'),
    # If you have a custom.css in the inst/app/www
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}

##################################################
# SERVER
##################################################
#' @import shiny
#' @import leaflet
mobile_app_server <- function(input, output, session) {
  
  define_risk <- function(data, n1, n2){
    data %>%
      mutate(at_risk = edad >= n1 & edad <= n2) %>%
      summarise(pop_at_risk = sum(total[at_risk], na.rm = TRUE),
                total_pop = sum(total, na.rm = TRUE)) %>%
      ungroup %>%
      mutate(p_at_risk = pop_at_risk / total_pop * 100)
  }
  # define_vulnerability <- function(data, n1, n2){
  #   data %>%
  #     mutate(vulnerable = edad >= n1 & edad <= n2) %>%
  #     summarise(pop_vulnerable = sum(total[vulnerable], na.rm = TRUE),
  #               total_pop = sum(total, na.rm = TRUE)) %>%
  #     ungroup %>%
  #     mutate(p_vulnerable = pop_vulnerable / total_pop * 100)
  # }
  
  data_list <- reactiveValues(data = data.frame())
  
  output$the_plot <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = esp0)
  })
  
  output$threshold_plot <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = esp0)
  })
  
  output$the_table <- DT::renderDataTable({
    out <- data_list$data
    if(!is.null(out)){
      if(nrow(out) > 0){
        out <- out  %>% dplyr::select(id, municipio,
                                      pop_at_risk,
                                      # pop_vulnerable,
                                      p_at_risk,
                                      # p_vulnerable,
                                      total_pop)
      }
    }
    out 
  })
  
  output$summary_table <- DT::renderDataTable({
    cut_off <- input$cut_off
    out <- data_list$data
    pd <- NULL
    if(!is.null(out)){
      if(nrow(out) > 0){
        pd <- out %>%
          mutate(`Category` = ifelse(p_at_risk >= cut_off,
                                     'Above threshold',
                                     'Below threshold')) %>%
          group_by(Category) %>%
          summarise(`Number of municipalities` = n(),
                    `Number of people in those municipalities` = sum(total_pop, na.rm = TRUE))
      }
    }
    pd 
  })
  
  observeEvent(input$generate_map, {
    
    # Get receptivity
    ns <- input$age_at_risk
    message('check1')
    risks <- census %>%
      group_by(municipio, id) %>%
      define_risk(n1 = ns[1],
                         n2 = ns[2]) %>%
      arrange(desc(p_at_risk))
    message('check2')

    map <- municipios
    map@data <- left_join(map@data, risks, by = 'id')

    map <- municipios
    map@data <- left_join(map@data, risks, by = 'id')
    show <- input$show
    map@data$var <- map@data$p_at_risk
    data_list$data <- map@data
    
    
    p_popup <- paste0(map@data$NAMEUNIT, ': Percent ', show, ': ',  round(map@data$var, digits = 2))

    pal_fun <- colorQuantile("YlOrRd", NULL, n = 8)
    leafletProxy("the_plot") %>%
      clearControls() %>%
      clearShapes() %>% 
        addPolygons(
          # highlightOptions = highlightOptions(stroke = 4, weight = 2),
          data = map,
          stroke = FALSE, # remove polygon borders
          fillColor = ~pal_fun(var), # set fill color with function from above and value
          fillOpacity = 0.8, smoothFactor = 0.5, # make it nicer
          label = p_popup)
      
  })
  
  observeEvent(input$generate_threshold_map, {
    
    # Get receptivity
    ns <- input$age_at_risk
    risks <- census %>%
      group_by(municipio, id) %>%
      define_risk(n1 = ns[1],
                  n2 = ns[2]) %>%
      arrange(desc(p_at_risk))

    map <- municipios
    map@data <- left_join(map@data, risks, by = 'id')
    
    map <- municipios
    map@data <- left_join(map@data, risks, by = 'id')
    show <- input$show
    map@data$var <- map@data$p_at_risk
    threshold <- input$cut_off
    map@data$var <- ifelse(map@data$var >= threshold, 'Above threshold',
                           'Below threshold')

    p_popup <- paste0(map@data$NAMEUNIT, ': Category ', show, ': ',  map@data$var)
    
    pal_fun <- colorFactor("YlOrRd", map@data$var, reverse = TRUE)
    leafletProxy("threshold_plot") %>%
      clearControls() %>%
      clearShapes() %>% 
      addPolygons(
        # highlightOptions = highlightOptions(stroke = 4, weight = 2),
        data = map,
        stroke = FALSE, # remove polygon borders
        fillColor = ~pal_fun(var), # set fill color with function from above and value
        fillOpacity = 0.8, smoothFactor = 0.5, # make it nicer
        label = p_popup)
    
  })
  
  observeEvent(input$clear_map, {
    leafletProxy("the_plot") %>%
      clearControls() %>%
      clearShapes() 
    
  })
}

app <- function(){
  # Detect the system. If on AWS, don't launch browswer
  is_aws <- grepl('aws', tolower(Sys.info()['release']))
  shinyApp(ui = mobile_app_ui,
           server = mobile_app_server,
           options = list('launch.browswer' = !is_aws))
}