# Choices for drop-down list
list1 <- c(
  # Categorical Variables
  "Region" = 1,
  "Country" = 2,
  "Airport" = 3
)

list2 <- c(
  # Continous Variables
  "Degree" = "degree_centrality",
  "Betweenness" = "betweenness_centrality",
  "Closeness" = "closeness_centrality"
)



navbarPage(
           (title=div(img(src="china.png",height=30,style="padding-left:0px"),
                      "China Civil Aviation on the Belt & Road")), 
           id="nav",
           theme = shinytheme("cerulean"),

           tabPanel("B&R Region Map",icon = icon("map"),
                    div(class="outer",
                        tags$head(
                          # Include CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ), 

                        # Display Map
                        leafletOutput("map", width="100%", height="100%"),
                        # Panel Design
                        absolutePanel(
                          
                          id = "controls", class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = 58, left = "auto", right = 8, bottom = "auto",
                          width = 450, height = 850,
                          # Explorer Panel Header
                          fluidRow(column(7,tags$b(h3("Explore Air-Route by"))),
                                   column(5,selectInput("level", 
                                               #tags$col("View by","#337ab7"),
                                               tags$h4(span(style="color:#337ab7", "")),
                                               choices = list1, selected = 1,width = 450
                                   ))
                                   #column(4,br(),actionButton("show","Reset",style="color: #337ab7; background-color: #2e6da4; border-color: #337ab7"))
                                   ),
                          #fluidRow(column(6,tags$b(h3("Explore Air-Route"))),
                          #         column(5,br(),actionButton("show","Show All"))),
        
                          # column(5,
                          #         radioButtons("year", tags$b("Year"), choices = c("2017" = "option1","2013" = "option2"),selected = "option1",inline=TRUE
                          #         ))
                          
                          # fluidRow(
                          #   column(6,selectInput("colors", "Node Color",
                          #                        rownames(subset(brewer.pal.info, category %in% "qual")),
                          #                        selected = "Accent")
                          #   ),
                          #   column(6,selectInput("size", "Node Size", list2))
                          # ),
                          # 
                          # fluidRow(
                          #   column(5,selectInput("filter", "Filter by", list2)),
                          #   column(7,sliderInput("range", "", min(location$degree_centrality),max(location$degree_centrality),
                          #                        range(location$degree_centrality),step=1,width=400)
                          #   )
                          # ),
                          fluidRow(column(6,selectInput("select", "Primary Selection:",choices = region_list,width = "100%")),
                                   column(6,selectInput("select2", "Choose For Comparison:",choices = region_list,width = "100%"))),
                          
                          bsCollapse(id = "collapse",
                                     bsCollapsePanel("Edit Node Property",
                                                     fluidRow(
                                                       column(6,selectInput("colors", "Node Color",
                                                                            rownames(subset(brewer.pal.info, category %in% "qual")),
                                                                            selected = "Accent")
                                                       ),
                                                       column(6,selectInput("size", "Node Size", list2))
                                                     ),

                                                     fluidRow(
                                                       column(5,selectInput("filter", "Filter by", list2)),
                                                       column(7,sliderInput("range", "", min(location$degree_centrality),max(location$degree_centrality),
                                                                            range(location$degree_centrality),step=1,width=400)
                                                       )
                                                     )
                                     )
                          ),
                               #DT::dataTableOutput("tb"),
                               
                               fluidRow(
                                            column(6,plotOutput(outputId = "bar1",height=180)),
                                            column(6,plotOutput(outputId = "bar2",height=180))
                               ),
                          
                              tableOutput("tb")
                              
                          

                        )
                        
                     )
                    ),
           
           
           tabPanel("Airline Analysis",icon=icon("plane"),
                    div(class="outer",
                        tags$head(
                          # Include CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ), 
                        # Display Map
                        leafletOutput("map2", width="100%", height="100%"),
                        # Panel Design
                        absolutePanel(
                          tags$b(h3("Airlines Explorer")),
                          id = "controls", 
                          class = "panel panel-default", fixed = TRUE,
                          draggable = TRUE, top = 58, left = "auto", right = 8, bottom = "auto",
                          width = 450, height = 590,
                          
                          #selectInput("select_country",tags$b(h4("Filter by Country")), 
                          #            choices = unique(airline[,"Country"]), selected = "China"),
                          
                          selectInput("select_airline",tags$b(h4("Select Airlines")), 
                                      choices = unique(airline[,"Airline"]), selected = "Air China"),
                          br(),
                          
                          fluidRow(
                            column(6,"Connected Airports in China",plotOutput(outputId = "bar3",height=300)),
                            column(6,"Connected Airports in B&R",plotOutput(outputId = "bar4",height=300))
                          )
                                                     
                          #"Explore using Tree Map",
                          #d3tree2Output("treemap",width = "100%",height = 310)
                          
                          #uiOutput("dependent1")
                          #radioButtons("year", tags$b("Year"), choices = c("2017" = "option1","2013" = "option2"),selected = "option1",inline=TRUE),
                        )
                    )
           ),
           
           tabPanel("Data Overview",icon = icon("database"),
                    
                    # Sidebar layout with a input and output definitions ----
                    sidebarLayout(
                      
                      # Sidebar panel for inputs ----
                      sidebarPanel(
                        width=3,
                        selectInput(inputId = "dataset",
                                    label = "Data Overview",
                                    choices = c("Airports", "Flights"))
                        ),
                      
                        mainPanel(
                        fluidRow(
                          DT::dataTableOutput("table")
                        )
                        
                      )
                    )
                    
           ),
           tabPanel("Insight Summary",icon = icon("bar-chart"),
                    
                    tabsetPanel(type="tabs",
                                tabPanel("B&R Analysis",
                                  br(),
                                  h4("Overview of B&R Analysis"),
                                  # Sidebar layout with a input and output definitions ----
                                  sidebarLayout(
                                    
                                    # Sidebar panel for inputs ----
                                    sidebarPanel(
                                      width=3,
                                      selectInput(inputId = "level2",
                                                  label = "View By Which Level?",
                                                  choices = list1),
                                      selectInput(inputId = "VAR",
                                                  label = "Which Variable to show?",
                                                  choices = c('No.of Air Routes'=1,"No.of Flights"=2))
                                    ),
                                    
                                    mainPanel(
                                      plotlyOutput(outputId = "plt",width = "100%")
                                    )
                                    
                                  )
                                ),
                                tabPanel("Airline Analysis",
                                         box(width=5,
                                             br(),
                                             h4("Explore using Tree Map"),
                                             h5(textOutput("clickedinfo")),
                                             d3tree2Output("tree",width = "100%",height = 310)),
                                         box(width = 7,"",
                                             plotlyOutput("plt2",width = "100%")
                                         )
                                         
                                )                      
                            )
                                  
                    ),
           
           conditionalPanel("false", icon("crosshair"))
)

