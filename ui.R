library(shinydashboard)
library(leaflet)
library(ggiraph)

header <- dashboardHeader( title = HTML("Tak Nak !"),  
                           tags$li(class = "dropdown",tags$a(href='http://taknak.myhealth.gov.my/',
                                         tags$img(src='taknak.png',width = 150, height = 50)) )
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Introduction", tabName = "Introduction", icon = icon("grin-beam")),
        menuItem("Malaysia", tabName = "Malaysia", icon = icon("map")),
        menuItem("Gender", tabName = "Gender", icon = icon("user")),
        menuItem("Location", tabName = "Location", icon = icon("city")),
        menuItem("Video", tabName = "Video", icon = icon("video")),
        menuItem("Contact", tabName = "Contact", icon = icon("phone"))
    )
)



## Body content
body <- dashboardBody(
    tabItems(
        # First tab content
        tabItem(tabName = "Introduction",
                fluidRow(
                    column(width = 9,
                           box( width = 12,
                                height = 150,
                                uiOutput("title")) ),
                    
                    column(width = 9,
                           box(title = tags$b("Our Team"),
                               width = 12,
                               height = 700,
                               status = "info",
                               solidHeader = TRUE,
                               align = "center",
                               collapsible = TRUE,
                               collapsed = TRUE,
                               uiOutput("team")
                           )
                    )
                )
        ),
        
        # Second tab content
        tabItem(tabName = "Malaysia",
                fluidRow(
                    leafletOutput("mymap"),
                    valueBox(value = tags$p("2:1", style = "font-size: 40px;"),
                             subtitle = tags$p("Tobbaco : Vape", style = "font-size: 130%;"),
                             width = 3,
                             icon = icon("smoking"),
                             color = "blue"),
                    valueBox(value = tags$p("Putrajaya | 1477", style = "font-size: 40px;"),
                             subtitle = tags$p("Least Serious - Tobacco", style = "font-size: 130%;"),
                             width = 3,
                             icon = icon("thumbs-up"),
                             color = "blue"),
                    valueBox(value = tags$p("Kuala Lumpur|179169", style = "font-size: 35px;"),
                             subtitle = tags$p("Serious State - Tobacco", style = "font-size: 150%;"),
                             width = 3,
                             icon = icon("thumbs-down"),
                             color = "blue"),
                    valueBox(value = tags$p("55552", style = "font-size: 40px;"),
                             subtitle = tags$p("Most Vapers", style = "font-size: 130%;"),
                             width = 3,
                             icon = icon("frown"),
                             color = "blue")
                ),
                
        ),
        
        # Third tab content
        tabItem(tabName = "Gender",
                fluidRow(
                    column(width = 9,
                           box(title = tags$b("Adolescents Smoker by Gender in Malaysia "),
                               width = 12,
                               height = 530,
                               status = "info",
                               solidHeader = TRUE,
                               align = "center",
                               uiOutput("human"),uiOutput("male"),uiOutput("female")
                           )
                    ),
                    column(width = 3,
                           box(width = NULL, status = "warning",
                               actionButton("human", "Overall"),
                               actionButton("female", "Female"),
                               actionButton("male", "Male"),
                           ),
                           box(title = tags$b("Gender"),
                               width = 12,
                               height = 350,
                               status = "info",
                               solidHeader = TRUE,
                               align = "center",
                               div(style="width:15;height:400px;position:absolute;visibility: visible;", ggiraphOutput(outputId = "humandonut")),
                               div(style="width:15;height:400px;position:absolute;visibility: visible;", ggiraphOutput(outputId = "femaledonut")),
                               div(style="width:15;height:400px;position:absolute;visibility: visible;", ggiraphOutput(outputId = "maledonut")),
                         )
                    )
                )
        ),
        
        
        # Fourth tab content
        tabItem(tabName = "Location",
                h3("Location"),
                fluidRow(
                    column(width = 3,
                           selectInput("choice",
                                       label = "Choose a location to display:",
                                       choices = c(Choose ="",
                                                   "Urban", 
                                                   "Rural")
                           ),
                           
                    ),
                    column(width = 10,
                           box(title = tags$b("Adolescents Smoker by Location in Malaysia "),
                               width = 12,
                               height = 530,
                               status = "info",
                               solidHeader = TRUE,
                               align = "center",
                               div(
                               plotOutput('urban',width = "100%", height = "400px"),)
                               
                               
                               )
                           )
                    )
                    
        ),
        
        tabItem(tabName = "Video",
                fluidRow(
                    box(uiOutput("video")),
                    
                    column(width = 3,
                           box(
                               title = "Video",
                               sliderInput("videoslider", "Drag to Change Video", 1, 3, 1)
                           )
                    )
                    
                )
        ),
        
        tabItem(tabName = "Contact",
                fluidPage(
                    fluidRow(
                        column(width = 9,
                               box(title = tags$b("CONTACT "),
                                   width = 800,
                                   height = 600,
                                   status = "info",
                                   solidHeader = TRUE,
                                   align = "center",
                                   uiOutput("contact"),
                                   h2( 'Please Contact' ), 
                                   h2('               03-88834400' ),
                                   h2('               myhealth@moh.gov.my' ),
                                   tags$style(HTML("
                                h2 {
                                    background-color: red;
                                    animation-name: example;
                                    animation-duration: 4s;
                                    animation-iteration-count: infinite;
                                }
                                 @keyframes example {
                                  from {background-color: red;}
                                  to {background-color: yellow;}
                                }"
                                                             
                                                             ))
                               ))
                    )
                )
        )
        
    )
)

dashboardPage(
    skin = "black",
    header,
    sidebar,
    body
)