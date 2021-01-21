#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(xlsx)
library(dplyr)
library(leaflet)
library(ggplot2)
library(ggiraph)
library(memisc) 

states_data <- read.csv("States.csv")
states_data

states_data <- states_data%>%mutate(popup_info=paste(State,"<br/>","Tobacco:",tobacco,"<br/>",
                                                     "Vape/E-cigarettes:",vape))


colors<-c("green","blue")
pal<-colorFactor(colors,states_data$tobacco)

mymap<-leaflet()%>%
    addTiles()%>%
    addCircleMarkers(data=states_data,
                     lat = ~lat, 
                     lng = ~long,radius = ~3, 
                     popup = ~popup_info, 
                     color = ~pal(tobacco))
mymap 

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

library(ggplot2)
library(dplyr)    # Data manipulation
library(ggiraph)

donut_male <- data.frame(type = c("Tobacco", "Vape"), value = c(457065, 252348)) %>%
    mutate(
        percentage = value / sum(value),
        hover_text = paste0(type, ": ", value)
    ) %>%
    mutate(percentage_label = paste0(round(100 * percentage, 1), "%"))

maledonut_plot <- ggplot(donut_male, aes(y = value, fill = type)) +
    geom_bar_interactive(
        aes(x = 1, tooltip = hover_text),
        width = 0.1,
        stat = "identity",
        show.legend = FALSE
    ) +
    annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = donut_male[["percentage_label"]][donut_male[["type"]] == "Tobacco"],
        size = 10,
        color = "blue"
    ) +
    scale_fill_manual(values = c(Tobacco = "blue", Vape = "magenta")) +
    coord_polar(theta = "y") +
    theme_void()

donut_female <- data.frame(type = c("Tobacco", "Vape"), value = c(67167, 46886)) %>%
    mutate(
        percentage = value / sum(value),
        hover_text = paste0(type, ": ", value)
    ) %>%
    mutate(percentage_label = paste0(round(100 * percentage, 1), "%"))

femaledonut_plot <- ggplot(donut_female, aes(y = value, fill = type)) +
    geom_bar_interactive(
        aes(x = 1, tooltip = hover_text),
        width = 0.1,
        stat = "identity",
        show.legend = FALSE
    ) +
    annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = donut_female[["percentage_label"]][donut_female[["type"]] == "Tobacco"],
        size = 10,
        color = "blue"
    ) +
    scale_fill_manual(values = c(Tobacco = "blue", Vape = "magenta")) +
    coord_polar(theta = "y") +
    theme_void()

donut_human <- data.frame(type = c("Male", "Female"), value = c( 709413, 114033)) %>%
    mutate(
        percentage = value / sum(value),
        hover_text = paste0(type, ": ", value)
    ) %>%
    mutate(percentage_label = paste0(round(100 * percentage, 1), "%"))

humandonut_plot <- ggplot(donut_human, aes(y = value, fill = type)) +
    geom_bar_interactive(
        aes(x = 1, tooltip = hover_text),
        width = 0.1,
        stat = "identity",
        show.legend = FALSE
    ) +
    annotate(
        geom = "text",
        x = 0,
        y = 0,
        label = donut_human[["percentage_label"]][donut_human[["type"]] == "Male"],
        size = 10,
        color = "blue"
    ) +
    scale_fill_manual(values = c(Male = "blue", Female = "magenta")) +
    coord_polar(theta = "y") +
    theme_void()

pie_urban <- data.frame(type = c("Tobacco", "Vape"), val = c(177980, 121995)) %>%
    arrange(desc(type)) %>%
    mutate(ypos = cumsum(val) - 0.5* val)

urban_plot <- ggplot(pie_urban, aes(x="", y = val, fill = type))+
    geom_bar(stat = "identity",width = 1, color = "white")+
    coord_polar("y", start = 0)+
    theme_void()+
    geom_text(aes(y = ypos, label = val), color = "white") +
    scale_fill_brewer(palette = "Set1")


pie_rural <- data.frame(type = c("Tobacco", "Vape"), value = c(346252, 177219)) %>%
    arrange(desc(type)) %>%
    mutate(ypos1 = cumsum(value) - 0.5*value)

rural_plot <- ggplot(pie_rural, aes(x= "", y = value, fill = type))+
    geom_bar(stat = "identity",width = 1, color = "white")+
    coord_polar("y", start = 0)+
    theme_void()+
    geom_text(aes(y = ypos1, label = value), color = "white") +
    scale_fill_brewer(palette = "Set1")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    output$mymap <- renderLeaflet({
        leaflet()%>%
            addTiles()%>%
            addCircleMarkers(data=states_data,
                             lat = ~lat, 
                             lng = ~long,radius = ~3, 
                             popup = ~popup_info, 
                             color = ~pal(tobacco))
    })
    
    output$human <- renderUI({
        tags$div(img(src = "Human.jpg", width = 400, height = 400) )
    })
    
    output$contact <- renderUI({
        tags$div(img(src = "bahaya_merokok.png", width = 500, height = 300) )
    })
    
    output$team <- renderUI({
        tags$div(img(src = "Team.jpg", width = 1000, height = 600) )
    })
    
    output$title <- renderUI({
        tags$div(img(src = "title.jpg", width = 600, height = 100) )
    })
    
    output$humandonut <- renderggiraph( {ggiraph( ggobj = humandonut_plot )})
    
    
    observeEvent(input$human, {
        output$male <- NULL
        output$female <- NULL
        output$maledonut <- NULL
        output$femaledonut <- NULL
        output$human <- renderUI({
            img(src = "Human.jpg", width = 400, height = 400)
        })
        output$humandonut <- renderggiraph( {ggiraph( ggobj = humandonut_plot )})
    })
    
    observeEvent(input$female, {
        output$human <- NULL
        output$female <- NULL
        output$maledonut <- NULL
        output$humandonut <- NULL
        output$male <- renderUI({
            img(src = "Female.jpg", width = 400, height = 400)
        })
        output$femaledonut <- renderggiraph( {ggiraph( ggobj = femaledonut_plot )})
    })
    
    observeEvent(input$male, {
        output$human <- NULL
        output$male <- NULL
        output$femaledonut <- NULL
        output$humandonut <- NULL 
        output$female <- renderUI({
            img(src = "Male.jpg", width = 400, height = 400)
        })
        output$maledonut <- renderggiraph( {ggiraph( ggobj = maledonut_plot )})
    })
    
    
    
    observeEvent(input$choice, {
        output$urban <- NULL
        output$rural <- NULL
        
        #output$choice <- NULL
        if((input$choice) == "Rural")
        {
            output$urban <- NULL
            output$urban <- renderPlot(rural_plot)
        }
        else if((input$choice) == "Urban")
        {
            output$urban <- NULL
            output$urban <- renderPlot(urban_plot)
        }
    })
    
    
    
    observeEvent(input$videoslider, {
        if( (input$videoslider) == 1 )
        {
            output$video <- renderUI({HTML(paste0('<iframe width="800" height="500" src="https://www.youtube.com/embed/', 'Y18Vz51Nkos' ,'" frameborder="0" ></iframe>')) }) 
        }
        else if( (input$videoslider) == 2 )
        {
            output$video <- renderUI({HTML(paste0('<iframe width="800" height="500" src="https://www.youtube.com/embed/', 'wGJpGSCBzZ8' ,'" frameborder="0" ></iframe>')) }) 
        }
        else if( (input$videoslider) == 3 )
        {
            output$video <- renderUI({HTML(paste0('<iframe width="800" height="500" src="https://www.youtube.com/embed/', 'haqi4xvjvKo' ,'" frameborder="0" ></iframe>')) }) 
        }
    })
    
    
})
