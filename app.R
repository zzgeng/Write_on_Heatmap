#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(pheatmap)

# Define UI for application that draws a histogram
ui <- fluidPage(
    textInput(inputId = "input" , "Enter Here"),
    plotOutput(outputId = "Heatmap"),
    sliderInput("slider1", label = h3("Noise"), min = 0, 
                max = 10, value = 2)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    letter <- read.delim("./data/letter.txt", header = F)
    letter_order <- data.frame(num = c(1:27), letter = tolower(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", " ")))
    
    output$Heatmap <- renderPlot({
        str_spliting <- strsplit(tolower(input$input), "")[[1]]
        
        drawing <- matrix()
        for(x in str_spliting){
            num <- letter_order[letter_order$letter == x, 1]
            start = (num-1)*10+1
            end = num*10
            drawing <- cbind(drawing, letter[, start:end])
        }
        drawing <- drawing[,-1]
        
        rownumnber = as.numeric(dim(drawing)[1])
        colnumber = as.numeric(dim(drawing)[2])
        
        dimension = rownumnber * colnumber
        noise <- matrix(sample(1:100, dimension, replace = TRUE), ncol = colnumber, nrow = rownumnber)
        
        pheatmap(as.matrix(drawing + noise*(input$slider1 * 1/500)), 
                    cluster_rows = FALSE, 
                    cluster_cols = FALSE)
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
