#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(rsconnect)
library(shiny)
library(tidyverse)
library(datasauRus)
library(plotly)
library(rmarkdown)#rmarkdown package was added in order to read the subject_template.rmd file in our directory

set.seed(2)#I added this on 29/11/2024 for reproducible-ness as per Andrew's advice in lectures

student_data<-tibble(student_number=c(1:200),English_score=rnorm(200,65,20),Maths_score=rnorm(200,55,20),History_score=rnorm(200,60,20),French_score=rnorm(200,62,20),Science_score=rnorm(200,65,20))
student_data
#we start with a tibble with 5 subjects, Maths, English, History, French and Science 
#and a year group of 200. This is our simulated dataset. The marks are in percentages, with the
#Scottish Higher system are a rough framework.

#firstly we will turn the data from wide to long like the datasaurus data.
#this was covered in week 3 of our Intro to R lectures and I have copied and pasted the code from there
#adjusting for the different subjects and subject word names

student_data_long<-pivot_longer(student_data,cols = c(English_score,Maths_score,History_score,French_score,Science_score), names_to = "subject",
                                values_to = "marks")
student_data_long

max(student_data_long$marks)#we see here at least one value is above 100, since these scores
#are percentages this doesn't make sense.

student_data_long$marks<-replace(student_data_long$marks,student_data_long$marks>100,100)#this piece
#of code was constructed after some trial and error using the following Digltal Ocean tutorial link
#https://www.digitalocean.com/community/tutorials/replace-in-r
#The above link was found from a basic Google search


max(student_data_long$marks)#we can now see there is no larger value than 100 in the dataset,
#we can view the data and check this in the 'environment' tab, we can also see this in the
#graphs generated in the app


#we will start this Shiny app by importing the 20th of November class Shiny app that
#Andrew created for us, this code is the closest to what we want and we can use
#this as a template, making as minor modifications as possible.
#The app files were unzipped into my Rstudio project folder and the code
#copied and pasted across.


# define available datasets for dropdown
dataset_choices <- unique(student_data_long$subject)
#firstly we will change the data set choices from the datasaurus datasets
#to our subject scores datasets,






# Define UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("Year Group Subject Scores"),
  
  # Sidebar with a select for the dataset
  sidebarLayout(
    sidebarPanel(
      # let the user choose one of the subject datasets
      selectInput(inputId = "dataset", 
                  label = "Please choose a subject",
                  #here I changed 'dataset' to 'subject'
                  choices = dataset_choices),
      actionButton(inputId = "plot",
                   label = "Change Plot")
    ),
    
    # Show a plot of the subject scores dataset
    mainPanel(
      plotlyOutput(outputId = "subject_plot"),#here I have changed the word 'datasaurus' to 'subject'
      downloadButton(outputId = "report", label = "Download Report")#here I copied and pasted code from 
      #Andrew's 20th of November class on Shiny, specifically the 'download' button for generating
      #a report for the user
    )
  )
)

#we can see after altering this section we have the user interface has changed the 
#way we want it to.





# Define server logic 
server <- function(input, output) {
  
  output$subject_plot <- renderPlotly({
    #here I have changed 'output$datasaurus_plot' to 'output$subject_plot'
    
    # listen for the choice of dataset - only when requested
    dataset_to_plot <- isolate(input$dataset)
    
    input$plot # listen to the actionButton
    
    # filter the data
    data_to_plot <- filter(student_data_long, subject == dataset_to_plot)#here I have again changed 'datasaurus_dozen' to 'student_data_long'#here I also changed 'dataset' to 'subject'
    
    # create a ggplot object
    p <- ggplot(data_to_plot, aes(x = marks)) +#here I have changed 'x=x' and 'y=y' to 'x = marks' and deleted the 'y' part
      geom_histogram(colour = 'black', fill = 'lightgreen')#I have also changed 'geom_point' to 'geom_histogram'#I have deleted the title part for simplicity
    
    # convert it to plotly
    ggplotly(p)
  })
  
  
  output$report <- downloadHandler(
    filename = "subject_report.html",#here the name was simply changed from 'datasaurus' to 'subject' #also the 'Report' was changed to 'report' for simplicity
    content = function(file) {
      filtered_data <- filter(student_data_long, subject == input$dataset) #I know I have filtered the data twice but I don't seem to be able to get the code to work without doing it this way
      params <- list(data_for_markdown = filtered_data#data_for_markdown is much clearer, as I initially had student_data_long twice in this code which is silly for different things 
                     #here I changed datasaurus_df to student_data_long, I removed dino_data()
                     )#I removed input$colour for simplicity
      
      id <- showNotification(
        "Rendering report...", 
        duration = NULL, 
        closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)
      
      render("subject_template.Rmd", #here 'datasaurus' was changed to 'subject' again,
             #I made sure to make the R markdown file to match this title when I named it
             output_file = file,
             params = params,
             envir = new.env(parent = globalenv())
             )#now I copied and pasted the above code from the point 'output$report' 
      #until this line adding brackets where appropriate to mimic Andrew's original code
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
