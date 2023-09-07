library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# fields for captured user input
fields <- c("obtaining",
	"storage",
	"finding",
	"sharing_internal",
	"understanding",
	"using",
	"documenting",
	"reporting",
	"preserving",
	"sharing_external")

# field labels
field_labels <- c(
  "Obtaining/\nCollecting",
  "Storing",
  "Finding\nin Storage",
  "Sharing with\nTeammates",
  "Understanding",
  "Using",
  "Documenting",
  "Reporting",
  "Preserving",
  "Sharing Outside\nof Team"
  )

#outputDir <- "../../"

saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
	  responses <<- rbind(responses, data)
  } else {
	  responses <<- data
  }
}
	
loadData <- function() {
  if (exists("responses")) {
	  responses
  } else {
    tibble(
      obtaining = character(),
      storage = character(),
      finding = character(),
      sharing_internal = character(),
      understanding = character(),
      using = character(),
      documenting = character(),
      reporting = character(),
      preserving = character(),
      sharing_external = character()
    )
  }
}
	
shinyApp(
	ui = fluidPage(
		## DT::dataTableOutput("responses", width = 300),
	  titlePanel("Rate Team Research Activity Difficulty"),
		tags$hr(),
		sidebarLayout(
		  sidebarPanel(
		    radioButtons("obtaining", "Obtaining/Collecting Data", c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5)),
		    radioButtons("storage", "Data Storage",  c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5)),
		    radioButtons("finding", "Finding Project Data", c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5)),
		    radioButtons("sharing_internal", "Data Sharing Within Team", c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5)),
		    radioButtons("understanding", "Understanding the Data",  c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5)),
		    radioButtons("using", "Using Data", c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5)),
		    radioButtons("documenting", "Documenting Data",  c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5)),
		    radioButtons("reporting", "Reporting Data Analysis Results", c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5)),
		    radioButtons("preserving", "Preserving Data", c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5)),
		    radioButtons("sharing_external", "Data Sharing Within Team", c("No Response"=NA, "Very Easy"=1, "Easy" =2, "neutral"=3, "Difficult"=4, "Very Difficult"=5)),
		    actionButton("submit","Submit")
		  ),
		  mainPanel(
		    plotOutput("meanScores_plot"),
		    plotOutput("meanScores_spider"),
		    tags$hr(),
		    p("Summary of Responses"),
		    tableOutput("meanScores"),
		    tags$hr(),
		    p("Individual Responses"),
		    tableOutput("inputData")
		  )
		)
	),
	
	server = function(input, output, session) {
		
		formData <- reactive({
			data <- sapply(fields, function(x) input[[x]])
			data
		})
		
		observeEvent(input$submit, {
			saveData(formData())
		})
		
		output$inputData <- renderTable({
		  input$submit
			loadData()
		})
		
		output$meanScores_plot <- renderPlot({
		  input$submit
		  if (nrow(loadData()) == 0) {
		    ggplot() + annotate("text", x=1, y=1, size=8, label="Please submit a survey response...") + theme_void()
		  } else {
  		  summaries <- loadData() %>% 
  		    pivot_longer(cols=all_of(fields)) %>% 
  		    group_by(name) %>% 
  		    summarize(Average=mean(as.numeric(value), na.rm=TRUE))
  		  summaries$name <- factor(summaries$name, levels=rev(fields))
  		  ggplot(summaries, aes(x=name, y=Average)) + 
  		    geom_point(stat="identity", size = 15) +
  		    coord_flip()+
  		    #scale_y_continuous(labels = NULL) +
  		    scale_x_discrete(labels = field_labels) +
  		    xlab("Challenge Areas") +
  		    ylab("Average Degree of Difficulty (Very Easy --> Very Difficult)") +
  		    ylim(1,5) +
  		    theme_light()
		  }
		})
		
		output$meanScores_spider <- renderPlot({
		  input$submit
		  if (nrow(loadData()) == 0) {
		    print("empty data")
		    ggplot() + annotate("text", x=1, y=1, size=8, label="Please submit a survey response...") + theme_void()
		  } else {
		    print("non-empty data")
  		  summaries <- loadData() %>% 
  		  pivot_longer(cols=all_of(fields)) %>% 
  		  group_by(name) %>% 
  		  summarize(Average=mean(as.numeric(value), na.rm=TRUE))
  		  summaries$name <- factor(summaries$name, levels=fields)
  		  ggplot(summaries, aes(x=name, y=Average)) + 
  		    geom_col() +
  		    coord_polar()+
  		    #scale_y_continuous(labels = NULL) +
  		    scale_x_discrete(labels = field_labels) +
  		    xlab(NULL) +
  		    #ylab(NULL) +
  		    #ylim(1,5) +
  		    theme_light() +
  		    theme(plot.margin = unit(c(0,1,0,1), "lines"),
  		          panel.border = element_rect(
  		            linetype = 0
  		          ))
		  }
		})
		
		
		output$meanScores <- renderTable({
		  input$submit
		  summaries <- loadData() %>% 
		    pivot_longer(cols=all_of(fields)) %>% 
		    group_by(name) %>% 
		    summarize(n=sum(value != ""), Average=mean(as.numeric(value), na.rm=TRUE)) %>% 
		    arrange(factor(name, levels=fields))
		  #summaries$name <- factor(summaries$name, levels=rev(fields))
		  #summaries <- summaries %>% 
		  #  arrange(name)
		  summaries
		})
		
	}
)