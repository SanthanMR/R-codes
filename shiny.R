library(shiny)
library(shinydashboard)
library(corrplot)
library(tidyverse)

df <- read.csv('C:/Users/santh/Desktop/diabetes.csv')

for(j in 2:8)
{
  for (i in 1:nrow(df))
  {
    if(df[i,j] == 0)
    {
      df[i,j]= round(mean(df[,j]))
    }
  }
}

ui <- dashboardPage( skin = "purple",
  dashboardHeader(title = "EDA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Scatter plot", tabName = "scatter", icon = icon("braille")),
      menuItem("Co-relation plot", tabName = "corr", icon = icon("table")),
      menuItem("Box plot", tabName = "box", icon = icon("square")),
      menuItem("Density", tabName = "density", icon = icon("chart-area")),
      menuItem("Histogram", tabName = "hist", icon = icon("chart-bar")),
      menuItem("Pie-chart", tabName = "pie", icon = icon("chart-pie"))
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("scatter",
              h1("Scatter plot"),
              box(selectInput("features0","Features",
                              c(
                                "Glucose",
                                "BloodPressure",
                                "SkinThickness",
                                "Insulin",
                                "BMI",
                                "DiabetesPedigreeFunction",
                                "Age",
                                "Pregnancies"
                              )),width = 4),
              box(selectInput("features1","Features",
                              c(
                                "Glucose",
                                "BloodPressure",
                                "SkinThickness",
                                "Insulin",
                                "BMI",
                                "DiabetesPedigreeFunction",
                                "Age",
                                "Pregnancies"
                              )),width = 4),
              box(selectInput("op0","factor",
                              list(
                                "TRUE"=1,
                                "FALSE"=0
                              )),width=4),
              box(plotOutput("scatter"), width = 12)
            ),
      
      tabItem("corr",
              h1("Co-relation plot"),
              box(plotOutput("corr_plot"), width = 12)
              ),
      
      tabItem("box",
              h1("Box Plot"),
              box(selectInput("features2","Features",
                              c(
                                "Glucose",
                                "BloodPressure",
                                "SkinThickness",
                                "Insulin",
                                "BMI",
                                "DiabetesPedigreeFunction",
                                "Age",
                                "Pregnancies"
                              )),width = 4),
              box(selectInput("op1","factor",
                              list(
                                "TRUE"=1,
                                "FALSE"=0
                              )),width=4),
              box(plotOutput("box_plot"), width = 12)
              ),
      tabItem("density",
              h1("Box Plot"),
              box(selectInput("features3","Features",
                              c(
                                "Glucose",
                                "BloodPressure",
                                "SkinThickness",
                                "Insulin",
                                "BMI",
                                "DiabetesPedigreeFunction",
                                "Age",
                                "Pregnancies"
                              )),width = 4),
              box(selectInput("op2","factor",
                              list(
                                "TRUE"=1,
                                "FALSE"=0
                              )),width=4),
              box(plotOutput("density_plot"), width = 12)
            ),
      tabItem("hist",
              h1("Histogram"),
              box(selectInput("features4","Features",
                              c(
                                "Glucose",
                                "BloodPressure",
                                "SkinThickness",
                                "Insulin",
                                "BMI",
                                "DiabetesPedigreeFunction",
                                "Age"
                              )),width = 4),
              box(plotOutput("histogram"), width = 12)
            ),
      tabItem("pie",
              h1("Pie-chart"),
              box(plotOutput("pie_plot"), width = 12)
      )
    
      
      
      )
  )
)

server <- function(input,output)
{
  output$scatter <- renderPlot(
    if(input$op0==1)
    {
      ggplot(data = df, aes(x=df[,input$features0],y=df[,input$features1],col=factor(Outcome)))+geom_point()+labs(caption = "Copyrights @20234")+xlab(input$features0)+ylab(input$features1)+ggtitle("Scatter plot")
    }
    else
    {
      ggplot(data = df, aes(x=df[,input$features0],y=df[,input$features1]))+geom_point()+labs(caption = "Copyrights @20234")+xlab(input$features0)+ylab(input$features1)+ggtitle("Scatter plot")
    }
    )
  output$corr_plot <- renderPlot(corrplot(cor(df),method = "color"))
  output$box_plot <- renderPlot(
    if(input$op1==1)
    {
      ggplot(df,aes(df[,input$features2],fill = factor(Outcome)))+geom_boxplot()+labs(caption = "Copyrights @20234")+ggtitle("Box plot")+xlab(input$features2)
    }
    else
    {
      ggplot(df,aes(df[,input$features2]))+geom_boxplot()+labs(caption = "Copyrights @20234")+ggtitle("Box plot")+xlab(input$features2)
    }
  )
  output$density_plot <- renderPlot(
    if(input$op2==1)
    {
      ggplot(data = df, aes(x=df[,input$features3],fill= factor(Outcome)))+geom_density()+xlab(input$features3)+labs(caption = "Copyrights @20234")+ggtitle("Density Plot")
    }
    else
    {
      ggplot(data = df, aes(x=df[,input$features3]))+geom_density()+xlab(input$features3)+labs(caption = "Copyrights @20234")+ggtitle("Density Plot")
    }
    )
  output$histogram <- renderPlot(ggplot(df,aes(df[,input$features4]))+geom_histogram(bins=40,col="purple",aes(y=..density..,fill=..count..)) +scale_fill_gradient("Count",low="turquoise",high="violet")+geom_density())
  output$pie_plot <- renderImage(
                    {
                    imgname=normalizePath(file.path("./pie.png")) 
                    list(src=imgname)
                    },
                    deleteFile=FALSE)
}

shinyApp(ui,server)