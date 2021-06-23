library(affy)
library(shiny)
library(affyQCReport)
library(simpleaffy)
library(GEOquery)
library(bslib)

metadata <- getGEO(filename = "GSE19804_series_matrix.txt")
data <- metadata@phenoData@data
selected <- select(data, c(1,2,10,11))



ui <- fluidPage(
  theme = bs_theme(bg = "#e1e5eb",
                   fg = "#46474a",
                   base_font = "Arial"),
  titlePanel("GEO data - App demo"),
  navlistPanel(
    id = "tabset",
    "1",
    tabPanel("panel 1", "First panel",
             
             selectInput("CN", "Cancer or Normal", choices = unique(selected$characteristics_ch1)),
             
             dataTableOutput("table")),
    "2",
    tabPanel("panel 2", "second panel")
  )
)


server <- function(input, output, session){
  output$table <- renderDT({
    reactive(input$CN)
    dat <- selected[selected$characteristics_ch1 == input$CN,]
    datatable(dat,extensions = c('Responsive'), class = 'cell-border stripe',
              options = list(pageLength = 5,responsive = TRUE)
    )
  })
}
shinyApp(ui, server)