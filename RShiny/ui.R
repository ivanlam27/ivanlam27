#Contributer: Group B2 - Hou Wang (Ivan), Lam; Roman Ramirez; Ananya Kaushik; Arian Veyssi; Aditi Verma
#setting lime to custom colour 
custom_colors_theme <- create_theme(
  bs4dash_color(
    lime = "#76ad7c"
  )
)

dashboardPage(
    freshTheme = custom_colors_theme,
    title = "Basic Dashboard",
    fullscreen = TRUE,
    #Icon and hyperlink, button
    header = dashboardHeader(
      title = dashboardBrand(
        title = "STEM-away Inc.",
        color = "white",
        href = "https://stemaway.com/",
        image = "https://raw.githubusercontent.com/stemaway-repo/stemaway-unified/master/assets/bulb-icon_Large-Icon-branded.svg",
      ),
      skin = "light",
      status = "white",
      border = TRUE,
      sidebarIcon = icon("bars"),
      controlbarIcon = icon("th"),
      fixed = TRUE,
      leftUi = tagList(
      )
    ),
    
    #left sidepanel 
    sidebar = dashboardSidebar(
      skin = "light",
      status = "lime",
      elevation = 3,
      sidebarMenu(
        #changing font size and stating Menu
        sidebarHeader(
          title = span("Menu",
          style = "font-size: 30px; font-weight:bold"
            )
          ),
        #creating all the buttons
        menuItem(
          "About",
          tabName = "about",
          icon = icon("door-open")
        ),
        menuItem(
          "Import data",
          tabName = "importdata",
          icon = icon("file-import")
        ),
        menuItem(
          "Quality control",
          tabName = "qualitycontrol",
          menuSubItem(
            "QC visualisation",
            tabName = "QCvisualisation",
            icon = icon("blank")
          ),
          icon = icon("tools")
        ),
        menuItem(
          "Grouping of samples",
          tabName = "grouping",
          icon = icon("object-group")
        ),
        menuItem(
          "Statistical analysis",
          tabName = "statisticalanalysis",
          icon = icon("calculator")
        ),
        menuItem(
          "Functional Analysis",
          tabName = "functionalanalysis",
          icon = icon("chart-bar")
        ),
        menuItem(
          "Documentation",
          tabName = "documentation",
          icon = icon("sticky-note")
        )
        )
    ),
    footer = dashboardFooter(
      left = a(
        href = "https://stemaway.com/",
        target = "_blank", "@STEM-away Inc."
      ),
      right = "2021 Session-1"
    ),
    #Main page
    body = dashboardBody(
      tabItems(
        #Welcome page
        tabItem(
          tabName = "about",
          title = "About",
          #Introduction block 
          jumbotron(
            status = "info",
            title = "R Shiny app",
            lead = "An app for processing quality control, statistical and functional analysis of a 
            GEO dataset in order to find a potential biomarker",
            "This app is created by STEM-away RShiny project team - Session 1",
            href = "https://stemaway.com/" 
          )
        ),
        #Import data page
        tabItem(
          tabName = "importdata",
          #Selection Input to decide what type of file they want to input: CSV/txt, CEL files, IDAT files, or GEO Accession number
          selectInput("dat_type","Input Type",choices=list("Processed Expression Data (CSV/txt File)","Raw Affymetrix Data (.tar File Containing CEL Files)","Raw Illumina Data (.tar File Containing IDAT Files)","GEO Accession Number")),
          #Panel that appears if CSV/txt file type selected
          conditionalPanel(condition="input.dat_type=='Processed Expression Data (CSV/txt File)'",
                           #ShreyaVora14
                           fileInput("file", "Processed Expression Data (CSV/txt)",multiple=FALSE,accept=c(".csv",".txt")),
                           #Input that comes from server function; takes columns of metadata, asks user which feature to analyze
          ),
          #nk468188
          #Panel that appears if Raw Illumina data selected
          conditionalPanel(condition="input.dat_type=='Raw Illumina Data (.tar File Containing IDAT Files)'",
                           fileInput("zipfile","Raw Illumina Data (.tar)",multiple=FALSE,accept=c(".tar")),
                           
          ),
          #Panel that appears if GEO Database desired
          conditionalPanel(condition="input.dat_type=='GEO Accession Number'",
                           textInput("geoname","Input the GEO accession number of the series matrix or dataset of interest."),
                           radioButtons("GEOtype","Specify what data you wish to use from the GEO database",choices=list("Series Matrix","Raw Data"))    
          ),
          #Panel that appears if Raw Affymetrix data selected
          conditionalPanel(condition="input.dat_type=='Raw Affymetrix Data (.tar File Containing CEL Files)'",
                           fileInput("celzip","Raw Affymetrix Data (.tar)"),
                           #Check if the user used microarray requiring oligo package
                           selectInput("oligo","What microarray platform did you use?",choices=list("Gene ST Array","Exon ST Array","Other")),
          ),
          #Input that comes from server function; takes columns of metadata, asks user which feature to analyze
          fileInput("metadata","Metadata (CSV)"),
          htmlOutput("col_selection"),
          actionButton("loaddat","Load Data")
        ),
        #Quality control page
        tabItem(
          title = "Quality Control",
          tabName = "qualitycontrol",
          tabPanel("Quality Control",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("qc_method", "Choose a QC visualization method before normalization.", choices = c("NUSE","RLE")),
                       actionButton("vis_dat","Visualize Data"),
                       #Disha
                       radioButtons("normlztype","Which normalization method do you want to use?", choices=list("RMA","GCRMA","MAS5")),
                       actionButton("normlzdata","Begin Normalization"),
                       htmlOutput("batch_cat"),
                       actionButton("startbatch","Perform Batch Correction"),
                       selectInput("qc_method2", "Choose a QC visualization method after normalization.", choices = c("Boxplot","PCA")),
                       actionButton("vis_button", "Generate Plot", icon = icon("play")),
                       selectInput("outmethod","Outlier Detection Method",choices=c("KS","sum", "upperquartile")),
                       actionButton("getout","Find Potential Outliers"),
                       actionButton("update","Show Updated List of Samples")
                       
                     ),
                     mainPanel(
                       plotOutput("plot_raw"),
                       textOutput("normal"),
                       textOutput("norm_comp"),
                       textOutput("batch_com"),
                       htmlOutput("pc_comp"),
                       htmlOutput("feat"),
                       actionButton("pcplot","Plot Principal Components"),
                       textOutput("pcwarn"),
                       textOutput("plot_status"),
                       plotOutput("qcplot"),
                       htmlOutput("remove"),
                       p("Potential Outliers:"),
                       uiOutput("potout"),
                       plotOutput("outplot"),
                       dataTableOutput("newexprs")
                     )
                   )
          )
        ),
        tabItem(
          tabName = "QCvisualisation",
          sidebarPanel(
            selectInput("qc_method", "Choose a QC visualization method before normalization.", choices = c("NUSE","RLE")),
            actionButton("vis_dat","Visualize Data"),
            mainPanel(
              plotOutput("plot_raw")
            )
        )
        ),
        #Documenation page
        tabItem(
          title = "documentation",
          tabName = "documentation",
          "Documentation page"
        )
      ), 
      
      uiOutput("obj")
    )
)
