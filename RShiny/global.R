#Group B1's code for importing files, including some lines of code from Group A members

#Increase maximum upload size
options(shiny.maxRequestSize = 100000000000000000000000000000000000000000000000000000000000)

#Install and load needed packages
packages1<-c("oligo","GEOquery","affy","limma","arrayQualityMetrics","sva","Biobase","affyPLM")
for(x in packages1){
  if(!(x %in% rownames(installed.packages()))){
    BiocManager::install(x,dependencies=TRUE)
  }
  library(x,character.only=TRUE)
}

packages2<-c("ggplot2", "pheatmap")
for(y in packages2){
  if(!(y %in% rownames(installed.packages()))){
    install.packages(y,dependencies=TRUE)
  }
  library(y,character.only=TRUE)
}

packages12<-c("stringr","R.utils", "EnhancedVolcano", "shinyWidgets", "bs4Dash", "shiny", "fresh")
for(y in packages12){
  if(!(y %in% rownames(installed.packages()))){
    install.packages(y,dependencies=TRUE)
  }
  library(y,character.only=TRUE)
}

qcoutlierserver<-function(id,exprset){
  moduleServer(id,
               function(input,output,session){
                 expr_matrix<-reactive({as.matrix(exprset())})
                 observeEvent(input$getout,{
                   outlier_affy<-outliers(expr_matrix,method=input$outmethod)
                   output$potout<-renderUI(names(outlier_affy@which))
                   values<-outlier_affy@statistic
                   dat_fram<-data.frame(colnames(expr_matrix()),values)
                   p<-ggplot(data=dat_fram,aes(x=dat_fram[,1],y=dat_fram[,2]))+geom_col()+geom_hline(yintercept=outlier_affy@threshold)+ggtitle("Potential Outliers")+labs(y="Value of Selected Statistic",x="Sample")+theme(axis.text.x=element_text(size=7))
                   output$outplot<-renderPlot(p)
                   output$remove<-renderUI(selectInput("torem","Select outlier candidates you would like to remove.",multiple=TRUE,options=names(outlier_affy@which)))
                   observeEvent(input$update,{
                     expr_mat_2<-expr_matrix()
                     for(name in input$torem){
                       sample_names<-colnames(expr_mat_2)
                       ind_to_remove<-which(sample_names==name)
                       expr_mat_2<-expr_mat_2()[,-ind_to_remove]
                     }
                     output$newexprs<-renderTable(expr_mat_2())
                   })
                 })
               }
  )
}