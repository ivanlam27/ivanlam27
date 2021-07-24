source("./global.R", local = TRUE)
source("./ui.R", local = TRUE)

function(input,output,session){
  #nk468188
  #Code for uploading Idat files
  #returns null if nothing uploaded
  unzi<-reactive({
    if(is.null(input$zipfile$datapath)){
      return(NULL)
    }
    
    #otherwise reads in the data path
    else{uni<-untar(input$zipfile$datapath,list=TRUE,exdir=getwd())
    return(uni)}})
  bgx_file<-reactive({
    if(is.null(input$zipfile$datapath)){
      return(NULL)
    }
    #as long as file input not empty, goes through for loop to find bgx file out of zipped files
    else{ bg<-c()
    for(file_index in 1:length(unzi())){
      if(grepl("bgx",unzi()[file_index],fixed=TRUE)==TRUE){
        bg[1]<-unzi()[file_index]
      }
    }
    return(bg[1])}})
  
  #code for storing all of the files from the tar zipped file besides the bgx file (basically storing all IDAT files with this code chunk)
  unzi2<-reactive({
    unzizi<-c()
    if(is.null(input$zipfile$datapath)){
      return(NULL)
    }
    else{for(file_index in 1:length(unzi())){
      if(grepl("bgx",unzi()[file_index],fixed=TRUE)==FALSE){
        unzizi[length(unzizi)+1]<-unzi()[file_index]
      }
    }
      return(unzizi)
    }})
  
  #After hitting button, reads in using read.idat function and taking list of IDAT and bgx file as inputs
  observeEvent(input$loaddat,{
    expression_object_raw<-NULL
    if(length(unzi2())!=0){
      if(grepl('.gz',unzi2()[1])){
        lapply(unzi2(),gunzip())
      }
      expression_object_raw<-read.idat(unzi2(),bgx_file())}
  })
  
  #Code for using GEO accession number
  geo_data<-reactive({
    if(input$geoname==""){
      return(NULL)
    }
    
    #If user wants series matrix data, uses getGEO function
    else if(input$GEOtype=="Series Matrix"){
      return(getGEO(input$geoname)[[1]])
    }
    
    #If user wants raw data, gets link to file and untar
    else if(input$GEOtype=="Raw Data"){
      return(ReadAffy(filenames=untar(rownames(getGEOSuppFiles(input$geoname)),list=TRUE)))
    }
  })
  
  
  #metadata input
  meet<-reactive({
    if(is.null(input$metadata)){
      return(NULL)
    }
    else if(!(input$geoname=="") && input$GEOtype=="Series Matrix"){
      metadat<-pData(geo_data())
      names_meta<-colnames(metadat)
      new_metadat<-as.matrix(rownames(metadat))
      names_vec<-c("Sample")
      for(name_ind in 1:length(names_meta)){
        if(grepl("characteristics",names_meta[name_ind])){
          new_metadat<-cbind(new_metadat,as.vector(metadat[,name_ind]))
          first_val<-metadat[,name_ind][1]
          ind_colon<-gregexpr(pattern=":",first_val)
          ind_colon<-unlist(ind_colon)
          namey<-substr(first_val,1,ind_colon-1)
          names_vec[length(names_vec)+1]<-namey
        }
      }
      new_metadat<-as.data.frame(new_metadat)
      colnames(new_metadat)<-names_vec
      return(new_metadat)
    }
    else{
      return(read.csv(input$metadata$datapath))
    }
  })
  
  selections<-reactive({
    selections<-colnames(meet())[-1]
  })
  
  #nk468188
  #Adds to UI features of samples from metadata, user can select which features they wish to compare based on
  output$col_selection<-renderUI({
    selectInput("col_int","Select the feature you wish to analyze further",choices=colnames(meet()))
  })
  
  
  
  #Reading in CEL files from tar zipped user upload
  celdat<-reactive({
    if(is.null(input$celzip$datapath)){
      return(NULL)
    }
    if(input$oligo=="Gene ST Array" || input$oligo=="Exon ST Array"){
      
      #sneha-mr
      #function to be used if user has chip that requires oligo package
      return(read.celfiles(untar(input$celzip$datapath,list=TRUE)))
    }
    
    #nk468188
    #if using any other chip, read in using affy package
    #nk468188 and Modupe001
    else{
      taro<-untar(input$celzip$datapath,list=TRUE,exdir=tempdir())
      return(ReadAffy(celfile.path=tempdir()))
    }
  })
  
  #ShreyaVora14
  #reading in txt and cSV data
  data<-reactive({
    file1<- input$file
    if(is.null(file1)){return()}
    if(grepl("\\.txt$", file1)[1]){
      return(read.delim(file = file1$datapath, sep = input$sep, header = input$header, stringsAsFactors = input$stringAsFactors))
    }
    else{
      return(read.csv(file = file1$datapath, sep = input$sep, header = input$header, stringsAsFactors = input$stringAsFactors))
    }
  })
  
  observeEvent(input$loaddat,output$obj<-renderText({
    if(is.null(data()) && is.null(celdat()) && is.null(meet)==FALSE && is.null(geo_data)){
      return("Please input raw or preprocessed expression data.")
    }
    else if(is.null(data())==FALSE && is.null(celdat()) && is.null(meet)==FALSE && is.null(geo_data())){
      return("Your data has been loaded.")
    }
    else if(is.null(data()) && is.null(celdat())==FALSE && is.null(meet)==FALSE && is.null(geo_data())){
      return("Your data has been loaded.")
    }
    else if(is.null(data()) && is.null(celdat()) && is.null(meet)==FALSE && is.null(geo_data())==FALSE){
      return("Your data has been loaded.")
    }
    else if(is.null(meet())){
      return("Please upload metadata.")
    }
    else if(is.null(data()) && is.null(celdat()) && is.null(meet) && is.null(geo_data)){
      return("Please upload expression data and metadata.")
    }
    else{
      return("Your data has been loaded.")
    }
  }))
  
  
  observeEvent(input$normlzdata,{output$normal<-{
    if(is.null(celdat()) && is.null(geo_data())){
      renderText("Normalization only needed for CEL Files")
    }
    else{
      if(input$normlztype=="RMA" && is.null(geo_data()) && input$oligo=="Other"){
        renderText("Starting RMA Normalization")
      }
      else if(input$normlztype=="GCRMA" && is.null(geo_data()) && input$oligo=="Other"){
        renderText("Starting GCRMA Normalization")
      }
      else if(input$normlztype=="MAS5" && is.null(geo_data()) && input$oligo=="Other"){
        renderText("Starting MAS5 Normalization")
      }
      
      else if(input$normlztype=="RMA" && is.null(celdat()) && input$GEOtype=="Raw Data"){
        renderText("Starting RMA Normalization")
      }
      
      else if(input$normlztype=="GCRMA" && is.null(celdat()) && input$GEOtype=="Raw Data"){
        renderText("Starting GCRMA Normalization")
      }
      else if(input$normlztype=="MAS5" && is.null(celdat()) && input$GEOtype=="Raw Data"){
        renderText("Starting MAS5 Normalization")
      }
      else if(input$normlztype=="RMA" && is.null(geo_data()) && (input$oligo=="Gene ST Array" || input$oligo=="Exon ST Array")){
        renderText("Starting RMA Normalization")
      }
      else if(input$normlztype=="GCRMA" && is.null(geo_data()) && (input$oligo=="Gene ST Array" || input$oligo=="Exon ST Array")){
        renderText("You can only conduct RMA normalization for a data from a Gene ST Array or Exon ST Array.")
      }
      else if(input$normlztype=="MAS5" && is.null(geo_data()) && (input$oligo=="Gene ST Array" || input$oligo=="Exon ST Array")){
        renderText("You can only conduct RMA normalization for a data from a Gene ST Array or Exon ST Array.")
      }
    }
    
  }})
  
  
  final_qc_dat<-reactiveVal()
  
  observeEvent(input$normlzdata,
               {
                 if(is.null(celdat()) && is.null(geo_data())){
                   final_qc_dat(NULL)
                 }
                 else{
                   if(input$normlztype=="RMA" && is.null(geo_data()) && input$oligo=="Other"){
                     norm_affy<-exprs(affy::rma(celdat()))
                     final_qc_dat(norm_affy)
                     output$norm_comp<-renderText("Normalization is complete.")
                   }
                   else if(input$normlztype=="GCRMA" && is.null(geo_data()) && input$oligo=="Other"){
                     norm_affy<-exprs(gcrma(celdat()))
                     final_qc_dat(norm_affy)
                     output$norm_comp<-renderText("Normalization is complete.")
                   }
                   else if(input$normlztype=="MAS5" && is.null(geo_data()) && input$oligo=="Other"){
                     norm_affy<-log(exprs(mas5(celdat())),2)
                     final_qc_dat(norm_affy)
                     output$norm_comp<-renderText("Normalization is complete.")
                   }
                   
                   else if(input$normlztype=="RMA" && is.null(celdat()) && input$GEOtype=="Raw Data"){
                     norm_affy<-exprs(affy::rma(geo_data()))
                     final_qc_dat(norm_affy)
                     output$norm_comp<-renderText("Normalization is complete.")
                   }
                   
                   else if(input$normlztype=="GCRMA" && is.null(celdat()) && input$GEOtype=="Raw Data"){
                     norm_affy<-exprs(gcrma(geo_data()))
                     final_qc_dat(norm_affy)
                     output$norm_comp<-renderText("Normalization is complete.")
                   }
                   else if(input$normlztype=="MAS5" && is.null(celdat()) && input$GEOtype=="Raw Data"){
                     norm_affy<-log(exprs(mas5(geo_data())),2)
                     final_qc_dat(norm_affy)
                     output$norm_comp<-renderText("Normalization is complete.")
                   }
                   else if(input$normlztype=="RMA" && is.null(geo_data()) && (input$oligo=="Gene ST Array" || input$oligo=="Exon ST Array")){
                     norm_affy<-exprs(oligo::rma(celdat()))
                     final_qc_dat(norm_affy)
                     output$norm_comp<-renderText("Normalization is complete.")
                   }
                   else if(input$normlztype=="GCRMA" && is.null(geo_data()) && (input$oligo=="Gene ST Array" || input$oligo=="Exon ST Array")){
                     norm_affy<-NULL
                     final_qc_dat(norm_affy)
                   }
                   else if(input$normlztype=="MAS5" && is.null(geo_data()) && (input$oligo=="Gene ST Array" || input$oligo=="Exon ST Array")){
                     norm_affy<-NULL
                     final_qc_dat(norm_affy)
                   }
                 }
                 
               })
  
  
  output$batch_cat<-renderUI({
    selectInput("batch_feat","If samples come from different batches, specify which metadata feature indicates the batch each sample belongs to.",choices=colnames(meet()))
  })
  
  observeEvent(input$startbatch,{
    batch_cor_dat<-ComBat(final_qc_dat(),batch=meet()[,input$batch_feat])
    final_qc_dat(batch_cor_dat)
    output$batch_com<-renderText("Batch Correction is complete.")
  }
  )
  
  
  observeEvent(input$vis_dat,output$plot_raw<-renderPlot({
    if(input$qc_method=="RLE" && input$oligo=="Other"){
      affy.data=fitPLM(celdat())
      RLE(affy.data,main="RLE")
    }
    else if(input$qc_method=="NUSE" && input$oligo=="Other"){
      affy.data=fitPLM(celdat())
      NUSE(affy.data,main="NUSE")
    }
    
    else if(input$qc_method=="RLE" && (input$oligo=="Gene ST Array" || input$oligo=="Exon ST Array")){
      oligo.data=oligo::fitProbeLevelModel(celdat())
      RLE(oligo.data,main="RLE")
    }
    
    else if(input$qc_method=="NUSE"&& (input$oligo=="Gene ST Array" || input$oligo=="Exon ST Array")){
      oligo.data=oligo::fitProbeLevelModel(celdat())
      NUSE(oligo.data,main="RLE")
    }
    
  }))
  
  observeEvent(input$vis_button,{
    if(input$qc_method2=="Boxplot"){
      output$plot_status<-renderText("Your plot is being generated.")
      output$qcplot<-renderPlot({
        boxplot(final_qc_dat(),xlab="Sample Number",ylab="Gene Expression Values",main="Boxplot of Gene Expression for Each Sample",cex.axis=0.1,las=3)
      })
    }
    else if(input$qc_method2=="PCA"){
      pcacomps<-prcomp(final_qc_dat(),center=FALSE,scale=FALSE)
      comps<-pcacomps$rotation
      output$pc_comp<-renderUI({
        selectInput("comp_plot","Which components do you want to plot?",choices=colnames(comps),multiple=TRUE)
      })
      output$feat<-renderUI({
        selectInput("feat_color","Which feature do you want to group samples by?",choices=colnames(meet()))
      })
      
    }
  })
  
  observeEvent(input$pcplot,{
    if(length(input$comp_plot)>2){
      output$pcwarn<-renderText("Please only select two principal components.")
    }
    else if (length(input$comp_plot)<2){
      output$pcwarn<-renderText("Please select two principal components.")
    }
    if(is.null(input$feat_color)){
      output$pcwarn<-renderText("Please specify a feature to group samples by.")
    }
    else{
      output$pcwarn<-NULL
      pcacomps1<-prcomp(final_qc_dat(),center=FALSE,scale=FALSE)
      comps1<-pcacomps1$rotation
      input_comp<-as.vector(input$comp_plot)
      pcs<-comps1[,input_comp]
      pc1<-pcs[,1]
      pc2<-pcs[,2]
      colors<-meet()[,input$feat_color]
      data_to_plot<-data.frame(pc1,pc2,colors)
      p<-ggplot(data_to_plot,aes(x=pc1,y=pc2,color=colors))
      p<-p+geom_point()+labs(color=input$feat_color)+ggtitle("PCA Plot for Normalized Data")+xlab("PC1")+ylab("PC2")
      output$qcplot<-renderPlot(p)
    }
  })
  
  observeEvent(input$getout,{
    outlier_affy<-outliers(final_qc_dat(),method=as.vector(input$outmethod))
    output$potout<-renderUI(names(as.vector(outlier_affy@which)))
    values<-outlier_affy@statistic
    dat_fram<-data.frame(colnames(final_qc_dat()),values)
    p<-ggplot(data=dat_fram,aes(x=dat_fram[,1],y=dat_fram[,2]))+geom_col()+geom_hline(yintercept=outlier_affy@threshold)+ggtitle("Potential Outliers")+labs(y="Value of Selected Statistic",x="Sample")+theme(axis.text.x=element_text(size=7))
    output$outplot<-renderPlot(p)
    output$remove<-renderUI(selectInput("torem","Select outlier candidates you would like to remove.",multiple=TRUE,choices=as.list(names(outlier_affy@which))))
    observeEvent(input$update,{
      expr_mat_2<-final_qc_dat()
      for(name in input$torem){
        sample_names<-colnames(expr_mat_2)
        ind_to_remove<-which(sample_names==name)
        expr_mat_2<-expr_mat_2[,-ind_to_remove]
      }
      output$newexprs<-renderDataTable(expr_mat_2)
    })
  })
}