

########################SAMPLE SIZE REQUIRED TO TARGET PRECISE ESTIMATES OF PREDICTIVE PERFORMANCE###################################

#' Minimum sample size for external validation of a clinical prediction model with a continuous outcome
#'
#' Precise estimate of R2val
#'
#'@import gt tidyverse stats scales
#'@param R2val Assumed coefficient of determination
#'@param width Expected width of confidence interval
#'@param alpha Level of Significance
#'
#'@return A html file contains minimum sample size
#'@export
#'@references Archer L, Snell KIE, Ensor J, Hudda MT, Collins GS, Riley RD. Minimum sample size for external validation of a clinical prediction model with a continuous outcome. Stat Med. 2020;40:133-46.
#'
#'@seealso ss_citl, ss_cal_slope , ss_res_var , ss_sens , ss_spec
#'@examples
#'#Eg.To target a 95% confidence interval for R2val that has a
#'#narrow width of about 0.1 i.e SE of R2val will be 0.0255 and
#'#assuming R2val is 0.5.
#'ss_R2val( R2val= 0.5,width=0.1,alpha=0.05 )
#'
#'#for different combinations of parameters
#'ss_R2val( R2val= c(0.6,0.9),width=c( 0.1),alpha=c( 0.01,0.05) )


ss_R2val<-function( R2val , width , alpha){

  n<-c()
  SE<-c()
  sample_size<-c()
  R2<- R2val
  w<-width
  parameters<-expand.grid( R2,w,alpha)
  colnames(parameters)<-c( "R2","w","alpha")
  for (i in 1:nrow(parameters) ) {

    SE[i] = parameters$w[i] / (2* qnorm(parameters$alpha[i]/2 ,lower.tail=FALSE) )

    n[i]=((4*parameters$R2[i])*(1-parameters$R2[i])^2)/(SE[i]^2)

  }

  output <- data.frame( alpha=parameters$alpha,
                        R2=parameters$R2,
                        width=parameters$w ,
                        SE=round(SE,2),
                        sample_size=ceiling(n) )

  return( output%>%gt() %>%
            tab_options(data_row.padding = px(5),heading.title.font.size = "large",
                        heading.subtitle.font.size = "medium",
                        table.background.color = "lightcyan",
                        heading.title.font.weight="bold")%>%
            tab_style(
              locations = cells_column_labels(columns = everything()),
              style     = list(
                cell_borders(sides = "bottom", weight = px(3)),
                cell_text(weight = "bold"))) %>%
            cols_label(alpha="Level of Significance",
                       R2="Expected coefficient of determination (R2)",
                       width="Width of confidence interval" ,
                       SE="Standard error",
                       sample_size="Minimum sample size")%>%
            data_color(columns =sample_size , colors = scales::col_numeric("cyan", domain = NULL))%>%
            tab_header(
              title = "Minimum sample size for external validation of a clinical prediction model with a continuous outcome.",
              subtitle = "Precise estimate of R2 value" )%>%
            tab_source_note( source_note =md( "Reference: Archer L, Snell KIE, Ensor J, Hudda
            MT, Collins GS,Riley RD. Minimum sample size for external validation of a clinical
            prediction model with a continuous outcome. Stat Med. 2020;40:133-46.")) %>%
            cols_width(
              #num ~ px(150),
              ends_with("r") ~ px(500),
              starts_with("date") ~ px(500),
              everything() ~ px(120))%>%
            cols_align( align = "center" )
  )
}


#2.  Precise estimate of CITL(calibration-in-the-large)

#' Minimum sample size for external validation of a clinical prediction model with a continuous outcome
#'
#' Precise estimate of CITL (calibration-in-the-large)
#'
#'@import gt tidyverse stats scales
#'@param varY Variance of the observed Yi
#'@param R2 Expected coefficient of determination (R2) of CITL
#'@param width Expected Width of confidence interval of CITL
#'@param alpha Level of Significance
#'
#'@return A html file contains minimum sample size
#'@export
#'@references Archer L, Snell KIE, Ensor J, Hudda MT, Collins GS, Riley RD. Minimum sample size for external validation of a clinical prediction model with a continuous outcome. Stat Med. 2020;40:133-46.
#'@seealso ss_R2val, ss_cal_slope , ss_res_var , ss_sens , ss_spec
#'@examples
#'
#'#Eg. To target SE of CITL model of 2.55 (width = 10)at 95%
#'#confidence interval , Assuming R2 CITL = R2val = 0.5 and
#'# variance of the observed Yi is 400
#'ss_citl( R2= 0.5,width = 10,alpha = 0.05, varY = 400)
#'
#'#for different combinations of parameters
#'ss_citl( R2= c(0.4,0.7,0.2),width =c( 1.17),alpha = 0.05, varY = 400)


 ss_citl <- function( varY,R2, width,alpha){
  n<-c()
  sample_size<-c()
  SE = width / (2* qnorm(alpha/2 ,lower.tail=FALSE) )
  parameters<-expand.grid( R2,varY,SE,width,alpha)
  colnames(parameters)<-c( "R2","varY","SE","width","alpha")
  for (i in 1:nrow(parameters) ) {

    n[i]=(parameters$varY[i] * (1-parameters$R2[i])) / (parameters$SE[i]^2)

  }


  output <- data.frame( alpha=parameters$alpha,
                        R2=parameters$R2,
                        varY=parameters$varY,
                        width=parameters$w ,
                        SE=round(parameters$SE,2),
                        sample_size=ceiling(n) )

  return( output%>%gt() %>%
            tab_options(data_row.padding = px(5),heading.title.font.size = "large",
                        heading.subtitle.font.size = "medium",
                        table.background.color = "lightcyan",
                        heading.title.font.weight="bold"
            )%>%tab_style(
              locations = cells_column_labels(columns = everything()),
              style     = list(
                #Give a thick border below
                cell_borders(sides = "bottom", weight = px(3)),
                #Make text bold
                cell_text(weight = "bold")
              )
            ) %>%
            cols_label(alpha="Level of Significance",
                       R2="Expected coefficient of determination (R2) of CITL",
                       varY="Variance of the observed Yi",
                       width="Width of confidence interval of CITL" ,
                       SE="Standard Error of CITL",
                       sample_size="Minimum sample size")%>%
            data_color(columns =sample_size , colors = scales::col_numeric("cyan", domain = NULL))%>%
            #tab_style(style = list(cell_fill(color = "lightcyan"), "font-variant: small-caps;" ),locations = cells_body(columns = sample_size))%>%
            tab_header(
              title = "Minimum sample size for external validation of a clinical prediction model with a continuous outcome.",
              subtitle = "Precise estimate of calibration-in-the-large (CITL)" )%>%
            tab_source_note( source_note =md( "Reference: Archer L, Snell KIE, Ensor J, Hudda MT, Collins GS, Riley RD.
                                              Minimum sample size for external validation of a clinical prediction model with a
                                              continuous outcome. Stat Med. 2020;40:133-46.")) %>%
            cols_width(
              #num ~ px(150),
              ends_with("r") ~ px(500),
              starts_with("date") ~ px(500),
              everything() ~ px(120))%>%
            cols_align( align = "center" )

  )
}



#####################################################################################################################
#3. Precise estimate of calibration slope

#' Minimum sample size for external validation of a clinical prediction model with a continuous outcome
#'
#' Precise estimate of calibration slope
#'
#'@import gt tidyverse stats scales
#'@param lambda Anticipated (mis)calibration across the range of predicted values
#'@param R2 Expected coefficient of determination (R2) of the calibration model
#'@param width Width of confidence interval of lambda
#'@param alpha Level of Significance
#'
#'@return A html file contains minimum sample size
#'@export
#'@references Archer L, Snell KIE, Ensor J, Hudda MT, Collins GS, Riley RD. Minimum sample size for external validation of a clinical prediction model with a continuous outcome. Stat Med. 2020;40:133-46.
#'@seealso ss_citl, ss_R2val , ss_res_var , ss_sens , ss_spec
#'@examples
#'
#'#Eg. Sample size to target a 95% confidence interval for 𝜆cal
#'#that has a narrow width ≤ 0.2 (eg, if the calibration slope was 1,
#'# the confidence interval would be 0.9 to 1.1 assuming confidence
#'#intervals derived by 𝜆̂cal ± 1.96SE𝜆̂cal) and assuming R2val = 0.5 .
#'ss_cal_slope( R2= 0.5,width = 0.2,alpha = 0.05, lambda = 1)
#'
#'#for different combinations of parameters
#'ss_cal_slope( R2= c(0.7,0.8),width =c( 1.1,1.2),alpha = 0.05, lambda = 1)



ss_cal_slope<-function( R2,lambda, width,alpha){
  n<-c()
  sample_size<-c()

  SE = width / (2* qnorm(alpha/2 ,lower.tail=FALSE) )
  parameters<-expand.grid( R2,lambda,SE,width,alpha )
  colnames(parameters)<-c( "R2","lambda","SE","width","alpha")
  for (i in 1:nrow(parameters) ) {

    n[i]=((parameters$lambda[i] * (1-parameters$R2[i])) /( (parameters$SE[i]^2)* parameters$R2[i]))+1

  }


  output <- data.frame( alpha=parameters$alpha,
                        R2=parameters$R2,
                        lambda=parameters$lambda,
                        width=parameters$w ,
                        SE=round(parameters$SE, 2 ) ,
                        sample_size=ceiling(n) )

  return( output%>%gt() %>%
            tab_options(data_row.padding = px(5),heading.title.font.size = "large",
                        heading.subtitle.font.size = "medium",
                        table.background.color = "lightcyan",
                        heading.title.font.weight="bold"
            )%>%tab_style(
              locations = cells_column_labels(columns = everything()),
              style     = list(
                #Give a thick border below
                cell_borders(sides = "bottom", weight = px(3)),
                #Make text bold
                cell_text(weight = "bold")
              )
            ) %>%
            cols_label(alpha="Level of Significance",
                       R2="Expected coefficient of determination (R2) of the calibration model",
                       lambda="Anticipated (mis)calibration across the range of predicted values ",
                       width="Width of confidence interval of lambda" ,
                       SE="Standard Error of lambda",
                       sample_size="Minimum sample size")%>%
            data_color(columns =sample_size , colors = scales::col_numeric("cyan", domain = NULL))%>%
            #tab_style(style = list(cell_fill(color = "lightcyan"), "font-variant: small-caps;" ),locations = cells_body(columns = sample_size))%>%
            tab_header(
              title = "Minimum sample size for external validation of a clinical prediction model with a continuous outcome.",
              subtitle = "Precise estimate of calibration model" )%>%
            tab_source_note( source_note =md( "Reference: Archer L, Snell KIE, Ensor J, Hudda MT, Collins GS,
                                              Riley RD. Minimum sample size for external validation of a clinical prediction model
                                              with a continuous outcome. Stat Med. 2020;40:133-46."))%>%
            cols_width(
              #num ~ px(150),
              ends_with("r") ~ px(500),
              starts_with("date") ~ px(500),
              everything() ~ px(120))%>%
            cols_align( align = "center" )
  )
}



#######################################################################################################################################################
#4. Precise estimates of residual variances (small multiplicative margin of error (MMOE) around the true value)

#' Minimum sample size for external validation of a clinical prediction model with a continuous outcome
#'
#' Precise estimates of residual variances (small multiplicative margin of error (MMOE) around the true value)
#'
#'@import gt tidyverse stats scales
#'@param alpha Level of Significance
#'@param max_MOE Multiplicative margin of error
#'
#'@return A html file contains minimum sample size
#'@export
#'@references Archer L, Snell KIE, Ensor J, Hudda MT, Collins GS, Riley RD. Minimum sample size for external validation of a clinical prediction model with a continuous outcome. Stat Med. 2020;40:133-46.
#'@seealso ss_citl, ss_cal_slope , ss_R2val , ss_sens , ss_spec
#'@examples
#'#Eg.Sample size for margin of error of within 10% (1.0 <=MMOE <=1.1)
#'# of the true value at 95% confidence level.
#'ss_res_var( max_MOE=1.1, alpha=0.05)
#'
#'#for different combinations of parameters
#'ss_res_var( max_MOE=c(1.1,1.2,1.3), alpha=c(0.05,0.01))



ss_res_var <-function( alpha, max_MOE){
  sample_size<-c()

  sample_size_MMOE<-function(alpha, max_MOE ){
    i<-1
    repeat {
      i <- i + 1
      MMOE=sqrt(  max((qchisq(p=1-(alpha/2), df=i-1, lower.tail=T)/(i-1)) ,(i-1)/ qchisq(p=alpha/2, df=i-1, lower.tail=T)) )
      if (MMOE < max_MOE) break
      n<-i+1
    }
    return(n)
  }

  parameters<-expand.grid( alpha,max_MOE)

  colnames(parameters)<-c( "alpha","max_MOE")
  n=mapply(sample_size_MMOE,parameters$alpha, parameters$max_MOE)

  output <- data.frame( alpha=parameters$alpha,
                        max_MOE=parameters$max_MOE,
                        sample_size=n )

  return( output%>%gt() %>%
            tab_options(data_row.padding = px(5),heading.title.font.size = "large",
                        heading.subtitle.font.size = "medium",
                        table.background.color = "lightcyan",
                        heading.title.font.weight="bold"
            )%>%tab_style(
              locations = cells_column_labels(columns = everything()),
              style     = list(
                #Give a thick border below
                cell_borders(sides = "bottom", weight = px(3)),
                #Make text bold
                cell_text(weight = "bold")
              )
            ) %>%
            cols_label(alpha="Level of Significance",
                       max_MOE="Multiplicative margin of error",
                       sample_size="Minimum sample size")%>%
            data_color(columns =sample_size , colors = scales::col_numeric("cyan", domain = NULL))%>%
            #tab_style(style = list(cell_fill(color = "lightcyan"), "font-variant: small-caps;" ),locations = cells_body(columns = sample_size))%>%
            tab_header(
              title = "Minimum sample size for external validation of a clinical prediction model with a continuous outcome.",
              subtitle = "Precise estimate of residual variances" )%>%
            tab_source_note( source_note =md( "Reference: Archer L, Snell KIE, Ensor J, Hudda MT, Collins GS,
                                              Riley RD. Minimum sample size for external validation of a clinical prediction model
                                              with a continuous outcome. Stat Med. 2020;40:133-46."))%>%
            cols_width(
              #num ~ px(150),
              ends_with("r") ~ px(500),
              starts_with("date") ~ px(500),
              everything() ~ px(120))%>%
            cols_align( align = "center" )
  )
}

###########################FOR CATEGORICAL OUTCOME############################################################################################################################################################


##1. for sensitivity

#' Minimum sample size for external validation of a clinical prediction model with a dichotomous outcome
#'
#' Sample size required for a given sensitivity
#'
#'@import gt tidyverse stats scales
#'
#'@param alpha Level of Significance
#'@param se Expected sensitivity
#'@param d Maximum margin of error
#'@param prev Population level prevalence
#'
#'@return A html file contains minimum sample size
#'@export
#'@references Arenas-Cavalli JT, Abarca I, Rojas-Contreras M, Bernuy F, Donoso R. Clinical validation of an artificial intelligence-based diabetic retinopathy screening tool for a national health system. Eye (Lond). 2021 Jan 11. doi: 10.1038/s41433-020-01366-0. Epub ahead of print. Erratum in: Eye (Lond). 2021 Jul 23;: PMID: 33432168.
#'@seealso ss_citl, ss_cal_slope , ss_R2val , ss_res_var , ss_spec
#'@examples
#'
#'#Eg .To target 80% sensitivity at 95% confidence level,
#'# maximum margin of error 5% for a precision of 95% and
#'#assuming 30% of the population with particular disease.
#'ss_sens( alpha=0.05, se= 0.8, d=0.05,prev=0.3)
#'
#'#for different combinations of parameters
#'ss_sens( alpha=c(0.05), se=c(0.8 , 0.9), d=c(0.05 , 0.1 ),prev=0.3)


ss_sens<-function( alpha, se, d,prev){
  n<-c()
  sample_size<-c()
  parameters<-expand.grid( alpha,se,d,prev )
  colnames(parameters)<-c(  "alpha","se","d","prev" )
  for ( i  in 1: nrow(parameters)) {


    n[i]= ((( qnorm(parameters$alpha[i]/2 ,lower.tail=FALSE) )^2) *parameters$se[i] * (1-parameters$se[i]))/((parameters$d[i]^2)*(parameters$prev[i]))

  }
  output <- data.frame( alpha=parameters$alpha,
                        max_MOE=parameters$d,
                        se=parameters$se,
                        prev=parameters$prev,
                        sample_size=ceiling(n) )



  return( output%>%gt() %>%
            tab_options(data_row.padding = px(5),heading.title.font.size = "large",
                        heading.subtitle.font.size = "medium",
                        table.background.color = "lightcyan",
                        heading.title.font.weight="bold")%>%
            tab_style(
              locations = cells_column_labels(columns = everything()),
              style     = list(
                cell_borders(sides = "bottom", weight = px(3)),
                cell_text(weight = "bold"))) %>%
            cols_label(alpha="Level of Significance",
                       max_MOE="Maximum margin of error",
                       prev="Population level prevalence" ,
                       se="Expected sensitivity",
                       sample_size="Minimum sample size")%>%
            data_color(columns =sample_size , colors = scales::col_numeric("cyan", domain = NULL))%>%
            tab_header(
              title = "Minimum sample size for external validation of a clinical prediction model with a categorical outcome.",
              subtitle = "Precise estimate of R2 value" )%>%
            tab_source_note( source_note =md("Arenas-Cavalli JT, Abarca I, Rojas-Contreras M, Bernuy F, Donoso R. Clinical
                                             validation of an artificial intelligence-based diabetic retinopathy screening
                                             tool for a national health system. Eye (Lond). 2021 Jan 11.
                                             doi: 10.1038/s41433-020-01366-0. Epub ahead of print. Erratum in: Eye (Lond). 2021 Jul 23;: PMID: 33432168." )) %>%
            cols_width(
              #num ~ px(150),
              ends_with("r") ~ px(500),
              starts_with("date") ~ px(500),
              everything() ~ px(120))%>%
            cols_align( align = "center" )
  )
}



##2. for specificity

#' Minimum sample size for external validation of a clinical prediction model with a dichotomous outcome
#'
#' Sample size required for a given specificity
#'
#'@import gt tidyverse stats scales
#'
#'@param alpha Level of Significance
#'@param sp Expected specificity
#'@param d Maximum margin of error
#'@param prev Population level prevalence
#'
#'@return A html file contains minimum sample size
#'@export
#'@references Arenas-Cavalli JT, Abarca I, Rojas-Contreras M, Bernuy F, Donoso R. Clinical validation of an artificial intelligence-based diabetic retinopathy screening tool for a national health system. Eye (Lond). 2021 Jan 11. doi: 10.1038/s41433-020-01366-0. Epub ahead of print. Erratum in: Eye (Lond). 2021 Jul 23;: PMID: 33432168.
#'@seealso ss_citl, ss_cal_slope , ss_res_var , ss_R2val , ss_sens
#'@examples
#'
#'#Eg .To target 50% specificity at 95% confidence level,
#'# maximum margin of error 5% for a precision of 95% and
#'# assuming 30% of the population with particular disease.
#'ss_spec( alpha=0.05, sp= 0.5, d=0.05,prev=0.3)
#'
#'#for different combinations of parameters
#'ss_spec( alpha=c(0.05), sp=c(0.8 , 0.9), d=c(0.05 , 0.1 ),prev=0.3)



ss_spec<-function( alpha, sp, d,prev){
  n<-c()
  sample_size<-c()
  parameters<-expand.grid( alpha,sp,d,prev )
  colnames(parameters)<-c(  "alpha","sp","d","prev" )
  for ( i  in 1: nrow(parameters)) {


    n[i]= ((( qnorm(parameters$alpha[i]/2 ,lower.tail=FALSE) )^2) *parameters$sp[i] * (1-parameters$sp[i]))/((parameters$d[i]^2)*(1-parameters$prev[i]))

  }
  output <- data.frame( alpha=parameters$alpha,
                        max_MOE=parameters$d,
                        se=parameters$sp,
                        prev=parameters$prev,
                        sample_size=ceiling(n) )



  return( output%>%gt() %>%
            tab_options(data_row.padding = px(5),heading.title.font.size = "large",
                        heading.subtitle.font.size = "medium",
                        table.background.color = "lightcyan",
                        heading.title.font.weight="bold")%>%
            tab_style(
              locations = cells_column_labels(columns = everything()),
              style     = list(
                cell_borders(sides = "bottom", weight = px(3)),
                cell_text(weight = "bold"))) %>%
            cols_label(alpha="Level of Significance",
                       max_MOE="Maximum margin of error",
                       prev="Population level prevalence" ,
                       se="Expected specificity",
                       sample_size="Minimum sample size")%>%
            data_color(columns =sample_size , colors = scales::col_numeric("cyan", domain = NULL))%>%
            tab_header(
              title = "Minimum sample size for external validation of a clinical prediction model with a categorical outcome.",
              subtitle = "Precise estimate of R2 value" )%>%
            tab_source_note( source_note =md( "Arenas-Cavalli JT, Abarca I, Rojas-Contreras M, Bernuy F, Donoso R. Clinical validation of an artificial intelligence-based diabetic retinopathy screening tool for a national health system. Eye (Lond). 2021 Jan 11. doi: 10.1038/s41433-020-01366-0. Epub ahead of print. Erratum in: Eye (Lond). 2021 Jul 23;: PMID: 33432168.")) %>%
            cols_width(
              #num ~ px(150),
              ends_with("r") ~ px(500),
              starts_with("date") ~ px(500),
              everything() ~ px(120))%>%
            cols_align( align = "center" )
  )
}




