#install.packages("https://github.com/carlanetto/M4comp2018/releases/download/0.2.0/M4comp2018_0.2.0.tar.gz",repos=NULL)
#install.packages('rlist')
#install.packages('forecast')
#install.packages('tidyverse')

library('rlist')
library('forecast')
library('ggplot2')
library('tidyverse')
library('M4comp2018')

####### MINI COMPETITION TAE - DATA GENERATING PROCESS ####### 

?M4
data(M4)

yearly_demographic_M4 <- Filter(function(l) l$period == "Yearly" & l$type == "Demographic",M4)
yearly_finance_M4 <- Filter(function(l) l$period == "Yearly" & l$type == "Finance",M4)
yearly_industry_M4 <- Filter(function(l) l$period == "Yearly" & l$type == "Industry",M4)
yearly_macro_M4 <- Filter(function(l) l$period == "Yearly" & l$type == "Macro",M4)
yearly_micro_M4 <- Filter(function(l) l$period == "Yearly" & l$type == "Micro",M4)
yearly_other_M4 <- Filter(function(l) l$period == "Yearly" & l$type == "Other",M4)

seq1 <- seq(1:1000)

yearly_demographic_train <- list()
yearly_demographic_test <- list()

yearly_finance_train <- list()
yearly_finance_test <- list()

yearly_industry_train <- list()
yearly_industry_test <- list()

yearly_macro_train <- list()
yearly_macro_test <- list()

yearly_micro_train <- list()
yearly_micro_test <- list()

yearly_other_train <- list()
yearly_other_test <- list()

for(i in seq1){
  yearly_demographic_train <- list.append(yearly_demographic_train,yearly_demographic_M4[[i]]$x)
  yearly_demographic_test <- list.append(yearly_demographic_test,yearly_demographic_M4[[i]]$xx)
  
  yearly_finance_train <- list.append(yearly_finance_train,yearly_finance_M4[[i]]$x)
  yearly_finance_test <- list.append(yearly_finance_test,yearly_finance_M4[[i]]$xx)
  
  yearly_industry_train <- list.append(yearly_industry_train,yearly_industry_M4[[i]]$x)
  yearly_industry_test <- list.append(yearly_industry_test,yearly_industry_M4[[i]]$xx)
  
  yearly_macro_train <- list.append(yearly_macro_train,yearly_macro_M4[[i]]$x)
  yearly_macro_test <- list.append(yearly_macro_test,yearly_macro_M4[[i]]$xx)
  
  yearly_micro_train <- list.append(yearly_micro_train,yearly_micro_M4[[i]]$x)
  yearly_micro_test <- list.append(yearly_micro_test,yearly_micro_M4[[i]]$xx)
  
  yearly_other_train <- list.append(yearly_other_train,yearly_other_M4[[i]]$x)
  yearly_other_test <- list.append(yearly_other_test,yearly_other_M4[[i]]$xx)
  
}

####### MINI COMPETITION TAE - BENCHMARK PROVIDED BY M4 #######

setwd('/Users/gilbe/Desktop/SoSe23/Topics AE/Presentation_M4 Competition')
source('m4_benchmarkmodels.R')

Names_benchmarks <- c("Naive1", "sNaive", "Naive2", "SES", "Holt", "Damped", "Theta", "Comb")
Names_competition <- c("ETS","ARIMA","NN")

get_performance_measures<-function(data_train_per_area,data_test_per_area){
  
  data_train <- data_train_per_area
  data_test <- data_test_per_area
  
  Total_smape_benchmark=Total_mase_benchmark <- array(NA,dim = c(length(Names_benchmarks), fh, length(data_train)))
  Total_smape_competition=Total_mase_competition <- array(NA,dim = c(length(Names_competition), fh, length(data_train)))
  
  for (i in 1:length(data_train)){
    
    insample <- data_train[[i]]
    outsample <- data_test[[i]]
    
    #fit benchmark models provided by the authors
    forecasts_benchmark <- Benchmarks(input=insample, fh=fh)
    
    # fit ETS model
    ets_model <- ets(insample)
    frc.temp_ets	<- forecast(ets_model, h = fh, level = c(80, 95))
    
    # fit ARIMA model
    arima_model <- auto.arima(insample)
    frc.temp_arima <- forecast(arima_model, h = fh)
    
    # fit NN
    nn_model <- nnetar(insample,repeats = 10)
    frc.temp_nn <- forecast(nn_model, h = fh)
    
    #sMAPE benchmark
    for (j in 1:length(Names_benchmarks)){
      Total_smape_benchmark[j,,i] <- smape_cal(outsample, forecasts_benchmark[[j]])
    }
    
    #MASE benchmark
    for (j in 1:length(Names_benchmarks)){
      Total_mase_benchmark[j,,i] <- mase_cal(insample, outsample, forecasts_benchmark[[j]]) 
    }
    
    #sMAPE competition
    Total_smape_competition[1,,i] <- smape_cal(outsample, frc.temp_ets$mean) 
    Total_smape_competition[2,,i] <- smape_cal(outsample, frc.temp_arima$mean)
    Total_smape_competition[3,,i] <- smape_cal(outsample, frc.temp_nn$mean)

    #MASE competition
    Total_mase_competition[1,,i] <- mase_cal(insample, outsample, frc.temp_ets$mean)
    Total_mase_competition[2,,i] <- mase_cal(insample, outsample, frc.temp_arima$mean)
    Total_mase_competition[3,,i] <- mase_cal(insample, outsample, frc.temp_nn$mean)

    print(paste("Time series",i,"fitted."))
    }
  
  SMAPE_benchmark<- array()
  MASE_benchmark<- array()
  OWA_benchmark<- array()
  
  SMAPE_competition<- array()
  MASE_competition<- array()
  OWA_competition<- array()
   
  for (i in 1:length(Names_benchmarks)){
    SMAPE_benchmark[i] <- round(mean(Total_smape_benchmark[i,,]), 3)
    MASE_benchmark[i] <- round(mean(Total_mase_benchmark[i,,]), 3)
    # third row of benchmark matrix is Naive 2, which is used to calc. OWA
    OWA_benchmark[i] <- round(((mean(Total_mase_benchmark[i,,])/mean(Total_mase_benchmark[3,,]))+(mean(Total_smape_benchmark[i,,])/mean(Total_smape_benchmark[3,,])))/2, 3)
  }
  
  for (i in 1:length(Names_competition)){
    SMAPE_competition[i] <- round(mean(Total_smape_competition[i,,]), 3)
    MASE_competition[i] <- round(mean(Total_mase_competition[i,,]), 3)
    # third row of benchmark matrix is Naive 2, which is used to calc. OWA
    OWA_competition[i] <- round(((mean(Total_mase_competition[i,,])/mean(Total_mase_benchmark[3,,]))+(mean(Total_smape_competition[i,,])/mean(Total_smape_benchmark[3,,])))/2, 3)
  }
  
  names(SMAPE_benchmark)<-Names_benchmarks
  names(MASE_benchmark)<-Names_benchmarks
  names(OWA_benchmark)<-Names_benchmarks
  
  names(SMAPE_competition)<-Names_competition
  names(MASE_competition)<-Names_competition
  names(OWA_competition)<-Names_competition

  return(list(SMAPE_benchmark,
              MASE_benchmark,
              OWA_benchmark,
              SMAPE_competition,
              MASE_competition,
              OWA_competition))
}

start <- Sys.time()
performance_scores_yearly_demopgrahic <-get_performance_measures(yearly_demographic_train,yearly_demographic_test)
performance_scores_yearly_finance <-get_performance_measures(yearly_finance_train,yearly_finance_test)
performance_scores_yearly_industry <-get_performance_measures(yearly_industry_train,yearly_industry_test)
performance_scores_yearly_macro <-get_performance_measures(yearly_macro_train,yearly_macro_test)
perfromance_scores_yearly_micro <-get_performance_measures(yearly_micro_train,yearly_micro_test)
performance_scores_yearly_other <-get_performance_measures(yearly_other_train,yearly_other_test)
print(Sys.time() - start)

data_total_SMAPE<-cbind(rbind(performance_scores_yearly_demopgrahic[[1]],
                        performance_scores_yearly_finance[[1]],
                        performance_scores_yearly_industry[[1]],
                        performance_scores_yearly_macro[[1]],
                        perfromance_scores_yearly_micro[[1]],
                        performance_scores_yearly_other[[1]]),
                        
                        rbind(performance_scores_yearly_demopgrahic[[4]],
                        performance_scores_yearly_finance[[4]],
                        performance_scores_yearly_industry[[4]],
                        performance_scores_yearly_macro[[4]],
                        perfromance_scores_yearly_micro[[4]],
                        performance_scores_yearly_other[[4]]))

rownames(data_total_SMAPE)<-c('demogr.','finance','industry','macro','micro','other')

data_total_MASE<-cbind(rbind(performance_scores_yearly_demopgrahic[[2]],
                       performance_scores_yearly_finance[[2]],
                       performance_scores_yearly_industry[[2]],
                       performance_scores_yearly_macro[[2]],
                       perfromance_scores_yearly_micro[[2]],
                       performance_scores_yearly_other[[2]]),
                       
                       rbind(performance_scores_yearly_demopgrahic[[5]],
                       performance_scores_yearly_finance[[5]],
                       performance_scores_yearly_industry[[5]],
                       performance_scores_yearly_macro[[5]],
                       perfromance_scores_yearly_micro[[5]],
                       performance_scores_yearly_other[[5]]))

rownames(data_total_MASE)<-c('demogr.','finance','industry','macro','micro','other')

data_total_OWA<-cbind(rbind(performance_scores_yearly_demopgrahic[[3]],
                      performance_scores_yearly_finance[[3]],
                      performance_scores_yearly_industry[[3]],
                      performance_scores_yearly_macro[[3]],
                      perfromance_scores_yearly_micro[[3]],
                      performance_scores_yearly_other[[3]]),
                      
                      rbind(performance_scores_yearly_demopgrahic[[6]],
                      performance_scores_yearly_finance[[6]],
                      performance_scores_yearly_industry[[6]],
                      performance_scores_yearly_macro[[6]],
                      perfromance_scores_yearly_micro[[6]],
                      performance_scores_yearly_other[[6]]))

rownames(data_total_OWA)<-c('demogr.','finance','industry','macro','micro','other')

data_total_SMAPE <- data.frame(data_total_SMAPE)
data_total_SMAPE$area <- rownames(data_total_SMAPE) 
datalong_total_SMAPE <- pivot_longer(data_total_SMAPE, cols = Naive1:NN, names_to = "name")
datalong_total_SMAPE <- data.frame(datalong_total_SMAPE)

data_total_MASE <- data.frame(data_total_MASE)
data_total_MASE$area <- rownames(data_total_MASE) 
datalong_total_MASE <- pivot_longer(data_total_MASE, cols = Naive1:NN, names_to = "name")
datalong_total_MASE <- data.frame(datalong_total_MASE)

data_total_OWA <- data.frame(data_total_OWA)
data_total_OWA$area <- rownames(data_total_OWA) 
datalong_total_OWA <- pivot_longer(data_total_OWA, cols = Naive1:NN, names_to = "name")
datalong_total_OWA <- data.frame(datalong_total_OWA)

plot_barchart_SMAPE_per_area<-function(input){
  datalong_total_SMAPE%>%
    filter(area==input)%>%
    ggplot()+
    labs(title = paste('Forecasting performance in the area:',input,"(1_000 time series)"),x="Benchmark model",y="Average SMAPE")+
    scale_fill_manual(values=c("Naive1"="grey",
                               "sNaive"="grey",
                               "Naive2"="grey",
                               "SES"="grey",
                               "Holt"="grey",
                               "Damped"="grey",
                               "Theta"= "grey",
                               "Comb"="grey",
                               "ETS"="#FF7F24",
                               "ARIMA"="#FF7F24",
                               "NN"="#FF7F24"))+
    geom_bar(aes(x=reorder(name,value),y=value,fill=name),stat="identity",show.legend = FALSE)+
    geom_text(aes(x=name,y=value,label=round(value,2)),vjust=-0.2)+
    theme(panel.background = element_blank())
}

plot_barchart_MASE_per_area<-function(input){
  datalong_total_MASE%>%
    filter(area==input)%>%
    ggplot()+
    labs(title = paste('Forecasting performance in the area:',input,"(1_000 time series)"),x="Benchmark model",y="Average MASE")+
    scale_fill_manual(values=c("Naive1"="grey",
                               "sNaive"="grey",
                               "Naive2"="grey",
                               "SES"="grey",
                               "Holt"="grey",
                               "Damped"="grey",
                               "Theta"= "grey",
                               "Comb"="grey",
                               "ETS"="#FF7F24",
                               "ARIMA"="#FF7F24",
                               "NN"="#FF7F24"))+
    geom_bar(aes(x=reorder(name,value),y=value,fill=name),stat="identity",show.legend = FALSE)+
    geom_text(aes(x=name,y=value,label=round(value,2)),vjust=-0.2)+
    theme(panel.background = element_blank())
}

plot_barchart_OWA_per_area<-function(input){
  datalong_total_OWA%>%
    filter(area==input)%>%
    ggplot()+
    labs(title = paste('Forecasting performance in the area:',input,"(1_000 time series)"),x="Benchmark model",y="Average OWA")+
    scale_fill_manual(values=c("Naive1"="grey",
                               "sNaive"="grey",
                               "Naive2"="grey",
                               "SES"="grey",
                               "Holt"="grey",
                               "Damped"="grey",
                               "Theta"= "grey",
                               "Comb"="grey",
                               "ETS"="#FF7F24",
                               "ARIMA"="#FF7F24",
                               "NN"="#FF7F24"))+
    geom_bar(aes(x=reorder(name,value),y=value,fill=name),stat="identity",show.legend = FALSE)+
    geom_text(aes(x=name,y=value,label=round(value,2)),vjust=-0.2)+
    theme(panel.background = element_blank())
}

plot_barchart_TotalSMAPE <- function(){
  datalong_total_SMAPE%>%group_by(name)%>%summarize(mean=mean(value))%>%
    ggplot()+
    labs(x="Model",y="Average SMAPE")+
    scale_fill_manual(values=c("Naive1"="grey",
                               "sNaive"="grey",
                               "Naive2"="grey",
                               "SES"="grey",
                               "Holt"="grey",
                               "Damped"="grey",
                               "Theta"= "grey",
                               "Comb"="grey",
                               "ETS"="#FF7F24",
                               "ARIMA"="#FF7F24",
                               "NN"="#FF7F24"))+
    geom_bar(aes(x=reorder(name,mean),y=mean,fill=name),stat="identity",show.legend = FALSE)+
    geom_text(aes(x=name,y=mean,label=round(mean,2)),vjust=-0.2)+
    theme(panel.background = element_blank())
}

plot_barchart_TotalMASE <- function(){
  datalong_total_MASE%>%group_by(name)%>%summarize(mean=mean(value))%>%
    ggplot()+
    labs(x="Model",y="Average MASE")+
    scale_fill_manual(values=c("Naive1"="grey",
                               "sNaive"="grey",
                               "Naive2"="grey",
                               "SES"="grey",
                               "Holt"="grey",
                               "Damped"="grey",
                               "Theta"= "grey",
                               "Comb"="grey",
                               "ETS"="#FF7F24",
                               "ARIMA"="#FF7F24",
                               "NN"="#FF7F24"))+
    geom_bar(aes(x=reorder(name,mean),y=mean,fill=name),stat="identity",show.legend = FALSE)+
    geom_text(aes(x=name,y=mean,label=round(mean,2)),vjust=-0.2)+
    theme(panel.background = element_blank())
}

plot_barchart_TotalOWA <- function(){
  datalong_total_OWA%>%group_by(name)%>%summarize(mean=mean(value))%>%
    ggplot()+
    labs(x="Model",y="Average OWA")+
    scale_fill_manual(values=c("Naive1"="grey",
                               "sNaive"="grey",
                               "Naive2"="grey",
                               "SES"="grey",
                               "Holt"="grey",
                               "Damped"="grey",
                               "Theta"= "grey",
                               "Comb"="grey",
                               "ETS"="#FF7F24",
                               "ARIMA"="#FF7F24",
                               "NN"="#FF7F24"))+
    geom_bar(aes(x=reorder(name,mean),y=mean,fill=name),stat="identity",show.legend = FALSE)+
    geom_text(aes(x=name,y=mean,label=round(mean,2)),vjust=-0.2)+
    theme(panel.background = element_blank())
}

plot_barchart_SMAPE_per_area('finance')
plot_barchart_MASE_per_area('macro')
plot_barchart_OWA_per_area('macro')

plot_SMAPE <- plot_barchart_TotalSMAPE()
#ggsave("plot_SMAPE.png", plot=plot_SMAPE, height=10, width=14, units=c("cm"), dpi=600)

plot_MASE<-plot_barchart_TotalMASE()
#ggsave("plot_MASE.png", plot=plot_MASE, height=10, width=14, units=c("cm"), dpi=600)

plot_OWA<-plot_barchart_TotalOWA()
#ggsave("plot_OWA.png", plot=plot_OWA, height=10, width=14, units=c("cm"), dpi=600)

#Possible next step -> spider plots to have a look on how your model performed in each area regarding a performance measure
