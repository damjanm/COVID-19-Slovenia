library(ggplot2)
library(tidyr)
library(dplyr)
library(rstan)
library(data.table)
library(lubridate)
library(gdata)
library(EnvStats)
library(matrixStats)
library(scales)
library(gridExtra)
library(ggpubr)
library(bayesplot)
library(cowplot)
library(gsheet)

source("geom-stepribbon.r")
#---------------------------------------------------------------------------
make_forecast_plot <- function(){
  
  args <- commandArgs(trailingOnly = TRUE)
  filename <- args[1]
  
  load(paste0("combined_hospitals/results/", filename))
  
  filename <- strsplit(filename, "-stanfit.Rdata")[[1]][1]
  
  for(i in 1:length(countries)){
    N <- length(dates[[i]])
    N2 <- N + forecast
    country <- countries[[i]]
    
    predicted_cases <- colMeans(prediction[,1:N,i])
    predicted_cases_li <- colQuantiles(prediction[,1:N,i], probs=.025)
    predicted_cases_ui <- colQuantiles(prediction[,1:N,i], probs=.975)
    
    estimated_deaths <- colMeans(estimated.deaths[,1:N,i])
    estimated_deaths_li <- colQuantiles(estimated.deaths[,1:N,i], probs=.025)
    estimated_deaths_ui <- colQuantiles(estimated.deaths[,1:N,i], probs=.975)
    
    estimated_deaths_forecast <- colMeans(estimated.deaths[,1:N2,i])[N:N2]
    estimated_deaths_li_forecast <- colQuantiles(estimated.deaths[,1:N2,i], probs=.025)[N:N2]
    estimated_deaths_ui_forecast <- colQuantiles(estimated.deaths[,1:N2,i], probs=.975)[N:N2]

    estimated_deathsh <- colMeans(estimated.deathsh[,1:N,i])
    estimated_deathsh_li <- colQuantiles(estimated.deathsh[,1:N,i], probs=.025)
    estimated_deathsh_ui <- colQuantiles(estimated.deathsh[,1:N,i], probs=.975)
    estimated_deathsh_li2 <- colQuantiles(estimated.deathsh[,1:N,i], probs=.25)
    estimated_deathsh_ui2 <- colQuantiles(estimated.deathsh[,1:N,i], probs=.75)
    
    estimated_deathsh_forecast <- colMeans(estimated.deathsh[,1:N2,i])[N:N2]
    estimated_deathsh_li_forecast <- colQuantiles(estimated.deathsh[,1:N2,i], probs=.025)[N:N2]
    estimated_deathsh_ui_forecast <- colQuantiles(estimated.deathsh[,1:N2,i], probs=.975)[N:N2]
    
    estimated_deathsc <- colMeans(estimated.deathsc[,1:N,i])
    estimated_deathsc_li <- colQuantiles(estimated.deathsc[,1:N,i], probs=.025)
    estimated_deathsc_ui <- colQuantiles(estimated.deathsc[,1:N,i], probs=.975)
    estimated_deathsc_li2 <- colQuantiles(estimated.deathsc[,1:N,i], probs=.25)
    estimated_deathsc_ui2 <- colQuantiles(estimated.deathsc[,1:N,i], probs=.75)
    
    estimated_deathsc_forecast <- colMeans(estimated.deathsc[,1:N2,i])[N:N2]
    estimated_deathsc_li_forecast <- colQuantiles(estimated.deathsc[,1:N2,i], probs=.025)[N:N2]
    estimated_deathsc_ui_forecast <- colQuantiles(estimated.deathsc[,1:N2,i], probs=.975)[N:N2]
    
    estimated_cases <- colMeans(estimated.cases[,1:N,i])
    estimated_cases_li <- colQuantiles(estimated.cases[,1:N,i], probs=.025)
    estimated_cases_ui <- colQuantiles(estimated.cases[,1:N,i], probs=.975)
    estimated_cases_li2 <- colQuantiles(estimated.cases[,1:N,i], probs=.25)
    estimated_cases_ui2 <- colQuantiles(estimated.cases[,1:N,i], probs=.75)

    estimated_cases_forecast <- colMeans(estimated.cases[,1:N2,i])[N:N2]
    estimated_cases_li_forecast <- colQuantiles(estimated.cases[,1:N2,i], probs=.025)[N:N2]
    estimated_cases_ui_forecast <- colQuantiles(estimated.cases[,1:N2,i], probs=.975)[N:N2]
    
    estimated_hosp <- colMeans(estimated.hospitals[,1:N,i])
    estimated_hosp_li <- colQuantiles(estimated.hospitals[,1:N,i], probs=.025)
    estimated_hosp_ui <- colQuantiles(estimated.hospitals[,1:N,i], probs=.975)
    estimated_hosp_li2 <- colQuantiles(estimated.hospitals[,1:N,i], probs=.25)
    estimated_hosp_ui2 <- colQuantiles(estimated.hospitals[,1:N,i], probs=.75)
    
    estimated_hosp_forecast <- colMeans(estimated.hospitals[,1:N2,i])[N:N2]
    estimated_hosp_li_forecast <- colQuantiles(estimated.hospitals[,1:N2,i], probs=.025)[N:N2]
    estimated_hosp_ui_forecast <- colQuantiles(estimated.hospitals[,1:N2,i], probs=.975)[N:N2]

    estimated_hospi <- colMeans(estimated.hospitalsi[,1:N,i])
    estimated_hospi_li <- colQuantiles(estimated.hospitalsi[,1:N,i], probs=.025)
    estimated_hospi_ui <- colQuantiles(estimated.hospitalsi[,1:N,i], probs=.975)
    estimated_hospi_li2 <- colQuantiles(estimated.hospitalsi[,1:N,i], probs=.25)
    estimated_hospi_ui2 <- colQuantiles(estimated.hospitalsi[,1:N,i], probs=.75)
    
    estimated_hospi_forecast <- colMeans(estimated.hospitalsi[,1:N2,i])[N:N2]
    estimated_hospi_li_forecast <- colQuantiles(estimated.hospitalsi[,1:N2,i], probs=.025)[N:N2]
    estimated_hospi_ui_forecast <- colQuantiles(estimated.hospitalsi[,1:N2,i], probs=.975)[N:N2]

    estimated_hospo <- colMeans(estimated.hospitalso[,1:N,i])
    estimated_hospo_li <- colQuantiles(estimated.hospitalso[,1:N,i], probs=.025)
    estimated_hospo_ui <- colQuantiles(estimated.hospitalso[,1:N,i], probs=.975)
    estimated_hospo_li2 <- colQuantiles(estimated.hospitalso[,1:N,i], probs=.25)
    estimated_hospo_ui2 <- colQuantiles(estimated.hospitalso[,1:N,i], probs=.75)
    
    estimated_hospo_forecast <- colMeans(estimated.hospitalso[,1:N2,i])[N:N2]
    estimated_hospo_li_forecast <- colQuantiles(estimated.hospitalso[,1:N2,i], probs=.025)[N:N2]
    estimated_hospo_ui_forecast <- colQuantiles(estimated.hospitalso[,1:N2,i], probs=.975)[N:N2]
    
    estimated_icu <- colMeans(estimated.icus[,1:N,i])
    estimated_icu_li <- colQuantiles(estimated.icus[,1:N,i], probs=.025)
    estimated_icu_ui <- colQuantiles(estimated.icus[,1:N,i], probs=.975)
    estimated_icu_li2 <- colQuantiles(estimated.icus[,1:N,i], probs=.25)
    estimated_icu_ui2 <- colQuantiles(estimated.icus[,1:N,i], probs=.75)
    
    estimated_icu_forecast <- colMeans(estimated.icus[,1:N2,i])[N:N2]
    estimated_icu_li_forecast <- colQuantiles(estimated.icus[,1:N2,i], probs=.025)[N:N2]
    estimated_icu_ui_forecast <- colQuantiles(estimated.icus[,1:N2,i], probs=.975)[N:N2]

    estimated_icui <- colMeans(estimated.icusi[,1:N,i])
    estimated_icui_li <- colQuantiles(estimated.icusi[,1:N,i], probs=.025)
    estimated_icui_ui <- colQuantiles(estimated.icusi[,1:N,i], probs=.975)
    estimated_icui_li2 <- colQuantiles(estimated.icusi[,1:N,i], probs=.25)
    estimated_icui_ui2 <- colQuantiles(estimated.icusi[,1:N,i], probs=.75)
    
    estimated_icui_forecast <- colMeans(estimated.icusi[,1:N2,i])[N:N2]
    estimated_icui_li_forecast <- colQuantiles(estimated.icusi[,1:N2,i], probs=.025)[N:N2]
    estimated_icui_ui_forecast <- colQuantiles(estimated.icusi[,1:N2,i], probs=.975)[N:N2]

    estimated_icuo <- colMeans(estimated.icuso[,1:N,i])
    estimated_icuo_li <- colQuantiles(estimated.icuso[,1:N,i], probs=.025)
    estimated_icuo_ui <- colQuantiles(estimated.icuso[,1:N,i], probs=.975)
    estimated_icuo_li2 <- colQuantiles(estimated.icuso[,1:N,i], probs=.25)
    estimated_icuo_ui2 <- colQuantiles(estimated.icuso[,1:N,i], probs=.75)
    
    estimated_icuo_forecast <- colMeans(estimated.icuso[,1:N2,i])[N:N2]
    estimated_icuo_li_forecast <- colQuantiles(estimated.icuso[,1:N2,i], probs=.025)[N:N2]
    estimated_icuo_ui_forecast <- colQuantiles(estimated.icuso[,1:N2,i], probs=.975)[N:N2]
    
    rt <- colMeans(out$Rt_adj[,1:N,i])
    rt_li <- colQuantiles(out$Rt_adj[,1:N,i],probs=.025)
    rt_ui <- colQuantiles(out$Rt_adj[,1:N,i],probs=.975)
    
    data_country <- data.frame("time" = as_date(as.character(dates[[i]])),
                               "country" = rep(country, length(dates[[i]])),
                               #"country_population" = rep(country_population, length(dates[[i]])),
                               "reported_cases" = reported_cases[[i]], 
                               "reported_cases_c" = cumsum(reported_cases[[i]]), 
                               "predicted_cases_c" = cumsum(predicted_cases),
                               "predicted_min_c" = cumsum(predicted_cases_li),
                               "predicted_max_c" = cumsum(predicted_cases_ui),
                               "predicted_cases" = predicted_cases,
                               "predicted_min" = predicted_cases_li,
                               "predicted_max" = predicted_cases_ui,
                               "deaths" = deaths_by_country[[i]],
                               "deaths_c" = cumsum(deaths_by_country[[i]]),
                               "estimated_deaths_c" =  cumsum(estimated_deaths),
                               "death_min_c" = cumsum(estimated_deaths_li),
                               "death_max_c"= cumsum(estimated_deaths_ui),
                               "estimated_deaths" = estimated_deaths,
                               "death_min" = estimated_deaths_li,
                               "death_max"= estimated_deaths_ui,
                               "rt" = rt,
                               "rt_min" = rt_li,
                               "rt_max" = rt_ui)
    
    times <- as_date(as.character(dates[[i]]))
    if(is.null(forecast)) forecast <- 0
    times_forecast <- times[length(times)] + 0:forecast
    data_country_forecast <- data.frame("time" = times_forecast,
                                        "country" = rep(country, forecast+1),
                                        "estimated_deaths_forecast" = estimated_deaths_forecast,
                                        "death_min_forecast" = estimated_deaths_li_forecast,
                                        "death_max_forecast"= estimated_deaths_ui_forecast)
    
    
    
    data_country2 <- data.frame("time" = as_date(as.character(dates[[i]])),
                                "estimated_cases_c" =  cumsum(estimated_cases),
                                "cases_min_c" = cumsum(estimated_cases_li),
                                "cases_max_c"= cumsum(estimated_cases_ui),
                                "estimated_cases" = estimated_cases,
                                "cases_min" = estimated_cases_li,
                                "cases_max"= estimated_cases_ui,
                                "cases_min2" = estimated_cases_li2,
                                "cases_max2"= estimated_cases_ui2)
    
    data_cases_forecast <- data.frame("time" = times_forecast,
                                        "country" = rep(country, forecast+1),
                                        "estimated_deaths_forecast" = estimated_cases_forecast,
                                        "death_min_forecast" = estimated_cases_li_forecast,
                                        "death_max_forecast"= estimated_cases_ui_forecast)
    
    data_deathsh <- data.frame("time" = as_date(as.character(dates[[i]])),
                               "deaths" = deathsh_by_country[[i]],
                               "deaths_c" = cumsum(deathsh_by_country[[i]]),
                               "estimated_deaths_c" =  cumsum(estimated_deathsh),
                               "death_min_c" = cumsum(estimated_deathsh_li),
                               "death_max_c"= cumsum(estimated_deathsh_ui),
                               "estimated_deaths" = estimated_deathsh,
                               "death_min" = estimated_deathsh_li,
                               "death_max"= estimated_deathsh_ui,
                               "death_min2" = estimated_deathsh_li2,
                               "death_max2"= estimated_deathsh_ui2)
    
    data_deathsh_forecast <- data.frame("time" = times_forecast,
                                      "country" = rep(country, forecast+1),
                                      "estimated_deaths_forecast" = estimated_deathsh_forecast,
                                      "death_min_forecast" = estimated_deathsh_li_forecast,
                                      "death_max_forecast"= estimated_deathsh_ui_forecast)
    
    data_deathsc <- data.frame("time" = as_date(as.character(dates[[i]])),
                               "deaths" = deathsc_by_country[[i]],
                               "deaths_c" = cumsum(deathsc_by_country[[i]]),
                               "estimated_deaths_c" =  cumsum(estimated_deathsc),
                               "death_min_c" = cumsum(estimated_deathsc_li),
                               "death_max_c"= cumsum(estimated_deathsc_ui),
                               "estimated_deaths" = estimated_deathsc,
                               "death_min" = estimated_deathsc_li,
                               "death_max"= estimated_deathsc_ui,
                               "death_min2" = estimated_deathsc_li2,
                               "death_max2"= estimated_deathsc_ui2)
    
    data_deathsc_forecast <- data.frame("time" = times_forecast,
                                        "country" = rep(country, forecast+1),
                                        "estimated_deaths_forecast" = estimated_deathsc_forecast,
                                        "death_min_forecast" = estimated_deathsc_li_forecast,
                                        "death_max_forecast"= estimated_deathsc_ui_forecast)
    
    data_hosp <- data.frame("time" = as_date(as.character(dates[[i]])),
                            "estimated_hosp_c" =  cumsum(estimated_hosp),
                            "hosp_min_c" = cumsum(estimated_hosp_li),
                            "hosp_max_c"= cumsum(estimated_hosp_ui),
                            "estimated_hosp" = estimated_hosp,
                            "hosp_min" = estimated_hosp_li,
                            "hosp_max"= estimated_hosp_ui,
                            "hosp_min2" = estimated_hosp_li2,
                            "hosp_max2"= estimated_hosp_ui2,
                            "reported_hosp" = hosps_by_country[[i]])

    data_hosp_forecast <- data.frame("time" = times_forecast,
                                     "country" = rep(country, forecast+1),
                                     "estimated_deaths_forecast" = estimated_hosp_forecast,
                                     "death_min_forecast" = estimated_hosp_li_forecast,
                                     "death_max_forecast"= estimated_hosp_ui_forecast)
    
    data_hospi <- data.frame("time" = as_date(as.character(dates[[i]])),
                             "estimated_hosp_c" =  cumsum(estimated_hospi),
                             "hosp_min_c" = cumsum(estimated_hospi_li),
                             "hosp_max_c"= cumsum(estimated_hospi_ui),
                             "estimated_hosp" = estimated_hospi,
                             "hosp_min" = estimated_hospi_li,
                             "hosp_max"= estimated_hospi_ui,
                             "hosp_min2" = estimated_hospi_li2,
                             "hosp_max2"= estimated_hospi_ui2,
                             "reported_hosp" = hospsin_by_country[[i]])
    
    data_hospi_forecast <- data.frame("time" = times_forecast,
                                     "country" = rep(country, forecast+1),
                                     "estimated_deaths_forecast" = estimated_hospi_forecast,
                                     "death_min_forecast" = estimated_hospi_li_forecast,
                                     "death_max_forecast"= estimated_hospi_ui_forecast)

    data_hospo <- data.frame("time" = as_date(as.character(dates[[i]])),
                             "estimated_hosp_c" =  cumsum(estimated_hospo),
                             "hosp_min_c" = cumsum(estimated_hospo_li),
                             "hosp_max_c"= cumsum(estimated_hospo_ui),
                             "estimated_hosp" = estimated_hospo,
                             "hosp_min" = estimated_hospo_li,
                             "hosp_max"= estimated_hospo_ui,
                             "hosp_min2" = estimated_hospo_li2,
                             "hosp_max2"= estimated_hospo_ui2,
                             "reported_hosp" = hospsout_by_country[[i]])
    
    data_hospo_forecast <- data.frame("time" = times_forecast,
                                      "country" = rep(country, forecast+1),
                                      "estimated_deaths_forecast" = estimated_hospo_forecast,
                                      "death_min_forecast" = estimated_hospo_li_forecast,
                                      "death_max_forecast"= estimated_hospo_ui_forecast)
    
    data_icu <- data.frame("time" = as_date(as.character(dates[[i]])),
                            "estimated_icu_c" =  cumsum(estimated_icu),
                            "icu_min_c" = cumsum(estimated_icu_li),
                            "icu_max_c"= cumsum(estimated_icu_ui),
                            "estimated_icu" = estimated_icu,
                            "icu_min" = estimated_icu_li,
                            "icu_max"= estimated_icu_ui,
                            "icu_min2" = estimated_icu_li2,
                            "icu_max2"= estimated_icu_ui2,
                            "reported_icu" = icus_by_country[[i]])
    
    data_icu_forecast <- data.frame("time" = times_forecast,
                                     "country" = rep(country, forecast+1),
                                     "estimated_deaths_forecast" = estimated_icu_forecast,
                                     "death_min_forecast" = estimated_icu_li_forecast,
                                     "death_max_forecast"= estimated_icu_ui_forecast)
    
    data_icui <- data.frame("time" = as_date(as.character(dates[[i]])),
                           "estimated_icu_c" =  cumsum(estimated_icui),
                           "icu_min_c" = cumsum(estimated_icui_li),
                           "icu_max_c"= cumsum(estimated_icui_ui),
                           "estimated_icu" = estimated_icui,
                           "icu_min" = estimated_icui_li,
                           "icu_max"= estimated_icui_ui,
                           "icu_min2" = estimated_icui_li2,
                           "icu_max2"= estimated_icui_ui2,
                           "reported_icu" = icusin_by_country[[i]])
    
    data_icui_forecast <- data.frame("time" = times_forecast,
                                    "country" = rep(country, forecast+1),
                                    "estimated_deaths_forecast" = estimated_icui_forecast,
                                    "death_min_forecast" = estimated_icui_li_forecast,
                                    "death_max_forecast"= estimated_icui_ui_forecast)
  
    data_icuo <- data.frame("time" = as_date(as.character(dates[[i]])),
                            "estimated_icu_c" =  cumsum(estimated_icuo),
                            "icu_min_c" = cumsum(estimated_icuo_li),
                            "icu_max_c"= cumsum(estimated_icuo_ui),
                            "estimated_icu" = estimated_icuo,
                            "icu_min" = estimated_icuo_li,
                            "icu_max"= estimated_icuo_ui,
                            "icu_min2" = estimated_icuo_li2,
                            "icu_max2"= estimated_icuo_ui2,
                            "reported_icu" = icusout_by_country[[i]])
    
    data_icuo_forecast <- data.frame("time" = times_forecast,
                                     "country" = rep(country, forecast+1),
                                     "estimated_deaths_forecast" = estimated_icuo_forecast,
                                     "death_min_forecast" = estimated_icuo_li_forecast,
                                     "death_max_forecast"= estimated_icuo_ui_forecast)
    
    ###
    # Add later data:
    
    load("data.Rdata")
    day<-strsplit(dd[-1,2],split="\\.")
    ff<-function(i,x){
      decembr <- which(unlist(lapply(x, function(iter) iter[2]=='12')))
      if(identical(integer(0), decembr)){
        leto <- '.2020'
      } else{
        if(max(december)<i){
          leto <- '.2021'
        } else{
          leto <- '.2020'
        }
      }
      paste(x[[i]][1],".",strsplit(day[[i]][2]," ")[[1]][2],leto,sep="")
    }
    day<-unlist(lapply(1:length(day),ff,day))
    day<-as.Date(day,format="%d.%m.%Y")
    day<-c((day[1]-1:50)[order(day[1]-1:50)],day)

    if(max(dates[[i]]) < max(day)){
      # Find all later dates:
      wh_dates <- which(day > max(dates[[i]]))
      if(forecast<length(wh_dates)){
        wh_dates <- wh_dates[1:forecast]
      }
      # Prepare data:
      # Deaths:
      deaths<- as.numeric(dd$umrli..vsi.[-1]) #cumulative!
      deaths[is.na(deaths)]<-0
      deaths<-c(rep(0,50),deaths)
      deathsi<-deaths
      for (ite in 2:length(deaths)){
        deathsi[ite]<-deaths[ite]-deaths[ite-1]
      }
      deaths<-deathsi
      
      deathsc<-c(rep(0,50), as.numeric(dd$D.u[-1])) #cumulative! prispevek je lahko negativen!
      deathsc[is.na(deathsc)] <- 0
      
      deathsh<-c(rep(0,50), as.numeric(dd$H.D[-1])) #cumulative! prispevek je lahko negativen!
      deathsh[is.na(deathsh)] <- 0
      
      # Cases:
      cases<- c(rep(0,50),as.numeric(dd$potrjeni.danes[-1]))
      cases[is.na(cases)]<-0

      # Hosp:
      hospitalizirani<-as.numeric(dd$hospitalizirani..trenutno.[-1]) #cumulative! prispevek je lahko negativen!
      hospitalizirani[is.na(hospitalizirani)]<-0
      hospitalizirani<-c(rep(0,50),hospitalizirani)
      
      hospitaliziraniin<-dd$novi[-1]
      hospitaliziraniin[is.na(hospitaliziraniin)]<-0
      hospitaliziraniin<-c(rep(0,50),hospitaliziraniin)
      
      hospitaliziraniout<-as.numeric(dd$iz.bol..oskrbe..vsi.[-1])
      hospitaliziraniout[is.na(hospitaliziraniout)]<-0
      hospitaliziraniout<-c(rep(0,50),hospitaliziraniout)
      
      hospitaliziraniouti<-hospitaliziraniout
      for (ite in 2:length(hospitaliziraniout)){
        hospitaliziraniouti[ite]<-hospitaliziraniout[ite]-hospitaliziraniout[ite-1]
      }
      hospitaliziraniout<-hospitaliziraniouti
      
      # ICU:
      icu<-c(rep(0,50), as.numeric(dd$intenzivna.enota..trenutno.[-1])) #cumulative! prispevek je lahko negativen!
      icu[is.na(icu)] <- 0
      
      icuin<-c(rep(0,50), as.numeric(dd$I.i[-1])) #cumulative! prispevek je lahko negativen!
      icuin[is.na(icuin)] <- 0
      
      icuout<-c(rep(0,50), as.numeric(dd$I.o[-1])) #cumulative! prispevek je lahko negativen!
      icuout[is.na(icuout)] <- 0
      

      later_data <- data.frame("time" = day[wh_dates],
                               "deaths" = deaths[wh_dates],
                               "deathsh" = deathsh[wh_dates],
                               "deathsc" = deathsc[wh_dates],
                               "cases" = cases[wh_dates],
                               "hosp" = hospitalizirani[wh_dates],
                               "hospi" = hospitaliziraniin[wh_dates],
                               "hospo" = hospitaliziraniout[wh_dates],
                               "icu" = icu[wh_dates],
                               "icui" = icuin[wh_dates],
                               "icuo" = icuout[wh_dates])
    } else{
      later_data <- data.frame()
    }
    ###
    posteriors_out_of_time <- list(
      estimated.deaths=estimated.deaths[,(N+1):N2,i],
      estimated.deathsh=estimated.deathsh[,(N+1):N2,i],
      estimated.deathsc=estimated.deathsc[,(N+1):N2,i],
      estimated.cases=estimated.cases[,(N+1):N2,i],
      estimated.hospitals=estimated.hospitals[,(N+1):N2,i],
      estimated.hospitalsi=estimated.hospitalsi[,(N+1):N2,i],
      estimated.hospitalso=estimated.hospitalso[,(N+1):N2,i],
      estimated.icus=estimated.icus[,(N+1):N2,i],
      estimated.icusi=estimated.icusi[,(N+1):N2,i],
      estimated.icuso=estimated.icuso[,(N+1):N2,i])
    save(posteriors_out_of_time, file=paste0("combinedTDI/results/", country, "-posteriors-", filename, ".RData"))
    ###
    
    make_single_plot(data_country = data_country, data_country2 = data_country2,
                     data_deathsh = data_deathsh, data_deathsc = data_deathsc,
                     data_hosp = data_hosp, data_hospi = data_hospi, data_hospo = data_hospo,
                     data_icu = data_icu, data_icui = data_icui, data_icuo = data_icuo,
                     data_country_forecast = data_country_forecast,
                     data_deathsh_forecast = data_deathsh_forecast,
                     data_deathsc_forecast = data_deathsc_forecast,
                     data_cases_forecast = data_cases_forecast,
                     data_hosp_forecast = data_hosp_forecast,
                     data_hospi_forecast = data_hospi_forecast,
                     data_hospo_forecast = data_hospo_forecast,
                     data_icu_forecast = data_icu_forecast,
                     data_icui_forecast = data_icui_forecast,
                     data_icuo_forecast = data_icuo_forecast,
                     filename = filename,
                     country = country,
                     model_input = model_input,
                     later_data = later_data)
  }
  
  
  
  
  
  
  
  
  p_hosp <- list()
  p_icu <- list()
  p_deathsh <- list()
  
  load("hosp_data.Rdata")
  # Columns: Ljubljana, Maribor, Golnik, Celje:
  hosp.hospitalizirani <- cbind(dd_hosp$H.c.2, dd_hosp$H.c.3, dd_hosp$H.c.4, dd_hosp$H.c.5)
  hosp.hospitaliziraniin <- cbind(dd_hosp$H.i.1, dd_hosp$H.i.2, dd_hosp$H.i.3, dd_hosp$H.i.4)
  hosp.hospitaliziraniout <- cbind(dd_hosp$H.o.2, dd_hosp$H.o.3, dd_hosp$H.o.4, dd_hosp$H.o.5)
  hosp.deathsh <- cbind(dd_hosp$H.D.t.1, dd_hosp$H.D.t.2, dd_hosp$H.D.t.3, dd_hosp$H.D.t.4)
  hosp.icuin <- cbind(dd_hosp$I.i.1, dd_hosp$I.i.2, dd_hosp$I.i.3, dd_hosp$I.i.4)
  hosp.icuout <- cbind(dd_hosp$I.o.1, dd_hosp$I.o.2, dd_hosp$I.o.3, dd_hosp$I.o.4)
  hosp.icu <- cbind(dd_hosp$I.c.2, dd_hosp$I.c.3, dd_hosp$I.c.4, dd_hosp$I.c.5)
  for(iti in 1:ncol(hosp.deathsh)) hosp.deathsh[,iti] <- c(0,diff(hosp.deathsh[,iti]))
  # hosp.deathsh[(nrow(hosp.deathsh)-forecast+1):nrow(hosp.deathsh),] <- -1
  
  
  for(i in 1:dim(estimated.hospitals)[3]){
    print(i)
    N <- length(dates[[1]])
    N2 <- N + forecast
    country <- countries[[1]]
    
    predicted_cases <- colMeans(prediction[,1:N,1])
    predicted_cases_li <- colQuantiles(prediction[,1:N,1], probs=.025)
    predicted_cases_ui <- colQuantiles(prediction[,1:N,1], probs=.975)
    
    estimated_deaths <- colMeans(estimated.deaths[,1:N,1])
    estimated_deaths_li <- colQuantiles(estimated.deaths[,1:N,1], probs=.025)
    estimated_deaths_ui <- colQuantiles(estimated.deaths[,1:N,1], probs=.975)
    
    estimated_deaths_forecast <- colMeans(estimated.deaths[,1:N2,1])[N:N2]
    estimated_deaths_li_forecast <- colQuantiles(estimated.deaths[,1:N2,1], probs=.025)[N:N2]
    estimated_deaths_ui_forecast <- colQuantiles(estimated.deaths[,1:N2,1], probs=.975)[N:N2]
    
    estimated_deathsh <- colMeans(estimated.deathsh[,1:N,i])
    estimated_deathsh_li <- colQuantiles(estimated.deathsh[,1:N,i], probs=.025)
    estimated_deathsh_ui <- colQuantiles(estimated.deathsh[,1:N,i], probs=.975)
    estimated_deathsh_li2 <- colQuantiles(estimated.deathsh[,1:N,i], probs=.25)
    estimated_deathsh_ui2 <- colQuantiles(estimated.deathsh[,1:N,i], probs=.75)
    
    estimated_deathsh_forecast <- colMeans(estimated.deathsh[,1:N2,i])[N:N2]
    estimated_deathsh_li_forecast <- colQuantiles(estimated.deathsh[,1:N2,i], probs=.025)[N:N2]
    estimated_deathsh_ui_forecast <- colQuantiles(estimated.deathsh[,1:N2,i], probs=.975)[N:N2]
    
    estimated_deathsc <- colMeans(estimated.deathsc[,1:N,1])
    estimated_deathsc_li <- colQuantiles(estimated.deathsc[,1:N,1], probs=.025)
    estimated_deathsc_ui <- colQuantiles(estimated.deathsc[,1:N,1], probs=.975)
    estimated_deathsc_li2 <- colQuantiles(estimated.deathsc[,1:N,1], probs=.25)
    estimated_deathsc_ui2 <- colQuantiles(estimated.deathsc[,1:N,1], probs=.75)
    
    estimated_deathsc_forecast <- colMeans(estimated.deathsc[,1:N2,1])[N:N2]
    estimated_deathsc_li_forecast <- colQuantiles(estimated.deathsc[,1:N2,1], probs=.025)[N:N2]
    estimated_deathsc_ui_forecast <- colQuantiles(estimated.deathsc[,1:N2,1], probs=.975)[N:N2]
    
    estimated_cases <- colMeans(estimated.cases[,1:N,1])
    estimated_cases_li <- colQuantiles(estimated.cases[,1:N,1], probs=.025)
    estimated_cases_ui <- colQuantiles(estimated.cases[,1:N,1], probs=.975)
    estimated_cases_li2 <- colQuantiles(estimated.cases[,1:N,1], probs=.25)
    estimated_cases_ui2 <- colQuantiles(estimated.cases[,1:N,1], probs=.75)
    
    estimated_cases_forecast <- colMeans(estimated.cases[,1:N2,1])[N:N2]
    estimated_cases_li_forecast <- colQuantiles(estimated.cases[,1:N2,1], probs=.025)[N:N2]
    estimated_cases_ui_forecast <- colQuantiles(estimated.cases[,1:N2,1], probs=.975)[N:N2]
    
    estimated_hosp <- colMeans(estimated.hospitals[,1:N,i])
    estimated_hosp_li <- colQuantiles(estimated.hospitals[,1:N,i], probs=.025)
    estimated_hosp_ui <- colQuantiles(estimated.hospitals[,1:N,i], probs=.975)
    estimated_hosp_li2 <- colQuantiles(estimated.hospitals[,1:N,i], probs=.25)
    estimated_hosp_ui2 <- colQuantiles(estimated.hospitals[,1:N,i], probs=.75)
    
    estimated_hosp_forecast <- colMeans(estimated.hospitals[,1:N2,i])[N:N2]
    estimated_hosp_li_forecast <- colQuantiles(estimated.hospitals[,1:N2,i], probs=.025)[N:N2]
    estimated_hosp_ui_forecast <- colQuantiles(estimated.hospitals[,1:N2,i], probs=.975)[N:N2]
    
    estimated_hospi <- colMeans(estimated.hospitalsi[,1:N,i])
    estimated_hospi_li <- colQuantiles(estimated.hospitalsi[,1:N,i], probs=.025)
    estimated_hospi_ui <- colQuantiles(estimated.hospitalsi[,1:N,i], probs=.975)
    estimated_hospi_li2 <- colQuantiles(estimated.hospitalsi[,1:N,i], probs=.25)
    estimated_hospi_ui2 <- colQuantiles(estimated.hospitalsi[,1:N,i], probs=.75)
    
    estimated_hospi_forecast <- colMeans(estimated.hospitalsi[,1:N2,i])[N:N2]
    estimated_hospi_li_forecast <- colQuantiles(estimated.hospitalsi[,1:N2,i], probs=.025)[N:N2]
    estimated_hospi_ui_forecast <- colQuantiles(estimated.hospitalsi[,1:N2,i], probs=.975)[N:N2]
    
    estimated_hospo <- colMeans(estimated.hospitalso[,1:N,i])
    estimated_hospo_li <- colQuantiles(estimated.hospitalso[,1:N,i], probs=.025)
    estimated_hospo_ui <- colQuantiles(estimated.hospitalso[,1:N,i], probs=.975)
    estimated_hospo_li2 <- colQuantiles(estimated.hospitalso[,1:N,i], probs=.25)
    estimated_hospo_ui2 <- colQuantiles(estimated.hospitalso[,1:N,i], probs=.75)
    
    estimated_hospo_forecast <- colMeans(estimated.hospitalso[,1:N2,i])[N:N2]
    estimated_hospo_li_forecast <- colQuantiles(estimated.hospitalso[,1:N2,i], probs=.025)[N:N2]
    estimated_hospo_ui_forecast <- colQuantiles(estimated.hospitalso[,1:N2,i], probs=.975)[N:N2]
    
    estimated_icu <- colMeans(estimated.icus[,1:N,i])
    estimated_icu_li <- colQuantiles(estimated.icus[,1:N,i], probs=.025)
    estimated_icu_ui <- colQuantiles(estimated.icus[,1:N,i], probs=.975)
    estimated_icu_li2 <- colQuantiles(estimated.icus[,1:N,i], probs=.25)
    estimated_icu_ui2 <- colQuantiles(estimated.icus[,1:N,i], probs=.75)
    
    estimated_icu_forecast <- colMeans(estimated.icus[,1:N2,i])[N:N2]
    estimated_icu_li_forecast <- colQuantiles(estimated.icus[,1:N2,i], probs=.025)[N:N2]
    estimated_icu_ui_forecast <- colQuantiles(estimated.icus[,1:N2,i], probs=.975)[N:N2]
    
    estimated_icui <- colMeans(estimated.icusi[,1:N,i])
    estimated_icui_li <- colQuantiles(estimated.icusi[,1:N,i], probs=.025)
    estimated_icui_ui <- colQuantiles(estimated.icusi[,1:N,i], probs=.975)
    estimated_icui_li2 <- colQuantiles(estimated.icusi[,1:N,i], probs=.25)
    estimated_icui_ui2 <- colQuantiles(estimated.icusi[,1:N,i], probs=.75)
    
    estimated_icui_forecast <- colMeans(estimated.icusi[,1:N2,i])[N:N2]
    estimated_icui_li_forecast <- colQuantiles(estimated.icusi[,1:N2,i], probs=.025)[N:N2]
    estimated_icui_ui_forecast <- colQuantiles(estimated.icusi[,1:N2,i], probs=.975)[N:N2]
    
    estimated_icuo <- colMeans(estimated.icuso[,1:N,i])
    estimated_icuo_li <- colQuantiles(estimated.icuso[,1:N,i], probs=.025)
    estimated_icuo_ui <- colQuantiles(estimated.icuso[,1:N,i], probs=.975)
    estimated_icuo_li2 <- colQuantiles(estimated.icuso[,1:N,i], probs=.25)
    estimated_icuo_ui2 <- colQuantiles(estimated.icuso[,1:N,i], probs=.75)
    
    estimated_icuo_forecast <- colMeans(estimated.icuso[,1:N2,i])[N:N2]
    estimated_icuo_li_forecast <- colQuantiles(estimated.icuso[,1:N2,i], probs=.025)[N:N2]
    estimated_icuo_ui_forecast <- colQuantiles(estimated.icuso[,1:N2,i], probs=.975)[N:N2]
    
    rt <- colMeans(out$Rt_adj[,1:N,1])
    rt_li <- colQuantiles(out$Rt_adj[,1:N,1],probs=.025)
    rt_ui <- colQuantiles(out$Rt_adj[,1:N,1],probs=.975)
    
    data_country <- data.frame("time" = as_date(as.character(dates[[1]])),
                               "country" = rep(country, length(dates[[1]])),
                               #"country_population" = rep(country_population, length(dates[[1]])),
                               "reported_cases" = reported_cases[[1]], 
                               "reported_cases_c" = cumsum(reported_cases[[1]]), 
                               "predicted_cases_c" = cumsum(predicted_cases),
                               "predicted_min_c" = cumsum(predicted_cases_li),
                               "predicted_max_c" = cumsum(predicted_cases_ui),
                               "predicted_cases" = predicted_cases,
                               "predicted_min" = predicted_cases_li,
                               "predicted_max" = predicted_cases_ui,
                               "deaths" = deaths_by_country[[1]],
                               "deaths_c" = cumsum(deaths_by_country[[1]]),
                               "estimated_deaths_c" =  cumsum(estimated_deaths),
                               "death_min_c" = cumsum(estimated_deaths_li),
                               "death_max_c"= cumsum(estimated_deaths_ui),
                               "estimated_deaths" = estimated_deaths,
                               "death_min" = estimated_deaths_li,
                               "death_max"= estimated_deaths_ui,
                               "rt" = rt,
                               "rt_min" = rt_li,
                               "rt_max" = rt_ui)
    
    times <- as_date(as.character(dates[[1]]))
    if(is.null(forecast)) forecast <- 0
    times_forecast <- times[length(times)] + 0:forecast
    data_country_forecast <- data.frame("time" = times_forecast,
                                        "country" = rep(country, forecast+1),
                                        "estimated_deaths_forecast" = estimated_deaths_forecast,
                                        "death_min_forecast" = estimated_deaths_li_forecast,
                                        "death_max_forecast"= estimated_deaths_ui_forecast)
    
    
    
    data_country2 <- data.frame("time" = as_date(as.character(dates[[1]])),
                                "estimated_cases_c" =  cumsum(estimated_cases),
                                "cases_min_c" = cumsum(estimated_cases_li),
                                "cases_max_c"= cumsum(estimated_cases_ui),
                                "estimated_cases" = estimated_cases,
                                "cases_min" = estimated_cases_li,
                                "cases_max"= estimated_cases_ui,
                                "cases_min2" = estimated_cases_li2,
                                "cases_max2"= estimated_cases_ui2)
    
    data_cases_forecast <- data.frame("time" = times_forecast,
                                      "country" = rep(country, forecast+1),
                                      "estimated_deaths_forecast" = estimated_cases_forecast,
                                      "death_min_forecast" = estimated_cases_li_forecast,
                                      "death_max_forecast"= estimated_cases_ui_forecast)
    
    data_deathsh <- data.frame("time" = as_date(as.character(dates[[1]])),
                               "deaths" = hosp.deathsh[1:N,i],
                               "deaths_c" = cumsum(hosp.deathsh[1:N,i]),
                               "estimated_deaths_c" =  cumsum(estimated_deathsh),
                               "death_min_c" = cumsum(estimated_deathsh_li),
                               "death_max_c"= cumsum(estimated_deathsh_ui),
                               "estimated_deaths" = estimated_deathsh,
                               "death_min" = estimated_deathsh_li,
                               "death_max"= estimated_deathsh_ui,
                               "death_min2" = estimated_deathsh_li2,
                               "death_max2"= estimated_deathsh_ui2)
    
    data_deathsh_forecast <- data.frame("time" = times_forecast,
                                        "country" = rep(country, forecast+1),
                                        "estimated_deaths_forecast" = estimated_deathsh_forecast,
                                        "death_min_forecast" = estimated_deathsh_li_forecast,
                                        "death_max_forecast"= estimated_deathsh_ui_forecast)
    
    data_deathsc <- data.frame("time" = as_date(as.character(dates[[1]])),
                               "deaths" = deathsc_by_country[[1]],
                               "deaths_c" = cumsum(deathsc_by_country[[1]]),
                               "estimated_deaths_c" =  cumsum(estimated_deathsc),
                               "death_min_c" = cumsum(estimated_deathsc_li),
                               "death_max_c"= cumsum(estimated_deathsc_ui),
                               "estimated_deaths" = estimated_deathsc,
                               "death_min" = estimated_deathsc_li,
                               "death_max"= estimated_deathsc_ui,
                               "death_min2" = estimated_deathsc_li2,
                               "death_max2"= estimated_deathsc_ui2)
    
    data_deathsc_forecast <- data.frame("time" = times_forecast,
                                        "country" = rep(country, forecast+1),
                                        "estimated_deaths_forecast" = estimated_deathsc_forecast,
                                        "death_min_forecast" = estimated_deathsc_li_forecast,
                                        "death_max_forecast"= estimated_deathsc_ui_forecast)
    
    data_hosp <- data.frame("time" = as_date(as.character(dates[[1]])),
                            "estimated_hosp_c" =  cumsum(estimated_hosp),
                            "hosp_min_c" = cumsum(estimated_hosp_li),
                            "hosp_max_c"= cumsum(estimated_hosp_ui),
                            "estimated_hosp" = estimated_hosp,
                            "hosp_min" = estimated_hosp_li,
                            "hosp_max"= estimated_hosp_ui,
                            "hosp_min2" = estimated_hosp_li2,
                            "hosp_max2"= estimated_hosp_ui2,
                            "reported_hosp" = hosp.hospitalizirani[1:N,i])
    
    data_hosp_forecast <- data.frame("time" = times_forecast,
                                     "country" = rep(country, forecast+1),
                                     "estimated_deaths_forecast" = estimated_hosp_forecast,
                                     "death_min_forecast" = estimated_hosp_li_forecast,
                                     "death_max_forecast"= estimated_hosp_ui_forecast)
    
    data_hospi <- data.frame("time" = as_date(as.character(dates[[1]])),
                             "estimated_hosp_c" =  cumsum(estimated_hospi),
                             "hosp_min_c" = cumsum(estimated_hospi_li),
                             "hosp_max_c"= cumsum(estimated_hospi_ui),
                             "estimated_hosp" = estimated_hospi,
                             "hosp_min" = estimated_hospi_li,
                             "hosp_max"= estimated_hospi_ui,
                             "hosp_min2" = estimated_hospi_li2,
                             "hosp_max2"= estimated_hospi_ui2,
                             "reported_hosp" = hosp.hospitaliziraniin[1:N,i])
    
    data_hospi_forecast <- data.frame("time" = times_forecast,
                                      "country" = rep(country, forecast+1),
                                      "estimated_deaths_forecast" = estimated_hospi_forecast,
                                      "death_min_forecast" = estimated_hospi_li_forecast,
                                      "death_max_forecast"= estimated_hospi_ui_forecast)
    
    data_hospo <- data.frame("time" = as_date(as.character(dates[[1]])),
                             "estimated_hosp_c" =  cumsum(estimated_hospo),
                             "hosp_min_c" = cumsum(estimated_hospo_li),
                             "hosp_max_c"= cumsum(estimated_hospo_ui),
                             "estimated_hosp" = estimated_hospo,
                             "hosp_min" = estimated_hospo_li,
                             "hosp_max"= estimated_hospo_ui,
                             "hosp_min2" = estimated_hospo_li2,
                             "hosp_max2"= estimated_hospo_ui2,
                             "reported_hosp" = hosp.hospitaliziraniout[1:N,i])
    
    data_hospo_forecast <- data.frame("time" = times_forecast,
                                      "country" = rep(country, forecast+1),
                                      "estimated_deaths_forecast" = estimated_hospo_forecast,
                                      "death_min_forecast" = estimated_hospo_li_forecast,
                                      "death_max_forecast"= estimated_hospo_ui_forecast)
    
    data_icu <- data.frame("time" = as_date(as.character(dates[[1]])),
                           "estimated_icu_c" =  cumsum(estimated_icu),
                           "icu_min_c" = cumsum(estimated_icu_li),
                           "icu_max_c"= cumsum(estimated_icu_ui),
                           "estimated_icu" = estimated_icu,
                           "icu_min" = estimated_icu_li,
                           "icu_max"= estimated_icu_ui,
                           "icu_min2" = estimated_icu_li2,
                           "icu_max2"= estimated_icu_ui2,
                           "reported_icu" = hosp.icu[1:N,i])
    
    data_icu_forecast <- data.frame("time" = times_forecast,
                                    "country" = rep(country, forecast+1),
                                    "estimated_deaths_forecast" = estimated_icu_forecast,
                                    "death_min_forecast" = estimated_icu_li_forecast,
                                    "death_max_forecast"= estimated_icu_ui_forecast)
    
    data_icui <- data.frame("time" = as_date(as.character(dates[[1]])),
                            "estimated_icu_c" =  cumsum(estimated_icui),
                            "icu_min_c" = cumsum(estimated_icui_li),
                            "icu_max_c"= cumsum(estimated_icui_ui),
                            "estimated_icu" = estimated_icui,
                            "icu_min" = estimated_icui_li,
                            "icu_max"= estimated_icui_ui,
                            "icu_min2" = estimated_icui_li2,
                            "icu_max2"= estimated_icui_ui2,
                            "reported_icu" = hosp.icuin[1:N,i])
    
    data_icui_forecast <- data.frame("time" = times_forecast,
                                     "country" = rep(country, forecast+1),
                                     "estimated_deaths_forecast" = estimated_icui_forecast,
                                     "death_min_forecast" = estimated_icui_li_forecast,
                                     "death_max_forecast"= estimated_icui_ui_forecast)
    
    data_icuo <- data.frame("time" = as_date(as.character(dates[[1]])),
                            "estimated_icu_c" =  cumsum(estimated_icuo),
                            "icu_min_c" = cumsum(estimated_icuo_li),
                            "icu_max_c"= cumsum(estimated_icuo_ui),
                            "estimated_icu" = estimated_icuo,
                            "icu_min" = estimated_icuo_li,
                            "icu_max"= estimated_icuo_ui,
                            "icu_min2" = estimated_icuo_li2,
                            "icu_max2"= estimated_icuo_ui2,
                            "reported_icu" = hosp.icuout[1:N,i])
    
    data_icuo_forecast <- data.frame("time" = times_forecast,
                                     "country" = rep(country, forecast+1),
                                     "estimated_deaths_forecast" = estimated_icuo_forecast,
                                     "death_min_forecast" = estimated_icuo_li_forecast,
                                     "death_max_forecast"= estimated_icuo_ui_forecast)
    
    ###
    # Add later data:
    
    load("data.Rdata")
    day<-strsplit(dd[-1,2],split="\\.")
    ff<-function(i,x){
      decembr <- which(unlist(lapply(x, function(iter) iter[2]=='12')))
      if(identical(integer(0), decembr)){
        leto <- '.2020'
      } else{
        if(max(december)<i){
          leto <- '.2021'
        } else{
          leto <- '.2020'
        }
      }
      paste(x[[i]][1],".",strsplit(day[[i]][2]," ")[[1]][2],leto,sep="")
    }
    day<-unlist(lapply(1:length(day),ff,day))
    day<-as.Date(day,format="%d.%m.%Y")
    day<-c((day[1]-1:50)[order(day[1]-1:50)],day)
    
    if(max(dates[[1]]) < max(day)){
      # Find all later dates:
      wh_dates <- which(day > max(dates[[1]]))
      if(forecast<length(wh_dates)){
        wh_dates <- wh_dates[1:forecast]
      }
      # Prepare data:
      # Deaths:
      deaths<- as.numeric(dd$umrli..vsi.[-1]) #cumulative!
      deaths[is.na(deaths)]<-0
      deaths<-c(rep(0,50),deaths)
      deathsi<-deaths
      for (ite in 2:length(deaths)){
        deathsi[ite]<-deaths[ite]-deaths[ite-1]
      }
      deaths<-deathsi
      
      deathsc<-c(rep(0,50), as.numeric(dd$D.u[-1])) #cumulative! prispevek je lahko negativen!
      deathsc[is.na(deathsc)] <- 0
      
      # deathsh<-c(rep(0,50), as.numeric(dd$H.D[-1])) #cumulative! prispevek je lahko negativen!
      deathsh<-c(rep(0,54), as.numeric(hosp.deathsh[,i])) #cumulative! prispevek je lahko negativen!
      deathsh[is.na(deathsh)] <- 0
      
      # Cases:
      cases<- c(rep(0,50),as.numeric(dd$potrjeni.danes[-1]))
      cases[is.na(cases)]<-0
      
      # Hosp:
      # hospitalizirani<-as.numeric(dd$hospitalizirani..trenutno.[-1]) #cumulative! prispevek je lahko negativen!
      hospitalizirani<-as.numeric(hosp.hospitalizirani[,i]) #cumulative! prispevek je lahko negativen!
      
      hospitalizirani[is.na(hospitalizirani)]<-0
      
      # hospitalizirani<-c(rep(0,50),hospitalizirani)
      hospitalizirani<-c(rep(0,54),hospitalizirani)
      
      # hospitaliziraniin<-dd$novi[-1]
      hospitaliziraniin<-hosp.hospitaliziraniin[,i]
      
      hospitaliziraniin[is.na(hospitaliziraniin)]<-0
      
      # hospitaliziraniin<-c(rep(0,50),hospitaliziraniin)
      hospitaliziraniin<-c(rep(0,54),hospitaliziraniin)
      
      # hospitaliziraniout<-as.numeric(dd$iz.bol..oskrbe..vsi.[-1])
      hospitaliziraniout<-as.numeric(hosp.hospitaliziraniout[,i])
      
      hospitaliziraniout[is.na(hospitaliziraniout)]<-0
      
      # hospitaliziraniout<-c(rep(0,50),hospitaliziraniout)
      hospitaliziraniout<-c(rep(0,54),hospitaliziraniout)
      
      hospitaliziraniouti<-hospitaliziraniout
      for (ite in 2:length(hospitaliziraniout)){
        hospitaliziraniouti[ite]<-hospitaliziraniout[ite]-hospitaliziraniout[ite-1]
      }
      hospitaliziraniout<-hospitaliziraniouti
      
      # ICU:
      # icu<-c(rep(0,50), as.numeric(dd$intenzivna.enota..trenutno.[-1])) #cumulative! prispevek je lahko negativen!
      icu<-c(rep(0,54), as.numeric(hosp.icu[,i])) #cumulative! prispevek je lahko negativen!
      icu[is.na(icu)] <- 0
      
      # icuin<-c(rep(0,50), as.numeric(dd$I.i[-1])) #cumulative! prispevek je lahko negativen!
      icuin<-c(rep(0,54), as.numeric(hosp.icuin[,i])) #cumulative! prispevek je lahko negativen!
      icuin[is.na(icuin)] <- 0
      
      # icuout<-c(rep(0,50), as.numeric(dd$I.o[-1])) #cumulative! prispevek je lahko negativen!
      icuout<-c(rep(0,54), as.numeric(hosp.icuout[,i])) #cumulative! prispevek je lahko negativen!
      icuout[is.na(icuout)] <- 0
      
      
      later_data <- data.frame("time" = day[wh_dates],
                               "deaths" = deaths[wh_dates],
                               "deathsh" = deathsh[wh_dates],
                               "deathsc" = deathsc[wh_dates],
                               "cases" = cases[wh_dates],
                               "hosp" = hospitalizirani[wh_dates],
                               "hospi" = hospitaliziraniin[wh_dates],
                               "hospo" = hospitaliziraniout[wh_dates],
                               "icu" = icu[wh_dates],
                               "icui" = icuin[wh_dates],
                               "icuo" = icuout[wh_dates])
    } else{
      later_data <- data.frame()
    }
    ###
    
    p_iter <- make_single_plot_hospitals(data_country = data_country, data_country2 = data_country2,
                     data_deathsh = data_deathsh, data_deathsc = data_deathsc,
                     data_hosp = data_hosp, data_hospi = data_hospi, data_hospo = data_hospo,
                     data_icu = data_icu, data_icui = data_icui, data_icuo = data_icuo,
                     data_country_forecast = data_country_forecast,
                     data_deathsh_forecast = data_deathsh_forecast,
                     data_deathsc_forecast = data_deathsc_forecast,
                     data_cases_forecast = data_cases_forecast,
                     data_hosp_forecast = data_hosp_forecast,
                     data_hospi_forecast = data_hospi_forecast,
                     data_hospo_forecast = data_hospo_forecast,
                     data_icu_forecast = data_icu_forecast,
                     data_icui_forecast = data_icui_forecast,
                     data_icuo_forecast = data_icuo_forecast,
                     filename = filename,
                     country = country,
                     model_input = model_input,
                     later_data = later_data,
                     hospital.name=hospital.names[i])
    
    p_hosp[[i]] <- p_iter[[1]]
    p_icu[[i]] <- p_iter[[2]]
    p_deathsh[[i]] <- p_iter[[3]]
  }
  
  
  # Hosp:
  p_hosp <- plot_grid(p_hosp[[1]],p_hosp[[2]],p_hosp[[3]],p_hosp[[4]],
                      nrow=dim(estimated.hospitals)[3])
  cowplot::save_plot(filename = paste0("combined_hospitals/figures/", country, "_forecast_hosp_", filename, ".pdf"),
                     p_hosp, base_width = 14, base_height=7.42*2)
  # ICU: 
  p_icu <- plot_grid(p_icu[[1]],p_icu[[2]],p_icu[[3]],p_icu[[4]],
                     nrow=dim(estimated.hospitals)[3])
  cowplot::save_plot(filename = paste0("combined_hospitals/figures/", country, "_forecast_icu_", filename, ".pdf"),
                     p_icu, base_width = 14, base_height=7.42*2)
  # Deaths H: 
  p_deathsh <- plot_grid(p_deathsh[[1]],p_deathsh[[2]],p_deathsh[[3]],p_deathsh[[4]],
                         nrow=2)
  cowplot::save_plot(filename = paste0("combined_hospitals/figures/", country, "_forecast_deathsh_", filename, ".pdf"),
                     p_deathsh, base_width = 14*2/3, base_height=7.42)
  
  # Plots for Web, Mobile version
  dir.create("combined_hospitals/web/figures/mobile/", showWarnings = FALSE, recursive = TRUE)
  cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, '_', filename, "_forecast_hosp", ".svg"),
                     p_hosp, base_width = 14, base_height=7.42*2)
  cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, '_', filename, "_forecast_icu", ".svg"),
                     p_icu, base_width = 14, base_height=7.42*2)
  cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, '_', filename, "_forecast_deathsh", ".svg"),
                     p_deathsh, base_width = 14*2/3, base_height=7.42)
}



make_single_plot <- function(data_country, data_country2, 
                             data_deathsh, data_deathsc,
                             data_hosp, data_hospi, data_hospo, 
                             data_icu, data_icui, data_icuo,
                             data_country_forecast, 
                             data_deathsh_forecast, data_deathsc_forecast,
                             data_cases_forecast, 
                             data_hosp_forecast, data_hospi_forecast, data_hospo_forecast, 
                             data_icu_forecast, data_icui_forecast, data_icuo_forecast,
                             filename, country, model_input, later_data){
  
  language_english <- TRUE
  
  data_deaths <- data_country %>%
    select(time, deaths, estimated_deaths) %>%
    gather("key" = key, "value" = value, -time)
  
  data_deaths_forecast <- data_country_forecast %>%
    select(time, estimated_deaths_forecast) %>%
    gather("key" = key, "value" = value, -time)
  
  # Force less than 1 case to zero
  data_deaths$value[data_deaths$value < 1] <- NA
  data_deaths_forecast$value[data_deaths_forecast$value < 1] <- NA
  data_deaths_all <- rbind(data_deaths, data_deaths_forecast)
  
  # Lab texts:
  if(language_english) xlab_text <- 'Date'
  else xlab_text <- 'Datum'
  
  if(language_english) ylab_text_deaths <- 'Daily number of deaths\n'
  else ylab_text_deaths <- "Dnevno stevilo umrlih\n"

  
  ## Deaths:
  p1 <- ggplot(data_country) +
    geom_bar(data = data_country, aes(x = time, y = deaths), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p1 <- p1 +
      geom_bar(data = later_data, aes(x = time, y = deaths), 
               fill = "coral4", stat='identity', alpha=0.5)
  }

  p1 <- p1 + 
    geom_line(data = data_country, aes(x = time, y = estimated_deaths), 
              col = "deepskyblue4") + 
    geom_line(data = data_country_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_country, aes(x = time, 
                                         ymin = death_min, 
                                         ymax = death_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_country_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_deaths) + 
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_country$death_max, data_country$deaths, data_country_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_country$death_max, data_country$deaths, data_country_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")
  
  if(language_english) ylab_text_cases <- "Daily number of infections\n"
  else ylab_text_cases <- "Dnevno stevilo potrjeno okuzenih\n"

  
  
  ## Deaths H:
  # Lab texts:
  if(language_english) xlab_text <- 'Date'
  else xlab_text <- 'Datum'
  
  if(language_english) ylab_text_deaths <- 'Deaths H\n'
  else ylab_text_deaths <- "Dnevno stevilo umrlih\n"
  
  p1_h <- ggplot(data_deathsh) +
    geom_bar(data = data_deathsh, aes(x = time, y = deaths), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p1_h <- p1_h +
      geom_bar(data = later_data, aes(x = time, y = deathsh), 
               fill = "coral4", stat='identity', alpha=0.5)
  }
  
  p1_h <- p1_h + 
    geom_line(data = data_deathsh, aes(x = time, y = estimated_deaths), 
              col = "deepskyblue4") + 
    geom_line(data = data_deathsh_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_deathsh, aes(x = time, 
                                         ymin = death_min, 
                                         ymax = death_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_deathsh_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_deaths) + 
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_deathsh$death_max, data_deathsh$deaths, data_deathsh_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_deathsh$death_max, data_deathsh$deaths, data_deathsh_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")
  
  
  
  ## Deaths C:
  # Lab texts:
  if(language_english) xlab_text <- 'Date'
  else xlab_text <- 'Datum'
  
  if(language_english) ylab_text_deaths <- 'Deaths C\n'
  else ylab_text_deaths <- "Dnevno stevilo umrlih\n"
  
  p1_c <- ggplot(data_deathsc) +
    geom_bar(data = data_deathsc, aes(x = time, y = deaths), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p1_c <- p1_c +
      geom_bar(data = later_data, aes(x = time, y = deathsc), 
               fill = "coral4", stat='identity', alpha=0.5)
  }
  
  p1_c <- p1_c + 
    geom_line(data = data_deathsc, aes(x = time, y = estimated_deaths), 
              col = "deepskyblue4") + 
    geom_line(data = data_deathsc_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_deathsc, aes(x = time, 
                                         ymin = death_min, 
                                         ymax = death_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_deathsc_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_deaths) + 
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_deathsc$death_max, data_deathsc$deaths, data_deathsc_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_deathsc$death_max, data_deathsc$deaths, data_deathsc_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")
  
  
  
  
  if(language_english) ylab_text_cases <- "Daily number of confirmed infections\n"
  else ylab_text_cases <- "Dnevno stevilo potrjeno okuzenih\n"
  
  ## Cases:
  p2 <- ggplot(data_country) +
    geom_bar(data = data_country, aes(x = time, y = reported_cases), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p2 <- p2  +
      geom_bar(data = later_data, aes(x = time, y = cases), 
               fill = "coral4", stat='identity', alpha=0.5)
  }
  
  # if(data_country_forecast$country[1]=='Slovenia'){
  #   p <- p +
  #     geom_bar(data = data_country_forecast[2:nrow(data_country_forecast),], aes(x = time, y = deaths), 
  #              fill = "coral4", stat='identity', alpha=0.5)
  # }
  
  p2 <- p2 + 
    geom_line(data = data_country2, aes(x = time, y = estimated_cases), 
              col = "deepskyblue4") + 
    geom_line(data = data_cases_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_country2, aes(x = time, 
                                         ymin = cases_min, 
                                         ymax = cases_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_cases_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_cases) + 
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_country2$cases_max, data_country$reported_cases, data_cases_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_country2$cases_max, data_country$reported_cases, data_cases_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")
  
  
  if(language_english) ylab_text_hosp <- "Number of hospitalized patients\n"
  else ylab_text_hosp <- "Stevilo hospitaliziranih\n"

  ## Hospitalizations:
  p3 <- ggplot(data_country) +
    geom_bar(data = data_hosp, aes(x = time, y = reported_hosp), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p3 <- p3  +
      geom_bar(data = later_data, aes(x = time, y = hosp), 
               fill = "coral4", stat='identity', alpha=0.5)
  }
  
  # if(data_country_forecast$country[1]=='Slovenia'){
  #   p <- p +
  #     geom_bar(data = data_country_forecast[2:nrow(data_country_forecast),], aes(x = time, y = deaths), 
  #              fill = "coral4", stat='identity', alpha=0.5)
  # }

  p3 <- p3 + 
    geom_line(data = data_hosp, aes(x = time, y = estimated_hosp), 
              col = "deepskyblue4") + 
    geom_line(data = data_hosp_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_hosp, aes(x = time, 
                                          ymin = hosp_min, 
                                          ymax = hosp_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_hosp_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_hosp)+
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_hosp$hosp_max, data_hosp$reported_hosp, data_hosp_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_hosp$hosp_max, data_hosp$reported_hosp, data_hosp_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")
  
  if(language_english) ylab_text_hosp <- "Hospitalized IN\n"
  else ylab_text_hosp <- "Stevilo hospitaliziranih\n"
  
  ## Hospitalizations IN:
  p3_IN <- ggplot(data_country) +
    geom_bar(data = data_hospi, aes(x = time, y = reported_hosp), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p3_IN <- p3_IN  +
      geom_bar(data = later_data, aes(x = time, y = hospi), 
               fill = "coral4", stat='identity', alpha=0.5)
  }
  
  # if(data_country_forecast$country[1]=='Slovenia'){
  #   p <- p +
  #     geom_bar(data = data_country_forecast[2:nrow(data_country_forecast),], aes(x = time, y = deaths), 
  #              fill = "coral4", stat='identity', alpha=0.5)
  # }
  
  p3_IN <- p3_IN + 
    geom_line(data = data_hospi, aes(x = time, y = estimated_hosp), 
              col = "deepskyblue4") + 
    geom_line(data = data_hospi_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_hospi, aes(x = time, 
                                      ymin = hosp_min, 
                                      ymax = hosp_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_hospi_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_hosp)+
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_hospi$hosp_max, data_hospi$reported_hosp, data_hospi_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_hospi$hosp_max, data_hospi$reported_hosp, data_hospi_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")

  if(language_english) ylab_text_hosp <- "Hospitalized OUT\n"
  else ylab_text_hosp <- "Stevilo hospitaliziranih\n"
  
  ## Hospitalizations OUT:
  p3_OUT <- ggplot(data_country) +
    geom_bar(data = data_hospo, aes(x = time, y = reported_hosp), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p3_OUT <- p3_OUT  +
      geom_bar(data = later_data, aes(x = time, y = hospo), 
               fill = "coral4", stat='identity', alpha=0.5)
  }
  
  # if(data_country_forecast$country[1]=='Slovenia'){
  #   p <- p +
  #     geom_bar(data = data_country_forecast[2:nrow(data_country_forecast),], aes(x = time, y = deaths), 
  #              fill = "coral4", stat='identity', alpha=0.5)
  # }
  
  p3_OUT <- p3_OUT + 
    geom_line(data = data_hospo, aes(x = time, y = estimated_hosp), 
              col = "deepskyblue4") + 
    geom_line(data = data_hospo_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_hospo, aes(x = time, 
                                       ymin = hosp_min, 
                                       ymax = hosp_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_hospo_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_hosp)+
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_hospo$hosp_max, data_hospo$reported_hosp, data_hospo_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_hospo$hosp_max, data_hospo$reported_hosp, data_hospo_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")
    
  if(language_english) ylab_text_icu <- "Number of patients in ICU\n"
  else ylab_text_icu <- "Stevilo bolnikov na intenzivni negi\n"
  
  ## ICUs:
  p4 <- ggplot(data_country) +
    geom_bar(data = data_icu, aes(x = time, y = reported_icu), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p4 <- p4  +
      geom_bar(data = later_data, aes(x = time, y = icu), 
               fill = "coral4", stat='identity', alpha=0.5)
  }
  
  # if(data_country_forecast$country[1]=='Slovenia'){
  #   p <- p +
  #     geom_bar(data = data_country_forecast[2:nrow(data_country_forecast),], aes(x = time, y = deaths), 
  #              fill = "coral4", stat='identity', alpha=0.5)
  # }
  
  p4 <- p4 + 
    geom_line(data = data_icu, aes(x = time, y = estimated_icu), 
              col = "deepskyblue4") + 
    geom_line(data = data_icu_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_icu, aes(x = time, 
                                      ymin = icu_min, 
                                      ymax = icu_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_icu_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_icu)+
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_icu$icu_max, data_icu$reported_icu, data_icu_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_icu$icu_max, data_icu$reported_icu, data_icu_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")

  if(language_english) ylab_text_icu <- "ICU IN\n"
  else ylab_text_icu <- "Stevilo bolnikov na intenzivni negi\n"
  
  ## ICUs IN:
  p4_IN <- ggplot(data_country) +
    geom_bar(data = data_icui, aes(x = time, y = reported_icu), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p4_IN <- p4_IN  +
      geom_bar(data = later_data, aes(x = time, y = icui), 
               fill = "coral4", stat='identity', alpha=0.5)
  }
  
  # if(data_country_forecast$country[1]=='Slovenia'){
  #   p <- p +
  #     geom_bar(data = data_country_forecast[2:nrow(data_country_forecast),], aes(x = time, y = deaths), 
  #              fill = "coral4", stat='identity', alpha=0.5)
  # }
  
  p4_IN <- p4_IN + 
    geom_line(data = data_icui, aes(x = time, y = estimated_icu), 
              col = "deepskyblue4") + 
    geom_line(data = data_icui_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_icui, aes(x = time, 
                                     ymin = icu_min, 
                                     ymax = icu_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_icui_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_icu)+
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_icui$icu_max, data_icui$reported_icu, data_icui_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_icui$icu_max, data_icui$reported_icu, data_icui_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")
    
  if(language_english) ylab_text_icu <- "ICU OUT\n"
  else ylab_text_icu <- "Stevilo bolnikov na intenzivni negi\n"
  
  ## ICUs OUT:
  p4_OUT <- ggplot(data_country) +
    geom_bar(data = data_icuo, aes(x = time, y = reported_icu), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p4_OUT <- p4_OUT  +
      geom_bar(data = later_data, aes(x = time, y = icuo), 
               fill = "coral4", stat='identity', alpha=0.5)
  }
  
  # if(data_country_forecast$country[1]=='Slovenia'){
  #   p <- p +
  #     geom_bar(data = data_country_forecast[2:nrow(data_country_forecast),], aes(x = time, y = deaths), 
  #              fill = "coral4", stat='identity', alpha=0.5)
  # }
  
  p4_OUT <- p4_OUT + 
    geom_line(data = data_icuo, aes(x = time, y = estimated_icu), 
              col = "deepskyblue4") + 
    geom_line(data = data_icuo_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_icuo, aes(x = time, 
                                      ymin = icu_min, 
                                      ymax = icu_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_icuo_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_icu)+
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_icuo$icu_max, data_icuo$reported_icu, data_icuo_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_icuo$icu_max, data_icuo$reported_icu, data_icuo_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")
  
  # p <- plot_grid(p1,p2, p3, p4, 
  #                p1_h, p1_c, p3_IN, p3_OUT, p4_IN, p4_OUT, 
  #                ncol = 4)
  p <- plot_grid(p2, p1, p1_h, ncol=3)
  save_plot(filename = paste0("combined_hospitals/figures/", country, "_forecast_", filename, ".pdf"), 
            # p, base_width = 10*4/3, base_height=7*3/2)
            p, base_width = 14, base_height=5)

  # Produce plots for Website
  dir.create("combined_hospitals/web/figures/desktop/", showWarnings = FALSE, recursive = TRUE)
  save_plot(filename = paste0("combined_hospitals/web/figures/desktop/", country, "_forecast_", filename, ".svg"), 
            p, base_width = 14, base_height=7)
  dir.create("combined_hospitals/web/figures/mobile/", showWarnings = FALSE, recursive = TRUE)
  save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, "_forecast_", filename, ".svg"), 
            p, base_width = 10, base_height=3
            # base_height = 4, base_asp = 1.1
            )
  
  ###
  # Plot only 4 graphs:
  p <- plot_grid(p1,p2, p3, p4, 
                 ncol = 4)
  save_plot(filename = paste0("combinedTDI/figures/", country, "_forecast4_", filename, ".pdf"), 
            p, base_width = 14)
  
  # Produce plots for Website
  dir.create("combinedTDI/web/figures/desktop/", showWarnings = FALSE, recursive = TRUE)
  save_plot(filename = paste0("combinedTDI/web/figures/desktop/", country, "_forecast4_", filename, ".svg"), 
            p, base_width = 14#, base_height=7
  )
  dir.create("combinedTDI/web/figures/mobile/", showWarnings = FALSE, recursive = TRUE)
  save_plot(filename = paste0("combinedTDI/web/figures/mobile/", country, "_forecast4_", filename, ".svg"), 
            p, base_width = 14#, base_height=7
            # base_height = 4, base_asp = 1.1
  )
  
  
  
  # Additional plot for ibmi html:
  if(file.exists(paste0("combined_hospitals/results/", filename, '_', "graphs.RData"))){
    
    p6 <- p4
    p5 <- p3
    p4 <- p2
    p2 <- p1
    load(file=paste0("combined_hospitals/results/", filename, '_', "graphs.RData"))
    p3_mobile <- p3_mobile +
      theme(legend.text=element_text(size=7))
    p_grouped <- plot_grid(p1, p4, p3_mobile, 
                           ncol = 3, rel_widths = c(1, 1, 1))
    p_grouped2 <- plot_grid(p2, p1_h, p1_c, 
                            ncol = 3, rel_widths = c(1,1,1))
    # p_grouped3 <- plot_grid(p5, p3_IN, p3_OUT, 
    #                         ncol = 3, rel_widths = c(1,1,1))
    # p_grouped4 <- plot_grid(p6, p4_IN, p4_OUT,
    #                         ncol = 3, rel_widths = c(1,1,1))
    p <- plot_grid(p_grouped, p_grouped2, 
                   # p_grouped3, p_grouped4, 
                   nrow=2)
    # cowplot::save_plot(filename = paste0("figures/", country, "_together_", filename, ".pdf"), 
    #                    p, base_width = 14, base_height=7.42)
    save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, "_together_", filename, ".svg"), 
              p, base_width = 14, base_height=7.42
    )
  }
  
  # Calculate MSE:
  if(nrow(later_data)>0){
    deaths_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
      (later_data$deaths[iti] -  data_country_forecast$estimated_deaths_forecast[iti+1])^2))
    deathsh_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
      (later_data$deathsh[iti] -  data_deathsh_forecast$estimated_deaths_forecast[iti+1])^2))
    deathsc_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
      (later_data$deathsc[iti] -  data_deathsc_forecast$estimated_deaths_forecast[iti+1])^2))
    
    cases_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
      (later_data$cases[iti] -  data_cases_forecast$estimated_deaths_forecast[iti+1])^2))
    
    hosp_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
      (later_data$hosp[iti] -  data_hosp_forecast$estimated_deaths_forecast[iti+1])^2))
    hospi_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
      (later_data$hospi[iti] -  data_hospi_forecast$estimated_deaths_forecast[iti+1])^2))
    hospo_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
      (later_data$hospo[iti] -  data_hospo_forecast$estimated_deaths_forecast[iti+1])^2))
    
    icu_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
      (later_data$icu[iti] -  data_icu_forecast$estimated_deaths_forecast[iti+1])^2))
    icui_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
      (later_data$icui[iti] -  data_icui_forecast$estimated_deaths_forecast[iti+1])^2))
    icuo_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
      (later_data$icuo[iti] -  data_icuo_forecast$estimated_deaths_forecast[iti+1])^2))
    
    mse_df <- data.frame(forecast=nrow(later_data),
                         deaths_MSE=deaths_MSE,
                         deathsh_MSE=deathsh_MSE,
                         deathsc_MSE=deathsc_MSE,
                         cases_MSE=cases_MSE,
                         hosp_MSE=hosp_MSE,
                         hospi_MSE=hospi_MSE,
                         hospo_MSE=hospo_MSE,
                         icu_MSE=icu_MSE,
                         icui_MSE=icui_MSE,
                         icuo_MSE=icuo_MSE)
  } else{
    mse_df <- data.frame(forecast=0,
                         deaths_MSE=0,
                         deathsh_MSE=0,
                         deathsc_MSE=0,
                         cases_MSE=0,
                         hosp_MSE=0,
                         hospi_MSE=0,
                         hospo_MSE=0,
                         icu_MSE=0,
                         icui_MSE=0,
                         icuo_MSE=0)
  }
  save(mse_df, file=paste0("combined_hospitals/results/", country, "-MSE-", filename, ".RData"))
  
}








make_single_plot_hospitals <- function(data_country, data_country2, 
                             data_deathsh, data_deathsc,
                             data_hosp, data_hospi, data_hospo, 
                             data_icu, data_icui, data_icuo,
                             data_country_forecast, 
                             data_deathsh_forecast, data_deathsc_forecast,
                             data_cases_forecast, 
                             data_hosp_forecast, data_hospi_forecast, data_hospo_forecast, 
                             data_icu_forecast, data_icui_forecast, data_icuo_forecast,
                             filename, country, model_input, later_data, hospital.name){
  
  language_english <- TRUE
  
  data_deaths <- data_country %>%
    select(time, deaths, estimated_deaths) %>%
    gather("key" = key, "value" = value, -time)
  
  data_deaths_forecast <- data_country_forecast %>%
    select(time, estimated_deaths_forecast) %>%
    gather("key" = key, "value" = value, -time)
  
  # Force less than 1 case to zero
  data_deaths$value[data_deaths$value < 1] <- NA
  data_deaths_forecast$value[data_deaths_forecast$value < 1] <- NA
  data_deaths_all <- rbind(data_deaths, data_deaths_forecast)
  
  # Lab texts:
  if(language_english) xlab_text <- 'Date'
  else xlab_text <- 'Datum'
  
  if(language_english) ylab_text_deaths <- 'Daily number of deaths\n'
  else ylab_text_deaths <- "Dnevno stevilo umrlih\n"
  
  
  ## Deaths:
  p1 <- ggplot(data_country) +
    geom_bar(data = data_country, aes(x = time, y = deaths), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p1 <- p1 +
      geom_bar(data = later_data, aes(x = time, y = deaths), 
               fill = "coral4", stat='identity', alpha=0.5)
  }
  
  p1 <- p1 + 
    geom_line(data = data_country, aes(x = time, y = estimated_deaths), 
              col = "deepskyblue4") + 
    geom_line(data = data_country_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_country, aes(x = time, 
                                         ymin = death_min, 
                                         ymax = death_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_country_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_deaths) + 
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_country$death_max, data_country$deaths, data_country_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_country$death_max, data_country$deaths, data_country_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")
  
  if(language_english) ylab_text_cases <- "Daily number of infections\n"
  else ylab_text_cases <- "Dnevno stevilo potrjeno okuzenih\n"
  
  
  
  ## Deaths H:
  # Lab texts:
  if(language_english) xlab_text <- 'Date'
  else xlab_text <- 'Datum'
  
  if(language_english) ylab_text_deaths <- 'Deaths H\n'
  else ylab_text_deaths <- "Dnevno stevilo umrlih\n"
  
  p1_h <- ggplot(data_deathsh) +
    geom_bar(data = data_deathsh, aes(x = time, y = deaths), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p1_h <- p1_h +
      geom_bar(data = later_data, aes(x = time, y = deathsh), 
               fill = "coral4", stat='identity', alpha=0.5)
  }
  
  p1_h <- p1_h + 
    geom_line(data = data_deathsh, aes(x = time, y = estimated_deaths), 
              col = "deepskyblue4") + 
    geom_line(data = data_deathsh_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_deathsh, aes(x = time, 
                                         ymin = death_min, 
                                         ymax = death_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_deathsh_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_deaths) + 
    ggtitle(hospital.name)+
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_deathsh$death_max, data_deathsh$deaths, data_deathsh_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_deathsh$death_max, data_deathsh$deaths, data_deathsh_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")
  
  
  
  ## Deaths C:
  # Lab texts:
  if(language_english) xlab_text <- 'Date'
  else xlab_text <- 'Datum'
  
  if(language_english) ylab_text_deaths <- 'Deaths C\n'
  else ylab_text_deaths <- "Dnevno stevilo umrlih\n"
  
  p1_c <- ggplot(data_deathsc) +
    geom_bar(data = data_deathsc, aes(x = time, y = deaths), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p1_c <- p1_c +
      geom_bar(data = later_data, aes(x = time, y = deathsc), 
               fill = "coral4", stat='identity', alpha=0.5)
  }
  
  p1_c <- p1_c + 
    geom_line(data = data_deathsc, aes(x = time, y = estimated_deaths), 
              col = "deepskyblue4") + 
    geom_line(data = data_deathsc_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_deathsc, aes(x = time, 
                                         ymin = death_min, 
                                         ymax = death_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_deathsc_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_deaths) + 
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_deathsc$death_max, data_deathsc$deaths, data_deathsc_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_deathsc$death_max, data_deathsc$deaths, data_deathsc_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")
  
  
  
  
  if(language_english) ylab_text_cases <- "Daily number of confirmed infections\n"
  else ylab_text_cases <- "Dnevno stevilo potrjeno okuzenih\n"
  
  ## Cases:
  p2 <- ggplot(data_country) +
    geom_bar(data = data_country, aes(x = time, y = reported_cases), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p2 <- p2  +
      geom_bar(data = later_data, aes(x = time, y = cases), 
               fill = "coral4", stat='identity', alpha=0.5)
  }
  
  # if(data_country_forecast$country[1]=='Slovenia'){
  #   p <- p +
  #     geom_bar(data = data_country_forecast[2:nrow(data_country_forecast),], aes(x = time, y = deaths), 
  #              fill = "coral4", stat='identity', alpha=0.5)
  # }
  
  p2 <- p2 + 
    geom_line(data = data_country2, aes(x = time, y = estimated_cases), 
              col = "deepskyblue4") + 
    geom_line(data = data_cases_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_country2, aes(x = time, 
                                          ymin = cases_min, 
                                          ymax = cases_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_cases_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_cases) + 
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_country2$cases_max, data_country$reported_cases, data_cases_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_country2$cases_max, data_country$reported_cases, data_cases_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")
  
  
  if(language_english) ylab_text_hosp <- "Number of hospitalized patients\n"
  else ylab_text_hosp <- "Stevilo hospitaliziranih\n"
  
  ## Hospitalizations:
  p3 <- ggplot(data_country) +
    geom_bar(data = data_hosp, aes(x = time, y = reported_hosp), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p3 <- p3  +
      geom_bar(data = later_data, aes(x = time, y = hosp), 
               fill = "coral4", stat='identity', alpha=0.5)
  }
  
  # if(data_country_forecast$country[1]=='Slovenia'){
  #   p <- p +
  #     geom_bar(data = data_country_forecast[2:nrow(data_country_forecast),], aes(x = time, y = deaths), 
  #              fill = "coral4", stat='identity', alpha=0.5)
  # }
  
  p3 <- p3 + 
    geom_line(data = data_hosp, aes(x = time, y = estimated_hosp), 
              col = "deepskyblue4") + 
    geom_line(data = data_hosp_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_hosp, aes(x = time, 
                                      ymin = hosp_min, 
                                      ymax = hosp_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_hosp_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_hosp)+
    ggtitle(hospital.name)+
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_hosp$hosp_max, data_hosp$reported_hosp, data_hosp_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_hosp$hosp_max, data_hosp$reported_hosp, data_hosp_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")
  
  if(language_english) ylab_text_hosp <- "Hospitalized IN\n"
  else ylab_text_hosp <- "Stevilo hospitaliziranih\n"
  
  ## Hospitalizations IN:
  p3_IN <- ggplot(data_country) +
    geom_bar(data = data_hospi, aes(x = time, y = reported_hosp), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p3_IN <- p3_IN  +
      geom_bar(data = later_data, aes(x = time, y = hospi), 
               fill = "coral4", stat='identity', alpha=0.5)
  }
  
  # if(data_country_forecast$country[1]=='Slovenia'){
  #   p <- p +
  #     geom_bar(data = data_country_forecast[2:nrow(data_country_forecast),], aes(x = time, y = deaths), 
  #              fill = "coral4", stat='identity', alpha=0.5)
  # }
  
  p3_IN <- p3_IN + 
    geom_line(data = data_hospi, aes(x = time, y = estimated_hosp), 
              col = "deepskyblue4") + 
    geom_line(data = data_hospi_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_hospi, aes(x = time, 
                                       ymin = hosp_min, 
                                       ymax = hosp_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_hospi_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_hosp)+
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_hospi$hosp_max, data_hospi$reported_hosp, data_hospi_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_hospi$hosp_max, data_hospi$reported_hosp, data_hospi_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")
  
  if(language_english) ylab_text_hosp <- "Hospitalized OUT\n"
  else ylab_text_hosp <- "Stevilo hospitaliziranih\n"
  
  ## Hospitalizations OUT:
  p3_OUT <- ggplot(data_country) +
    geom_bar(data = data_hospo, aes(x = time, y = reported_hosp), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p3_OUT <- p3_OUT  +
      geom_bar(data = later_data, aes(x = time, y = hospo), 
               fill = "coral4", stat='identity', alpha=0.5)
  }
  
  # if(data_country_forecast$country[1]=='Slovenia'){
  #   p <- p +
  #     geom_bar(data = data_country_forecast[2:nrow(data_country_forecast),], aes(x = time, y = deaths), 
  #              fill = "coral4", stat='identity', alpha=0.5)
  # }
  
  p3_OUT <- p3_OUT + 
    geom_line(data = data_hospo, aes(x = time, y = estimated_hosp), 
              col = "deepskyblue4") + 
    geom_line(data = data_hospo_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_hospo, aes(x = time, 
                                       ymin = hosp_min, 
                                       ymax = hosp_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_hospo_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_hosp)+
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_hospo$hosp_max, data_hospo$reported_hosp, data_hospo_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_hospo$hosp_max, data_hospo$reported_hosp, data_hospo_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")
  
  if(language_english) ylab_text_icu <- "Number of patients in ICU\n"
  else ylab_text_icu <- "Stevilo bolnikov na intenzivni negi\n"
  
  ## ICUs:
  p4 <- ggplot(data_country) +
    geom_bar(data = data_icu, aes(x = time, y = reported_icu), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p4 <- p4  +
      geom_bar(data = later_data, aes(x = time, y = icu), 
               fill = "coral4", stat='identity', alpha=0.5)
  }
  
  # if(data_country_forecast$country[1]=='Slovenia'){
  #   p <- p +
  #     geom_bar(data = data_country_forecast[2:nrow(data_country_forecast),], aes(x = time, y = deaths), 
  #              fill = "coral4", stat='identity', alpha=0.5)
  # }
  
  p4 <- p4 + 
    geom_line(data = data_icu, aes(x = time, y = estimated_icu), 
              col = "deepskyblue4") + 
    geom_line(data = data_icu_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_icu, aes(x = time, 
                                     ymin = icu_min, 
                                     ymax = icu_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_icu_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_icu)+
    ggtitle(hospital.name)+
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_icu$icu_max, data_icu$reported_icu, data_icu_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_icu$icu_max, data_icu$reported_icu, data_icu_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")
  
  if(language_english) ylab_text_icu <- "ICU IN\n"
  else ylab_text_icu <- "Stevilo bolnikov na intenzivni negi\n"
  
  ## ICUs IN:
  p4_IN <- ggplot(data_country) +
    geom_bar(data = data_icui, aes(x = time, y = reported_icu), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p4_IN <- p4_IN  +
      geom_bar(data = later_data, aes(x = time, y = icui), 
               fill = "coral4", stat='identity', alpha=0.5)
  }
  
  # if(data_country_forecast$country[1]=='Slovenia'){
  #   p <- p +
  #     geom_bar(data = data_country_forecast[2:nrow(data_country_forecast),], aes(x = time, y = deaths), 
  #              fill = "coral4", stat='identity', alpha=0.5)
  # }
  
  p4_IN <- p4_IN + 
    geom_line(data = data_icui, aes(x = time, y = estimated_icu), 
              col = "deepskyblue4") + 
    geom_line(data = data_icui_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_icui, aes(x = time, 
                                      ymin = icu_min, 
                                      ymax = icu_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_icui_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_icu)+
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_icui$icu_max, data_icui$reported_icu, data_icui_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_icui$icu_max, data_icui$reported_icu, data_icui_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")
  
  if(language_english) ylab_text_icu <- "ICU OUT\n"
  else ylab_text_icu <- "Stevilo bolnikov na intenzivni negi\n"
  
  ## ICUs OUT:
  p4_OUT <- ggplot(data_country) +
    geom_bar(data = data_icuo, aes(x = time, y = reported_icu), 
             fill = "coral4", stat='identity', alpha=0.5)
  
  if(!identical(later_data, data.frame())){
    p4_OUT <- p4_OUT  +
      geom_bar(data = later_data, aes(x = time, y = icuo), 
               fill = "coral4", stat='identity', alpha=0.5)
  }
  
  # if(data_country_forecast$country[1]=='Slovenia'){
  #   p <- p +
  #     geom_bar(data = data_country_forecast[2:nrow(data_country_forecast),], aes(x = time, y = deaths), 
  #              fill = "coral4", stat='identity', alpha=0.5)
  # }
  
  p4_OUT <- p4_OUT + 
    geom_line(data = data_icuo, aes(x = time, y = estimated_icu), 
              col = "deepskyblue4") + 
    geom_line(data = data_icuo_forecast, 
              aes(x = time, y = estimated_deaths_forecast), 
              col = "black", alpha = 0.5) + 
    geom_ribbon(data = data_icuo, aes(x = time, 
                                      ymin = icu_min, 
                                      ymax = icu_max),
                fill="deepskyblue4", alpha=0.3) +
    geom_ribbon(data = data_icuo_forecast, 
                aes(x = time, 
                    ymin = death_min_forecast, 
                    ymax = death_max_forecast),
                fill = "black", alpha=0.35) +
    geom_vline(xintercept = data_deaths$time[length(data_deaths$time)], 
               col = "black", linetype = "dashed", alpha = 0.5) + 
    #scale_fill_manual(name = "", 
    #                 labels = c("Confirmed deaths", "Predicted deaths"),
    #                 values = c("coral4", "deepskyblue4")) + 
    xlab(xlab_text) +
    ylab(ylab_text_icu)+
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    # scale_y_continuous(trans='log10', labels=comma) + 
    scale_y_continuous(breaks = seq(0, ceiling(max(c(data_icuo$icu_max, data_icuo$reported_icu, data_icuo_forecast$death_max_forecast))), length.out = 6))+
    coord_cartesian(ylim = c(0, ceiling(max(c(data_icuo$icu_max, data_icuo$reported_icu, data_icuo_forecast$death_max_forecast)))), expand = FALSE) +
    # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    # coord_cartesian(ylim = c(0, 10), expand = FALSE) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) + 
    annotate(geom="text", x=data_country$time[length(data_country$time)]+8, 
             y=10000, label="",
             color="black")
  
  # p <- plot_grid(p1,p2, p3, p4, 
  #                p1_h, p1_c, p3_IN, p3_OUT, p4_IN, p4_OUT, 
  #                ncol = 4)
  # save_plot(filename = paste0("combined_hospitals/figures/", country, "_forecast_", filename, ".pdf"), 
  #           p, base_width = 10*4/3, base_height=7*3/2)
  
  # Produce plots for Website
  # dir.create("combined_hospitals/web/figures/desktop/", showWarnings = FALSE, recursive = TRUE)
  # save_plot(filename = paste0("combined_hospitals/web/figures/desktop/", country, "_forecast_", filename, ".svg"), 
  #           p, base_width = 14, base_height=7)
  # dir.create("combined_hospitals/web/figures/mobile/", showWarnings = FALSE, recursive = TRUE)
  # save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, "_forecast_", filename, ".svg"), 
  #           p, base_width = 10, base_height=7
  #           # base_height = 4, base_asp = 1.1
  # )
  
  p_hosp = plot_grid(p3, p3_IN, p3_OUT, ncol=3)
  p_icu = plot_grid(p4, p4_IN, p4_OUT, ncol=3)
  
  return(list(p_hosp, p_icu, p1_h))
  
  # Additional plot for ibmi html:
  # if(file.exists(paste0("combined_hospitals/results/", filename, '_', "graphs.RData"))){
  #   
  #   p6 <- p4
  #   p5 <- p3
  #   p4 <- p2
  #   p2 <- p1
  #   load(file=paste0("combined_hospitals/results/", filename, '_', "graphs.RData"))
  #   p3_mobile <- p3_mobile +
  #     theme(legend.text=element_text(size=7))
  #   p_grouped <- plot_grid(p1, p4, p3_mobile, 
  #                          ncol = 3, rel_widths = c(1, 1, 1))
  #   p_grouped2 <- plot_grid(p2, p1_h, p1_c, 
  #                           ncol = 3, rel_widths = c(1,1,1))
  #   p_grouped3 <- plot_grid(p5, p3_IN, p3_OUT, 
  #                           ncol = 3, rel_widths = c(1,1,1))
  #   p_grouped4 <- plot_grid(p6, p4_IN, p4_OUT, 
  #                           ncol = 3, rel_widths = c(1,1,1))
  #   p <- plot_grid(p_grouped, p_grouped2, 
  #                  p_grouped3, p_grouped4, nrow=4)
  #   # cowplot::save_plot(filename = paste0("figures/", country, "_together_", filename, ".pdf"), 
  #   #                    p, base_width = 14, base_height=7.42)
  #   save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, "_together_", filename, ".svg"), 
  #             p, base_width = 14, base_height=7.42*2
  #   )
  # }
  # 
  # # Calculate MSE:
  # if(nrow(later_data)>0){
  #   deaths_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
  #     (later_data$deaths[iti] -  data_country_forecast$estimated_deaths_forecast[iti+1])^2))
  #   deathsh_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
  #     (later_data$deathsh[iti] -  data_deathsh_forecast$estimated_deaths_forecast[iti+1])^2))
  #   deathsc_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
  #     (later_data$deathsc[iti] -  data_deathsc_forecast$estimated_deaths_forecast[iti+1])^2))
  #   
  #   cases_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
  #     (later_data$cases[iti] -  data_cases_forecast$estimated_deaths_forecast[iti+1])^2))
  #   
  #   hosp_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
  #     (later_data$hosp[iti] -  data_hosp_forecast$estimated_deaths_forecast[iti+1])^2))
  #   hospi_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
  #     (later_data$hospi[iti] -  data_hospi_forecast$estimated_deaths_forecast[iti+1])^2))
  #   hospo_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
  #     (later_data$hospo[iti] -  data_hospo_forecast$estimated_deaths_forecast[iti+1])^2))
  #   
  #   icu_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
  #     (later_data$icu[iti] -  data_icu_forecast$estimated_deaths_forecast[iti+1])^2))
  #   icui_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
  #     (later_data$icui[iti] -  data_icui_forecast$estimated_deaths_forecast[iti+1])^2))
  #   icuo_MSE <- mean(sapply(1:nrow(later_data), function(iti) 
  #     (later_data$icuo[iti] -  data_icuo_forecast$estimated_deaths_forecast[iti+1])^2))
  #   
  #   mse_df <- data.frame(forecast=nrow(later_data),
  #                        deaths_MSE=deaths_MSE,
  #                        deathsh_MSE=deathsh_MSE,
  #                        deathsc_MSE=deathsc_MSE,
  #                        cases_MSE=cases_MSE,
  #                        hosp_MSE=hosp_MSE,
  #                        hospi_MSE=hospi_MSE,
  #                        hospo_MSE=hospo_MSE,
  #                        icu_MSE=icu_MSE,
  #                        icui_MSE=icui_MSE,
  #                        icuo_MSE=icuo_MSE)
  # } else{
  #   mse_df <- data.frame(forecast=0,
  #                        deaths_MSE=0,
  #                        deathsh_MSE=0,
  #                        deathsc_MSE=0,
  #                        cases_MSE=0,
  #                        hosp_MSE=0,
  #                        hospi_MSE=0,
  #                        hospo_MSE=0,
  #                        icu_MSE=0,
  #                        icui_MSE=0,
  #                        icuo_MSE=0)
  # }
  # save(mse_df, file=paste0("combined_hospitals/results/", country, "-MSE-", filename, ".RData"))
  
}
#-----------------------------------------------------------------------------------------------


make_forecast_plot()