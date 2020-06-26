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
library(svglite)
library(ggplot2)

source("geom-stepribbon.r")
#---------------------------------------------------------------------------
make_three_pannel_plot <- function(){
  
  args <- commandArgs(trailingOnly = TRUE)
  
  filename2 = args[1]
  percent_pop = FALSE
  
  load(paste0("combined_hospitals/results/", filename2))
  print(sprintf("loading: %s",paste0("results/",filename2)))

  filename2 <- strsplit(filename2, "-stanfit.Rdata")[[1]][1]
  
  # covariates = read.csv('data/interventions.csv', stringsAsFactors = FALSE)
  # 
  # # ADD SLOVENIA:
  # covariates_slo <- covariates %>% filter(Country=='Sweden')
  # covariates_slo$Country <- 'Slovenia'
  # covariates_slo$Event <- ''
  # covariates_slo$Date.effective <- c("16.03.2020", "10.03.2020", 
  #                                    "20.03.2020", "16.03.2020",
  #                                    "09.03.2020")
  # covariates <- rbind(covariates, covariates_slo)
  # 
  # names_covariates = c('Schools + Universities','Self-isolating if ill', 'Public events', 
  #                      'Lockdown', 'Social distancing encouraged')
  # covariates <- covariates %>%
  #   filter((Type %in% names_covariates))
  # covariates <- covariates[,c(1,2,4)]
  # covariates <- spread(covariates, Type, Date.effective)
  # names(covariates) <- c('Country','lockdown', 'public_events', 'schools_universities','self_isolating_if_ill', 'social_distancing_encouraged')
  # covariates <- covariates[c('Country','schools_universities', 'self_isolating_if_ill', 'public_events', 'lockdown', 'social_distancing_encouraged')]
  # covariates$schools_universities <- as.Date(covariates$schools_universities, format = "%d.%m.%Y")
  # covariates$lockdown <- as.Date(covariates$lockdown, format = "%d.%m.%Y")
  # covariates$public_events <- as.Date(covariates$public_events, format = "%d.%m.%Y")
  # covariates$self_isolating_if_ill <- as.Date(covariates$self_isolating_if_ill, format = "%d.%m.%Y")
  # covariates$social_distancing_encouraged <- as.Date(covariates$social_distancing_encouraged, format = "%d.%m.%Y")

  all_data <- data.frame()
  intervention_data <- data.frame()
  
  for(i in 1:length(countries)){
    print(i)
    N <- length(dates[[i]])
    country <- countries[[i]]
    
    predicted_cases <- colMeans(prediction[,1:N,i])
    predicted_cases_li <- colQuantiles(prediction[,1:N,i], probs=.025)
    predicted_cases_ui <- colQuantiles(prediction[,1:N,i], probs=.975)
    predicted_cases_li2 <- colQuantiles(prediction[,1:N,i], probs=.25)
    predicted_cases_ui2 <- colQuantiles(prediction[,1:N,i], probs=.75)
    
    
    estimated_deaths <- colMeans(estimated.deaths[,1:N,i])
    estimated_deaths_li <- colQuantiles(estimated.deaths[,1:N,i], probs=.025)
    estimated_deaths_ui <- colQuantiles(estimated.deaths[,1:N,i], probs=.975)
    estimated_deaths_li2 <- colQuantiles(estimated.deaths[,1:N,i], probs=.25)
    estimated_deaths_ui2 <- colQuantiles(estimated.deaths[,1:N,i], probs=.75)

    estimated_deathsh <- colMeans(estimated.deathsht[,1:N,i])
    estimated_deathsh_li <- colQuantiles(estimated.deathsht[,1:N,i], probs=.025)
    estimated_deathsh_ui <- colQuantiles(estimated.deathsht[,1:N,i], probs=.975)
    estimated_deathsh_li2 <- colQuantiles(estimated.deathsht[,1:N,i], probs=.25)
    estimated_deathsh_ui2 <- colQuantiles(estimated.deathsht[,1:N,i], probs=.75)
    
    estimated_deathsc <- colMeans(estimated.deathsc[,1:N,i])
    estimated_deathsc_li <- colQuantiles(estimated.deathsc[,1:N,i], probs=.025)
    estimated_deathsc_ui <- colQuantiles(estimated.deathsc[,1:N,i], probs=.975)
    estimated_deathsc_li2 <- colQuantiles(estimated.deathsc[,1:N,i], probs=.25)
    estimated_deathsc_ui2 <- colQuantiles(estimated.deathsc[,1:N,i], probs=.75)
    
    estimated_cases <- colMeans(estimated.cases[,1:N,i])
    estimated_cases_li <- colQuantiles(estimated.cases[,1:N,i], probs=.025)
    estimated_cases_ui <- colQuantiles(estimated.cases[,1:N,i], probs=.975)
    estimated_cases_li2 <- colQuantiles(estimated.cases[,1:N,i], probs=.25)
    estimated_cases_ui2 <- colQuantiles(estimated.cases[,1:N,i], probs=.75)

    estimated_hosp <- colMeans(estimated.hospitals[,1:N,i])
    estimated_hosp_li <- colQuantiles(estimated.hospitals[,1:N,i], probs=.025)
    estimated_hosp_ui <- colQuantiles(estimated.hospitals[,1:N,i], probs=.975)
    estimated_hosp_li2 <- colQuantiles(estimated.hospitals[,1:N,i], probs=.25)
    estimated_hosp_ui2 <- colQuantiles(estimated.hospitals[,1:N,i], probs=.75)

    estimated_hospi <- colMeans(estimated.hospitalsi[,1:N,i])
    estimated_hospi_li <- colQuantiles(estimated.hospitalsi[,1:N,i], probs=.025)
    estimated_hospi_ui <- colQuantiles(estimated.hospitalsi[,1:N,i], probs=.975)
    estimated_hospi_li2 <- colQuantiles(estimated.hospitalsi[,1:N,i], probs=.25)
    estimated_hospi_ui2 <- colQuantiles(estimated.hospitalsi[,1:N,i], probs=.75)
    
    estimated_hospo <- colMeans(estimated.hospitalso[,1:N,i])
    estimated_hospo_li <- colQuantiles(estimated.hospitalso[,1:N,i], probs=.025)
    estimated_hospo_ui <- colQuantiles(estimated.hospitalso[,1:N,i], probs=.975)
    estimated_hospo_li2 <- colQuantiles(estimated.hospitalso[,1:N,i], probs=.25)
    estimated_hospo_ui2 <- colQuantiles(estimated.hospitalso[,1:N,i], probs=.75)
    
    estimated_icu <- colMeans(estimated.icus[,1:N,i])
    estimated_icu_li <- colQuantiles(estimated.icus[,1:N,i], probs=.025)
    estimated_icu_ui <- colQuantiles(estimated.icus[,1:N,i], probs=.975)
    estimated_icu_li2 <- colQuantiles(estimated.icus[,1:N,i], probs=.25)
    estimated_icu_ui2 <- colQuantiles(estimated.icus[,1:N,i], probs=.75)
    
    estimated_icui <- colMeans(estimated.icusi[,1:N,i])
    estimated_icui_li <- colQuantiles(estimated.icusi[,1:N,i], probs=.025)
    estimated_icui_ui <- colQuantiles(estimated.icusi[,1:N,i], probs=.975)
    estimated_icui_li2 <- colQuantiles(estimated.icusi[,1:N,i], probs=.25)
    estimated_icui_ui2 <- colQuantiles(estimated.icusi[,1:N,i], probs=.75)
    
    estimated_icuo <- colMeans(estimated.icuso[,1:N,i])
    estimated_icuo_li <- colQuantiles(estimated.icuso[,1:N,i], probs=.025)
    estimated_icuo_ui <- colQuantiles(estimated.icuso[,1:N,i], probs=.975)
    estimated_icuo_li2 <- colQuantiles(estimated.icuso[,1:N,i], probs=.25)
    estimated_icuo_ui2 <- colQuantiles(estimated.icuso[,1:N,i], probs=.75)
    
    rt <- colMeans(out$Rt_adj[,1:N,i])
    rt_li <- colQuantiles(out$Rt_adj[,1:N,i],probs=.025)
    rt_ui <- colQuantiles(out$Rt_adj[,1:N,i],probs=.975)
    rt_li2 <- colQuantiles(out$Rt_adj[,1:N,i],probs=.25)
    rt_ui2 <- colQuantiles(out$Rt_adj[,1:N,i],probs=.75)
    
    
    covariates_country <- as.data.frame(matrix(NA, nrow=1, ncol=length(interventions)))
    colnames(covariates_country) <- paste0("interventions",1:length(interventions))
    for(iter in 1:ncol(covariates_country)){ 
        class(covariates_country[,iter]) <- "Date"
        covariates_country[1,iter] <- interventions[iter]
    }
    covariates_country_long <- gather(covariates_country, key = "key", 
                                      value = "value")
    covariates_country_long$x <- rep(NULL, length(covariates_country_long$key))
    un_dates <- unique(covariates_country_long$value)
    
    for (k in 1:length(un_dates)){
      idxs <- which(covariates_country_long$value == un_dates[k])
      max_val <- round(max(rt_ui)) + 0.3
      for (j in idxs){
        covariates_country_long$x[j] <- max_val
        max_val <- max_val - 0.3
      }
    }
    
    
    covariates_country_long$value <- as_date(covariates_country_long$value) 
    covariates_country_long$country <- rep(country, 
                                           length(covariates_country_long$value))
    
    data_country <- data.frame("time" = as_date(as.character(dates[[i]])),
                               "country" = rep(country, length(dates[[i]])),
                               "reported_cases" = reported_cases[[i]], 
                               "reported_cases_c" = cumsum(reported_cases[[i]]), 
                               "predicted_cases_c" = cumsum(predicted_cases),
                               "predicted_min_c" = cumsum(predicted_cases_li),
                               "predicted_max_c" = cumsum(predicted_cases_ui),
                               "predicted_cases" = predicted_cases,
                               "predicted_min" = predicted_cases_li,
                               "predicted_max" = predicted_cases_ui,
                               "predicted_min2" = predicted_cases_li2,
                               "predicted_max2" = predicted_cases_ui2,
                               "deaths" = deaths_by_country[[i]],
                               "deaths_c" = cumsum(deaths_by_country[[i]]),
                               "estimated_deaths_c" =  cumsum(estimated_deaths),
                               "death_min_c" = cumsum(estimated_deaths_li),
                               "death_max_c"= cumsum(estimated_deaths_ui),
                               "estimated_deaths" = estimated_deaths,
                               "death_min" = estimated_deaths_li,
                               "death_max"= estimated_deaths_ui,
                               "death_min2" = estimated_deaths_li2,
                               "death_max2"= estimated_deaths_ui2,
                               "rt" = rt,
                               "rt_min" = rt_li,
                               "rt_max" = rt_ui,
                               "rt_min2" = rt_li2,
                               "rt_max2" = rt_ui2
    )
    
    data_country2 <- data.frame("estimated_cases_c" =  cumsum(estimated_cases),
                                "cases_min_c" = cumsum(estimated_cases_li),
                                "cases_max_c"= cumsum(estimated_cases_ui),
                                "estimated_cases" = estimated_cases,
                                "cases_min" = estimated_cases_li,
                                "cases_max"= estimated_cases_ui,
                                "cases_min2" = estimated_cases_li2,
                                "cases_max2"= estimated_cases_ui2)
    
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
    
    data_icu <- data.frame("time" = as_date(as.character(dates[[i]])),
                            "estimated_hosp_c" =  cumsum(estimated_icu),
                            "icu_min_c" = cumsum(estimated_icu_li),
                            "icu_max_c"= cumsum(estimated_icu_ui),
                            "estimated_icu" = estimated_icu,
                            "icu_min" = estimated_icu_li,
                            "icu_max"= estimated_icu_ui,
                            "icu_min2" = estimated_icu_li2,
                            "icu_max2"= estimated_icu_ui2,
                            "reported_icu" = icus_by_country[[i]])
    
    data_icui <- data.frame("time" = as_date(as.character(dates[[i]])),
                           "estimated_hosp_c" =  cumsum(estimated_icui),
                           "icu_min_c" = cumsum(estimated_icui_li),
                           "icu_max_c"= cumsum(estimated_icui_ui),
                           "estimated_icu" = estimated_icui,
                           "icu_min" = estimated_icui_li,
                           "icu_max"= estimated_icui_ui,
                           "icu_min2" = estimated_icui_li2,
                           "icu_max2"= estimated_icui_ui2,
                           "reported_icu" = icusin_by_country[[i]])
    
    data_icuo <- data.frame("time" = as_date(as.character(dates[[i]])),
                            "estimated_hosp_c" =  cumsum(estimated_icuo),
                            "icu_min_c" = cumsum(estimated_icuo_li),
                            "icu_max_c"= cumsum(estimated_icuo_ui),
                            "estimated_icu" = estimated_icuo,
                            "icu_min" = estimated_icuo_li,
                            "icu_max"= estimated_icuo_ui,
                            "icu_min2" = estimated_icuo_li2,
                            "icu_max2"= estimated_icuo_ui2,
                            "reported_icu" = icusout_by_country[[i]])

    all_data <- rbind(all_data, data_country)
    intervention_data <- rbind(intervention_data, covariates_country_long)
    
    make_plots(data_country = data_country, 
               data_country2 = data_country2,
               data_deathsh = data_deathsh,
               data_deathsc = data_deathsc,
               data_hosp = data_hosp,
               data_hospi = data_hospi,
               data_hospo = data_hospo,
               data_icu = data_icu,
               data_icui = data_icui,
               data_icuo = data_icuo,
               covariates_country_long = covariates_country_long,
               filename2 = filename2,
               country = country,
               percent_pop = percent_pop, 
               fit.spline = fit.spline)
    
  }
  # write.csv(all_data, paste0("results/", "base-plot.csv"))
  # write.csv(intervention_data, paste0("results/", "base-intervention.csv"))

  p_hosp <- list()
  p_icu <- list()
  p_deathsh <- list()
  
  for(i in 1:dim(estimated.hospitals)[3]){
    print(i)
    N <- length(dates[[1]])
    country <- countries[[1]]

    predicted_cases <- colMeans(prediction[,1:N,1])
    predicted_cases_li <- colQuantiles(prediction[,1:N,1], probs=.025)
    predicted_cases_ui <- colQuantiles(prediction[,1:N,1], probs=.975)
    predicted_cases_li2 <- colQuantiles(prediction[,1:N,1], probs=.25)
    predicted_cases_ui2 <- colQuantiles(prediction[,1:N,1], probs=.75)

    estimated_deaths <- colMeans(estimated.deaths[,1:N,1])
    estimated_deaths_li <- colQuantiles(estimated.deaths[,1:N,1], probs=.025)
    estimated_deaths_ui <- colQuantiles(estimated.deaths[,1:N,1], probs=.975)
    estimated_deaths_li2 <- colQuantiles(estimated.deaths[,1:N,1], probs=.25)
    estimated_deaths_ui2 <- colQuantiles(estimated.deaths[,1:N,1], probs=.75)

    estimated_deathsh <- colMeans(estimated.deathsh[,1:N,i])
    estimated_deathsh_li <- colQuantiles(estimated.deathsh[,1:N,i], probs=.025)
    estimated_deathsh_ui <- colQuantiles(estimated.deathsh[,1:N,i], probs=.975)
    estimated_deathsh_li2 <- colQuantiles(estimated.deathsh[,1:N,i], probs=.25)
    estimated_deathsh_ui2 <- colQuantiles(estimated.deathsh[,1:N,i], probs=.75)

    estimated_deathsc <- colMeans(estimated.deathsc[,1:N,1])
    estimated_deathsc_li <- colQuantiles(estimated.deathsc[,1:N,1], probs=.025)
    estimated_deathsc_ui <- colQuantiles(estimated.deathsc[,1:N,1], probs=.975)
    estimated_deathsc_li2 <- colQuantiles(estimated.deathsc[,1:N,1], probs=.25)
    estimated_deathsc_ui2 <- colQuantiles(estimated.deathsc[,1:N,1], probs=.75)

    estimated_cases <- colMeans(estimated.cases[,1:N,1])
    estimated_cases_li <- colQuantiles(estimated.cases[,1:N,1], probs=.025)
    estimated_cases_ui <- colQuantiles(estimated.cases[,1:N,1], probs=.975)
    estimated_cases_li2 <- colQuantiles(estimated.cases[,1:N,1], probs=.25)
    estimated_cases_ui2 <- colQuantiles(estimated.cases[,1:N,1], probs=.75)

    estimated_hosp <- colMeans(estimated.hospitals[,1:N,i])
    estimated_hosp_li <- colQuantiles(estimated.hospitals[,1:N,i], probs=.025)
    estimated_hosp_ui <- colQuantiles(estimated.hospitals[,1:N,i], probs=.975)
    estimated_hosp_li2 <- colQuantiles(estimated.hospitals[,1:N,i], probs=.25)
    estimated_hosp_ui2 <- colQuantiles(estimated.hospitals[,1:N,i], probs=.75)

    estimated_hospi <- colMeans(estimated.hospitalsi[,1:N,i])
    estimated_hospi_li <- colQuantiles(estimated.hospitalsi[,1:N,i], probs=.025)
    estimated_hospi_ui <- colQuantiles(estimated.hospitalsi[,1:N,i], probs=.975)
    estimated_hospi_li2 <- colQuantiles(estimated.hospitalsi[,1:N,i], probs=.25)
    estimated_hospi_ui2 <- colQuantiles(estimated.hospitalsi[,1:N,i], probs=.75)

    estimated_hospo <- colMeans(estimated.hospitalso[,1:N,i])
    estimated_hospo_li <- colQuantiles(estimated.hospitalso[,1:N,i], probs=.025)
    estimated_hospo_ui <- colQuantiles(estimated.hospitalso[,1:N,i], probs=.975)
    estimated_hospo_li2 <- colQuantiles(estimated.hospitalso[,1:N,i], probs=.25)
    estimated_hospo_ui2 <- colQuantiles(estimated.hospitalso[,1:N,i], probs=.75)

    estimated_icu <- colMeans(estimated.icus[,1:N,i])
    estimated_icu_li <- colQuantiles(estimated.icus[,1:N,i], probs=.025)
    estimated_icu_ui <- colQuantiles(estimated.icus[,1:N,i], probs=.975)
    estimated_icu_li2 <- colQuantiles(estimated.icus[,1:N,i], probs=.25)
    estimated_icu_ui2 <- colQuantiles(estimated.icus[,1:N,i], probs=.75)

    estimated_icui <- colMeans(estimated.icusi[,1:N,i])
    estimated_icui_li <- colQuantiles(estimated.icusi[,1:N,i], probs=.025)
    estimated_icui_ui <- colQuantiles(estimated.icusi[,1:N,i], probs=.975)
    estimated_icui_li2 <- colQuantiles(estimated.icusi[,1:N,i], probs=.25)
    estimated_icui_ui2 <- colQuantiles(estimated.icusi[,1:N,i], probs=.75)

    estimated_icuo <- colMeans(estimated.icuso[,1:N,i])
    estimated_icuo_li <- colQuantiles(estimated.icuso[,1:N,i], probs=.025)
    estimated_icuo_ui <- colQuantiles(estimated.icuso[,1:N,i], probs=.975)
    estimated_icuo_li2 <- colQuantiles(estimated.icuso[,1:N,i], probs=.25)
    estimated_icuo_ui2 <- colQuantiles(estimated.icuso[,1:N,i], probs=.75)

    rt <- colMeans(out$Rt_adj[,1:N,1])
    rt_li <- colQuantiles(out$Rt_adj[,1:N,1],probs=.025)
    rt_ui <- colQuantiles(out$Rt_adj[,1:N,1],probs=.975)
    rt_li2 <- colQuantiles(out$Rt_adj[,1:N,1],probs=.25)
    rt_ui2 <- colQuantiles(out$Rt_adj[,1:N,1],probs=.75)


    covariates_country <- as.data.frame(matrix(NA, nrow=1, ncol=length(interventions)))
    colnames(covariates_country) <- paste0("interventions",1:length(interventions))
    for(iter in 1:ncol(covariates_country)){
      class(covariates_country[,iter]) <- "Date"
      covariates_country[1,iter] <- interventions[iter]
    }
    covariates_country_long <- gather(covariates_country, key = "key",
                                      value = "value")
    covariates_country_long$x <- rep(NULL, length(covariates_country_long$key))
    un_dates <- unique(covariates_country_long$value)

    for (k in 1:length(un_dates)){
      idxs <- which(covariates_country_long$value == un_dates[k])
      max_val <- round(max(rt_ui)) + 0.3
      for (j in idxs){
        covariates_country_long$x[j] <- max_val
        max_val <- max_val - 0.3
      }
    }


    covariates_country_long$value <- as_date(covariates_country_long$value)
    covariates_country_long$country <- rep(country,
                                           length(covariates_country_long$value))

    data_country <- data.frame("time" = as_date(as.character(dates[[1]])),
                               "country" = rep(country, length(dates[[1]])),
                               "reported_cases" = reported_cases[[1]],
                               "reported_cases_c" = cumsum(reported_cases[[1]]),
                               "predicted_cases_c" = cumsum(predicted_cases),
                               "predicted_min_c" = cumsum(predicted_cases_li),
                               "predicted_max_c" = cumsum(predicted_cases_ui),
                               "predicted_cases" = predicted_cases,
                               "predicted_min" = predicted_cases_li,
                               "predicted_max" = predicted_cases_ui,
                               "predicted_min2" = predicted_cases_li2,
                               "predicted_max2" = predicted_cases_ui2,
                               "deaths" = deaths_by_country[[1]],
                               "deaths_c" = cumsum(deaths_by_country[[1]]),
                               "estimated_deaths_c" =  cumsum(estimated_deaths),
                               "death_min_c" = cumsum(estimated_deaths_li),
                               "death_max_c"= cumsum(estimated_deaths_ui),
                               "estimated_deaths" = estimated_deaths,
                               "death_min" = estimated_deaths_li,
                               "death_max"= estimated_deaths_ui,
                               "death_min2" = estimated_deaths_li2,
                               "death_max2"= estimated_deaths_ui2,
                               "rt" = rt,
                               "rt_min" = rt_li,
                               "rt_max" = rt_ui,
                               "rt_min2" = rt_li2,
                               "rt_max2" = rt_ui2
    )

    data_country2 <- data.frame("estimated_cases_c" =  cumsum(estimated_cases),
                                "cases_min_c" = cumsum(estimated_cases_li),
                                "cases_max_c"= cumsum(estimated_cases_ui),
                                "estimated_cases" = estimated_cases,
                                "cases_min" = estimated_cases_li,
                                "cases_max"= estimated_cases_ui,
                                "cases_min2" = estimated_cases_li2,
                                "cases_max2"= estimated_cases_ui2)

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

    data_icu <- data.frame("time" = as_date(as.character(dates[[1]])),
                           "estimated_hosp_c" =  cumsum(estimated_icu),
                           "icu_min_c" = cumsum(estimated_icu_li),
                           "icu_max_c"= cumsum(estimated_icu_ui),
                           "estimated_icu" = estimated_icu,
                           "icu_min" = estimated_icu_li,
                           "icu_max"= estimated_icu_ui,
                           "icu_min2" = estimated_icu_li2,
                           "icu_max2"= estimated_icu_ui2,
                           "reported_icu" = hosp.icu[1:N,i])

    data_icui <- data.frame("time" = as_date(as.character(dates[[1]])),
                            "estimated_hosp_c" =  cumsum(estimated_icui),
                            "icu_min_c" = cumsum(estimated_icui_li),
                            "icu_max_c"= cumsum(estimated_icui_ui),
                            "estimated_icu" = estimated_icui,
                            "icu_min" = estimated_icui_li,
                            "icu_max"= estimated_icui_ui,
                            "icu_min2" = estimated_icui_li2,
                            "icu_max2"= estimated_icui_ui2,
                            "reported_icu" = hosp.icuin[1:N,i])

    data_icuo <- data.frame("time" = as_date(as.character(dates[[1]])),
                            "estimated_hosp_c" =  cumsum(estimated_icuo),
                            "icu_min_c" = cumsum(estimated_icuo_li),
                            "icu_max_c"= cumsum(estimated_icuo_ui),
                            "estimated_icu" = estimated_icuo,
                            "icu_min" = estimated_icuo_li,
                            "icu_max"= estimated_icuo_ui,
                            "icu_min2" = estimated_icuo_li2,
                            "icu_max2"= estimated_icuo_ui2,
                            "reported_icu" = hosp.icuout[1:N,i])

    all_data <- rbind(all_data, data_country)
    intervention_data <- rbind(intervention_data, covariates_country_long)

    p_iter <- make_plots_hospitals(data_country = data_country,
               data_country2 = data_country2,
               data_deathsh = data_deathsh,
               data_deathsc = data_deathsc,
               data_hosp = data_hosp,
               data_hospi = data_hospi,
               data_hospo = data_hospo,
               data_icu = data_icu,
               data_icui = data_icui,
               data_icuo = data_icuo,
               covariates_country_long = covariates_country_long,
               filename2 = filename2,
               country = country,
               percent_pop = percent_pop,
               fit.spline = fit.spline,
               hospital.name=hospital.names[i])

    p_hosp[[i]] <- p_iter[[1]]
    p_icu[[i]] <- p_iter[[2]]
    p_deathsh[[i]] <- p_iter[[3]]
  }

  # Hosp:
  p_hosp <- plot_grid(p_hosp[[1]],p_hosp[[2]],p_hosp[[3]],p_hosp[[4]],
                 nrow=dim(estimated.hospitals)[3])
  cowplot::save_plot(filename = paste0("combined_hospitals/figures/", country, "_hosp_", filename2, ".pdf"),
                     p_hosp, base_width = 14, base_height=7.42*2)
  # ICU:
  p_icu <- plot_grid(p_icu[[1]],p_icu[[2]],p_icu[[3]],p_icu[[4]],
                      nrow=dim(estimated.hospitals)[3])
  cowplot::save_plot(filename = paste0("combined_hospitals/figures/", country, "_icu_", filename2, ".pdf"),
                     p_icu, base_width = 14, base_height=7.42*2)
  # Deaths H:
  p_deathsh <- plot_grid(p_deathsh[[1]],p_deathsh[[2]],p_deathsh[[3]],p_deathsh[[4]],
                     nrow=2)
  cowplot::save_plot(filename = paste0("combined_hospitals/figures/", country, "_deathsh_", filename2, ".pdf"),
                     p_deathsh, base_width = 14*2/3, base_height=7.42
                     )

  # Plots for Web, Mobile version
  dir.create("combined_hospitals/web/figures/mobile/", showWarnings = FALSE, recursive = TRUE)
  cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, '_', filename2, "_hosp", ".svg"),
                     p_hosp, base_width = 14, base_height=7.42*2)
  cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, '_', filename2, "_icu", ".svg"),
                     p_icu, base_width = 14, base_height=7.42*2)
  cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, '_', filename2, "_deathsh", ".svg"),
                     p_deathsh, base_width = 14*2/3, base_height=7.42)
  
  
}

#---------------------------------------------------------------------------
make_plots <- function(data_country, data_country2, 
                       data_deathsh, data_deathsc,
                       data_hosp, data_hospi, data_hospo, 
                       data_icu, data_icui, data_icuo,
                       covariates_country_long, 
                       filename2, country, percent_pop, fit.spline){
  
  language_english <- TRUE
  
  if (country == 'United_Kingdom')
    country = 'United Kingdom'
  data_cases_95 <- data.frame(data_country$time, data_country$predicted_min, 
                              data_country$predicted_max)
  names(data_cases_95) <- c("time", "cases_min", "cases_max")
  data_cases_95$key <- rep("nintyfive", length(data_cases_95$time))
  data_cases_50 <- data.frame(data_country$time, data_country$predicted_min2, 
                              data_country$predicted_max2)
  names(data_cases_50) <- c("time", "cases_min", "cases_max")
  data_cases_50$key <- rep("fifty", length(data_cases_50$time))
  data_cases <- rbind(data_cases_95, data_cases_50)
  levels(data_cases$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "Daily number of infections"
  else ylab_text <- "Dnevno stevilo okuzb"
  
  p1 <- ggplot(data_country) +
    geom_bar(data = data_country, aes(x = time, y = reported_cases), 
             fill = "coral4", stat='identity', alpha=0.5) + 
    geom_ribbon(data = data_cases, 
                aes(x = time, ymin = cases_min, ymax = cases_max, fill = key)) +
    # xlab("") +
    # ylab("Daily number of infections\n") +
    labs(x="",	
         y=ylab_text)+
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) + 
    scale_y_continuous(expand = c(0, 0), labels = comma) + 
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55), 
                                 alpha("deepskyblue4", 0.45))) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "None") + ggtitle(country) +
    guides(fill=guide_legend(ncol=1))
  
  ##
  # Deaths:
  data_deaths_95 <- data.frame(data_country$time, data_country$death_min, 
                               data_country$death_max)
  names(data_deaths_95) <- c("time", "death_min", "death_max")
  data_deaths_95$key <- rep("nintyfive", length(data_deaths_95$time))
  data_deaths_50 <- data.frame(data_country$time, data_country$death_min2, 
                               data_country$death_max2)
  names(data_deaths_50) <- c("time", "death_min", "death_max")
  data_deaths_50$key <- rep("fifty", length(data_deaths_50$time))
  data_deaths_CI <- rbind(data_deaths_95, data_deaths_50)
  levels(data_deaths_CI$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "Daily number of deaths"
  else ylab_text <- "Dnevno stevilo umrlih\n"

  if(language_english) xlab_text <- "Date"
  else xlab_text <- "Datum"
  
  p2 <-   ggplot(data_country, aes(x = time)) +
    geom_bar(data = data_country, aes(y = deaths, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_deaths_CI,
      aes(ymin = death_min, ymax = death_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) + 
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55), 
                                 alpha("deepskyblue4", 0.45))) + 
    xlab(xlab_text)+	
    ylab(ylab_text)+                                 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "None") + 
    guides(fill=guide_legend(ncol=1))
  
  # Deaths H:
  data_deaths_95 <- data.frame(data_country$time, data_deathsh$death_min, 
                               data_deathsh$death_max)
  names(data_deaths_95) <- c("time", "death_min", "death_max")
  data_deaths_95$key <- rep("nintyfive", length(data_deaths_95$time))
  data_deaths_50 <- data.frame(data_country$time, data_deathsh$death_min2, 
                               data_deathsh$death_max2)
  names(data_deaths_50) <- c("time", "death_min", "death_max")
  data_deaths_50$key <- rep("fifty", length(data_deaths_50$time))
  data_deathsh_ci <- rbind(data_deaths_95, data_deaths_50)
  levels(data_deathsh_ci$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "Deaths H"
  else ylab_text <- "Dnevno stevilo umrlih\n"
  
  if(language_english) xlab_text <- "Date"
  else xlab_text <- "Datum"
  
  p2_h <-   ggplot(data_deathsh, aes(x = time)) +
    geom_bar(data = data_deathsh, aes(y = deaths, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_deathsh_ci,
      aes(ymin = death_min, ymax = death_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) + 
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55), 
                                 alpha("deepskyblue4", 0.45))) + 
    xlab(xlab_text)+	
    ylab(ylab_text)+                                 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "None") + 
    guides(fill=guide_legend(ncol=1))

  # Deaths C:
  data_deaths_95 <- data.frame(data_country$time, data_deathsc$death_min, 
                               data_deathsc$death_max)
  names(data_deaths_95) <- c("time", "death_min", "death_max")
  data_deaths_95$key <- rep("nintyfive", length(data_deaths_95$time))
  data_deaths_50 <- data.frame(data_country$time, data_deathsc$death_min2, 
                               data_deathsc$death_max2)
  names(data_deaths_50) <- c("time", "death_min", "death_max")
  data_deaths_50$key <- rep("fifty", length(data_deaths_50$time))
  data_deathsc_ci <- rbind(data_deaths_95, data_deaths_50)
  levels(data_deathsc_ci$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "Deaths C"
  else ylab_text <- "Dnevno stevilo umrlih\n"
  
  if(language_english) xlab_text <- "Date"
  else xlab_text <- "Datum"
  
  p2_c <-   ggplot(data_deathsc, aes(x = time)) +
    geom_bar(data = data_deathsc, aes(y = deaths, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_deathsc_ci,
      aes(ymin = death_min, ymax = death_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) + 
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55), 
                                 alpha("deepskyblue4", 0.45))) + 
    xlab(xlab_text)+	
    ylab(ylab_text)+                                 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "None") + 
    guides(fill=guide_legend(ncol=1))
  
  # plot_labels <- c(	
  #   "Popolna zapora", 	
  #   # "Zaprtje obcin",
  #   "Prepoved javnih dogodkov",	
  #   "Zaprtje sol",	
  #   "Samoizolacija",	
  #   "Socialno distanciranje")
  
  # Rt:
  plot_labels <- covariates_country_long$key
  plot_values <- c(21, 22, 23, 24, 25, 12:20,26:40)[1:length(plot_labels)]
  
  # Plotting interventions
  data_rt_95 <- data.frame(data_country$time, 
                           data_country$rt_min, data_country$rt_max)
  names(data_rt_95) <- c("time", "rt_min", "rt_max")
  data_rt_95$key <- rep("nintyfive", length(data_rt_95$time))
  data_rt_50 <- data.frame(data_country$time, data_country$rt_min2, 
                           data_country$rt_max2)
  names(data_rt_50) <- c("time", "rt_min", "rt_max")
  data_rt_50$key <- rep("fifty", length(data_rt_50$time))
  data_rt <- rbind(data_rt_95, data_rt_50)
  levels(data_rt$key) <- c("ninetyfive", "fifth")
  
  if(language_english) interven <- "Interventions"
  else interven <- "Intervencije"
  
  p3 <- ggplot(data_country)
  
  if(!fit.spline){
    p3 <- p3 +
      geom_stepribbon(data = data_rt, aes(x = time, ymin = rt_min, ymax = rt_max, 
                                          group = key,
                                          fill = key))
  } else{
    p3 <- p3 +
      geom_ribbon(data = data_rt, aes(x = time, ymin = rt_min, ymax = rt_max, 
                                          group = key,
                                          fill = key))
  }
  
  p3 <- p3 +  
    geom_hline(yintercept = 1, color = 'black', size = 0.1) + 
    geom_segment(data = covariates_country_long,
                 aes(x = value, y = 0, xend = value, yend = max(x)), 
                 linetype = "dashed", colour = "grey", alpha = 0.75) +
    geom_point(data = covariates_country_long, aes(x = value, 
                                                   y = x, 
                                                   group = key, 
                                                   shape = key, 
                                                   col = key), size = 2) +
    xlab("") +
    ylab(expression(R[t])) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("seagreen", 0.75), alpha("seagreen", 0.5))) + 
    scale_shape_manual(name = interven, labels = plot_labels
                       ,values = plot_values) +
    scale_colour_discrete(name = interven, labels = plot_labels) + 
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b"), 
                 limits = c(data_country$time[1], 
                            data_country$time[length(data_country$time)])) + 
    scale_y_continuous(expand = expansion(mult=c(0,0.1))) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position="right")
  if (country == 'United Kingdom')
    country = 'United_Kingdom'
  
  ## Confirmed cases:
  data_cases_952 <- data.frame(data_country$time, data_country2$cases_min, 
                              data_country2$cases_max)
  names(data_cases_952) <- c("time", "cases_min", "cases_max")
  data_cases_952$key <- rep("nintyfive", length(data_cases_952$time))
  data_cases_502 <- data.frame(data_country$time, data_country2$cases_min2, 
                              data_country2$cases_max2)
  names(data_cases_502) <- c("time", "cases_min", "cases_max")
  data_cases_502$key <- rep("fifty", length(data_cases_502$time))
  data_cases2 <- rbind(data_cases_952, data_cases_502)
  levels(data_cases2$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "Daily number of confirmed infections\n"
  else ylab_text <- "Dnevno stevilo potrjeno okuzenih\n"
  
  p4 <-   ggplot(data_country, aes(x = time)) +
    geom_bar(data = data_country, aes(y = reported_cases, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_cases2,
      aes(ymin = cases_min, ymax = cases_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) + 
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55), 
                                 alpha("deepskyblue4", 0.45))) + 
    xlab(xlab_text)+	
    ylab(ylab_text)+                                 ### Nina spremenila
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "None") + 
    guides(fill=guide_legend(ncol=1))
  
  ## Hospitalization:
  data_hosp_95 <- data.frame(data_country$time, data_hosp$hosp_min, 
                               data_hosp$hosp_max)
  names(data_hosp_95) <- c("time", "hosp_min", "hosp_max")
  data_hosp_95$key <- rep("nintyfive", length(data_hosp_95$time))
  data_hosp_50 <- data.frame(data_country$time, data_hosp$hosp_min2, 
                               data_hosp$hosp_max2)
  names(data_hosp_50) <- c("time", "hosp_min", "hosp_max")
  data_hosp_50$key <- rep("fifty", length(data_hosp_50$time))
  data_hosp_ci <- rbind(data_hosp_95, data_hosp_50)
  levels(data_hosp_ci$key) <- c("ninetyfive", "fifty")

  if(language_english) ylab_text <- "Number of hospitalized patients\n"
  else ylab_text <- "Stevilo hospitaliziranih\n"
  
  p5 <- ggplot(data_country, aes(x = time)) +
    geom_bar(data = data_hosp, aes(x=time, y = reported_hosp, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_hosp_ci,
      aes(ymin = hosp_min, ymax = hosp_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55),
                                 alpha("deepskyblue4", 0.45))) +
    xlab(xlab_text)+
    ylab(ylab_text)+                                 ### Nina spremenila
    theme_pubr(base_family="sans") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "None") +
    guides(fill=guide_legend(ncol=1))

  ## Hospitalization IN:
  data_hosp_95 <- data.frame(data_country$time, data_hospo$hosp_min, 
                             data_hospo$hosp_max)
  names(data_hosp_95) <- c("time", "hosp_min", "hosp_max")
  data_hosp_95$key <- rep("nintyfive", length(data_hosp_95$time))
  data_hosp_50 <- data.frame(data_country$time, data_hospo$hosp_min2, 
                             data_hospo$hosp_max2)
  names(data_hosp_50) <- c("time", "hosp_min", "hosp_max")
  data_hosp_50$key <- rep("fifty", length(data_hosp_50$time))
  data_hosp_ci <- rbind(data_hosp_95, data_hosp_50)
  levels(data_hosp_ci$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "Hospitalizations IN\n"
  else ylab_text <- "Stevilo hospitaliziranih\n"
  
  p5_IN <- ggplot(data_country, aes(x = time)) +
    geom_bar(data = data_hospo, aes(x=time, y = reported_hosp, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_hosp_ci,
      aes(ymin = hosp_min, ymax = hosp_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55),
                                 alpha("deepskyblue4", 0.45))) +
    xlab(xlab_text)+
    ylab(ylab_text)+                                 ### Nina spremenila
    theme_pubr(base_family="sans") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "None") +
    guides(fill=guide_legend(ncol=1))
  
  ## Hospitalization OUT:
  data_hosp_95 <- data.frame(data_country$time, data_hospi$hosp_min, 
                             data_hospi$hosp_max)
  names(data_hosp_95) <- c("time", "hosp_min", "hosp_max")
  data_hosp_95$key <- rep("nintyfive", length(data_hosp_95$time))
  data_hosp_50 <- data.frame(data_country$time, data_hospi$hosp_min2, 
                             data_hospi$hosp_max2)
  names(data_hosp_50) <- c("time", "hosp_min", "hosp_max")
  data_hosp_50$key <- rep("fifty", length(data_hosp_50$time))
  data_hosp_ci <- rbind(data_hosp_95, data_hosp_50)
  levels(data_hosp_ci$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "Hospitalizations OUT\n"
  else ylab_text <- "Stevilo hospitaliziranih\n"
  
  ## ICUs:
  data_icu_95 <- data.frame(data_country$time, data_icu$icu_min, 
                             data_icu$icu_max)
  names(data_icu_95) <- c("time", "icu_min", "icu_max")
  data_icu_95$key <- rep("nintyfive", length(data_icu_95$time))
  data_icu_50 <- data.frame(data_country$time, data_icu$icu_min2, 
                             data_icu$icu_max2)
  names(data_icu_50) <- c("time", "icu_min", "icu_max")
  data_icu_50$key <- rep("fifty", length(data_icu_50$time))
  data_icu_ci <- rbind(data_icu_95, data_icu_50)
  levels(data_icu_ci$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "Number of patients in ICU\n"
  else ylab_text <- "Stevilo bolnikov na intenzivni negi\n"
  
  p6 <- ggplot(data_country, aes(x = time)) +
    geom_bar(data = data_icu, aes(x=time, y = reported_icu, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_icu_ci,
      aes(ymin = icu_min, ymax = icu_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55),
                                 alpha("deepskyblue4", 0.45))) +
    xlab(xlab_text)+
    ylab(ylab_text)+                                 ### Nina spremenila
    theme_pubr(base_family="sans") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "None") +
    guides(fill=guide_legend(ncol=1))

  ## ICUs IN:
  data_icu_95 <- data.frame(data_country$time, data_icui$icu_min, 
                            data_icui$icu_max)
  names(data_icu_95) <- c("time", "icu_min", "icu_max")
  data_icu_95$key <- rep("nintyfive", length(data_icu_95$time))
  data_icu_50 <- data.frame(data_country$time, data_icui$icu_min2, 
                            data_icui$icu_max2)
  names(data_icu_50) <- c("time", "icu_min", "icu_max")
  data_icu_50$key <- rep("fifty", length(data_icu_50$time))
  data_icu_ci <- rbind(data_icu_95, data_icu_50)
  levels(data_icu_ci$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "ICU IN\n"
  else ylab_text <- "Stevilo bolnikov na intenzivni negi\n"
  
  p6_IN <- ggplot(data_country, aes(x = time)) +
    geom_bar(data = data_icui, aes(x=time, y = reported_icu, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_icu_ci,
      aes(ymin = icu_min, ymax = icu_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55),
                                 alpha("deepskyblue4", 0.45))) +
    xlab(xlab_text)+
    ylab(ylab_text)+                                 ### Nina spremenila
    theme_pubr(base_family="sans") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "None") +
    guides(fill=guide_legend(ncol=1))

  ## ICUs OUT:
  data_icu_95 <- data.frame(data_country$time, data_icuo$icu_min, 
                            data_icuo$icu_max)
  names(data_icu_95) <- c("time", "icu_min", "icu_max")
  data_icu_95$key <- rep("nintyfive", length(data_icu_95$time))
  data_icu_50 <- data.frame(data_country$time, data_icuo$icu_min2, 
                            data_icuo$icu_max2)
  names(data_icu_50) <- c("time", "icu_min", "icu_max")
  data_icu_50$key <- rep("fifty", length(data_icu_50$time))
  data_icu_ci <- rbind(data_icu_95, data_icu_50)
  levels(data_icu_ci$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "ICU OUT\n"
  else ylab_text <- "Stevilo bolnikov na intenzivni negi\n"
  
  p6_OUT <- ggplot(data_country, aes(x = time)) +
    geom_bar(data = data_icuo, aes(x=time, y = reported_icu, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_icu_ci,
      aes(ymin = icu_min, ymax = icu_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55),
                                 alpha("deepskyblue4", 0.45))) +
    xlab(xlab_text)+
    ylab(ylab_text)+                                 ### Nina spremenila
    theme_pubr(base_family="sans") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "None") +
    guides(fill=guide_legend(ncol=1))
  
  # Special plot settings for mobile
  p3_mobile <- p3
  # +
  #   theme(legend.position="below")
  
  # Plots for Web, Desktop version
  dir.create("combined_hospitals/web/figures/desktop/", showWarnings = FALSE, recursive = TRUE)
  cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/desktop/", country, '_', filename2, "_infections", ".svg"), 
            p1, base_height = 4, base_asp = 1.618)
  cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/desktop/", country, '_', filename2, "_deaths", ".svg"), 
            p2, base_height = 4, base_asp = 1.618)
  cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/desktop/", country, '_', filename2, "_rt", ".svg"), 
            p3, base_height = 4, base_asp = 1.618 * 2)
  cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/desktop/", country, '_', filename2, "_cases", ".svg"), 
            p4, base_height = 4, base_asp = 1.618)
  cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/desktop/", country, '_', filename2, "_hosp", ".svg"), 
            p5, base_height = 4, base_asp = 1.618)
  cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/desktop/", country, '_', filename2, "_icu", ".svg"), 
            p6, base_height = 4, base_asp = 1.618)
  

  # Plots for Web, Mobile version
  dir.create("combined_hospitals/web/figures/mobile/", showWarnings = FALSE, recursive = TRUE)
  cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, '_', filename2, "_infections", ".svg"),
            p1, base_height = 4, base_asp = 1.1)
  cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, '_', filename2, "_deaths", ".svg"),
            p2, base_height = 4, base_asp = 1.1) 
  cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, '_', filename2, "_rt", ".svg"),
            p3_mobile, base_height = 4, base_asp = 1.5)
  cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, '_', filename2, "_cases", ".svg"),
            p4, base_height = 4, base_asp = 1.1)
  cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, '_', filename2, "_hosp", ".svg"),
            p5, base_height = 4, base_asp = 1.1)
  cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, '_', filename2, "_icu", ".svg"),
            p6, base_height = 4, base_asp = 1.1)

  # Save this for plot-forecast:
  save(p1, p3_mobile, file = paste0("combined_hospitals/results/", filename2, '_', "graphs.RData"))

  # p <- plot_grid(p1, p4, p3,
  #                ncol = 3)
  # p2 <- plot_grid(p2, p2_h, p2_c,
  #                 ncol = 3)
  # p3 <- plot_grid(p5, p5_IN, p5_OUT,
  #                 ncol = 3)
  # p4 <- plot_grid(p6, p6_IN, p6_OUT,
  #                 ncol = 3)
  # p <- plot_grid(p, p2, p3, p4, nrow=4)
  # cowplot::save_plot(filename = paste0("combined_hospitals/figures/", country, "_three_pannel_", filename2, ".pdf"),
  #                    p, base_width = 14, base_height=7.42*2)

  p <- plot_grid(p1, p4, p3,
                 ncol = 3)
  p2 <- plot_grid(p2, p2_h, p2_c,
                 ncol = 3)

  p <- plot_grid(p, p2, nrow=2)
  cowplot::save_plot(filename = paste0("combined_hospitals/figures/", country, "_three_pannel_", filename2, ".pdf"),
                     p, base_width = 14, base_height=7.42)
  cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, '_three_panel_', filename2, ".svg"),
                     p, base_height = 8, base_asp = 2)
}


make_plots_hospitals <- function(data_country, data_country2, 
                       data_deathsh, data_deathsc,
                       data_hosp, data_hospi, data_hospo, 
                       data_icu, data_icui, data_icuo,
                       covariates_country_long, 
                       filename2, country, percent_pop, fit.spline,
                       hospital.name){
  
  language_english <- TRUE
  
  if (country == 'United_Kingdom')
    country = 'United Kingdom'
  data_cases_95 <- data.frame(data_country$time, data_country$predicted_min, 
                              data_country$predicted_max)
  names(data_cases_95) <- c("time", "cases_min", "cases_max")
  data_cases_95$key <- rep("nintyfive", length(data_cases_95$time))
  data_cases_50 <- data.frame(data_country$time, data_country$predicted_min2, 
                              data_country$predicted_max2)
  names(data_cases_50) <- c("time", "cases_min", "cases_max")
  data_cases_50$key <- rep("fifty", length(data_cases_50$time))
  data_cases <- rbind(data_cases_95, data_cases_50)
  levels(data_cases$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "Daily number of infections"
  else ylab_text <- "Dnevno stevilo okuzb"

  ##
  # Deaths:
  data_deaths_95 <- data.frame(data_country$time, data_country$death_min, 
                               data_country$death_max)
  names(data_deaths_95) <- c("time", "death_min", "death_max")
  data_deaths_95$key <- rep("nintyfive", length(data_deaths_95$time))
  data_deaths_50 <- data.frame(data_country$time, data_country$death_min2, 
                               data_country$death_max2)
  names(data_deaths_50) <- c("time", "death_min", "death_max")
  data_deaths_50$key <- rep("fifty", length(data_deaths_50$time))
  data_deaths <- rbind(data_deaths_95, data_deaths_50)
  levels(data_deaths$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "Daily number of deaths"
  else ylab_text <- "Dnevno stevilo umrlih\n"
  
  if(language_english) xlab_text <- "Date"
  else xlab_text <- "Datum"
  
  # Deaths H:
  data_deaths_95 <- data.frame(data_country$time, data_deathsh$death_min, 
                               data_deathsh$death_max)
  names(data_deaths_95) <- c("time", "death_min", "death_max")
  data_deaths_95$key <- rep("nintyfive", length(data_deaths_95$time))
  data_deaths_50 <- data.frame(data_country$time, data_deathsh$death_min2, 
                               data_deathsh$death_max2)
  names(data_deaths_50) <- c("time", "death_min", "death_max")
  data_deaths_50$key <- rep("fifty", length(data_deaths_50$time))
  data_deaths <- rbind(data_deaths_95, data_deaths_50)
  levels(data_deaths$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "Deaths H"
  else ylab_text <- "Dnevno stevilo umrlih\n"
  
  if(language_english) xlab_text <- "Date"
  else xlab_text <- "Datum"
  
  p2_h <-   ggplot(data_deathsh, aes(x = time)) +
    geom_bar(data = data_deathsh, aes(y = deaths, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_deaths,
      aes(ymin = death_min, ymax = death_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) + 
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55), 
                                 alpha("deepskyblue4", 0.45))) + 
    xlab(xlab_text)+	
    ylab(ylab_text)+  
    ggtitle(hospital.name)+
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "None") + 
    guides(fill=guide_legend(ncol=1))
  
  # Deaths C:
  data_deaths_95 <- data.frame(data_country$time, data_deathsc$death_min, 
                               data_deathsc$death_max)
  names(data_deaths_95) <- c("time", "death_min", "death_max")
  data_deaths_95$key <- rep("nintyfive", length(data_deaths_95$time))
  data_deaths_50 <- data.frame(data_country$time, data_deathsc$death_min2, 
                               data_deathsc$death_max2)
  names(data_deaths_50) <- c("time", "death_min", "death_max")
  data_deaths_50$key <- rep("fifty", length(data_deaths_50$time))
  data_deaths <- rbind(data_deaths_95, data_deaths_50)
  levels(data_deaths$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "Deaths C"
  else ylab_text <- "Dnevno stevilo umrlih\n"
  
  if(language_english) xlab_text <- "Date"
  else xlab_text <- "Datum"
  
  
  # plot_labels <- c(	
  #   "Popolna zapora", 	
  #   # "Zaprtje obcin",
  #   "Prepoved javnih dogodkov",	
  #   "Zaprtje sol",	
  #   "Samoizolacija",	
  #   "Socialno distanciranje")
  
  # Rt:
  plot_labels <- covariates_country_long$key
  plot_values <- c(21, 22, 23, 24, 25, 12:20,26:40)[1:length(plot_labels)]
  
  # Plotting interventions
  data_rt_95 <- data.frame(data_country$time, 
                           data_country$rt_min, data_country$rt_max)
  names(data_rt_95) <- c("time", "rt_min", "rt_max")
  data_rt_95$key <- rep("nintyfive", length(data_rt_95$time))
  data_rt_50 <- data.frame(data_country$time, data_country$rt_min2, 
                           data_country$rt_max2)
  names(data_rt_50) <- c("time", "rt_min", "rt_max")
  data_rt_50$key <- rep("fifty", length(data_rt_50$time))
  data_rt <- rbind(data_rt_95, data_rt_50)
  levels(data_rt$key) <- c("ninetyfive", "fifth")
  
  if(language_english) interven <- "Interventions"
  else interven <- "Intervencije"
  
  p3 <- ggplot(data_country)
  
  if(!fit.spline){
    p3 <- p3 +
      geom_stepribbon(data = data_rt, aes(x = time, ymin = rt_min, ymax = rt_max, 
                                          group = key,
                                          fill = key))
  } else{
    p3 <- p3 +
      geom_ribbon(data = data_rt, aes(x = time, ymin = rt_min, ymax = rt_max, 
                                      group = key,
                                      fill = key))
  }
  
  p3 <- p3 +  
    geom_hline(yintercept = 1, color = 'black', size = 0.1) + 
    geom_segment(data = covariates_country_long,
                 aes(x = value, y = 0, xend = value, yend = max(x)), 
                 linetype = "dashed", colour = "grey", alpha = 0.75) +
    geom_point(data = covariates_country_long, aes(x = value, 
                                                   y = x, 
                                                   group = key, 
                                                   shape = key, 
                                                   col = key), size = 2) +
    xlab("") +
    ylab(expression(R[t])) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("seagreen", 0.75), alpha("seagreen", 0.5))) + 
    scale_shape_manual(name = interven, labels = plot_labels
                       ,values = plot_values) +
    scale_colour_discrete(name = interven, labels = plot_labels) + 
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b"), 
                 limits = c(data_country$time[1], 
                            data_country$time[length(data_country$time)])) + 
    scale_y_continuous(expand = expansion(mult=c(0,0.1))) + 
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position="right")
  if (country == 'United Kingdom')
    country = 'United_Kingdom'
  
  ## Confirmed cases:
  data_cases_952 <- data.frame(data_country$time, data_country2$cases_min, 
                               data_country2$cases_max)
  names(data_cases_952) <- c("time", "cases_min", "cases_max")
  data_cases_952$key <- rep("nintyfive", length(data_cases_952$time))
  data_cases_502 <- data.frame(data_country$time, data_country2$cases_min2, 
                               data_country2$cases_max2)
  names(data_cases_502) <- c("time", "cases_min", "cases_max")
  data_cases_502$key <- rep("fifty", length(data_cases_502$time))
  data_cases2 <- rbind(data_cases_952, data_cases_502)
  levels(data_cases2$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "Daily number of confirmed infections\n"
  else ylab_text <- "Dnevno stevilo potrjeno okuzenih\n"
  
  p4 <-   ggplot(data_country, aes(x = time)) +
    geom_bar(data = data_country, aes(y = reported_cases, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_cases2,
      aes(ymin = cases_min, ymax = cases_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) + 
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55), 
                                 alpha("deepskyblue4", 0.45))) + 
    xlab(xlab_text)+	
    ylab(ylab_text)+                                 ### Nina spremenila
    theme_pubr(base_family="sans") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          legend.position = "None") + 
    guides(fill=guide_legend(ncol=1))
  
  ## Hospitalization:
  data_hosp_95 <- data.frame(data_country$time, data_hosp$hosp_min, 
                             data_hosp$hosp_max)
  names(data_hosp_95) <- c("time", "hosp_min", "hosp_max")
  data_hosp_95$key <- rep("nintyfive", length(data_hosp_95$time))
  data_hosp_50 <- data.frame(data_country$time, data_hosp$hosp_min2, 
                             data_hosp$hosp_max2)
  names(data_hosp_50) <- c("time", "hosp_min", "hosp_max")
  data_hosp_50$key <- rep("fifty", length(data_hosp_50$time))
  data_hosp_ci <- rbind(data_hosp_95, data_hosp_50)
  levels(data_hosp_ci$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "Number of hospitalized patients\n"
  else ylab_text <- "Stevilo hospitaliziranih\n"
  
  p5 <- ggplot(data_country, aes(x = time)) +
    geom_bar(data = data_hosp, aes(x=time, y = reported_hosp, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_hosp_ci,
      aes(ymin = hosp_min, ymax = hosp_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55),
                                 alpha("deepskyblue4", 0.45))) +
    xlab(xlab_text)+
    ylab(ylab_text)+                                 ### Nina spremenila
    ggtitle(hospital.name)+
    theme_pubr(base_family="sans") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "None") +
    guides(fill=guide_legend(ncol=1))
  
  ## Hospitalization IN:
  data_hosp_95 <- data.frame(data_country$time, data_hospo$hosp_min, 
                             data_hospo$hosp_max)
  names(data_hosp_95) <- c("time", "hosp_min", "hosp_max")
  data_hosp_95$key <- rep("nintyfive", length(data_hosp_95$time))
  data_hosp_50 <- data.frame(data_country$time, data_hospo$hosp_min2, 
                             data_hospo$hosp_max2)
  names(data_hosp_50) <- c("time", "hosp_min", "hosp_max")
  data_hosp_50$key <- rep("fifty", length(data_hosp_50$time))
  data_hosp_ci <- rbind(data_hosp_95, data_hosp_50)
  levels(data_hosp_ci$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "Hospitalizations IN\n"
  else ylab_text <- "Stevilo hospitaliziranih\n"
  
  p5_IN <- ggplot(data_country, aes(x = time)) +
    geom_bar(data = data_hospo, aes(x=time, y = reported_hosp, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_hosp_ci,
      aes(ymin = hosp_min, ymax = hosp_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55),
                                 alpha("deepskyblue4", 0.45))) +
    xlab(xlab_text)+
    ylab(ylab_text)+                                 ### Nina spremenila
    theme_pubr(base_family="sans") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "None") +
    guides(fill=guide_legend(ncol=1))
  
  ## Hospitalization OUT:
  data_hosp_95 <- data.frame(data_country$time, data_hospi$hosp_min, 
                             data_hospi$hosp_max)
  names(data_hosp_95) <- c("time", "hosp_min", "hosp_max")
  data_hosp_95$key <- rep("nintyfive", length(data_hosp_95$time))
  data_hosp_50 <- data.frame(data_country$time, data_hospi$hosp_min2, 
                             data_hospi$hosp_max2)
  names(data_hosp_50) <- c("time", "hosp_min", "hosp_max")
  data_hosp_50$key <- rep("fifty", length(data_hosp_50$time))
  data_hosp_ci <- rbind(data_hosp_95, data_hosp_50)
  levels(data_hosp_ci$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "Hospitalizations OUT\n"
  else ylab_text <- "Stevilo hospitaliziranih\n"
  
  p5_OUT <- ggplot(data_country, aes(x = time)) +
    geom_bar(data = data_hospi, aes(x=time, y = reported_hosp, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_hosp_ci,
      aes(ymin = hosp_min, ymax = hosp_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55),
                                 alpha("deepskyblue4", 0.45))) +
    xlab(xlab_text)+
    ylab(ylab_text)+                                 ### Nina spremenila
    theme_pubr(base_family="sans") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "None") +
    guides(fill=guide_legend(ncol=1))
  
  ## ICUs:
  data_icu_95 <- data.frame(data_country$time, data_icu$icu_min, 
                            data_icu$icu_max)
  names(data_icu_95) <- c("time", "icu_min", "icu_max")
  data_icu_95$key <- rep("nintyfive", length(data_icu_95$time))
  data_icu_50 <- data.frame(data_country$time, data_icu$icu_min2, 
                            data_icu$icu_max2)
  names(data_icu_50) <- c("time", "icu_min", "icu_max")
  data_icu_50$key <- rep("fifty", length(data_icu_50$time))
  data_icu_ci <- rbind(data_icu_95, data_icu_50)
  levels(data_icu_ci$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "Number of patients in ICU\n"
  else ylab_text <- "Stevilo bolnikov na intenzivni negi\n"
  
  p6 <- ggplot(data_country, aes(x = time)) +
    geom_bar(data = data_icu, aes(x=time, y = reported_icu, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_icu_ci,
      aes(ymin = icu_min, ymax = icu_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55),
                                 alpha("deepskyblue4", 0.45))) +
    xlab(xlab_text)+
    ylab(ylab_text)+                                 ### Nina spremenila
    ggtitle(hospital.name)+
    theme_pubr(base_family="sans") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "None") +
    guides(fill=guide_legend(ncol=1))
  
  ## ICUs IN:
  data_icu_95 <- data.frame(data_country$time, data_icui$icu_min, 
                            data_icui$icu_max)
  names(data_icu_95) <- c("time", "icu_min", "icu_max")
  data_icu_95$key <- rep("nintyfive", length(data_icu_95$time))
  data_icu_50 <- data.frame(data_country$time, data_icui$icu_min2, 
                            data_icui$icu_max2)
  names(data_icu_50) <- c("time", "icu_min", "icu_max")
  data_icu_50$key <- rep("fifty", length(data_icu_50$time))
  data_icu_ci <- rbind(data_icu_95, data_icu_50)
  levels(data_icu_ci$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "ICU IN\n"
  else ylab_text <- "Stevilo bolnikov na intenzivni negi\n"
  
  p6_IN <- ggplot(data_country, aes(x = time)) +
    geom_bar(data = data_icui, aes(x=time, y = reported_icu, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_icu_ci,
      aes(ymin = icu_min, ymax = icu_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55),
                                 alpha("deepskyblue4", 0.45))) +
    xlab(xlab_text)+
    ylab(ylab_text)+                                 ### Nina spremenila
    theme_pubr(base_family="sans") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "None") +
    guides(fill=guide_legend(ncol=1))
  
  ## ICUs OUT:
  data_icu_95 <- data.frame(data_country$time, data_icuo$icu_min, 
                            data_icuo$icu_max)
  names(data_icu_95) <- c("time", "icu_min", "icu_max")
  data_icu_95$key <- rep("nintyfive", length(data_icu_95$time))
  data_icu_50 <- data.frame(data_country$time, data_icuo$icu_min2, 
                            data_icuo$icu_max2)
  names(data_icu_50) <- c("time", "icu_min", "icu_max")
  data_icu_50$key <- rep("fifty", length(data_icu_50$time))
  data_icu_ci <- rbind(data_icu_95, data_icu_50)
  levels(data_icu_ci$key) <- c("ninetyfive", "fifty")
  
  if(language_english) ylab_text <- "ICU OUT\n"
  else ylab_text <- "Stevilo bolnikov na intenzivni negi\n"
  
  p6_OUT <- ggplot(data_country, aes(x = time)) +
    geom_bar(data = data_icuo, aes(x=time, y = reported_icu, fill = "reported"),
             fill = "coral4", stat='identity', alpha=0.5) +
    geom_ribbon(
      data = data_icu_ci,
      aes(ymin = icu_min, ymax = icu_max, fill = key)) +
    scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
    scale_y_continuous(expand = c(0, 0), labels = comma) +
    scale_fill_manual(name = "", labels = c("50%", "95%"),
                      values = c(alpha("deepskyblue4", 0.55),
                                 alpha("deepskyblue4", 0.45))) +
    xlab(xlab_text)+
    ylab(ylab_text)+                                 ### Nina spremenila
    theme_pubr(base_family="sans") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "None") +
    guides(fill=guide_legend(ncol=1))
  
  # Special plot settings for mobile
  # p3_mobile <- p3
  # +
  #   theme(legend.position="below")
  
  # Plots for Web, Desktop version
  # dir.create("combined_hospitals/web/figures/desktop/", showWarnings = FALSE, recursive = TRUE)
  # cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/desktop/", country, '_', filename2, "_infections", ".svg"), 
  #                    p1, base_height = 4, base_asp = 1.618)
  # cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/desktop/", country, '_', filename2, "_deaths", ".svg"), 
  #                    p2, base_height = 4, base_asp = 1.618)
  # cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/desktop/", country, '_', filename2, "_rt", ".svg"), 
  #                    p3, base_height = 4, base_asp = 1.618 * 2)
  # cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/desktop/", country, '_', filename2, "_cases", ".svg"), 
  #                    p4, base_height = 4, base_asp = 1.618)
  # cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/desktop/", country, '_', filename2, "_hosp", ".svg"), 
  #                    p5, base_height = 4, base_asp = 1.618)
  # cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/desktop/", country, '_', filename2, "_icu", ".svg"), 
  #                    p6, base_height = 4, base_asp = 1.618)
  
  
  # Plots for Web, Mobile version
  # dir.create("combined_hospitals/web/figures/mobile/", showWarnings = FALSE, recursive = TRUE)
  # cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, '_', filename2, "_infections", ".svg"), 
  #                    p1, base_height = 4, base_asp = 1.1)
  # cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, '_', filename2, "_deaths", ".svg"), 
  #                    p2, base_height = 4, base_asp = 1.1)
  # cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, '_', filename2, "_rt", ".svg"), 
  #                    p3_mobile, base_height = 4, base_asp = 1.5)
  # cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, '_', filename2, "_cases", ".svg"), 
  #                    p4, base_height = 4, base_asp = 1.1)
  # cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, '_', filename2, "_hosp", ".svg"), 
  #                    p5, base_height = 4, base_asp = 1.1)
  # cowplot::save_plot(filename = paste0("combined_hospitals/web/figures/mobile/", country, '_', filename2, "_icu", ".svg"), 
  #                    p6, base_height = 4, base_asp = 1.1)
  
  # Save this for plot-forecast:
  # save(p1, p3_mobile, file = paste0("combined_hospitals/results/", filename2, '_', "graphs.RData"))
  
  # p <- plot_grid(p1, p4, p3, 
  #                ncol = 3)
  # p2 <- plot_grid(p2, p2_h, p2_c, 
  #                 ncol = 3)
  # p3 <- plot_grid(p5, p5_IN, p5_OUT, 
  #                 ncol = 3)
  # p4 <- plot_grid(p6, p6_IN, p6_OUT,
  #                 ncol = 3)
  # p <- plot_grid(p, p2, p3, p4, nrow=4)
  # cowplot::save_plot(filename = paste0("combined_hospitals/figures/", country, "_three_pannel_", filename2, ".pdf"),
  #                    p, base_width = 14, base_height=7.42*2)
  
  # Hospitals:
  p3 <- plot_grid(p5, p5_IN, p5_OUT,
                  ncol = 3)
  # ICU:
  p4 <- plot_grid(p6, p6_IN, p6_OUT,
                  ncol = 3)
  # Deaths_h: p2_h
  
  return(list(p3, p4, p2_h))
  # p <- plot_grid(p, p2, nrow=2)
  # cowplot::save_plot(filename = paste0("combined_hospitals/figures/", country, "_three_pannel_", filename2, ".pdf"),
  #                    p, base_width = 14, base_height=7.42)
}


make_three_pannel_plot()