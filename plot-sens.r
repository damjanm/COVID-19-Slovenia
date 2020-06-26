rt_fun <- function(){
  
  args <- commandArgs(trailingOnly = TRUE)
  filename <- args[1]
  model_input <- args[2]
  
  if(model_input %in% c('combined', 'combinedTDI', 'combined_hospitals')){
    load(paste0(model_input, "/results/", filename))
  } else{
    load(paste0("single/",model_input, "/results/", filename))
  }
  
  
  filename <- strsplit(filename, "-stanfit.Rdata")[[1]][1]
  
  out = rstan::extract(fit)
  
  Rt_adj <- out$Rt_adj[,1:length(dates[[1]]),]
  R_0_tmp <- Rt_adj[,dates[[1]]<interventions[1]]
  R_0 <- mean(R_0_tmp, na.rm = TRUE)
  R_0_li <- quantile(R_0_tmp, probs = 0.025)
  R_0_ui <- quantile(R_0_tmp, probs = 0.975)

  if(dates[[1]][length(dates[[1]])] == interventions[length(interventions)]) interventions <- interventions[-length(interventions)]
  R_N_tmp <- Rt_adj[,dates[[1]]>interventions[length(interventions)]]
  R_N <- mean(R_N_tmp, na.rm = TRUE)
  R_N_li <- quantile(R_N_tmp, probs = 0.025)
  R_N_ui <- quantile(R_N_tmp, probs = 0.975)
  
  ###
  # Alphas:
  
  alphas <- colMeans(out$alpha[,,1], na.rm = TRUE)
  alphas_li <- matrixStats::colQuantiles(out$alpha[,,1], probs=.025)
  alphas_ui <- matrixStats::colQuantiles(out$alpha[,,1], probs=.975)
  
  if(model_input %in% c('combined', 'combinedTDI', 'combined_hospitals')){
    file_output <- paste0(model_input, '/results/sens_', filename, '.RData')
  } else{
    file_output <- paste0('single/',model_input, '/results/sens_', filename, '.RData')
  }
  save(R_0, R_0_li, R_0_ui, R_N, R_N_li, R_N_ui, 
       alphas, alphas_li, alphas_ui,
       file=file_output)
  
}

rt_fun()

# for(i in 1:nrow(Rt_adj)){
#   if(i==1) plot(1:ncol(Rt_adj), Rt_adj[i,], ylim=c(0,10))
#   else lines(1:ncol(Rt_adj), Rt_adj[i,])
# }
# lines(1:ncol(Rt_adj), colMeans(Rt_adj[,]), col='red')