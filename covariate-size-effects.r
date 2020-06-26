library(ggplot2)
library(scales)
library(ggpubr)
library(bayesplot)
library(matrixStats)
library(cowplot)
library(svglite)
library(gridExtra)

args <- commandArgs(trailingOnly = TRUE)
filename <- args[1]
model_input <- args[2]

if(model_input %in% c('combined', 'combinedTDI', 'combined_hospitals')){
  load(paste0(model_input, "/results/", filename))
} else{
  load(paste0('single/',model_input, "/results/", filename))
}


# plot_labels <- c("School Closure",
#                  "Self Isolation",
#                  "Public Events",
#                  "First Intervention",
#                  "Lockdown", 'Social distancing')

p_list <- list()


for(iter in 1:length(out$alpha[1,1,])){
  alpha = data.frame(as.matrix(out$alpha[,,iter]))
  
  # colnames(alpha) = plot_labels
  
  # suppressMessages({p = mcmc_intervals(alpha, prob = .9,transformations = function(x) exp(-x))+ 
  #   scale_x_continuous(limits = c(0,1.1), breaks=seq(0,1,0.2))+
  #   ggtitle(names(dates)[iter])})
  
  suppressMessages({p = mcmc_intervals(alpha, prob = .9,transformations = function(x) x)+ 
    ggtitle(names(dates)[iter])})
  
  class(p) <- 'ggplot'
  
  p_list[[iter]] <- p 
}

for(i in 1:length(dates)){
  p_list[[i]] = ggplotGrob(p_list[[i]])
}
# p_list <- rev(p_list)
class(p_list) <- c("arrangelist", class(p_list))
if(model_input %in% c('combined', 'combinedTDI', 'combined_hospitals')){
  ggsave(paste0(model_input, "/results/alpha-", filename, ".pdf"), p_list,height=4,width=6)
} else{
  ggsave(paste0('single/',model_input, "/results/alpha-", filename, ".pdf"), p_list,height=4,width=6)
}