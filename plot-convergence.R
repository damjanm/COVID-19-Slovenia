# Load libraries:
library(rstan)

stan_plot2 <- function (object, pars, include = TRUE, unconstrain = FALSE, 
            ...) 
  {
    inc_warmup <- FALSE
    rstan:::.check_object(object, unconstrain)
    thm <- rstan:::rstanvis_multiparam_theme()
    plot_data <- rstan:::.make_plot_data(object, pars, include, inc_warmup, 
                                 unconstrain)
    color_by_rhat <- FALSE
    dots <- list()
    defs <- list(point_est = "median", show_density = FALSE, 
                 show_outer_line = TRUE, ci_level = 0.8, outer_level = 0.95, 
                 fill_color = rstan:::rstanvis_aes_ops("fill"), outline_color = rstan:::rstanvis_aes_ops("color"), 
                 est_color = rstan:::rstanvis_aes_ops("color"))
    args <- names(defs)
    dotenv <- list()
    for (j in seq_along(args)) {
      if (args[j] %in% names(dots)) 
        dotenv[[args[j]]] <- dots[[args[j]]]
      else dotenv[[args[j]]] <- defs[[j]]
    }
    if (!(dotenv[["point_est"]] %in% c("mean", "median"))) 
      stop("Point estimate should be either 'mean' or 'median'", 
           call. = FALSE)
    if (color_by_rhat) 
      stop("'color_by_rhat' not yet available", call. = FALSE)
    if (dotenv[["ci_level"]] > dotenv[["outer_level"]]) 
      stop("'ci_level' should be less than 'outer_level'", 
           call. = FALSE)
    ci_level <- dotenv[["ci_level"]]
    outer_level <- dotenv[["outer_level"]]
    message("ci_level: ", ci_level, " (", 100 * ci_level, 
            "% intervals)")
    message("outer_level: ", outer_level, " (", 100 * 
              outer_level, "% intervals)")
    outer_level <- dotenv[["outer_level"]]
    probs.use <- c(0.5 - outer_level/2, 0.5 - ci_level/2, 0.5, 
                   0.5 + ci_level/2, 0.5 + outer_level/2)
    samp <- plot_data$samp
    nparams <- plot_data$nparams*plot_data$nchains
    statmat <- as.matrix(aggregate(samp$value, by = list(parameter = samp$parameter, chain=samp$chain), 
                                   FUN = function(x, ...) c(mean(x), quantile(x, ...)), 
                                   probs = probs.use))
    statmat <- statmat[order(statmat[,1]),]
    param_names <- rownames(statmat) <- paste0(statmat[, 1L], ' ', statmat[, 2L])
    statmat <- apply(statmat[, -1L, drop = FALSE], 1:2, as.numeric)
    colnames(statmat) <- c('chain',"mean", "2.5%", "25%", 
                           "50%", "75%", "97.5%")
    y <- as.numeric(seq(plot_data$nparams*plot_data$nchains, 1, by = -1))
    xlim.use <- c(min(statmat[, 3L]), max(statmat[, 7L]))
    xlim.use <- xlim.use + diff(xlim.use) * c(-0.05, 0.05)
    xy.df <- data.frame(params = rownames(statmat), y, statmat)
    colnames(xy.df) <- c("params", "y", 'chain', "mean", 
                         "ll", "l", "m", "h", "hh")
    if (dotenv[["point_est"]] == "mean") 
      xy.df$m <- xy.df$mean
    p.base <- ggplot2::ggplot(xy.df)
    p.name <- ggplot2::scale_y_continuous(breaks = y, labels = param_names, 
                                          limits = c(0.5, nparams + 1))
    p.all <- p.base + ggplot2::xlim(xlim.use) + p.name + thm
    show_density <- dotenv[["show_density"]]
    outline_color <- "black"
    fill_color <- dotenv[["fill_color"]]
    est_color <- dotenv[["est_color"]]
    if (dotenv[["show_outer_line"]] || show_density) {
      p.ci <- ggplot2::geom_segment(mapping = ggplot2::aes_string(x = "ll", 
                                                                  xend = "hh", y = "y", yend = "y"), 
                                    color = outline_color)
      p.all <- p.all + p.ci
    }
    if (show_density) {
      npoint.den <- 512
      y.den <- x.den <- matrix(0, nrow = npoint.den, ncol = nparams)
      for (i in 1:nparams) {
        d.temp <- density(samp[samp$parameter == param_names[i], 
                               "value"], from = statmat[i, 3L], to = statmat[i, 
                                                                             7L], n = npoint.den)
        x.den[, i] <- d.temp$x
        y.max <- max(d.temp$y)
        y.den[, i] <- d.temp$y/y.max * 0.8 + y[i]
      }
      df.den <- data.frame(x = as.vector(x.den), y = as.vector(y.den), 
                           name = rep(param_names, each = npoint.den))
      p.den <- ggplot2::geom_line(data = df.den, mapping = ggplot2::aes_string("x", 
                                                                               "y", group = "name"), color = outline_color)
      y.poly <- x.poly <- matrix(0, nrow = npoint.den + 2, 
                                 ncol = nparams)
      for (i in 1:nparams) {
        d.temp <- density(samp[samp$parameter == param_names[i], 
                               "value"], from = statmat[i, 4L], to = statmat[i, 
                                                                             6L], n = npoint.den)
        x.poly[, i] <- c(d.temp$x[1L], as.vector(d.temp$x), 
                         d.temp$x[npoint.den])
        y.max <- max(d.temp$y)
        y.poly[, i] <- as.vector(c(0, as.vector(d.temp$y)/y.max * 
                                     0.8, 0) + y[i])
      }
      df.poly <- data.frame(x = as.vector(x.poly), y = as.vector(y.poly), 
                            name = rep(param_names, each = npoint.den + 2))
      p.poly <- ggplot2::geom_polygon(data = df.poly, mapping = ggplot2::aes_string("x", 
                                                                                    "y", group = "name", fill = "y"))
      p.col <- ggplot2::scale_fill_gradient(low = fill_color, 
                                            high = fill_color, guide = "none")
      if (color_by_rhat) {
        rhat_colors <- dotenv[["rhat_colors"]]
        p.point <- ggplot2::geom_segment(ggplot2::aes_string(x = "m", 
                                                             xend = "m", y = "y", yend = "y + 0.25", 
                                                             color = "rhat_id"), size = 1.5)
        p.all + p.poly + p.den + p.col + p.point + rhat_colors
      }
      else {
        p.point <- ggplot2::geom_segment(ggplot2::aes_string(x = "m", 
                                                             xend = "m", y = "y", yend = "y + 0.25"), 
                                         colour = est_color, size = 1.5)
        p.all + p.poly + p.den + p.col + p.point
      }
    }
    else {
      p.ci.2 <- ggplot2::geom_segment(ggplot2::aes_string(x = "l", 
                                                          xend = "h", y = "y", yend = "y"), 
                                      colour = fill_color, size = 2)
      if (color_by_rhat) {
        p.point <- ggplot2::geom_point(ggplot2::aes_string(x = "m", 
                                                           y = "y", fill = "rhat_id"), color = "black", 
                                       shape = 21, size = 4)
        p.all + p.ci.2 + p.point + rhat_colors
      }
      else {
        p.point <- ggplot2::geom_point(ggplot2::aes_string(x = "m", 
                                                           y = "y"), size = 4, color = fill_color, 
                                       fill = est_color, shape = 21)
        p.all + p.ci.2 + p.point
      }
    }
  }


convergence_fun <- function(){
  
  args <- commandArgs(trailingOnly = TRUE)
  filename <- args[1]
  model_input2 <- args[2]
  
  if(!(model_input2 %in% c('combined', 'combinedTDI', 'combined_hospitals'))){
    model_input2 <- paste0("single/",model_input2)
  } 
  load(paste0(model_input2, "/results/", filename))
  
  filename <- strsplit(filename, "-stanfit.Rdata")[[1]][1]
  country <- 'Slovenia'
  
  if(model_input2 == 'combined'){ #, 'combinedTDI', 'combined_hospitals'
    load(paste0(model_input2,'/optimal_pars.RData'))
  } 
  if(model_input2 == 'single'){
    load(paste0('single/optimal_pars.RData'))
  }
  
  find_alphas <- names(fit)[substr(names(fit),1,6)=='alpha[']
  find_Rts <- names(fit)[substr(names(fit),1,7)=='Rt_adj[']
  find_Rts <- find_Rts[c(1, length(find_Rts)-14)] # Minus 14 for the MSE models.
  find_together <- c(find_alphas, find_Rts)
  
  p_trace <- stan_trace(fit,pars=find_together)+
    ggtitle('Traceplots') +
    theme(plot.title = element_text(hjust = 0.5))
  
  p_dens <- stan_dens(fit,pars=find_together, separate_chains = TRUE)+
    ggtitle('Posterior distributions') +
    theme(plot.title = element_text(hjust = 0.5))
  
  p_plot <- stan_plot2(fit,pars=find_together) +
    ggtitle('Posterior mean and CI (80%, 95%)') +
    theme(plot.title = element_text(hjust = 0.5))
  
  p_rhat <- stan_rhat(fit)+
    ggtitle('Rhat statistic - values close to 1 indicate MCMC convergence') +
    theme(plot.title = element_text(hjust = 0.5))
  

  p_grouped <- cowplot::plot_grid(p_trace, p_dens, p_plot, p_rhat, 
                         ncol = 1)
  
  cowplot::save_plot(filename = paste0(model_input2, "/web/figures/mobile/", country, "_convergence_", filename, ".svg"),
            p_grouped, base_width = 7, base_height=14)
  
  # Save eff:
  eff <- summary(fit)$summary[, "n_eff"][find_together]
  eff_df <- data.frame(Effective_sample_size=eff)
  colnames(eff_df)[1] <- 'Effective sample size'
  save(eff_df, file=paste0(model_input2, "/results/", country, "-EFF-", filename, ".RData"))
  
}

convergence_fun()
