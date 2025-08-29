# preliminary.R

# Linux box cannot install these packages
if (Sys.info()[4]!="jdube01"){
  library(Hmisc)
  library(multiwayvcov)
  library(rdd)
  library(sf)
  library(tidycensus)
  library(tidygeocoder)
  library(tidyverse)
  library(tigris)
  library(vtable)
}

library(dplyr)
library(data.table)
library(fixest)
library(ggplot2)
library(ggrepel)
library(glue)
library(httr)
library(jsonlite)
library(labelled)
library(lubridate)
library(purrr)
library(RColorBrewer)
library(readr)
library(readxl)
library(scales)
library(stringr)
library(textab)
library(tidyr)
library(zip)
library(zoo)

colours_set <- c("#77AADD", "#99DDFF", "#44BB99", "#BBCC33", 
                 "#AAAA00", "#EEDD88", "#EE8866", "#FFAABB", "#DDDDDD")

colours_set_sequential <- c("#72190E", "#DC050C", "#EE8026", "#F7CB45", 
                            "#CAE0AB", "#4EB265", "#6195CF", "#1965B0", 
                            "#994F88", "#BA8DB4", "#D9CCE3")

colours_set_contrast <- c("#004488", "#DDAA33", "#BB5566")

fte_theme <- function() {
  # Generate the colours for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background="white"
  color.grid.major=palette[4]
  color.grid.minor=palette[3]
  color.axis.text=palette[7]
  color.axis.title=palette[7]
  color.title=palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=18, base_family="serif") +
    
    # Set the entire chart region to a light gray colour
    theme(panel.background=element_rect(fill=color.background, color=color.background),
          plot.background=element_rect(fill=color.background, color=color.background), 
          panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major, size=.25),
          panel.grid.minor=element_line(color=color.grid.minor, size=.25),
          axis.ticks=element_blank()) +
    
    # Format the legend
    theme(legend.position="bottom",
          legend.background=element_rect(fill=color.background),
          legend.title=element_text(size=20, color=color.axis.title, family="serif"),
          legend.text=element_text(size=18, color=color.axis.title, family="serif"),
          legend.box.background=element_rect(colour=color.grid.major),
          legend.title.align=0.5) +
    
    # Set title and axis labels, and format these and tick marks
    theme(axis.text=element_text(size=rel(1), color=color.axis.text),
          axis.title.x=element_text(color=color.axis.title, vjust=0),
          axis.title.y=element_text(color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin=unit(c(0.35, 0.2, 0.3, 0.35), "cm")) +
    
    # Plot title
    theme(plot.title=element_text(size=24, hjust=0.5),
          plot.subtitle=element_text(size=18, hjust=0.5))
}

pie_theme <- function() {
  # Generate the colours for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background="white"
  color.grid.major=palette[3]
  color.axis.text=palette[7]
  color.axis.title=palette[7]
  color.title=palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=8, base_family="serif") +
    
    # Set the entire chart region to a light gray colour
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_blank()) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend
    theme(legend.position="bottom") +
    theme(legend.background=element_rect(fill=color.background)) +
    theme(legend.title=element_text(size=9, color=color.axis.title, family="serif")) +
    theme(legend.text=element_text(size=8, color=color.axis.title, family="serif")) +
    theme(legend.box.background=element_rect(colour=color.grid.major)) +
    theme(legend.title.align=0.5) +
    
    # Set title and axis labels, and format these and tick marks
    theme(axis.text=element_blank()) +
    theme(axis.title.x=element_blank()) +
    theme(axis.title.y=element_blank()) +
    
    # Plot margins
    theme(plot.margin=unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

map_theme <- function() {
  # Generate the colours for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background="white"
  color.grid.major="white"
  color.axis.text=palette[7]
  color.axis.title=palette[7]
  color.title=palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=8, base_family="serif") +
    
    # Set the entire chart region to a light gray colour
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_blank()) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend
    theme(legend.position="bottom") +
    theme(legend.background=element_rect(fill=color.background)) +
    theme(legend.title=element_text(size=9, color=color.axis.title, family="serif")) +
    theme(legend.text=element_text(size=8, color=color.axis.title, family="serif")) +
    theme(legend.box.background=element_rect(colour=color.grid.major)) +
    theme(legend.title.align=0.5) +
    
    # Set title and axis labels, and format these and tick marks
    theme(axis.text=element_blank()) +
    theme(axis.title.x=element_blank()) +
    theme(axis.title.y=element_blank()) +
    
    # Plot margins
    theme(plot.margin=unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

# Function to go around 0 digit issue in textable (use .00000 as default)
fix_0 <- function(tab) {
  nrow <- length(tab$row_list)
  for (row in 1:nrow) {
    if (length(tab$row_list[row][[1]])>1) {
      for (col in 1:length(tab$row_list[row][[1]])) {
        tab$row_list[row][[1]][col] <-
          str_replace(tab$row_list[row][[1]][col], ".00000", "")
      }
    }
  }
  return(tab)
}

# Export .tex file of a number
export_tex <- function(text, out_file) {
  sink(paste0(output_dir, "/numbers_in_doc/", out_file, ".tex"))
  cat(text)
  sink()
}

# Reapply variable labels
reapply_labels <- function(original_df, modified_df) {
  labels <- sapply(original_df, label, simplify=FALSE)
  for (name in names(modified_df)) {
    if (!is.null(labels[[name]])) {
      label(modified_df[[name]]) <- labels[[name]]
    }
  }
  modified_df
}

# Binomial confidence interval
binom_ci <- function(p, n) {
  if (p==0) {
    return(c(0, 0))
  } else if (p==1) {
    return(c(1, 1))
  } else {
    binomial_test <- prop.test(p*n, n)
    ci <- binomial_test$conf.int
    return(c(ci[1], ci[2]))
  }
}

# Binscatter function
binscatter <- function(df, y, x, z=NULL, 
                       group=NULL, n.cut=20, cluster=NULL, threshold=0) {
  # Create a group variable (1 if none provided)
  df <- df %>%
    mutate(.group=if (!is.null(group)) .data[[group]] else 1L)
  
  # Partial out controls, if any
  if (!is.null(z)) {
    ctrl_terms <- paste(z, collapse="=")
    x_form <- as.formula(paste(x, "~", ctrl_terms))
    y_form <- as.formula(paste(y, "~", ctrl_terms))
    
    df <- df %>%
      mutate(xRes=residuals(lm(x_form, data=df, na.action=na.exclude)) +
               mean(.data[[x]], na.rm=TRUE),
             yRes=residuals(lm(y_form, data=df, na.action=na.exclude)) +
               mean(.data[[y]], na.rm=TRUE))
    
    model_form <- as.formula(paste(y, "~", x, "+", ctrl_terms))
  } else {
    df <- df %>%
      mutate(xRes=.data[[x]],
             yRes=.data[[y]])
    
    model_form <- as.formula(paste(y, "~", x))
  }
  
  message("Binscatter with formula: ", deparse(model_form))
  
  # Fit the linear model
  lm_fit <- lm(model_form, data=df)
  
  # Compute cluster‐robust SE if requested
  cluster_se <- if (!is.null(cluster)) {
    vcov_cl <- cluster.vcov(lm_fit, as.factor(df[[cluster]]))
    sqrt(vcov_cl[2, 2])
  } else {
    NA_real_
  }
  
  # Build quantile breaks around threshold
  xR <- df$xRes
  neg_idx <- xR < threshold
  pos_idx <- xR >= threshold
  
  q_neg <- quantile(xR[neg_idx], probs=seq(0, 1, length.out=n.cut+1),
                    na.rm=TRUE)
  q_pos <- quantile(xR[pos_idx], probs=seq(0, 1, length.out=n.cut+1),
                    na.rm=TRUE)[-1]
  breaks <- unique(c(q_neg, q_pos))
  
  # Bin and average
  df_bin <- df %>%
    mutate(cut=cut(xRes, breaks=breaks, include.lowest=TRUE, labels=FALSE)) %>%
    group_by(cut, .group) %>%
    summarise(x=mean(xRes, na.rm=TRUE),
              y=mean(yRes, na.rm=TRUE),
              y_sd=sd(yRes, na.rm=TRUE),
              n=n(), .groups="drop") %>%
    ungroup() %>%
    mutate(y_se=y_sd/sqrt(n),
           y_lower=y-1.96*y_se,
           y_upper=y+1.96*y_se) %>%
    select(-y_sd)
  
  return(list(df_bin=df_bin,
              in_model=lm_fit,
              cluster_se=cluster_se))
}

loc_lin <- function(df, y, bw, range, cutoff=0, gran=40, kern="triangular") {
  stopifnot("x" %in% names(df), y %in% names(df))
  
  # 1) prep left/right datasets with consistent X, Y names
  df_left <- df %>% filter(x<cutoff) %>% rename(X=x, Y=!!sym(y))
  df_right <- df %>% filter(x>cutoff) %>% rename(X=x, Y=!!sym(y))
  
  # 2) build evaluation grids
  half <- floor(gran / 2)
  grid_left <- seq(range[1], cutoff, length.out=half+1)
  grid_right <- seq(cutoff, range[2], length.out=half+1)
  
  # 3) helper to fit local‐linear=get CIs at each grid point
  fit_side <- function(data, grid_pts) {
    map_dfr(grid_pts, function(pt) {
      sub_df <- data %>% filter(between(X, pt-bw, pt+bw))
      if (nrow(sub_df)<=2) {
        data.frame(x=pt, y=NA_real_, lwr=NA_real_, upr=NA_real_)
      } else {
        w <- kernelwts(X=sub_df$X, center=pt, bw=bw, kernel=kern)
        fit <- lm(Y~X, data=sub_df, weights=w)
        ci <- predict(fit, newdata=tibble(X=pt), interval="confidence")
        data.frame(x=pt, y=ci[, "fit"], lwr=ci[, "lwr"], upr=ci[, "upr"])
      }
    })
  }
  
  # 4) generate both half‐samples
  line_minus <- fit_side(df_left,  grid_left)
  line_plus  <- fit_side(df_right, grid_right)
  
  return(list(line_minus=line_minus,
              line_plus=line_plus))
}

coef_label <- function(text, loc, size) {
  if (loc=="ur") {
    annotate("text", x=Inf, y=Inf, hjust=1, vjust=1.07, family="serif", 
             color=brewer.pal("Greys", n=9)[7], size=size, label=text)
  }
  else if (loc=="lr") {
    annotate("text", x=Inf, y=-Inf, hjust=1, vjust=-0.1, family="serif", 
             color=brewer.pal("Greys", n=9)[7],  size=size, label=text)
  }
  else if (loc=="ul") {
    annotate("text", x=-Inf, y=Inf, hjust=0, vjust=1.07, family="serif", 
             color=brewer.pal("Greys", n=9)[7],  size=size, label=text)
  }
  else if (loc=="ll") {
    annotate("text", x=-Inf, y=-Inf, hjust=0, vjust=-0.03, family="serif", 
             color=brewer.pal("Greys", n=9)[7],  size=size, label=text)
  }
}

gg_bin <- function(df, dvs, x="x", bw=bw_preferred, n_cut=n_cut,
                   xlab=NULL, ylab=NULL, title=NULL, caption=NULL, caption_loc ="ur",
                   coef_size=4.23, scales=c("none", "percent", "dollar"),
                   legend_title="Outcome", legend_labels=dvs, legend_pos=c(1, 0),
                   y_range=NULL, y_div=NULL,
                   grid=TRUE, ll=TRUE, cutoff=0, gran=40,
                   file_suffix="", sample_name=sample_file_name, 
                   out_path=out_path, save_temp=FALSE) {
  
  scales <- match.arg(scales)
  
  # 1) Compute binned means for each DV
  tmp <- map_dfr(dvs, function(dv) {
    binscatter(df=df %>% filter(x>cutoff-bw, x<cutoff+bw),
               y=dv,
               x=x,
               threshold=cutoff,
               n.cut=n_cut)$df_bin %>%
      mutate(group=dv)
  })
  
  # 2) Base ggplot: single vs multiple DVs
  if (length(dvs)==1) {
    p <- ggplot(tmp, aes(x=x, y=y)) +
      geom_point(color="navy") +
      labs(x=xlab, y=ylab, title=title) +
      fte_theme()
    
    if (ll) {
      line_data <- loc_lin(
        df=df,
        y=dvs,
        bw=bw,
        cutoff=cutoff,
        range=c(cutoff-bw, cutoff+bw),
        gran=gran
      ) %>% 
        bind_rows(.id="side")
      
      p <- p +
        geom_line(data=line_data,
                  aes(x=x, y=y, group=side),
                  color="red")
    }
    
  } else {
    p <- ggplot(tmp, aes(x=x, y=y, color=group, shape=group)) +
      geom_point() +
      labs(x=xlab, 
           y=ylab,
           title=title,
           color=legend_title,
           shape=legend_title) +
      scale_color_manual(values=c("navy", cbPalette[2:length(dvs)]),
                         labels=legend_labels) +
      scale_shape_discrete(labels=legend_labels) +
      theme(legend.position=legend_pos,
            legend.justification =legend_pos) +
      fte_theme()
  }
  
  # 3) Y‐axis limits & scales
  if (!is.null(y_range)) {
    p <- p + coord_cartesian(ylim=y_range)
  }
  if (scales=="percent") {
    p <- p + scale_y_continuous(breaks=y_div,
                                labels=scales::percent)
  } else if (scales=="dollar") {
    p <- p + scale_y_continuous(labels=scales::dollar)
  }
  
  # 4) Grid/no‐grid
  if (!grid) {
    p <- p +
      theme(panel.grid.major=element_blank(),
            panel.grid.minor=element_blank()) +
      geom_vline(xintercept=0, color="gray", size=0.5)
  }
  
  # 5) Caption
  if (!is.null(caption)) {
    p <- p + coef_label(caption, caption_loc, coef_size)
  }
  
  # 6) Save out temp data if desired
  if (save_temp) {
    saveRDS(tmp, file.path(out_path, paste0("rd_data_temp",  file_suffix, ".rds")))
    if (ll) {
      saveRDS(line_data, file.path(out_path, paste0("rd_data_line", file_suffix, ".rds")))
    }
  }
  
  # 7) Write the plot to file
  ggsave(filename=file.path(out_path, paste0("rd_", sample_name, file_suffix, ".png")),
         plot=p, width=6, height=4)
  
  # 8) Return the ggplot object
  invisible(p)
}

# Select the optimal number of bins
bin_select <- function(df, y, x, bw, n_initial=10, threshold=0) {
  df <- df %>%
    filter(bill>threshold-bw, bill<threshold+bw) %>%
    mutate(xRes=.data[[x]],
           yRes=.data[[y]])
  
  n <- nrow(df)
  xR <- df$xRes
  neg_idx <- xR < threshold
  pos_idx <- xR >= threshold
  
  result <- data.frame()
  
  for (n.cut in 2:n_initial) {
    for (m in 1:2) {
      q_neg <- quantile(xR[neg_idx], probs=seq(0, 1, length.out=n.cut*m+1),
                        na.rm=TRUE)
      q_pos <- quantile(xR[pos_idx], probs=seq(0, 1, length.out=n.cut*m+1),
                        na.rm=TRUE)[-1]
      breaks <- unique(c(q_neg, q_pos))
      
      # Bin and average
      df_bin <- df %>%
        mutate(cut=cut(xRes, breaks=breaks, include.lowest=TRUE, labels=FALSE))
      
      assign(paste0("model_", m), lm(yRes~factor(cut), data=df_bin))
      assign(paste0("rsq_", m), summary(get(paste0("model_", m)))$r.squared)
    }
    
    f_stat <- ((rsq_2-rsq_1)/(n.cut*2))/((1-rsq_2)/(n-n.cut*2-1))
    p_val <- pf(f_stat, n.cut, n-n.cut-1, lower.tail=FALSE)
    
    result <- rbind(result, 
                    data.frame(n.cut=n.cut, f_stat=f_stat, p_val=p_val))
  }
  
  print(result)
  optimal_n <- result %>%
    filter(p_val<0.1)
  
  if (nrow(optimal_n)==0) {
    optimal_n <- result %>%
      filter(p_val==min(p_val, na.rm=TRUE)) %>%
      pull(n.cut)
    
    optimal_n <- ifelse(length(optimal_n)==0, 2, optimal_n)
  } else {
    optimal_n <- optimal_n %>%
      slice(1) %>%
      pull(n.cut)
  }
  
  return(optimal_n)
}
