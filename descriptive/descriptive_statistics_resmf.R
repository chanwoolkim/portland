# Descriptive Statistics

load(file=paste0(working_data_dir, "/account_info_analysis.RData"))
load(file=paste0(working_data_dir, "/portland_demographics_tract.RData"))

# Functions
stats_calculate <- function(df, group_var, var) {
  df <- df %>%
    group_by({{group_var}}) %>%
    summarise(variable=var,
              size=n(),
              mean := mean(get(var), na.rm=TRUE),
              min := min(get(var), na.rm=TRUE),
              max := max(get(var), na.rm=TRUE),
              sd := sd(get(var), na.rm=TRUE),
              sum := sum(get(var), na.rm=TRUE))
  return(df)
}

stats_df <- function(df, portland_demographics) {
  payment_stat <- data.frame()
  for (var in payment_var_list) {
    payment_stat <- rbind(payment_stat,
                          stats_calculate(df, "", var))
  }
  
  payment_stat <- payment_stat %>%
    mutate(mean=ifelse(variable %in% payment_var_round_list, round(mean), mean),
           min=ifelse(variable %in% payment_var_round_list, round(min), min),
           max=ifelse(variable %in% payment_var_round_list, round(max), max),
           sd=ifelse(variable %in% payment_var_round_list, round(sd), sd),
           sum=ifelse(variable %in% payment_var_round_list, round(sum), sum),
           mean=ifelse(variable %in% payment_var_percent_list, mean*100, mean),
           min=ifelse(variable %in% payment_var_percent_list, min*100, min),
           max=ifelse(variable %in% payment_var_percent_list, max*100, max),
           sd=ifelse(variable %in% payment_var_percent_list, sd*100, sd),
           sum=ifelse(variable %in% payment_var_percent_list, sum*100, sum))
  
  payment_stat[,1] <- payment_var_name_list
  colnames(payment_stat)[1] <- "Variable"
  df_out <- payment_stat
  return(df_out)
}

tab_df <- function(df, n) {
  tab_row <- function(row, hskip=TRUE, sum=FALSE, digit=2) {
    if (sum) {
      if (hskip) {
        out <- TR(paste0("\\quad ", df[row, 1] %>% as.character())) %:%
          TR(df[row, 2:6] %>% as.numeric(), dec=digit)
      } else {
        out <- TR(df[row, 1] %>% as.character()) %:%
          TR(df[row, 2:6] %>% as.numeric(), dec=digit)
      }
    } else {
      if (hskip) {
        out <- TR(paste0("\\quad ", df[row, 1] %>% as.character())) %:%
          TR(df[row, 2:5] %>% as.numeric(), dec=digit)
      } else {
        out <- TR(df[row, 1] %>% as.character()) %:%
          TR(df[row, 2:5] %>% as.numeric(), dec=digit)
      }
    }
    return(out)
  }
  
  add_dollar <- function(row_tab) {
    for (col in 2:row_tab$ncol) {
      row_tab$row_list[[1]][col] <-
        str_c("\\$", row_tab$row_list[[1]][col])
    }
    return(row_tab)
  }
  
  add_percent <- function(row_tab) {
    for (col in 2:row_tab$ncol) {
      row_tab$row_list[[1]][col] <-
        str_c(row_tab$row_list[[1]][col], "\\%")
    }
    return(row_tab)
  }
  
  tab <- TR(c("Variable", "Mean", "Min", "Max", "SD", "Sum")) +
    midrule() + add_percent(tab_row(1, FALSE))
  for (i in 2:5) {tab <- tab + add_percent(tab_row(i))}
  tab <- tab + midrule() + add_dollar(tab_row(6, FALSE, TRUE, 5))
  for (i in 7:10) {tab <- tab + add_dollar(tab_row(i, TRUE, TRUE, 5))}
  tab <- tab + midrule() + add_percent(tab_row(11, FALSE))
  for (j in 1:4) {
    tab <- tab + add_percent(tab_row(11+j))
  }
  tab <- tab + add_dollar(tab_row(16, TRUE, TRUE, 5)) +
    add_dollar(tab_row(17, TRUE, TRUE, 5)) + midrule() +
    TR("n") %:% TR(c(n, NA, NA, NA, NA), dec=5)
  tab <- fix_0(tab)
  return(tab)
}


# Calculate descriptive statistics ####
# List of variables to work with
payment_var_list <- c("delinquency_rate",
                      "delinquency_rate_2019",
                      "delinquency_rate_2020",
                      "delinquency_rate_2021",
                      "delinquency_rate_2022",
                      "delinquent_amount",
                      "delinquent_amount_2019",
                      "delinquent_amount_2020",
                      "delinquent_amount_2021",
                      "delinquent_amount_2022",
                      "payment_arrange",
                      "payment_arrange_2019",
                      "payment_arrange_2020",
                      "payment_arrange_2021",
                      "payment_arrange_2022",
                      "arrange_amount_paid",
                      "arrange_amount_terminated")

payment_var_name_list <- c("Delinquency Rate",
                           "Delinquency Rate - 2019",
                           "Delinquency Rate - 2020",
                           "Delinquency Rate - 2021",
                           "Delinquency Rate - 2022",
                           "Delinquent Amount",
                           "Delinquent Amount - 2019",
                           "Delinquent Amount - 2020",
                           "Delinquent Amount - 2021",
                           "Delinquent Amount - 2022",
                           "Payment Arrangement",
                           "Payment Arrangement - 2019",
                           "Payment Arrangement - 2020",
                           "Payment Arrangement - 2021",
                           "Payment Arrangement - 2022",
                           "Payment Arrangement - Good Amount",
                           "Payment Arrangement - Terminated Amount")

payment_var_round_list <- c("delinquent_amount",
                            "delinquent_amount_2019",
                            "delinquent_amount_2020",
                            "delinquent_amount_2021",
                            "delinquent_amount_2022",
                            "arrange_amount_paid",
                            "arrange_amount_terminated")

payment_var_percent_list <-
  payment_var_list[-which(payment_var_list %in% payment_var_round_list)]

# Only consider multi family
account_info_merge <- account_info_merge %>%
  filter(ACCOUNT_CLASS_DFLT %in% c("RESMF"))

# Aggregate for the city
city_payment <- stats_df(account_info_merge, portland_demographics_tract_wide) %>%
  select(Variable, mean, min, max, sd, sum)

tab <- tab_df(city_payment, n=nrow(account_info_merge))
TS(tab, file="city_payment_resmf", header=c("l|c|c|c|c|c"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)
