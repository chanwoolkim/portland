
# Draw map of all the descriptive statistics ####

# Choose relevant payment statistics
census_payment_base <- census_payment_stat %>%
  select(census_tract, variable, mean) %>%
  spread(key=variable, value=mean)

census_base <- left_join(census_payment_base,
                         portland_demographics_tract_wide,
                         by=c("census_tract"="tract"))

acs_tract_map_base <- left_join(acs_tract_base %>%
                                  mutate(census_tract=substr(GEOID, 6, 12)),
                                census_base,
                                by="census_tract")

# Names
var_list <- data.frame(var=c(payment_var_list, census_var_list),
                       var_name=c(payment_var_name_list, census_var_name_list))

var_list <- var_list %>%
  mutate(var_name=gsub("\\", "", var_name, fixed=TRUE))

# Draw graphs!
for (var in var_list$var) {
  gg <- ggplot(acs_tract_map_base,
               aes(fill=get(var))) + 
    geom_sf() +
    map_theme() +
    labs(fill=var_list$var_name[which(var_list$var==var)]) +
    scale_fill_gradient(low="#FBCF61", high="#00CC99", 
                        space="Lab", guide="colourbar")
  gg
  ggsave(plot=gg,
         file=paste0(output_dir, "/", var, "_map.png"),
         width=6, height=4)
}

acs_tract_map_base <- acs_tract_map_base %>%
  mutate(high_delinquency=census_tract %in% highest_delinquency_tracts,
         high_cutoff=census_tract %in% highest_cutoff_tracts,
         high_minority=census_tract %in% highest_minority_tracts,
         high_poverty=census_tract %in% highest_poverty_tracts,
         high_delinquency=ifelse(high_delinquency, "TRUE", "FALSE"),
         high_cutoff=ifelse(high_cutoff, "TRUE", "FALSE"),
         high_minority=ifelse(high_minority, "TRUE", "FALSE"),
         high_poverty=ifelse(high_poverty, "TRUE", "FALSE"),
         high_delinquency=factor(high_delinquency, levels=c("TRUE", "FALSE")),
         high_cutoff=factor(high_cutoff, levels=c("TRUE", "FALSE")),
         high_minority=factor(high_minority, levels=c("TRUE", "FALSE")),
         high_poverty=factor(high_poverty, levels=c("TRUE", "FALSE")))

gg <- ggplot(acs_tract_map_base,
             aes(fill=high_delinquency)) + 
  geom_sf() +
  map_theme() +
  labs(fill="Highest Delinquency Area")
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/high_delinquency_map.png"),
       width=6, height=4)

gg <- ggplot(acs_tract_map_base,
             aes(fill=high_cutoff)) + 
  geom_sf() +
  map_theme() +
  labs(fill="Highest Cutoff Area")
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/high_cutoff_map.png"),
       width=6, height=4)

gg <- ggplot(acs_tract_map_base,
             aes(fill=high_minority)) + 
  geom_sf() +
  map_theme() +
  labs(fill="Highest Minority Area")
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/high_minority_map.png"),
       width=6, height=4)

gg <- ggplot(acs_tract_map_base,
             aes(fill=high_poverty)) + 
  geom_sf() +
  map_theme() +
  labs(fill="Highest Poverty Area")
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/high_poverty_map.png"),
       width=6, height=4)

# Histogram for delinquency rates and cutoff rates
census_base <- census_base %>%
  mutate(minority=hispanic+black,
         delinquency_tract=census_tract %in% highest_delinquency_tracts,
         cutoff_tract=census_tract %in% highest_cutoff_tracts,
         minority_tract=census_tract %in% highest_minority_tracts,
         poverty_tract=census_tract %in% highest_poverty_tracts,
         delinquency_tract=ifelse(delinquency_tract, "TRUE", "FALSE"),
         cutoff_tract=ifelse(cutoff_tract, "TRUE", "FALSE"),
         minority_tract=ifelse(minority_tract, "TRUE", "FALSE"),
         poverty_tract=ifelse(poverty_tract, "TRUE", "FALSE"),
         delinquency_tract=factor(delinquency_tract, levels=c("TRUE", "FALSE")),
         cutoff_tract=factor(cutoff_tract, levels=c("TRUE", "FALSE")),
         minority_tract=factor(minority_tract, levels=c("TRUE", "FALSE")),
         poverty_tract=factor(poverty_tract, levels=c("TRUE", "FALSE")))

delinquency_var_list <- c("delinquency_rate",
                          "delinquency_rate_2019",
                          "delinquency_rate_2020",
                          "delinquency_rate_2021",
                          "delinquency_rate_2022")

for (var in delinquency_var_list) {
  gg <- ggplot(census_base,
               aes(x=get(var), fill=delinquency_tract)) + 
    geom_histogram() +
    fte_theme() +
    xlab("Delinquency Rate") + ylab("Count") +
    labs(fill="Highest Delinquency Area") +
    scale_x_continuous(labels=scales::percent)
  gg
  ggsave(plot=gg,
         file=paste0(output_dir, "/", var, "_hist.png"),
         width=6, height=4)
}

cutoff_var_list <- c("cutoff",
                     "cutoff_2019",
                     "cutoff_2020",
                     "cutoff_2021",
                     "cutoff_2022")

for (var in cutoff_var_list) {
  gg <- ggplot(census_base,
               aes(x=get(var), fill=cutoff_tract)) + 
    geom_histogram() +
    fte_theme() +
    xlab("Cutoff Rate") + ylab("Count") +
    labs(fill="Highest Cutoff Area") +
    scale_x_continuous(labels=scales::percent)
  gg
  ggsave(plot=gg,
         file=paste0(output_dir, "/", var, "_hist.png"),
         width=6, height=4)
}

gg <- ggplot(census_base,
             aes(x=minority/100, fill=minority_tract)) +
  geom_histogram() +
  fte_theme() +
  xlab("Minority Rate") + ylab("Count") +
  labs(fill="Highest Minority Area") +
  scale_x_continuous(labels=scales::percent)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/high_minority_hist.png"),
       width=6, height=4)

gg <- ggplot(census_base,
             aes(x=hh_poverty/100, fill=poverty_tract)) +
  geom_histogram() +
  fte_theme() +
  xlab("Poverty Rate") + ylab("Count") +
  labs(fill="Highest Poverty Area") +
  scale_x_continuous(labels=scales::percent)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/high_poverty_hist.png"),
       width=6, height=4)

gg <- ggplot(census_base,
             aes(x=delinquency_rate, fill=minority_tract)) +
  geom_histogram() +
  fte_theme() +
  xlab("Delinquency Rate") + ylab("Count") +
  labs(fill="Highest Minority Area") +
  scale_x_continuous(labels=scales::percent)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/high_delinquency_minority_hist.png"),
       width=6, height=4)

gg <- ggplot(census_base,
             aes(x=cutoff, fill=minority_tract)) +
  geom_histogram() +
  fte_theme() +
  xlab("Cutoff Rate") + ylab("Count") +
  labs(fill="Highest Minority Area") +
  scale_x_continuous(labels=scales::percent)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/high_cutoff_minority_hist.png"),
       width=6, height=4)

gg <- ggplot(census_base,
             aes(x=delinquency_rate, fill=poverty_tract)) +
  geom_histogram() +
  fte_theme() +
  xlab("Delinquency Rate") + ylab("Count") +
  labs(fill="Highest Poverty Area") +
  scale_x_continuous(labels=scales::percent)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/high_delinquency_poverty_hist.png"),
       width=6, height=4)

gg <- ggplot(census_base,
             aes(x=cutoff, fill=poverty_tract)) +
  geom_histogram() +
  fte_theme() +
  xlab("Cutoff Rate") + ylab("Count") +
  labs(fill="Highest Poverty Area") +
  scale_x_continuous(labels=scales::percent)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/high_cutoff_poverty_hist.png"),
       width=6, height=4)
