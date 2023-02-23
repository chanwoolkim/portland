# Graphs for descriptive statistics

# Draw map of all the descriptive statistics ####
# Choose relevant payment statistics
acs_tract_map_base <- left_join(acs_tract_base %>%
                                  mutate(census_tract=substr(GEOID, 6, 12)),
                                census_base,
                                by="census_tract")

# Names
var_list <- data.frame(var=c(payment_var_list, census_var_list),
                       var_name=c(payment_var_name_list, census_var_name_list))

var_list <- var_list %>%
  mutate(var_name=gsub("\\", "", var_name, fixed=TRUE))

# Draw maps!
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
  labs(fill="Highest Delinquency Area") +
  scale_fill_manual(values=colours_set)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/high_delinquency_map.png"),
       width=6, height=4)

gg <- ggplot(acs_tract_map_base,
             aes(fill=high_cutoff)) + 
  geom_sf() +
  map_theme() +
  labs(fill="Highest Cutoff Area") +
  scale_fill_manual(values=colours_set)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/high_cutoff_map.png"),
       width=6, height=4)

gg <- ggplot(acs_tract_map_base,
             aes(fill=high_minority)) + 
  geom_sf() +
  map_theme() +
  labs(fill="Highest Minority Area") +
  scale_fill_manual(values=colours_set)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/high_minority_map.png"),
       width=6, height=4)

gg <- ggplot(acs_tract_map_base,
             aes(fill=high_poverty)) + 
  geom_sf() +
  map_theme() +
  labs(fill="Highest Poverty Area") +
  scale_fill_manual(values=colours_set)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/high_poverty_map.png"),
       width=6, height=4)


# Histogram for delinquency rates and cutoff rates ####
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
    scale_x_continuous(labels=scales::percent) +
    scale_fill_manual(values=colours_set)
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
    scale_x_continuous(labels=scales::percent) +
    scale_fill_manual(values=colours_set)
  gg
  ggsave(plot=gg,
         file=paste0(output_dir, "/", var, "_hist.png"),
         width=6, height=4)
}


# Bar charts ####
# Prepare data
screen_census <- function(df, type) {
  df <- df %>%
    filter(Variable %in% c("\\% Food Stamp",
                           "\\% Below Poverty Line",
                           "\\% Hispanic",
                           "\\% Black")) %>%
    mutate(type=type)
  return(df)
}

character_by_hit <-
  rbind(screen_census(census_character, "All"),
        screen_census(census_character_high_delinquency, "High Delinquency"),
        screen_census(census_character_high_cutoff, "High Cutoff")) %>%
  mutate(Variable=factor(Variable, levels=c("\\% Food Stamp",
                                            "\\% Below Poverty Line",
                                            "\\% Hispanic",
                                            "\\% Black")),
         type=factor(type, levels=c("All", "High Delinquency", "High Cutoff")))

gg <- ggplot(character_by_hit,
             aes(x=Variable, y=mean, fill=type)) + 
  geom_bar(position=position_dodge(),
           stat="identity", alpha=0.7) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd),
                position=position_dodge(0.9),
                width=0.2, colour="orange", alpha=0.7, size=0.3) +
  coord_flip() +
  fte_theme() +
  xlab("") + ylab("") +
  labs(fill="Type") +
  scale_fill_manual(values=colours_set)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/character_by_hit_bar.png"),
       width=6, height=4)


# Pie charts ####
delinquency_pie <- account_info_merge %>%
  mutate(delinquent=delinquent>0,
         delinquent_linc=delinquent & financial_assist,
         delinquent_not_linc=delinquent & !financial_assist,
         not_delinquent_linc=!delinquent & financial_assist,
         not_delinquent_not_linc=!delinquent & !financial_assist) %>%
  group_by() %>%
  summarise(total=n(),
            delinquent_linc=sum(delinquent_linc, na.rm=TRUE),
            delinquent_not_linc=sum(delinquent_not_linc, na.rm=TRUE),
            not_delinquent_linc=sum(not_delinquent_linc, na.rm=TRUE),
            not_delinquent_not_linc=sum(not_delinquent_not_linc, na.rm=TRUE)) %>%
  ungroup() %>%
  gather() %>%
  mutate(Type=c("Total",
                "Delinquent, LINC",
                "Delinquent, Not LINC",
                "Not Delinquent, LINC",
                "Not Delinquent, Not LINC"),
         Type=factor(Type,
                     levels=c("Total",
                              "Delinquent, LINC",
                              "Delinquent, Not LINC",
                              "Not Delinquent, LINC",
                              "Not Delinquent, Not LINC")))

delinquency_pie_annotate <- delinquency_pie %>% 
  mutate(csum=rev(cumsum(rev(value))), 
         pos=value/2+lead(csum, 1),
         pos=if_else(is.na(pos), value/2, pos),
         value_percent=value/first(value))

gg <- ggplot(delinquency_pie[2:5,],
             aes(x="", y=value, fill=Type)) + 
  geom_bar(stat="identity", width=1) +
  geom_label_repel(data=delinquency_pie_annotate[2:5,],
                   aes(y=pos,
                       label=paste0(value, " (", round(value_percent*100, 1), "%)")),
                   size=3, nudge_x=0.65, family="serif", show.legend=FALSE,
                   colour=brewer.pal("Greys", n=9)[7],
                   segment.colour=brewer.pal("Greys", n=9)[7],
                   label.size=0.1) +
  coord_polar("y", start=0) +
  pie_theme() +
  scale_fill_manual(values=colours_set) +
  labs(fill="Delinquency Status") +
  guides(fill=guide_legend(nrow=2, byrow=TRUE))
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/delinquent_account_pie.png"),
       width=6, height=4)
