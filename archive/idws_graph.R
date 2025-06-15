load(paste0(working_data_dir, "/tu_data.RData"))

average_bill_impute <- 
  data.frame(year=2019:2024,
             bill=c(42.14, 44.89, 48.39, 52.11, 56.22, 60.66)*12+
               c(15.88, 16.92, 18.24, 19.64, 21.19, 22.86)*12)

average_bill_impute <- average_bill_impute %>%
  mutate(idws=bill/4.5*100,
         idws_1=bill/1*100,
         idws_2=bill/2*100,
         idws_3=bill/3*100,
         idws_4=bill/4*100,
         idws_5=bill/5*100,
         idws_6=bill/6*100,
         idws_7=bill/7*100,
         idws_8=bill/8*100,
         idws_9=bill/9*100,
         idws_10=bill/10*100)

tu_data <- tu_data %>%
  filter(!is.na(etie), !is.na(credit_score))

tu_summary <- bind_rows(tu_data %>%
                          mutate(year=year(credit_date)) %>%
                          group_by(tu_id, year) %>%
                          summarise(etie=mean(etie, na.rm=TRUE)) %>%
                          ungroup() %>%
                          mutate(etie=etie*1000) %>%
                          left_join(average_bill_impute, by="year"),
                        tu_data %>%
                          mutate(year=year(credit_date)) %>%
                          filter(year==2023) %>%
                          mutate(year=2024) %>%
                          group_by(tu_id, year) %>%
                          summarise(etie=mean(etie, na.rm=TRUE)) %>%
                          ungroup() %>%
                          mutate(etie=etie*1000*1.037) %>%
                          left_join(average_bill_impute, by="year")) %>%
  group_by(year) %>%
  summarise(n=n_distinct(tu_id),
            idws=sum(idws>etie, na.rm=TRUE)/n*100,
            idws_1=sum(idws_1>etie, na.rm=TRUE)/n*100,
            idws_2=sum(idws_2>etie, na.rm=TRUE)/n*100,
            idws_3=sum(idws_3>etie, na.rm=TRUE)/n*100,
            idws_4=sum(idws_4>etie, na.rm=TRUE)/n*100,
            idws_5=sum(idws_5>etie, na.rm=TRUE)/n*100,
            idws_6=sum(idws_6>etie, na.rm=TRUE)/n*100,
            idws_7=sum(idws_7>etie, na.rm=TRUE)/n*100,
            idws_8=sum(idws_8>etie, na.rm=TRUE)/n*100,
            idws_9=sum(idws_9>etie, na.rm=TRUE)/n*100,
            idws_10=sum(idws_10>etie, na.rm=TRUE)/n*100) %>%
  ungroup()

tu_summary_idws_threshold <- tu_summary %>%
  select(year, n, idws)

tu_summary <- tu_summary %>%
  select(-idws, -n) %>%
  gather(key="idws", value="value", -year) %>%
  mutate(idws=as.numeric(gsub("idws_", "", idws))/100,
         value=value/100)

gg <- ggplot(tu_summary, 
             aes(x=idws, y=value, colour=factor(year))) +
  geom_point(size=3) +
  geom_line() +
  labs(x="Percent of income (%)",
       y="Percent of households paying more (%)") +
  scale_x_continuous(breaks=seq(0, 0.1, 0.01),
                     minor_breaks=seq(0, 0.1, 0.01),
                     label=scales::label_percent(accuracy=1)) +
  scale_y_continuous(breaks=seq(0, 1, 0.1),
                     label=scales::percent) +
  fte_theme() +
  scale_colour_manual(values=colours_set,
                      name="Year") +
  guides(colour=guide_legend(nrow=1)) +
  geom_vline(xintercept=0.045, 
             colour="darkred") +
  geom_label(aes(x=0.045, y=0.8, label="UFH"), 
             colour="darkred", size=5, family="serif") +
  geom_label(aes(x=0.035, 
                 y=0.03, 
                 label=paste0("2019: ", round(tu_summary_idws_threshold[1, 3], 2), "%")), 
             colour="darkred", size=5, family="serif") +
  geom_label(aes(x=0.055, 
                 y=0.175, 
                 label=paste0("2024: ", round(tu_summary_idws_threshold[6, 3], 2), "%")), 
             colour="darkred", size=5, family="serif")
gg
ggsave(gg,
       file=paste0(output_dir, "/figures/idws.png"),
       width=12, height=8)
