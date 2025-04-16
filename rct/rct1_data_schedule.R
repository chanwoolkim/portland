rct_info <- read_csv(paste0(working_data_dir, "/servus_query/rct_cycle.csv")
estimation_dataset <- read_csv(paste0(working_data_dir, "/servus_query/estimation_dataset.csv"))

estimation_dataset <- estimation_dataset %>%
  filter(bill_date>mdy("12/11/2024"))

rct_info <- rct_info %>%
  mutate(next_due_date=bill_date %m+% months(3) + days(21))

rct_supposed_collection <- rct_info %>%
  group_by(next_due_date) %>%
  summarise(n=n_distinct(account_number),
            n_final=sum(billing_code=="FINAL")) %>%
  ungroup() %>%
  arrange(next_due_date)

plan_users <- estimation_dataset %>%
  group_by(bill_date) %>%
  summarise(plan=sum(payment_plan, na.rm=TRUE),
            monthly=sum(monthly_payment, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(extension=plan+monthly)

rct_supposed_collection <- rct_supposed_collection %>%
  left_join(plan_users %>% select(bill_date, extension),
            by=c("next_due_date"="bill_date")) %>%
  mutate(extension=ifelse(is.na(extension), 80, extension))

extension_due <- rct_supposed_collection %>%
  mutate(extension_due=next_due_date %m+% months(2))

rct_supposed_collection <- rct_supposed_collection %>%
  full_join(extension_due %>% select(extension_due, back_extension=extension),
            by=c("next_due_date"="extension_due"))

rct_supposed_collection <- rct_supposed_collection %>%
  filter(!is.na(next_due_date))

rct_supposed_collection <- na.fill(rct_supposed_collection, 0) %>%
  as.data.frame() %>%
  mutate_at(vars(n, n_final, extension, back_extension), as.integer) %>%
  mutate(next_due_date=ymd(next_due_date),
         n_final=ifelse(n_final==0 & next_due_date<mdy('02/15/2025'), 15, n_final))

rct_supposed_collection <- rct_supposed_collection %>%
  arrange(next_due_date) %>%
  mutate(net_n=n-n_final-extension+back_extension,
         cum_n_extension=cumsum(net_n),
         cum_n=cumsum(n-n_final),
         cum_n=ifelse(next_due_date>mdy('04/01/2025'), NA, cum_n)) %>%
  select(next_due_date, cum_n, cum_n_extension) %>%
  gather(key="type", value="count", -next_due_date) %>%
  filter(!is.na(count)) %>%
  mutate(type=ifelse(type=="cum_n", "Without Extension", "With Extension"))

gg <- ggplot(rct_supposed_collection, 
             aes(x=next_due_date, y=count, colour=type)) +
  geom_line() +
  geom_point() +
  labs(x="Date", y="Count of RCT Subjects with Full Information", colour="") +
  scale_x_date(breaks=date_breaks("2 week"),
               labels=date_format("%m/%d")) +
  scale_colour_manual(values=c("Without Extension"="black",
                               "With Extension"="red")) +
  fte_theme() +
  theme(plot.title=element_text(size=24, hjust=0.5),
        plot.subtitle=element_text(size=18, hjust=0.5),
        text=element_text(size=18),
        axis.text=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.text.y=element_text(size=18),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18),
        legend.position="bottom")
gg
ggsave(gg,
       filename=paste0(output_dir, "/figures/rct_supposed_data_collection.png"),
       width=12, height=8)
