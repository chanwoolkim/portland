#---------+---------+---------+---------+---------+---------+
# Preliminaries
#---------+---------+---------+---------+---------+---------+
rm(list=ls())
start_time <- Sys.time()

if (Sys.info()[4]=="JDUBE-LT"){
  wd = "C:/Users/jdube/Dropbox/Servus/Portland"
} else {
  wd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/../..")}
setwd(wd)
wd <- getwd()
code_dir <- paste0(wd, "/code")
data_dir <- paste0(wd, "/data/raw/servus")
auxiliary_data_dir <- paste0(wd, "/data/auxiliary")
working_data_dir <- paste0(wd, "/data/analysis")
output_dir <- paste0(code_dir, "/output")

source(paste0(code_dir, "/utilities/preliminary.R"))
load(file=paste0(working_data_dir, "/portland_panel_estimation.RData"))


# Draw graphs
portland_panel_estimation <- portland_panel_estimation %>%
  filter(year(bill_date)==2024, quarter(bill_date)==1,
         cycle_num<65, !senior_disabilities)

cycle_count <- portland_panel_estimation %>%
  group_by(cycle_num) %>%
  summarise(n_accounts=n())

gg <- ggplot(cycle_count, aes(x=cycle_num, y=n_accounts)) +
  geom_bar(stat="identity") +
  labs(title="Number of Accounts in Each Cycle (Q1, 2024)",
       x="Cycle Number",
       y="Number of Accounts") +
  fte_theme()

ggsave(plot=gg,
       file=paste0(output_dir, "/account_cycle.png"),
       width=6, height=4)

cycle_count_tier1 <- portland_panel_estimation %>%
  filter(linc_tier_type=="Tier1") %>%
  group_by(cycle_num) %>%
  summarise(n_accounts_tier1=n()) %>%
  ungroup() %>%
  left_join(cycle_count, by="cycle_num") %>%
  mutate(share_tier1=n_accounts_tier1/n_accounts)

gg <- ggplot(cycle_count_tier1, aes(x=cycle_num, y=n_accounts_tier1)) +
  geom_bar(stat="identity") +
  labs(title="Number of Tier 1 Accounts in Each Cycle (Q1, 2024)",
       x="Cycle Number",
       y="Number of Accounts") +
  fte_theme()

ggsave(plot=gg,
       file=paste0(output_dir, "/account_cycle_tier1.png"),
       width=6, height=4)

gg <- ggplot(cycle_count_tier1, aes(x=cycle_num, y=share_tier1)) +
  geom_bar(stat="identity") +
  labs(title="Share of Tier 1 Accounts in Each Cycle (Q1, 2024)",
       x="Cycle Number",
       y="Share of Accounts") +
  fte_theme()

ggsave(plot=gg,
       file=paste0(output_dir, "/account_cycle_tier1_share.png"),
       width=6, height=4)

cycle_count_tier2 <- portland_panel_estimation %>%
  filter(linc_tier_type=="Tier2") %>%
  group_by(cycle_num) %>%
  summarise(n_accounts_tier2=n()) %>%
  ungroup() %>%
  left_join(cycle_count, by="cycle_num") %>%
  mutate(share_tier2=n_accounts_tier2/n_accounts)

gg <- ggplot(cycle_count_tier2, aes(x=cycle_num, y=n_accounts)) +
  geom_bar(stat="identity") +
  labs(title="Number of Tier 2 Accounts in Each Cycle (Q1, 2024)",
       x="Cycle Number",
       y="Number of Accounts") +
  fte_theme()

ggsave(plot=gg,
       file=paste0(output_dir, "/account_cycle_tier2.png"),
       width=6, height=4)

gg <- ggplot(cycle_count_tier2, aes(x=cycle_num, y=share_tier2)) +
  geom_bar(stat="identity") +
  labs(title="Share of Tier 2 Accounts in Each Cycle (Q1, 2024)",
       x="Cycle Number",
       y="Share of Accounts") +
  fte_theme()

ggsave(plot=gg,
       file=paste0(output_dir, "/account_cycle_tier2_share.png"),
       width=6, height=4)
