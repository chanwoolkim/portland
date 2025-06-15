#=========================================================================#
# explore_income.R
# 
# Explore income variables in the data
#
# May 20, 2025
#=========================================================================#

#---------+---------+---------+---------+---------+---------+
# Preliminaries
#---------+---------+---------+---------+---------+---------+
if (Sys.info()[4]=="JDUBE-LT3"){
  wd = "C:/Users/jdube/Box/PWB"
} else {
  wd <- paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/../..")}
setwd(wd)
wd <- getwd()
code_dir <- paste0(wd, "/code")
data_dir <- paste0(wd, "/data/raw/servus")
auxiliary_data_dir <- paste0(wd, "/data/auxiliary")
working_data_dir <- paste0(wd, "/data/analysis")
output_dir <- paste0(wd, "/output")
# output_dir <- paste0(wd, "/../../Dropbox/Apps/Overleaf/Water Pricing/output")


#---------+---------+---------+---------+---------+---------+
# Load Utilities
#---------+---------+---------+---------+---------+---------+
source(paste0(code_dir, "/utilities/preliminary.R"))


#---------+---------+---------+---------+---------+---------+
# Load Data
#---------+---------+---------+---------+---------+---------+
# Full sample of everyone
load(paste0(working_data_dir, "/estimation_dataset_all.RData"))

# Aspire income from t==0 (2024Q4)
aspire_income_quartile <- estimation_dataset_all %>%
  filter(t==0) %>%
  select(id, aspire_income, bill_date) %>%
  arrange(id, bill_date) %>%
  distinct(id, aspire_income) %>%
  mutate(aspire_income_quartile=ntile(aspire_income, 4)) %>%
  select(id, aspire_income_quartile) %>%
  distinct()

# TU income from t==-17 (2020Q4)
tu_income_quartile_early <- estimation_dataset_all %>%
  filter(t==-17) %>%
  select(id, tu_income, bill_date) %>%
  arrange(id, bill_date) %>%
  distinct(id, tu_income) %>%
  mutate(tu_income_quartile_early=ntile(tu_income, 4)) %>%
  select(id, tu_income_quartile_early) %>%
  distinct()
  
# TU income from t==-9 (2022Q4)
tu_income_quartile_late <- estimation_dataset_all %>%
  filter(t==-9) %>%
  select(id, tu_income, bill_date) %>%
  arrange(id, bill_date) %>%
  distinct(id, tu_income) %>%
  mutate(tu_income_quartile_late=ntile(tu_income, 4)) %>%
  select(id, tu_income_quartile_late) %>%
  distinct()

income_combined <- aspire_income_quartile %>%
  left_join(tu_income_quartile_early, by="id") %>%
  left_join(tu_income_quartile_late, by="id")

change_2020_2022 <- income_combined %>%
  group_by(tu_income_quartile_early, tu_income_quartile_late) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  filter(!is.na(tu_income_quartile_early) & 
           !is.na(tu_income_quartile_late)) %>%
  group_by(tu_income_quartile_early) %>%
  mutate(share_n=n/sum(n, na.rm=TRUE)) %>%
  ungroup()

gg <- ggplot(data=change_2020_2022) +
  geom_tile(aes(x=tu_income_quartile_early, y=tu_income_quartile_late, 
                fill=share_n),
            colour="white") +
  scale_y_continuous(breaks=1:4,
                     minor_breaks=1:4) +
  scale_fill_viridis_c(limits=c(0, 1),
                       breaks=c(0, 0.5, 1),
                       label=scales::percent,
                       option="mako") +
  labs(x="Income Quartile in 2020Q4 (TU)",
       y="Income Quartile in 2022Q4 (TU)",
       fill="") +
  fte_theme() +
  theme(legend.position="bottom",
        legend.box.background=element_rect(color=NA))
gg
ggsave(paste0(output_dir, "/figures/income_quartile_change_2020_2022.png"),
       width=6, height=4)

change_2022_2024 <- income_combined %>%
  group_by(tu_income_quartile_late, aspire_income_quartile) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  filter(!is.na(tu_income_quartile_late) & 
           !is.na(aspire_income_quartile)) %>%
  group_by(tu_income_quartile_late) %>%
  mutate(share_n=n/sum(n, na.rm=TRUE)) %>%
  ungroup()

gg <- ggplot(data=change_2022_2024) +
  geom_tile(aes(x=tu_income_quartile_late, y=aspire_income_quartile, 
                fill=share_n),
            colour="white") +
  scale_y_continuous(breaks=1:4,
                     minor_breaks=1:4) +
  scale_fill_viridis_c(limits=c(0, 1),
                       breaks=c(0, 0.5, 1),
                       label=scales::percent,
                       option="mako") +
  labs(x="Income Quartile in 2022Q4 (TU)",
       y="Income Quartile in 2024Q4 (Aspire)",
       fill="") +
  fte_theme() +
  theme(legend.position="bottom",
        legend.box.background=element_rect(color=NA))
gg
ggsave(paste0(output_dir, "/figures/income_quartile_change_2022_2024.png"),
       width=6, height=4)
