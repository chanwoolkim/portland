# Diff-in-diff analysis for different assistance programmes

# Diff-in-diff ####
delinquency_status <- delinquency_status %>%
  mutate(due_ym=str_c(due_year, month(DUE_DT)))

model1 <- feols(delinquent ~ payment_arrange | ACCOUNT_NO + due_ym,
                data=delinquency_status)

model2 <- feols(delinquent ~ financial_assist | ACCOUNT_NO + due_ym,
                data=delinquency_status)

model3 <- feols(cutoff ~ payment_arrange | ACCOUNT_NO + due_ym,
                data=delinquency_status)

model4 <- feols(cutoff ~ financial_assist | ACCOUNT_NO + due_ym,
                data=delinquency_status)

tab_data <- data.frame(coef_arrange=c(summary(model1)$coeftable[1], NA,
                                      summary(model2)$coeftable[1], NA),
                       se_arrange=c(summary(model1)$coeftable[2], NA, 
                                    summary(model2)$coeftable[2], NA),
                       p_arrange=c(summary(model1)$coeftable[4], NA,
                                   summary(model2)$coeftable[4], NA),
                       coef_assist=c(NA, summary(model3)$coeftable[1],
                                     NA, summary(model4)$coeftable[1]),
                       se_assist=c(NA, summary(model3)$coeftable[2],
                                   NA, summary(model4)$coeftable[2]),
                       p_assist=c(NA, summary(model3)$coeftable[4],
                                  NA, summary(model4)$coeftable[4]))

tab <- TR(c("", "Delinquency", "Shutoff"), cspan=c(1, 2, 2)) +
  midrulep(list(c(2, 3), c(4, 5))) +
  TR(c("Enrolled Program", "(1)", "(2)", "(3)", "(4)")) +
  midrule() +
  TR("Payment Arrangement") %:% with(tab_data, TR(coef_arrange, dec=3, pvalues=p_arrange)) +
  TR("") %:% with(tab_data, TR(se_arrange, dec=3, se=T)) +
  TR("Financial Assistance") %:% with(tab_data, TR(coef_assist, dec=3, pvalues=p_assist)) +
  TR("") %:% with(tab_data, TR(se_assist, dec=3, se=T)) +
  midrule() +
  TR(c("Individual Fixed Effects", "$\\times$", "$\\times$", "$\\times$", "$\\times$")) +
  TR(c("Time Fixed Effects", "$\\times$", "$\\times$", "$\\times$", "$\\times$"))

tab
TS(tab, file="assistance_did", header=c("lcccc"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)


# Poor man's demand curve ####
delinquency_status <-
  left_join(delinquency_status,
            bill_info %>%
              mutate(ACCOUNT_NO=as.character(ACCOUNT_NO)) %>%
              select(ACCOUNT_NO, BILL_DT, DUE_DT),
            by=c("ACCOUNT_NO", "DUE_DT")) %>%
  mutate(BILL_DT=mdy(BILL_DT))

delinquency_status <-
  left_join(delinquency_status,
            financial_assist_detail %>%
              mutate(BILL_DT=mdy(BILL_DT)) %>%
              select(ACCOUNT_NO, BILL_DT, LINC_TIER_TYPE),
            by=c("ACCOUNT_NO", "BILL_DT"))

delinquency_status <- delinquency_status %>%
  mutate(LINC_TIER_TYPE=trimws(LINC_TIER_TYPE),
         tier_1=LINC_TIER_TYPE=="Tier1",
         tier_1=ifelse(is.na(tier_1), FALSE, tier_1),
         tier_2=LINC_TIER_TYPE=="Tier2",
         tier_2=ifelse(is.na(tier_2), FALSE, tier_2),
         LINC_TIER_TYPE=ifelse(LINC_TIER_TYPE=="Tier1", "Tier 1", LINC_TIER_TYPE),
         LINC_TIER_TYPE=ifelse(LINC_TIER_TYPE=="Tier2", "Tier 2", LINC_TIER_TYPE))

model <- feols(delinquent ~ tier_1 + tier_2 | ACCOUNT_NO + due_ym,
                data=delinquency_status)

delinquency_status <- delinquency_status %>%
  mutate(fit_value=model$fitted.values,
         residual=model$residuals)

gg <- ggplot(delinquency_status %>%
               filter(!is.na(LINC_TIER_TYPE)),
             aes(x=fit_value, y=residual, colour=LINC_TIER_TYPE)) + 
  geom_point() +
  fte_theme() +
  labs(colour="Type") +
  xlab("Fitted Value") + ylab("Residuals") +
  scale_colour_manual(values=colours_set)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/demand_curve.png"),
       width=6, height=4)
