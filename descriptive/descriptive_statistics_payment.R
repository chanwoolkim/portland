#=========================================================================#
# Descriptive_statistics_payment.R
#
# Create graphs for basic descriptive statistics (pie charts)
#
# Chanwool Kim, May 13, 2023
#
#=========================================================================#


#---------+---------+---------+---------+---------+---------+
# Additional analysis on payment arrangement
#---------+---------+---------+---------+---------+---------+

# LOAD DATA
load(file=paste0(working_data_dir, "/delinquency_status.RData"))


# Bills of someone who initiated a payment plan ####
delinquency_status <- delinquency_status %>%
  arrange(ACCOUNT_NO, DUE_DT) %>%
  mutate(period_month=round(interval(PERIOD_FROM_DT, PERIOD_TO_DT)/months(1)),
         period_month=ifelse(period_month==0, 1, period_month),
         delinquent_amount=delinquent_amount/period_month)

delinquency_payment_arrange <- delinquency_status %>%
  group_by(ACCOUNT_NO) %>%
  filter(any(payment_arrange)) %>%
  group_by(ACCOUNT_NO, payment_arrange) %>%
  summarise(n_bill=n(),
            delinquent=sum(delinquent, na.rm=TRUE),
            delinquent_amount=mean(delinquent_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(delinquency_rate=delinquent/n_bill) %>%
  group_by(payment_arrange) %>%
  summarise(nrow=n(),
            delinquency_rate_mean=mean(delinquency_rate, na.rm=TRUE),
            delinquency_rate_sd=sd(delinquency_rate, na.rm=TRUE),
            delinquent_amount_mean=mean(delinquent_amount, na.rm=TRUE),
            delinquent_amount_sd=sd(delinquent_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(delinquency_rate_se=delinquency_rate_sd/sqrt(nrow),
         delinquency_rate_lower_ci=
           delinquency_rate_mean-qt(1-0.05/2, nrow-1)*delinquency_rate_se,
         delinquency_rate_upper_ci=
           delinquency_rate_mean+qt(1-0.05/2, nrow-1)*delinquency_rate_se,
         delinquent_amount_se=delinquent_amount_sd/sqrt(nrow),
         delinquent_amount_lower_ci=
           delinquent_amount_mean-qt(1-0.05/2, nrow-1)*delinquent_amount_se,
         delinquent_amount_upper_ci=
           delinquent_amount_mean+qt(1-0.05/2, nrow-1)*delinquent_amount_se)

delinquency_financial_assist <- delinquency_status %>%
  group_by(ACCOUNT_NO) %>%
  filter(any(financial_assist)) %>%
  group_by(ACCOUNT_NO, financial_assist) %>%
  summarise(n_bill=n(),
            delinquent=sum(delinquent, na.rm=TRUE),
            delinquent_amount=mean(delinquent_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(delinquency_rate=delinquent/n_bill) %>%
  group_by(financial_assist) %>%
  summarise(nrow=n(),
            delinquency_rate_mean=mean(delinquency_rate, na.rm=TRUE),
            delinquency_rate_sd=sd(delinquency_rate, na.rm=TRUE),
            delinquent_amount_mean=mean(delinquent_amount, na.rm=TRUE),
            delinquent_amount_sd=sd(delinquent_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(delinquency_rate_se=delinquency_rate_sd/sqrt(nrow),
         delinquency_rate_lower_ci=
           delinquency_rate_mean-qt(1-0.05/2, nrow-1)*delinquency_rate_se,
         delinquency_rate_upper_ci=
           delinquency_rate_mean+qt(1-0.05/2, nrow-1)*delinquency_rate_se,
         delinquent_amount_se=delinquent_amount_sd/sqrt(nrow),
         delinquent_amount_lower_ci=
           delinquent_amount_mean-qt(1-0.05/2, nrow-1)*delinquent_amount_se,
         delinquent_amount_upper_ci=
           delinquent_amount_mean+qt(1-0.05/2, nrow-1)*delinquent_amount_se)

delinquency_bar <-
  rbind(delinquency_financial_assist %>%
          mutate(type=financial_assist,
                 Variable="Financial Assistance") %>%
          select(type, Variable, starts_with("delin")),
        delinquency_payment_arrange %>%
          mutate(type=payment_arrange,
                 Variable="Payment Arrangement") %>%
          select(type, Variable, starts_with("delin"))) %>%
  mutate(Variable=factor(Variable, levels=c("Financial Assistance",
                                            "Payment Arrangement")),
         type=ifelse(type, "Enrolled", "Not Enrolled"),
         type=factor(as.character(type), levels=c("Enrolled", "Not Enrolled")))

gg <- ggplot(delinquency_bar,
             aes(x=Variable, y=delinquency_rate_mean, fill=type)) + 
  geom_bar(position=position_dodge(),
           stat="identity", alpha=0.7) +
  geom_errorbar(aes(ymin=delinquency_rate_lower_ci,
                    ymax=delinquency_rate_upper_ci),
                position=position_dodge(0.9),
                width=0.2, colour="orange", alpha=0.7, size=0.3) +
  fte_theme() +
  xlab("") + ylab("") +
  labs(fill="Status") +
  scale_fill_manual(values=colours_set)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/delinquency_rate_bar.png"),
       width=6, height=4)

gg <- ggplot(delinquency_bar,
             aes(x=Variable, y=delinquent_amount_mean, fill=type)) + 
  geom_bar(position=position_dodge(),
           stat="identity", alpha=0.7) +
  geom_errorbar(aes(ymin=delinquent_amount_lower_ci,
                    ymax=delinquent_amount_upper_ci),
                position=position_dodge(0.9),
                width=0.2, colour="orange", alpha=0.7, size=0.3) +
  fte_theme() +
  xlab("") + ylab("") +
  labs(fill="Status") +
  scale_fill_manual(values=colours_set)
gg
ggsave(plot=gg,
       file=paste0(output_dir, "/delinquent_amount_bar.png"),
       width=6, height=4)


# Run regressions ####
delinquency_payment_arrange <- delinquency_status %>%
  group_by(ACCOUNT_NO) %>%
  filter(any(payment_arrange)) %>%
  group_by(ACCOUNT_NO, payment_arrange) %>%
  summarise(n_bill=n(),
            delinquent=sum(delinquent, na.rm=TRUE),
            delinquent_amount=mean(delinquent_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(delinquency_rate=delinquent/n_bill)

delinquency_financial_assist <- delinquency_status %>%
  group_by(ACCOUNT_NO) %>%
  filter(any(financial_assist)) %>%
  group_by(ACCOUNT_NO, financial_assist) %>%
  summarise(n_bill=n(),
            delinquent=sum(delinquent, na.rm=TRUE),
            delinquent_amount=mean(delinquent_amount, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(delinquency_rate=delinquent/n_bill)

model1 <- lm(delinquency_rate~payment_arrange,
             data=delinquency_payment_arrange)

model2 <- lm(delinquent_amount~payment_arrange,
             data=delinquency_payment_arrange)

model3 <- lm(delinquency_rate~financial_assist,
             data=delinquency_financial_assist)

model4 <- lm(delinquent_amount~financial_assist,
             data=delinquency_financial_assist)

tab_data <- data.frame(coef_arrange=c(summary(model1)$coefficients[2,1], NA,
                                      summary(model2)$coefficients[2,1], NA),
                       se_arrange=c(summary(model1)$coefficients[2,2], NA, 
                                    summary(model2)$coefficients[2,2], NA),
                       p_arrange=c(summary(model1)$coefficients[2,4], NA,
                                   summary(model2)$coefficients[2,4], NA),
                       coef_assist=c(NA, summary(model3)$coefficients[2,1],
                                     NA, summary(model4)$coefficients[2,1]),
                       se_assist=c(NA, summary(model3)$coefficients[2,2],
                                   NA, summary(model4)$coefficients[2,2]),
                       p_assist=c(NA, summary(model3)$coefficients[2,4],
                                  NA, summary(model4)$coefficients[2,4]),
                       coef_intercept=c(summary(model1)$coefficients[1,1],
                                        summary(model3)$coefficients[1,1],
                                        summary(model2)$coefficients[1,1],
                                        summary(model4)$coefficients[1,1]),
                       se_intercept=c(summary(model1)$coefficients[1,2],
                                      summary(model3)$coefficients[1,2],
                                      summary(model2)$coefficients[1,2],
                                      summary(model4)$coefficients[1,2]),
                       p_intercept=c(summary(model1)$coefficients[1,4],
                                     summary(model3)$coefficients[1,4],
                                     summary(model2)$coefficients[1,4],
                                     summary(model4)$coefficients[1,4]))

tab <- TR(c("", "Delinquency Rate", "Delinquent Amount"), cspan=c(1, 2, 2)) +
  midrulep(list(c(2, 3), c(4, 5))) +
  TR(c("", "(1)", "(2)", "(3)", "(4)")) +
  midrule() +
  TR("Payment Arrangement") %:% with(tab_data, TR(coef_arrange, dec=3, pvalues=p_arrange)) +
  TR("") %:% with(tab_data, TR(se_arrange, dec=3, se=T)) +
  TR("Financial Assistance") %:% with(tab_data, TR(coef_assist, dec=3, pvalues=p_assist)) +
  TR("") %:% with(tab_data, TR(se_assist, dec=3, se=T)) +
  TR("(Intercept)") %:% with(tab_data, TR(coef_intercept, dec=3, pvalues=p_intercept)) +
  TR("") %:% with(tab_data, TR(se_intercept, dec=3, se=T))

tab
TS(tab, file="delinquency_reg", header=c("lcccc"),
   pretty_rules=TRUE, output_path=output_dir, stand_alone=FALSE)
