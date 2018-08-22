#' Figure 4 Survival in patients with advanced lung cancer from the North Central
#' Cancer Treatment Group study. Kaplan-Meier survival curves by gender
#' (blue for men, pink for women). Rug plot on x-axis displays second-generation
#' p-values for the difference in survival time. Green ticks indicate incompatibility
#' with null hypotheses; red indicate compatibility; gray indicate inconclusive results.
#'
#'
#' Author: JDB and LDM
library(survival)
library(sgpvalue)

## Males are sex = 1

lung.Surv <- with(lung, Surv(time = time, event = status))
lung.survfit <- survfit(lung.Surv ~ lung$sex)
sCox <- coxph(lung.Surv ~ as.factor(sex), data = lung)

pred.1 <- summary(survfit(sCox, newdata = data.frame(sex = 1)))$surv
pred.2 <- summary(survfit(sCox, newdata = data.frame(sex = 2)))$surv
pred.diff <- pred.2 - pred.1
time.diff <- summary(survfit(sCox, newdata = data.frame(sex = 1)))$time
v.1 <- summary(survfit(sCox, newdata = data.frame(sex = 1)))$std.err ^ 2
v.2 <- summary(survfit(sCox, newdata = data.frame(sex=2)))$std.err ^ 2
se.diff <- sqrt(v.1+v.2)

lb <- pred.diff - 1.96 * se.diff
ub <- pred.diff + 1.96 * se.diff

# calculate pdelta
pdelt <- purrr::map2_dbl(lb, ub, p_delta, delta_lb = -0.05, delta_ub = 0.05)

# average pdelta
mean(pdelt)

#colors
colfunc <- colorRampPalette(c("grey90", "gray44"))
COL <- ifelse(pdelt == 0,"#00ea6e",
              ifelse(pdelt == 1, "#ff0000",
                     colfunc(length(
                       unique(pdelt[!(pdelt %in% c(0, 1))]))
                       )[as.numeric(cut(pdelt[!(pdelt %in% c(0, 1))],
                                        breaks = length(unique(pdelt[!(pdelt %in% c(0,1))])))
                                    )]
                     )
              )


plot(lung.survfit,
     col = c("dodgerblue1", "hotpink"),
     mark.time = F,
     lwd = c(2, 2),
     ylab = "Survival",
     xlab = "") ## sex = 1 is hotpink

for (i in 1:length(time.diff)){
  rug(time.diff[i],
      col = COL[i],
      lwd = 1.2,
      ticksize = 0.04)
}

axis(side = 1)
mtext("Days", side = 1, line = 2.25)

legend("topright",
       1,
       c("Women",
         "Men",
         " ",
         expression("p"[delta]*" = 0"),
         expression("0 < p"[delta]*" < 1"),
         expression("p"[delta]*" = 1")
         ),
       col = c("hotpink", "dodgerblue1", NA, "#00ea6e", "#D8D8D8", "#ff0000"),
       lty = 1,
       lwd = 2,
       bty = "n")

text(900, 0.75, "Interval Null Zone\n is +/- 5%")
