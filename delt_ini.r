source("./MouseConflict_Exp1.R")

# Factors
dat$vpNum     <- factor(dat$vpNum)
dat$comp      <- factor(dat$comp)
dat$resp_size <- factor(dat$resp_size)

dat <- dat %>%
  filter(!vpNum %in% badVPs, !isOut)

dat <- dat_err

# dat_err$start_rt %>% summary()
# dat_err$movement_rt %>% summary()
# dat_err$end_rt %>% summary()

###############################################################################
#                                    Start                                    #
###############################################################################
exp1_delta_start_rt_small  <- dmcObservedData(dat[dat$resp_size == "small", ], nDelta = 9, columns = c("vpNum", "comp", "start_rt", "isCorr"), errorCoding = c(1, 0))
exp1_delta_start_rt_large <- dmcObservedData(dat[dat$resp_size == "large", ], nDelta = 9, columns = c("vpNum", "comp", "start_rt", "isCorr"), errorCoding = c(1, 0))

# plot(exp1_delta_start_rt_small)

# plot(exp1_delta_start_rt_small, figType = "delta")
# plot(exp1_delta_start_rt_large, figType = "delta")

exp1_delta_start_combined <- dmcCombineObservedData(exp1_delta_start_rt_small, exp1_delta_start_rt_large)

# pdf("../BA_analysis/plots/delta_start.pdf")
# plot(exp1_delta_start_combined, legend = FALSE, resetPar = FALSE)
# title(main="Delta Plot for the Initiation times")
# legend(175, 90, legend = c("Small", "Large"), lty = c(1,1), col = c("black", "grey"))
# dev.off()

###########
#  start  #
###########
t_l <- NULL
t_s <- NULL

t_l <- exp1_delta_start_rt_large$deltaSubject %>%
    filter(Bin == "1")

t_s <- exp1_delta_start_rt_small$deltaSubject %>%
    filter(Bin == "1")

t_st <- t.test(
               t_l$meanEffect,
               t_s$meanEffect,
               paired = TRUE,
               alternative = "greater"
)

t_st <- statStrT(t_st)

#################
#  cleaning up  #
#################
rm(
   t_s,
   t_l
)
