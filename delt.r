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
exp2_delta_start_rt_small  <- dmcObservedData(dat[dat$resp_size == "small", ], nDelta = 5, columns = c("vpNum", "comp", "start_rt", "isCorr"), errorCoding = c(1, 0))
exp2_delta_start_rt_large <- dmcObservedData(dat[dat$resp_size == "large", ], nDelta = 5, columns = c("vpNum", "comp", "start_rt", "isCorr"), errorCoding = c(1, 0))

# plot(exp2_delta_start_rt_small)

# plot(exp2_delta_start_rt_small, figType = "delta")
# plot(exp2_delta_start_rt_large, figType = "delta")

exp2_delta_start_combined <- dmcCombineObservedData(exp2_delta_start_rt_small, exp2_delta_start_rt_large)

pdf("../BA_analysis/plots/delta_start.pdf")
plot(exp2_delta_start_combined, legend = FALSE, resetPar = FALSE)
# title(main="Delta Plot for the Initiation times")
legend(175, 90, legend = c("Small", "Large"), lty = c(1,1), col = c("black", "grey"))
dev.off()

###############################################################################
#                                  Movement                                   #
###############################################################################
exp2_delta_movement_rt_small  <- dmcObservedData(dat[dat$resp_size == "small", ],  nDelta = 5, outlier = c(50, 3000), columns = c("vpNum", "comp", "movement_rt", "isCorr"), errorCoding = c(1, 0))
exp2_delta_movement_rt_large <- dmcObservedData(dat[dat$resp_size == "large", ], nDelta = 5, outlier = c(50, 3000), columns = c("vpNum", "comp", "movement_rt", "isCorr"), errorCoding = c(1, 0))

# plot(exp2_delta_movement_rt_small)

# plot(exp2_delta_movement_rt_small, figType = "delta")
# plot(exp2_delta_movement_rt_large, figType = "delta")

exp2_delta_movement_combined <- dmcCombineObservedData(exp2_delta_movement_rt_small, exp2_delta_movement_rt_large)

# pdf("./plots/delta_movement.pdf")
plot(exp2_delta_movement_combined, legend = FALSE, resetPar = FALSE)
# title(main="Delta Plot for the Initiation times")
legend(450, 175, legend = c("Small", "Large"), lty = c(1,1), col = c("black", "grey"))
# dev.off()

###############################################################################
#                                     RT                                      #
###############################################################################
exp2_delta_end_rt_small    <- dmcObservedData(dat[dat$resp_size == "small", ], nDelta = 5, outlier = c(50, 3000), columns = c("vpNum", "comp", "end_rt", "isCorr"),   errorCoding = c(1, 0))
exp2_delta_end_rt_large    <- dmcObservedData(dat[dat$resp_size == "large", ], nDelta = 5, outlier = c(50, 3000), columns = c("vpNum", "comp", "end_rt", "isCorr"),   errorCoding = c(1, 0))

# plot(exp2_delta_end_rt_small, figType = "delta")
# plot(exp2_delta_end_rt_large, figType = "delta")

exp2_delta_end_combined <- dmcCombineObservedData(exp2_delta_end_rt_small, exp2_delta_end_rt_large)

# pdf("./plots/delta_end.pdf")
plot(exp2_delta_end_combined, legend = FALSE, resetPar = FALSE)
# title(main="Delta Plot for the Reaction times")
legend(750, 195, legend = c("Small", "Large"), lty = c(1,1), col = c("black", "grey"))
# dev.off()

# seem to reflect the ones that are usually found with key presses

# Black line is always below the grey
# seems to be smaller in high demand vs. low demand
# What could be the reason? -> can speculate a bit

# t-test of the Effect in the 4th vs 1st and 5th vs. 2nd

# vs. Nikos study: Maybe an effect in the flanker that isn't present in the
# simon
