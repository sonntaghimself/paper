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

pdf("../BA_analysis/plots/delta_start.pdf")
plot(exp1_delta_start_combined, legend = FALSE, resetPar = FALSE)
# title(main="Delta Plot for the Initiation times")
legend(175, 90, legend = c("Small", "Large"), lty = c(1,1), col = c("black", "grey"))
dev.off()

###############################################################################
#                                  Movement                                   #
###############################################################################
exp1_delta_movement_rt_small  <- dmcObservedData(dat[dat$resp_size == "small", ],  nDelta = 9, outlier = c(50, 3000), columns = c("vpNum", "comp", "movement_rt", "isCorr"), errorCoding = c(1, 0))
exp1_delta_movement_rt_large <- dmcObservedData(dat[dat$resp_size == "large", ], nDelta = 9, outlier = c(50, 3000), columns = c("vpNum", "comp", "movement_rt", "isCorr"), errorCoding = c(1, 0))

# plot(exp1_delta_movement_rt_small)

# plot(exp1_delta_movement_rt_small, figType = "delta")
# plot(exp1_delta_movement_rt_large, figType = "delta")

exp1_delta_movement_combined <- dmcCombineObservedData(exp1_delta_movement_rt_small, exp1_delta_movement_rt_large)

pdf("../BA_analysis/plots/delta_movement.pdf")
plot(exp1_delta_movement_combined, legend = FALSE, resetPar = FALSE)
# title(main="Delta Plot for the Initiation times")
legend(450, 175, legend = c("Small", "Large"), lty = c(1,1), col = c("black", "grey"))
dev.off()

###############################################################################
#                                     RT                                      #
###############################################################################
exp1_delta_end_rt_small    <- dmcObservedData(dat[dat$resp_size == "small", ], nDelta = 9, outlier = c(50, 3000), columns = c("vpNum", "comp", "end_rt", "isCorr"),   errorCoding = c(1, 0))
exp1_delta_end_rt_large    <- dmcObservedData(dat[dat$resp_size == "large", ], nDelta = 9, outlier = c(50, 3000), columns = c("vpNum", "comp", "end_rt", "isCorr"),   errorCoding = c(1, 0))

# plot(exp1_delta_end_rt_small, figType = "delta")
# plot(exp1_delta_end_rt_large, figType = "delta")

exp1_delta_end_combined <- dmcCombineObservedData(exp1_delta_end_rt_small, exp1_delta_end_rt_large)

pdf("../BA_analysis/plots/delta_end.pdf")
plot(exp1_delta_end_combined, legend = FALSE, resetPar = FALSE)
# title(main="Delta Plot for the Reaction times")
legend(750, 195, legend = c("Small", "Large"), lty = c(1,1), col = c("black", "grey"))
dev.off()

# seem to reflect the ones that are usually found with key presses

# Black line is always below the grey
# seems to be smaller in high demand vs. low demand
# What could be the reason? -> can speculate a bit

# t-test of the Effect in the 4th vs 1st and 5th vs. 2nd

# vs. Nikos study: Maybe an effect in the flanker that isn't present in the
# simon


###############################################################################
#                             Delta Plot analysis                             #
###############################################################################
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

############
#  end_rt  #
############
###########
#   4v1   #
###########
t_l <- NULL
t_s <- NULL

t_l <- exp1_delta_end_rt_large$deltaSubject %>%
    filter(Bin == "1")

t_s <- exp1_delta_end_rt_small$deltaSubject %>%
    filter(Bin == "4")

t_en <- t.test(
               t_s$meanEffect,
               t_l$meanEffect,
               paired = TRUE,
               alternative = "greater"
)

t_en_t <- statStrT(t_en)

#################
#  cleaning up  #
#################
rm(
   t_s,
   t_l
)

###########
#   5v2   #
###########
t_l <- NULL
t_s <- NULL

t_l <- exp1_delta_end_rt_large$deltaSubject %>%
    filter(Bin == "2")

t_s <- exp1_delta_end_rt_small$deltaSubject %>%
    filter(Bin == "5")

t_en_25 <- t.test(
               t_s$meanEffect,
               t_l$meanEffect,
               paired = TRUE,
               alternative = "greater"
)

t_en_25_t <- statStrT(t_en_25)

#################
#  cleaning up  #
#################
rm(
   t_s,
   t_l
)

###########
#   1v1   #
###########
t_l <- exp1_delta_end_rt_large$deltaSubject %>%
    filter(Bin == "1")

t_s <- exp1_delta_end_rt_small$deltaSubject %>%
    filter(Bin == "1")

t_en_1 <- t.test(
               t_s$meanEffect,
               t_l$meanEffect,
               paired = TRUE
)

t_en_1_t <- statStrT(t_en_1)

#################
#  cleaning up  #
#################
rm(
   t_s,
   t_l
)

###########
#   2v2   #
###########
t_l <- exp1_delta_end_rt_large$deltaSubject %>%
    filter(Bin == "2")

t_s <- exp1_delta_end_rt_small$deltaSubject %>%
    filter(Bin == "2")

t_en_2 <- t.test(
               t_s$meanEffect,
               t_l$meanEffect,
               paired = TRUE
)

t_en_2_t <- statStrT(t_en_2)

#################
#  cleaning up  #
#################
rm(
   t_s,
   t_l
)

###########
#   3v3   #
###########
t_l <- NULL
t_s <- NULL

t_l <- exp1_delta_end_rt_large$deltaSubject %>%
    filter(Bin == "3")

t_s <- exp1_delta_end_rt_small$deltaSubject %>%
    filter(Bin == "3")

t_en_3 <- t.test(
               t_s$meanEffect,
               t_l$meanEffect,
               paired = TRUE
)

t_en_3_t <- statStrT(t_en_3)

#################
#  cleaning up  #
#################
rm(
   t_s,
   t_l
)

###########
#   4v4   #
###########
t_l <- exp1_delta_end_rt_large$deltaSubject %>%
    filter(Bin == "4")

t_s <- exp1_delta_end_rt_small$deltaSubject %>%
    filter(Bin == "4")

t_en_4 <- t.test(
               t_s$meanEffect,
               t_l$meanEffect,
               paired = TRUE
)

t_en_4_t <- statStrT(t_en_4)

#################
#  cleaning up  #
#################
rm(
   t_s,
   t_l
)

###########
#   5v5   #
###########
t_l <- exp1_delta_end_rt_large$deltaSubject %>%
    filter(Bin == "5")

t_s <- exp1_delta_end_rt_small$deltaSubject %>%
    filter(Bin == "5")

t_en_5 <- t.test(
               t_s$meanEffect,
               t_l$meanEffect,
               paired = TRUE
)

t_en_5_t <- statStrT(t_en_5)

#################
#  cleaning up  #
#################
rm(
   t_s,
   t_l
)

###########
#   6v6   #
###########
t_l <- exp1_delta_end_rt_large$deltaSubject %>%
    filter(Bin == "6")

t_s <- exp1_delta_end_rt_small$deltaSubject %>%
    filter(Bin == "6")

t_en_6 <- t.test(
               t_s$meanEffect,
               t_l$meanEffect,
               paired = TRUE
)

t_en_6_t <- statStrT(t_en_6)

#################
#  cleaning up  #
#################
rm(
   t_s,
   t_l
)

###########
#   7v7   #
###########
t_l <- exp1_delta_end_rt_large$deltaSubject %>%
    filter(Bin == "7")

t_s <- exp1_delta_end_rt_small$deltaSubject %>%
    filter(Bin == "7")

t_en_7 <- t.test(
               t_s$meanEffect,
               t_l$meanEffect,
               paired = TRUE
)

t_en_7_t <- statStrT(t_en_7)

#################
#  cleaning up  #
#################
rm(
   t_s,
   t_l
)

###########
#   8v8   #
###########
t_l <- exp1_delta_end_rt_large$deltaSubject %>%
    filter(Bin == "8")

t_s <- exp1_delta_end_rt_small$deltaSubject %>%
    filter(Bin == "8")

t_en_8 <- t.test(
               t_s$meanEffect,
               t_l$meanEffect,
               paired = TRUE
)

t_en_8_t <- statStrT(t_en_8)

#################
#  cleaning up  #
#################
rm(
   t_s,
   t_l
)

###########
#   9v9   #
###########
t_l <- exp1_delta_end_rt_large$deltaSubject %>%
    filter(Bin == "9")

t_s <- exp1_delta_end_rt_small$deltaSubject %>%
    filter(Bin == "9")

t_en_9 <- t.test(
               t_s$meanEffect,
               t_l$meanEffect,
               paired = TRUE
)

t_en_9_t <- statStrT(t_en_9)

#################
#  cleaning up  #
#################
rm(
   t_s,
   t_l
)


##############
#  Movement  #
##############
###########
#   4v1   #
###########
t_l <- NULL
t_s <- NULL

t_l <- exp1_delta_movement_rt_large$deltaSubject %>%
    filter(Bin == "4")

t_s <- exp1_delta_movement_rt_small$deltaSubject %>%
    filter(Bin == "1")

t_mov_41 <- t.test(
               t_l$meanEffect,
               t_s$meanEffect,
               paired = TRUE,
               alternative = "greater"
)

t_mov_41 <- statStrT(t_mov_41)

#################
#  cleaning up  #
#################
rm(
   t_s,
   t_l
)

###########
#   5v3   #
###########
t_l <- NULL
t_s <- NULL

t_l <- exp1_delta_movement_rt_large$deltaSubject %>%
    filter(Bin == "5")

t_s <- exp1_delta_movement_rt_small$deltaSubject %>%
    filter(Bin == "3")

t_mov_53 <- t.test(
               t_l$meanEffect,
               t_s$meanEffect,
               paired = TRUE,
               alternative = "greater"
)

t_mov_53 <- statStrT(t_mov_53)

#################
#  cleaning up  #
#################
rm(
   t_s,
   t_l
)

############
#  trying  #
############
t_l <- NULL
t_s <- NULL

t_l <- exp1_delta_movement_rt_large$deltaSubject %>%
    filter(Bin == "5")

t_s <- exp1_delta_movement_rt_small$deltaSubject %>%
    filter(Bin == "5")

t.test(
               t_l$meanEffect,
               t_s$meanEffect,
               paired = TRUE
)

t_mov_53 <- statStrT(t_mov_53)

#################
#  cleaning up  #
#################
rm(
   t_s,
   t_l
)
