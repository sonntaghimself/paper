# source("./MouseConflict_Exp1.R")

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
#                                     RT                                      #
###############################################################################
exp1_delta_end_rt_small    <- dmcObservedData(dat[dat$resp_size == "small", ], nDelta = 9, outlier = c(50, 3000), columns = c("vpNum", "comp", "end_rt", "isCorr"),   errorCoding = c(1, 0))
exp1_delta_end_rt_large    <- dmcObservedData(dat[dat$resp_size == "large", ], nDelta = 9, outlier = c(50, 3000), columns = c("vpNum", "comp", "end_rt", "isCorr"),   errorCoding = c(1, 0))

# plot(exp1_delta_end_rt_small, figType = "delta")
# plot(exp1_delta_end_rt_large, figType = "delta")

exp1_delta_end_combined <- dmcCombineObservedData(exp1_delta_end_rt_small, exp1_delta_end_rt_large)

# pdf("../BA_analysis/plots/delta_end.pdf")
# plot(exp1_delta_end_combined, legend = FALSE, resetPar = FALSE)
# title(main="Delta Plot for the Reaction times")
# legend(750, 195, legend = c("Small", "Large"), lty = c(1,1), col = c("black", "grey"))
# dev.off()

###############################################################################
#                             Delta Plot analysis                             #
###############################################################################
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
