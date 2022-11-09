##############################################################################
#                                  packages                                   #
###############################################################################
library('tidyverse')
library('DMCfun')
library('ez')
library('psychReport')

###############################################################################
#                            Analysis Experiment 1                            #
###############################################################################
###############################################################################
#                             reading in the Data                             #
###############################################################################
datDir <- paste("~/Desktop/BA/BA_analysis", "/Exp1", sep="")
datFiles <- list.files(path = datDir, pattern = ("mouseFlanker_\\d+.json$"),
                       full.names = TRUE)

dat <- NULL
for (f in datFiles) {
  dat <- rbind(dat, jsonlite::fromJSON(f, flatten = TRUE))
}

###############################################################################
#                              Data preparation                               #
###############################################################################
dat$vpNum <- dat$vpNum %>%
    as.factor() %>%
    factor(labels = c(1:length(unique(dat$vpNum))))

dat$Comp <- dat$comp %>%
    as.factor()

dat$Size <- dat$resp_size %>%
    as.factor()
######################
#  descriptive data  #
######################
vpInfo <- function(dat) {
  vp <- dat %>%
    distinct(., vpNum, .keep_all = TRUE) %>%
    mutate(age = as.numeric(age)) %>%
    summarize(
      N = n(),
      meanAge = round(mean(age), 2),
      sdAge = round(sd(age), 2),
      minAge = range(age)[1],
      maxAge = range(age)[2],
      nFemale = sum(gender == "female"),
      nMale = sum(gender == "male"),
      nNa = sum(!(gender %in% c("male", "female"))),
      nRight = sum(handedness == "right")
    )
  return(vp)
}

#######
#  N  #
#######
N <- vpInfo(dat)
   # N meanAge sdAge minAge maxAge nFemale nMale nNa nRight
# 1 30    21.6  2.85     19     34      21     8   1     29
#######
#  N  #
#######

# clock started with 500 ms fixation so subtract 500 ms and calculate
# movement time
dat$start_rt <- dat$start_rt - 500
dat$end_rt <- dat$end_rt - 500

###############################################################################
#                                  Exclusion                                  #
###############################################################################
dat <- dat %>%
    mutate(movement_rt = end_rt - start_rt)

# exclusion criterion based on start time and movement time
rt_limit <- c(50, 3000)
dat <- dat %>%
  mutate(
    isCorr = ifelse(corrCode == 0 & start_rt > 0 & movement_rt > rt_limit[1] &
                    movement_rt < rt_limit[2], TRUE, FALSE),
    isError = ifelse(corrCode == 1 & start_rt > 0 & movement_rt > rt_limit[1] &
                     movement_rt < rt_limit[2], TRUE, FALSE),
    ClickOut = ifelse(nclicks != 2, TRUE, FALSE),
    isLeft = ifelse(handedness == "left", TRUE, FALSE),
    isPrac = ifelse(blockNum < 3, TRUE, FALSE),
    isSlow = end_rt > rt_limit[2],
    isFast = start_rt < 0 | movement_rt < rt_limit[1],
    isOut = ifelse(isFast | isSlow, TRUE, FALSE),
    Excl = ifelse(isOut | isError | ClickOut, TRUE, FALSE)
    )

dat <- dat %>%
    subset(!isPrac) %>%
    subset(!isLeft)

table(dat$isOut)
table(dat$isCorr)

##########################
#  some data for report  #
##########################
n_err_out = sum(!dat$isCorr) - sum(dat$isOut)
err_out_p <- n_err_out / nrow(dat)
rt_out_p  <- sum(dat$isOut) / nrow(dat)
cl_out_p  <- sum(dat$ClickOut) / nrow(dat)
out <- sum(dat$Excl)/nrow(dat)

# dataset for the anovas
dat_err <- dat %>%
    subset(!isOut & !ClickOut)

dat_aov <- dat %>%
    subset(!Excl)

table(dat_aov$vpNum)
  # 1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19
# 460 317 364   0 304 280  44 371 383   1 354 473 112 381 339 393 266 452 483
  #20  21  22  23  24  25  26  27  28  29  30
# 391 421 425 500 316 420 211 343 369 368 416

############################
#  excluding participants  #
############################
dat_aov <- dat_aov %>%
    filter(!vpNum %in% c(4, 7, 10))

dat_err <- dat_err %>%
    filter(!vpNum %in% c(4, 7, 10))

dat_stat <- dat %>%
    filter(!vpNum %in% c(4, 7, 10))

table(dat_aov$vpNum)
####
rm(datDir, datFiles, f)

#######
#  n  #
#######
n <- vpInfo(dat_aov)
   # N meanAge sdAge minAge maxAge nFemale nMale nNa nRight
# 1 26   21.69  2.95     19     34      19     6   1     26
#######
#  n  #
#######

##########################
#  some data for report  #
##########################
n_err_out_stat = sum(!dat_stat$isCorr) - sum(dat_stat$isOut)
err_out_p_stat <- n_err_out_stat / nrow(dat_stat)
# rt_out_p  <- sum(dat$isOut) / nrow(dat)
# cl_out_p  <- sum(dat$ClickOut) / nrow(dat)
out_stat <- sum(dat_stat$Excl)/nrow(dat_stat)

##########################
#  additional exclusion  #
##########################
# Aggregate across trials
datAggVP <- dat %>%
  group_by(vpNum, Comp, Size) %>%
  summarize(
    nTotal     = n(),
    nCorrect   = sum(corrCode == 0),
    nError     = sum(corrCode == 1),
    startRt    = mean(start_rt[isCorr]),
    endRt      = mean(end_rt[isCorr]),
    movementRt = mean(movement_rt[isCorr]),
    perError   = (nError / nTotal) * 100,
    perOut     = (sum(isOut) / nTotal) * 100
  )

# errtrials <- dat %>%
#     summarise(
#               nCorr  = sum(corrCode == 0),
#               nError = sum(corrCode == 0),
#     )

# VP Exclusion
errRate <- datAggVP %>%
  group_by(vpNum) %>%
  summarize(meanOut = mean(perOut))

badVPs <- errRate$vpNum[errRate$meanOut > 80]
if (length(badVPs) > 0) {
  datAggVP <- datAggVP %>%
    filter(!vpNum %in% badVPs)
}

dat_aov <- dat_aov %>%
  filter(!vpNum %in% badVPs, !isOut)

# luckily, no additional exclusions have to be made here
#######
#  n  #
#######
vpInfo(dat_aov)
   # N meanAge sdAge minAge maxAge nFemale nMale nNa nRight
# 1 26   21.69  2.95     19     34      19     6   1     26
#######
#  n  #
#######

###############################################################################
#                             Manipulation check                              #
###############################################################################

manipulation_target_size <- aggregate(end_rt ~ Size + vpNum, dat_aov, mean)

man_size  <- t.test(manipulation_target_size[manipulation_target_size$Size=="large", ]$end_rt,
       manipulation_target_size[manipulation_target_size$Size=="small", ]$end_rt,
       alternative = "less", paired = TRUE)

man_size_report <- statStrT(man_size)

manipulation_comp <- aggregate(end_rt ~ Comp + vpNum, dat_aov, mean)

man_comp  <- t.test(manipulation_comp[manipulation_comp$Comp=="comp", ]$end_rt,
       manipulation_comp[manipulation_comp$Comp=="incomp", ]$end_rt,
       alternative = "less", paired = TRUE)

man_comp_report <- statStrT(man_comp)

rm(manipulation_comp, manipulation_target_size, man_comp, man_size)

#########
#  ERs  #
#########
manipulation_size_Errors <- aggregate(
                                      corrCode ~ Size + vpNum,
                                      dat_err,
                                      mean
)

man_size_err  <- t.test(manipulation_size_Errors[manipulation_size_Errors$Size=="large", ]$corrCode,
       manipulation_size_Errors[manipulation_size_Errors$Size=="small", ]$corrCode,
       alternative = "less", paired = TRUE)

man_size_err_report <- statStrT(man_size_err)

manipulation_comp_Errors <- aggregate(
                                      corrCode ~ Comp + vpNum,
                                      dat_err,
                                      mean
                                     )

man_comp_err <- t.test(manipulation_comp_Errors[manipulation_comp_Errors$Comp=="comp", ]$corrCode,
       manipulation_comp_Errors[manipulation_comp_Errors$Comp=="incomp", ]$corrCode,
       alternative = "less", paired = TRUE)

man_comp_err_report  <- statStrT(man_comp_err)

rm(manipulation_comp_Errors, manipulation_size_Errors, man_comp_err, man_size_err)

###############################################################################
#                                 aggregating                                 #
###############################################################################
# old {{{{{ #
## agg_start <- aggregate(start_rt ~ Comp + Size + vpNum, dat_aov, mean)
# agg_end_1 <- aggregate(end_rt ~ Comp + Size + vpNum, dat_aov, mean)
# agg_movement_rt <- aggregate(
#                              movement_rt ~ Comp + Size + vpNum,
#                              dat_aov,
#                              mean
                             # )
# }}}}} old #
agg_end <- dat_aov %>%
    group_by(vpNum, Comp, Size) %>%
    summarise(N = n(), RT = mean(end_rt))

agg_start <- dat_aov %>%
    group_by(vpNum, Comp, Size) %>%
    summarise(N = n(), RT = mean(start_rt))

agg_movement <- dat_aov %>%
    group_by(vpNum, Comp, Size) %>%
    summarise(N = n(), RT = mean(movement_rt))

agg_errors <- dat_err %>%
    group_by(vpNum, Comp, Size) %>%
    summarise(N = n(), ER = mean(corrCode))

###############################################################################
#                                   Anovas                                    #
###############################################################################
# old {{{{{ #
# aov_end_rt <- with(agg_end, aov(end_rt ~ Comp * Size +
#                 Error(vpNum / (Comp * Size))))


# aov_start_rt  <- with(agg_start, aov(start_rt ~ Comp * Size +
#                 Error(vpNum / (Comp * Size))))

# aov_movement_rt <- with(agg_movement_rt, aov(movement_rt ~ Comp * Size +
#                 Error(vpNum / (Comp * Size))))

#####################
##  Error analysis  #
#####################
#agg_errors <- aggregate(corrCode ~ Comp + Size + vpNum, dat_err, mean)


#aov_errors <- with(agg_errors, aov(corrCode ~ Comp * Size +
#                Error(vpNum / (Comp * Size))))

# summary(aov_errors)
# }}}}} old #
#########
#  end  #
#########
anova_end_rt <- ezANOVA(agg_end, dv = .(RT),
                        wid = .(vpNum),
                        within = .(Comp, Size),
                        detailed = TRUE,
                        return_aov = TRUE
)
anova_end_rt <- aovTable(anova_end_rt)

# comp_end <- statStrAov(anova_end_rt, "Comp")
# size_end <- statStrAov(anova_end_rt, "Size")

# int_end <- statStrAov()

###########
#  start  #
###########

anova_start_rt <- ezANOVA(agg_start, dv = .(RT),
                        wid = .(vpNum),
                        within = .(Comp, Size),
                        detailed = TRUE,
                        return_aov = TRUE
)
anova_start_rt <- aovTable(anova_start_rt)

# comp_end <- statStrAov(anova_end_rt, "Comp")
# size_end <- statStrAov(anova_end_rt, "Size")

##############
#  movement  #
##############

anova_movement_rt <- ezANOVA(agg_movement, dv = .(RT),
                        wid = .(vpNum),
                        within = .(Comp, Size),
                        detailed = TRUE,
                        return_aov = TRUE
)
anova_movement_rt <- aovTable(anova_movement_rt)

############
#  errors  #
############

anova_errors <- ezANOVA(agg_errors, dv = .(ER),
                        wid = .(vpNum),
                        within = .(Comp, Size),
                        detailed = TRUE,
                        return_aov = TRUE
)

anova_errors <- aovTable(anova_errors)

#####################################
#  variables for the acutal report  #
#####################################
end_comp <- statStrAov(anova_end_rt, "Comp")
end_size <- statStrAov(anova_end_rt, "Size")
end_int <- statStrAov(anova_end_rt, "Comp:Size")

start_comp <- statStrAov(anova_start_rt, "Comp")
start_size <- statStrAov(anova_start_rt, "Size")
start_int <- statStrAov(anova_start_rt, "Comp:Size")

movement_comp <- statStrAov(anova_movement_rt, "Comp")
movement_size <- statStrAov(anova_movement_rt, "Size")
movement_int <- statStrAov(anova_movement_rt, "Comp:Size")

err_comp <- statStrAov(anova_errors, "Comp")
err_size <- statStrAov(anova_errors, "Size")
err_int <- statStrAov(anova_errors, "Comp:Size")

#########################
#  Raw data for tables  #
#########################
Raw <- datAggVP %>%
    group_by(Comp, Size) %>%
    summarise(
    N = n(),
    RT = round(mean(endRt)),
    RT_SD = round(sd(endRt)),
    Initiation = round(mean(startRt)),
    Initiation_SD = round(sd(startRt)),
    Movement = round(mean(movementRt)),
    Movement_SD = round(sd(movementRt)),
    ER = round(mean(nError/N), 2),
    ER_SD = round(sd(nError/N), 2)
    )


# printTable(anova_start_rt$ANOVA)

# Do we even include an error analysis? we don't seem to have a hypothesis for
# this
