# source("./MouseConflict_Exp1.R")
# library('psychReport')
###############################################################################
#                              Delta Plot; Start                              #
###############################################################################
##################
#  general Prep  #
##################
dat_st <- dat_aov %>%
    select(start_rt, vpNum, Comp, Size) %>%
    group_by(vpNum, Comp, Size)

dat_st_s <- dat_st %>%
    filter(Size == "small")

dat_st_l <- dat_st %>%
    filter(Size == "large")

dat_st_s <- dat_st_s %>%
    mutate(bin = ntile(x = start_rt, 5))

dat_st_l <- dat_st_l %>%
    mutate(bin = ntile(x = start_rt, 5))

dat_st_s <- dat_st_s %>%
    filter(bin == "1") %>%
    group_by(vpNum, Comp) %>%
    select(start_rt, vpNum, Comp) %>%
    summarise(start_RT_s=mean(start_rt))

dat_st_l <- dat_st_l %>%
    filter(bin == "1") %>%
    group_by(vpNum, Comp) %>%
    select(start_rt, vpNum, Comp) %>%
    summarise(start_RT_l=mean(start_rt))

st_s <- dat_st_s %>%
    group_by(vpNum, Comp) %>%
    summarise(N = n())

st_l <- dat_st_l %>%
    group_by(vpNum, Comp) %>%
    summarise(N = n())

not_enough_s <- st_s$vpNum[N < 1]
not_enough_l <- st_l$vpNum[N < 1]

if ((length(not_enough_l) > 0) | (length(not_enough_s) > 0)) {
    dat_st_s <- dat_st_s %>%
        filter(!vpNum %in% not_enough_s) %>%
        filter(!vpNum %in% not_enough_l)
    dat_st_l <- dat_st_l %>%
        filter(!vpNum %in% not_enough_s) %>%
        filter(!vpNum %in% not_enough_l)
}
################
#  prep small  #
################
dat_st_s_c <- dat_st_s %>% filter(Comp=="comp")
dat_st_s_i <- dat_st_s %>% filter(Comp=="incomp")

dat_st_s <- cbind(dat_st_s_c, dat_st_s_i$start_RT_s)
dat_st_s <- dat_st_s %>%
    select(-c(Comp))
names(dat_st_s) <- c("vpNum", "start_RT_s_c", "start_RT_s_i")

dat_st_s <- dat_st_s %>%
    mutate(Ini = start_RT_s_c - start_RT_s_i)

################
#  prep large  #
################
dat_st_l_c <- dat_st_l %>% filter(Comp=="comp")
dat_st_l_i <- dat_st_l %>% filter(Comp=="incomp")

dat_st_l <- cbind(dat_st_l_c, dat_st_l_i$start_RT_l)
dat_st_l <- dat_st_l %>%
    select(-c(Comp))
names(dat_st_l) <- c("vpNum", "start_RT_l_c", "start_RT_l_i")

dat_st_l <- dat_st_l %>%
    mutate(Ini = start_RT_l_c - start_RT_l_i)

############
#  t.test  #
############
t_st <- t.test(dat_st_l$Ini, dat_st_s$Ini, paired = TRUE, alternative = "greater")
t_st <- statStrT(t_st)

##############
#  cleaning  #
##############
rm(
   not_enough_l,
   not_enough_s,
   dat_st,
   dat_st_l,
   dat_st_s,
   dat_st_l_i,
   dat_st_l_c,
   st_s, st_l
)

###############################################################################
#                               Delta Plot; End                               #
###############################################################################
##################
#  general Prep  #
##################
dat_en <- dat_aov %>%
    select(end_rt, vpNum, Comp, Size) %>%
    group_by(vpNum, Comp, Size)

dat_en_s <- dat_en %>%
    filter(Size == "small")

dat_en_l <- dat_en %>%
    filter(Size == "large")

dat_en_s <- dat_en_s %>%
    mutate(bin = ntile(x = end_rt, 5))

dat_en_l <- dat_en_l %>%
    mutate(bin = ntile(x = end_rt, 5))

dat_en_s <- dat_en_s %>%
    filter(bin == "4") %>%
    group_by(vpNum, Comp) %>%
    select(end_rt, vpNum, Comp) %>%
    summarise(end_RT_s=mean(end_rt))

dat_en_l <- dat_en_l %>%
    filter(bin == "1") %>%
    group_by(vpNum, Comp) %>%
    select(end_rt, vpNum, Comp) %>%
    summarise(end_RT_l=mean(end_rt))

en_s <- dat_en_s %>%
    group_by(vpNum, Comp) %>%
    summarise(N = n())

en_l <- dat_en_l %>%
    group_by(vpNum, Comp) %>%
    summarise(N = n())

not_enough_s <- en_s$vpNum[N < 1]
not_enough_l <- en_l$vpNum[N < 1]

if ((length(not_enough_l) > 0) | (length(not_enough_s) > 0)) {
    dat_en_s <- dat_en_s %>%
        filter(!vpNum %in% not_enough_s) %>%
        filter(!vpNum %in% not_enough_l)
    dat_en_l <- dat_en_l %>%
        filter(!vpNum %in% not_enough_s) %>%
        filter(!vpNum %in% not_enough_l)
}
################
#  prep small  #
################
dat_en_s_c <- dat_en_s %>% filter(Comp == "comp")
dat_en_s_i <- dat_en_s %>% filter(Comp == "incomp")

dat_en_s <- cbind(dat_en_s_c, dat_en_s_i$end_RT_s)
dat_en_s <- dat_en_s %>%
    select(-c(Comp))
names(dat_en_s) <- c("vpNum", "end_RT_s_c", "end_RT_s_i")

dat_en_s <- dat_en_s %>%
    mutate(RT = end_RT_s_c - end_RT_s_i)

################
#  prep large  #
################
dat_en_l_c <- dat_en_l %>% filter(Comp == "comp")
dat_en_l_i <- dat_en_l %>% filter(Comp == "incomp")

dat_en_l <- cbind(dat_en_l_c, dat_en_l_i$end_RT_l)
dat_en_l <- dat_en_l %>%
    select(-c(Comp))
names(dat_en_l) <- c("vpNum", "end_RT_l_c", "end_RT_l_i")

dat_en_l <- dat_en_l %>%
    mutate(RT = end_RT_l_c - end_RT_l_i)

############
#  t.test  #
############
t_en <- t.test(dat_en_l$RT, dat_en_s$RT, paired = TRUE, alternative = "greater")
t_en <- statStrT(t_en)

##############
#  cleaning  #
##############
rm(
   not_enough_l,
   not_enough_s,
   dat_en,
   dat_en_l,
   dat_en_s,
   dat_en_l_i,
   dat_en_l_c,
   en_s, en_l
)

###############################################################################
#                               Delta Plot; End                               #
###############################################################################
##################
#  general Prep  #
##################
dat_en <- dat_aov %>%
    select(end_rt, vpNum, Comp, Size) %>%
    group_by(vpNum, Comp, Size)

dat_en_s <- dat_en %>%
    filter(Size == "small")

dat_en_l <- dat_en %>%
    filter(Size == "large")

dat_en_s <- dat_en_s %>%
    mutate(bin = ntile(x = end_rt, 5))

dat_en_l <- dat_en_l %>%
    mutate(bin = ntile(x = end_rt, 5))

dat_en_s <- dat_en_s %>%
    filter(bin == "5") %>%
    group_by(vpNum, Comp) %>%
    select(end_rt, vpNum, Comp) %>%
    summarise(end_RT_s=mean(end_rt))

dat_en_l <- dat_en_l %>%
    filter(bin == "2") %>%
    group_by(vpNum, Comp) %>%
    select(end_rt, vpNum, Comp) %>%
    summarise(end_RT_l=mean(end_rt))

en_s <- dat_en_s %>%
    group_by(vpNum, Comp) %>%
    summarise(N = n())

en_l <- dat_en_l %>%
    group_by(vpNum, Comp) %>%
    summarise(N = n())

not_enough_s <- en_s$vpNum[N < 1]
not_enough_l <- en_l$vpNum[N < 1]

if ((length(not_enough_l) > 0) | (length(not_enough_s) > 0)) {
    dat_en_s <- dat_en_s %>%
        filter(!vpNum %in% not_enough_s) %>%
        filter(!vpNum %in% not_enough_l)
    dat_en_l <- dat_en_l %>%
        filter(!vpNum %in% not_enough_s) %>%
        filter(!vpNum %in% not_enough_l)
}
################
#  prep small  #
################
dat_en_s_c <- dat_en_s %>% filter(Comp == "comp")
dat_en_s_i <- dat_en_s %>% filter(Comp == "incomp")

dat_en_s <- cbind(dat_en_s_c, dat_en_s_i$end_RT_s)
dat_en_s <- dat_en_s %>%
    select(-c(Comp))
names(dat_en_s) <- c("vpNum", "end_RT_s_c", "end_RT_s_i")

dat_en_s <- dat_en_s %>%
    mutate(RT = end_RT_s_c - end_RT_s_i)

################
#  prep large  #
################
dat_en_l_c <- dat_en_l %>% filter(Comp == "comp")
dat_en_l_i <- dat_en_l %>% filter(Comp == "incomp")

dat_en_l <- cbind(dat_en_l_c, dat_en_l_i$end_RT_l)
dat_en_l <- dat_en_l %>%
    select(-c(Comp))
names(dat_en_l) <- c("vpNum", "end_RT_l_c", "end_RT_l_i")

dat_en_l <- dat_en_l %>%
    mutate(RT = end_RT_l_c - end_RT_l_i)

############
#  t.test  #
############
t_en_25 <- t.test(dat_en_l$RT, dat_en_s$RT, paired = TRUE, alternative = "greater")
t_en_25 <- statStrT(t_en)

##############
#  cleaning  #
##############
rm(
   not_enough_l,
   not_enough_s,
   dat_en,
   dat_en_l,
   dat_en_s,
   dat_en_l_i,
   dat_en_l_c,
   en_s, en_l
)

###############################################################################
#                                   1 vs. 1                                   #
###############################################################################
##################
#  general Prep  #
##################
dat_en <- dat_aov %>%
    select(end_rt, vpNum, Comp, Size) %>%
    group_by(vpNum, Comp, Size)

dat_en_s <- dat_en %>%
    filter(Size == "small")

dat_en_l <- dat_en %>%
    filter(Size == "large")

dat_en_s <- dat_en_s %>%
    mutate(bin = ntile(x = end_rt, 5))

dat_en_l <- dat_en_l %>%
    mutate(bin = ntile(x = end_rt, 5))

dat_en_s <- dat_en_s %>%
    filter(bin == "1") %>%
    group_by(vpNum, Comp) %>%
    select(end_rt, vpNum, Comp) %>%
    summarise(end_RT_s=mean(end_rt))

dat_en_l <- dat_en_l %>%
    filter(bin == "1") %>%
    group_by(vpNum, Comp) %>%
    select(end_rt, vpNum, Comp) %>%
    summarise(end_RT_l=mean(end_rt))

en_s <- dat_en_s %>%
    group_by(vpNum, Comp) %>%
    summarise(N = n())

en_l <- dat_en_l %>%
    group_by(vpNum, Comp) %>%
    summarise(N = n())

not_enough_s <- en_s$vpNum[N < 1]
not_enough_l <- en_l$vpNum[N < 1]

if ((length(not_enough_l) > 0) | (length(not_enough_s) > 0)) {
    dat_en_s <- dat_en_s %>%
        filter(!vpNum %in% not_enough_s) %>%
        filter(!vpNum %in% not_enough_l)
    dat_en_l <- dat_en_l %>%
        filter(!vpNum %in% not_enough_s) %>%
        filter(!vpNum %in% not_enough_l)
}
################
#  prep small  #
################
dat_en_s_c <- dat_en_s %>% filter(Comp == "comp")
dat_en_s_i <- dat_en_s %>% filter(Comp == "incomp")

dat_en_s <- cbind(dat_en_s_c, dat_en_s_i$end_RT_s)
dat_en_s <- dat_en_s %>%
    select(-c(Comp))
names(dat_en_s) <- c("vpNum", "end_RT_s_c", "end_RT_s_i")

dat_en_s <- dat_en_s %>%
    mutate(RT = end_RT_s_c - end_RT_s_i)

################
#  prep large  #
################
dat_en_l_c <- dat_en_l %>% filter(Comp == "comp")
dat_en_l_i <- dat_en_l %>% filter(Comp == "incomp")

dat_en_l <- cbind(dat_en_l_c, dat_en_l_i$end_RT_l)
dat_en_l <- dat_en_l %>%
    select(-c(Comp))
names(dat_en_l) <- c("vpNum", "end_RT_l_c", "end_RT_l_i")

dat_en_l <- dat_en_l %>%
    mutate(RT = end_RT_l_c - end_RT_l_i)

############
#  t.test  #
############
t_en_1 <- t.test(dat_en_l$RT, dat_en_s$RT, paired = TRUE)
# t_en_1 <- statStrT(t_en_1)

##############
#  cleaning  #
##############
rm(
   not_enough_l,
   not_enough_s,
   dat_en,
   dat_en_l,
   dat_en_s,
   dat_en_l_i,
   dat_en_l_c,
   en_s, en_l
)

###############################################################################
#                                   2 vs. 2                                   #
###############################################################################
##################
#  general Prep  #
##################
dat_en <- dat_aov %>%
    select(end_rt, vpNum, Comp, Size) %>%
    group_by(vpNum, Comp, Size)

dat_en_s <- dat_en %>%
    filter(Size == "small")

dat_en_l <- dat_en %>%
    filter(Size == "large")

dat_en_s <- dat_en_s %>%
    mutate(bin = ntile(x = end_rt, 5))

dat_en_l <- dat_en_l %>%
    mutate(bin = ntile(x = end_rt, 5))

dat_en_s <- dat_en_s %>%
    filter(bin == "2") %>%
    group_by(vpNum, Comp) %>%
    select(end_rt, vpNum, Comp) %>%
    summarise(end_RT_s=mean(end_rt))

dat_en_l <- dat_en_l %>%
    filter(bin == "2") %>%
    group_by(vpNum, Comp) %>%
    select(end_rt, vpNum, Comp) %>%
    summarise(end_RT_l=mean(end_rt))

en_s <- dat_en_s %>%
    group_by(vpNum, Comp) %>%
    summarise(N = n())

en_l <- dat_en_l %>%
    group_by(vpNum, Comp) %>%
    summarise(N = n())

not_enough_s <- en_s$vpNum[N < 1]
not_enough_l <- en_l$vpNum[N < 1]

if ((length(not_enough_l) > 0) | (length(not_enough_s) > 0)) {
    dat_en_s <- dat_en_s %>%
        filter(!vpNum %in% not_enough_s) %>%
        filter(!vpNum %in% not_enough_l)
    dat_en_l <- dat_en_l %>%
        filter(!vpNum %in% not_enough_s) %>%
        filter(!vpNum %in% not_enough_l)
}
################
#  prep small  #
################
dat_en_s_c <- dat_en_s %>% filter(Comp == "comp")
dat_en_s_i <- dat_en_s %>% filter(Comp == "incomp")

dat_en_s <- cbind(dat_en_s_c, dat_en_s_i$end_RT_s)
dat_en_s <- dat_en_s %>%
    select(-c(Comp))
names(dat_en_s) <- c("vpNum", "end_RT_s_c", "end_RT_s_i")

dat_en_s <- dat_en_s %>%
    mutate(RT = end_RT_s_c - end_RT_s_i)

################
#  prep large  #
################
dat_en_l_c <- dat_en_l %>% filter(Comp == "comp")
dat_en_l_i <- dat_en_l %>% filter(Comp == "incomp")

dat_en_l <- cbind(dat_en_l_c, dat_en_l_i$end_RT_l)
dat_en_l <- dat_en_l %>%
    select(-c(Comp))
names(dat_en_l) <- c("vpNum", "end_RT_l_c", "end_RT_l_i")

dat_en_l <- dat_en_l %>%
    mutate(RT = end_RT_l_c - end_RT_l_i)

############
#  t.test  #
############
t_en_2 <- t.test(dat_en_l$RT, dat_en_s$RT, paired = TRUE)
# t_en_2 <- statStrT(t_en_2)

##############
#  cleaning  #
##############
rm(
   not_enough_l,
   not_enough_s,
   dat_en,
   dat_en_l,
   dat_en_s,
   dat_en_l_i,
   dat_en_l_c,
   en_s, en_l
)

###############################################################################
#                                   3 vs. 3                                   #
###############################################################################
##################
#  general Prep  #
##################
dat_en <- dat_aov %>%
    select(end_rt, vpNum, Comp, Size) %>%
    group_by(vpNum, Comp, Size)

dat_en_s <- dat_en %>%
    filter(Size == "small")

dat_en_l <- dat_en %>%
    filter(Size == "large")

dat_en_s <- dat_en_s %>%
    mutate(bin = ntile(x = end_rt, 5))

dat_en_l <- dat_en_l %>%
    mutate(bin = ntile(x = end_rt, 5))

dat_en_s <- dat_en_s %>%
    filter(bin == "3") %>%
    group_by(vpNum, Comp) %>%
    select(end_rt, vpNum, Comp) %>%
    summarise(end_RT_s=mean(end_rt))

dat_en_l <- dat_en_l %>%
    filter(bin == "3") %>%
    group_by(vpNum, Comp) %>%
    select(end_rt, vpNum, Comp) %>%
    summarise(end_RT_l=mean(end_rt))

en_s <- dat_en_s %>%
    group_by(vpNum, Comp) %>%
    summarise(N = n())

en_l <- dat_en_l %>%
    group_by(vpNum, Comp) %>%
    summarise(N = n())

not_enough_s <- en_s$vpNum[N < 1]
not_enough_l <- en_l$vpNum[N < 1]

if ((length(not_enough_l) > 0) | (length(not_enough_s) > 0)) {
    dat_en_s <- dat_en_s %>%
        filter(!vpNum %in% not_enough_s) %>%
        filter(!vpNum %in% not_enough_l)
    dat_en_l <- dat_en_l %>%
        filter(!vpNum %in% not_enough_s) %>%
        filter(!vpNum %in% not_enough_l)
}
################
#  prep small  #
################
dat_en_s_c <- dat_en_s %>% filter(Comp == "comp")
dat_en_s_i <- dat_en_s %>% filter(Comp == "incomp")

dat_en_s <- cbind(dat_en_s_c, dat_en_s_i$end_RT_s)
dat_en_s <- dat_en_s %>%
    select(-c(Comp))
names(dat_en_s) <- c("vpNum", "end_RT_s_c", "end_RT_s_i")

dat_en_s <- dat_en_s %>%
    mutate(RT = end_RT_s_c - end_RT_s_i)

################
#  prep large  #
################
dat_en_l_c <- dat_en_l %>% filter(Comp == "comp")
dat_en_l_i <- dat_en_l %>% filter(Comp == "incomp")

dat_en_l <- cbind(dat_en_l_c, dat_en_l_i$end_RT_l)
dat_en_l <- dat_en_l %>%
    select(-c(Comp))
names(dat_en_l) <- c("vpNum", "end_RT_l_c", "end_RT_l_i")

dat_en_l <- dat_en_l %>%
    mutate(RT = end_RT_l_c - end_RT_l_i)

############
#  t.test  #
############
t_en_3 <- t.test(dat_en_l$RT, dat_en_s$RT, paired = TRUE)
# t_en_3 <- statStrT(t_en_3)

##############
#  cleaning  #
##############
rm(
   not_enough_l,
   not_enough_s,
   dat_en,
   dat_en_l,
   dat_en_s,
   dat_en_l_i,
   dat_en_l_c,
   en_s, en_l
)

###############################################################################
#                                   4 vs. 4                                   #
###############################################################################
##################
#  general Prep  #
##################
dat_en <- dat_aov %>%
    select(end_rt, vpNum, Comp, Size) %>%
    group_by(vpNum, Comp, Size)

dat_en_s <- dat_en %>%
    filter(Size == "small")

dat_en_l <- dat_en %>%
    filter(Size == "large")

dat_en_s <- dat_en_s %>%
    mutate(bin = ntile(x = end_rt, 5))

dat_en_l <- dat_en_l %>%
    mutate(bin = ntile(x = end_rt, 5))

dat_en_s <- dat_en_s %>%
    filter(bin == "4") %>%
    group_by(vpNum, Comp) %>%
    select(end_rt, vpNum, Comp) %>%
    summarise(end_RT_s=mean(end_rt))

dat_en_l <- dat_en_l %>%
    filter(bin == "4") %>%
    group_by(vpNum, Comp) %>%
    select(end_rt, vpNum, Comp) %>%
    summarise(end_RT_l=mean(end_rt))

en_s <- dat_en_s %>%
    group_by(vpNum, Comp) %>%
    summarise(N = n())

en_l <- dat_en_l %>%
    group_by(vpNum, Comp) %>%
    summarise(N = n())

not_enough_s <- en_s$vpNum[N < 1]
not_enough_l <- en_l$vpNum[N < 1]

if ((length(not_enough_l) > 0) | (length(not_enough_s) > 0)) {
    dat_en_s <- dat_en_s %>%
        filter(!vpNum %in% not_enough_s) %>%
        filter(!vpNum %in% not_enough_l)
    dat_en_l <- dat_en_l %>%
        filter(!vpNum %in% not_enough_s) %>%
        filter(!vpNum %in% not_enough_l)
}
################
#  prep small  #
################
dat_en_s_c <- dat_en_s %>% filter(Comp == "comp")
dat_en_s_i <- dat_en_s %>% filter(Comp == "incomp")

dat_en_s <- cbind(dat_en_s_c, dat_en_s_i$end_RT_s)
dat_en_s <- dat_en_s %>%
    select(-c(Comp))
names(dat_en_s) <- c("vpNum", "end_RT_s_c", "end_RT_s_i")

dat_en_s <- dat_en_s %>%
    mutate(RT = end_RT_s_c - end_RT_s_i)

################
#  prep large  #
################
dat_en_l_c <- dat_en_l %>% filter(Comp == "comp")
dat_en_l_i <- dat_en_l %>% filter(Comp == "incomp")

dat_en_l <- cbind(dat_en_l_c, dat_en_l_i$end_RT_l)
dat_en_l <- dat_en_l %>%
    select(-c(Comp))
names(dat_en_l) <- c("vpNum", "end_RT_l_c", "end_RT_l_i")

dat_en_l <- dat_en_l %>%
    mutate(RT = end_RT_l_c - end_RT_l_i)

############
#  t.test  #
############
t_en_4 <- t.test(dat_en_l$RT, dat_en_s$RT, paired = TRUE)
# t_en_4 <- statStrT(t_en_4)

##############
#  cleaning  #
##############
rm(
   not_enough_l,
   not_enough_s,
   dat_en,
   dat_en_l,
   dat_en_s,
   dat_en_l_i,
   dat_en_l_c,
   en_s, en_l
)

###############################################################################
#                                   5 vs. 5                                   #
###############################################################################
##################
#  general Prep  #
##################
dat_en <- dat_aov %>%
    select(end_rt, vpNum, Comp, Size) %>%
    group_by(vpNum, Comp, Size)

dat_en_s <- dat_en %>%
    filter(Size == "small")

dat_en_l <- dat_en %>%
    filter(Size == "large")

dat_en_s <- dat_en_s %>%
    mutate(bin = ntile(x = end_rt, 5))

dat_en_l <- dat_en_l %>%
    mutate(bin = ntile(x = end_rt, 5))

dat_en_s <- dat_en_s %>%
    filter(bin == "5") %>%
    group_by(vpNum, Comp) %>%
    select(end_rt, vpNum, Comp) %>%
    summarise(end_RT_s=mean(end_rt))

dat_en_l <- dat_en_l %>%
    filter(bin == "5") %>%
    group_by(vpNum, Comp) %>%
    select(end_rt, vpNum, Comp) %>%
    summarise(end_RT_l=mean(end_rt))

en_s <- dat_en_s %>%
    group_by(vpNum, Comp) %>%
    summarise(N = n())

en_l <- dat_en_l %>%
    group_by(vpNum, Comp) %>%
    summarise(N = n())

not_enough_s <- en_s$vpNum[N < 1]
not_enough_l <- en_l$vpNum[N < 1]

if ((length(not_enough_l) > 0) | (length(not_enough_s) > 0)) {
    dat_en_s <- dat_en_s %>%
        filter(!vpNum %in% not_enough_s) %>%
        filter(!vpNum %in% not_enough_l)
    dat_en_l <- dat_en_l %>%
        filter(!vpNum %in% not_enough_s) %>%
        filter(!vpNum %in% not_enough_l)
}
################
#  prep small  #
################
dat_en_s_c <- dat_en_s %>% filter(Comp == "comp")
dat_en_s_i <- dat_en_s %>% filter(Comp == "incomp")

dat_en_s <- cbind(dat_en_s_c, dat_en_s_i$end_RT_s)
dat_en_s <- dat_en_s %>%
    select(-c(Comp))
names(dat_en_s) <- c("vpNum", "end_RT_s_c", "end_RT_s_i")

dat_en_s <- dat_en_s %>%
    mutate(RT = end_RT_s_c - end_RT_s_i)

################
#  prep large  #
################
dat_en_l_c <- dat_en_l %>% filter(Comp == "comp")
dat_en_l_i <- dat_en_l %>% filter(Comp == "incomp")

dat_en_l <- cbind(dat_en_l_c, dat_en_l_i$end_RT_l)
dat_en_l <- dat_en_l %>%
    select(-c(Comp))
names(dat_en_l) <- c("vpNum", "end_RT_l_c", "end_RT_l_i")

dat_en_l <- dat_en_l %>%
    mutate(RT = end_RT_l_c - end_RT_l_i)

############
#  t.test  #
############
t_en_5 <- t.test(dat_en_l$RT, dat_en_s$RT, paired = TRUE)
# t_en_5 <- statStrT(t_en_5)

##############
#  cleaning  #
##############
rm(
   not_enough_l,
   not_enough_s,
   dat_en,
   dat_en_l,
   dat_en_s,
   dat_en_l_i,
   dat_en_l_c,
   en_s, en_l
)
