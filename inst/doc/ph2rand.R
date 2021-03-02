## ----setup, include = FALSE, cache = TRUE-------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----ph2rand, echo = FALSE, cache = TRUE--------------------------------------
suppressPackageStartupMessages(library(ph2rand))

## ----eg41_1, cache = TRUE-----------------------------------------------------
des <- des_one_stage(type    = "binomial",
                     alpha   = 0.1,
                     beta    = 0.1,
                     delta   = 0.4,
                     ratio   = 1,
                     Pi0     = 0.1,
                     Pi1     = 0.1,
                     nCmax   = 20L)

## ----eg41_2, cache = TRUE-----------------------------------------------------
summary(des)

## ----eg41_3, cache = TRUE, dpi = 100, fig.retina = 4, fig.height = 5, fig.width = 7----
term_des <- terminal(des)
plot(term_des)

## ----eg41_4, cache = TRUE, dpi = 100, fig.retina = 4--------------------------
pmf_des <- pmf(des, pi = rbind(c(0.1, 0.1),
                               c(0.1, 0.5)))
plot(pmf_des)

## ----eg41_5, cache = TRUE-----------------------------------------------------
opchar_des <- opchar(des, pi = rbind(c(0.1, 0.1),
                                     c(0.1, 0.5)))
opchar_des$opchar

## ----eg41_6, cache = TRUE, dpi = 100, fig.retina = 4--------------------------
plot(des)

## ----eg42_1, cache = TRUE-----------------------------------------------------
binomial <- des_one_stage(type  = "binomial",
                          alpha = 0.05,
                          beta  = 0.2,
                          delta = 0.4,
                          ratio = 2,
                          Pi0   = c(0.1, 0.3),
                          Pi1   = c(0.1, 0.3),
                          nCmax = 20L)
barnard  <- des_one_stage(type  = "barnard",
                          alpha = 0.05,
                          beta  = 0.2,
                          delta = 0.4,
                          ratio = 2,
                          Pi0   = c(0.1, 0.3),
                          Pi1   = c(0.1, 0.3),
                          nCmax = 20L)
fisher   <- des_one_stage(type  = "fisher",
                          alpha = 0.05,
                          beta  = 0.2,
                          delta = 0.4,
                          ratio = 2,
                          Pi0   = c(0.1, 0.3),
                          Pi1   = c(0.1, 0.3),
                          nCmax = 20L)
sat      <- des_one_stage(type  = "sat",
                          alpha = 0.05,
                          beta  = 0.2,
                          delta = 0.4,
                          ratio = 2,
                          Pi0   = c(0.1, 0.3),
                          Pi1   = c(0.1, 0.3),
                          nCmax = 20L)

## ----eg42_2, cache = TRUE-----------------------------------------------------
binomial$nC
barnard$nC
fisher$nC
sat$nC

## ----eg43_1, cache = TRUE, dpi = 100, fig.retina = 4--------------------------
binomial_1 <- des_one_stage(type  = "binomial",
                            alpha = 0.05,
                            beta  = 0.1,
                            delta = 0.4,
                            ratio = 1,
                            Pi0   = 0.1,
                            Pi1   = 0.1)
binomial_1$nC
plot(binomial_1)

## ----eg43_2, cache = TRUE, dpi = 100, fig.retina = 4--------------------------
binomial_2 <- des_one_stage(type  = "binomial",
                            alpha = 0.05,
                            beta  = 0.1,
                            delta = 0.4,
                            ratio = 1,
                            Pi0   = c(0, 1),
                            Pi1   = c(0, 0.6))
binomial_2$nC
plot(binomial_2)

## ----eg44_1, cache = TRUE-----------------------------------------------------
binomial <- des_two_stage(type  = "binomial",
                          alpha = 0.05,
                          beta  = 0.2,
                          delta = 0.4,
                          ratio = 2,
                          Pi0   = 0.2,
                          Pi1   = 0.2,
                          nCmax = 10L)
barnard  <- des_two_stage(type  = "barnard",
                          alpha = 0.05,
                          beta  = 0.2,
                          delta = 0.4,
                          ratio = 2,
                          Pi0   = 0.2,
                          Pi1   = 0.2,
                          nCmax = 15L)
fisher   <- des_two_stage(type  = "fisher",
                          alpha = 0.05,
                          beta  = 0.2,
                          delta = 0.4,
                          ratio = 2,
                          Pi0   = 0.2,
                          Pi1   = 0.2,
                          nCmax = 20L)
sat      <- des_two_stage(type  = "sat",
                          alpha = 0.05,
                          beta  = 0.2,
                          delta = 0.4,
                          ratio = 2,
                          Pi0   = 0.2,
                          Pi1   = 0.2,
                          nCmax = 8L)

## ----eg44_2, cache = TRUE-----------------------------------------------------
binomial$nC
barnard$nC
fisher$nC
sat$nC

## ----eg44_3, cache = TRUE-----------------------------------------------------
binomial$opchar
barnard$opchar
fisher$opchar
sat$opchar

## ----eg44_4, cache = TRUE, dpi = 100, fig.retina = 4, eval = FALSE------------
#  plots <- plot(sat, output = TRUE)
#  plots$plots

## ----eg44_5, cache = TRUE, dpi = 100, fig.retina = 4, eval = TRUE, echo = FALSE----
plots <- suppressWarnings(plot(sat, output = TRUE))
plots$plots

## ----eg45_1, cache = TRUE-----------------------------------------------------
binomial_1 <- des_two_stage(type     = "binomial",
                            alpha    = 0.05,
                            beta     = 0.2,
                            delta    = 0.4,
                            ratio    = 2,
                            Pi0      = 0.2,
                            Pi1      = 0.2,
                            efficacy = TRUE,
                            w        = c(1, 0, 0, 0, 0),
                            nCmax    = 10L)
binomial_2 <- des_two_stage(type     = "binomial",
                            alpha    = 0.05,
                            beta     = 0.2,
                            delta    = 0.4,
                            ratio    = 2,
                            Pi0      = 0.2,
                            Pi1      = 0.2,
                            efficacy = TRUE,
                            w        = c(0, 1, 0, 0, 0),
                            nCmax    = 10L)
binomial_3 <- des_two_stage(type     = "binomial",
                            alpha    = 0.05,
                            beta     = 0.2,
                            delta    = 0.4,
                            ratio    = 2,
                            Pi0      = 0.2,
                            Pi1      = 0.2,
                            efficacy = TRUE,
                            w        = c(1, 1, 0, 0, 1)/3,
                            nCmax    = 10L)

## ----eg45_2, cache = TRUE-----------------------------------------------------
binomial_1$nC
binomial_1$boundaries
binomial_2$nC
binomial_2$boundaries
binomial_3$nC
binomial_3$boundaries

## ----eg45_3, cache = TRUE-----------------------------------------------------
binomial_1$opchar
binomial_2$opchar

## ----eg52_1, cache = TRUE-----------------------------------------------------
citation("ph2rand")

