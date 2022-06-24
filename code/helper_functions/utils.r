library(stargazer)

# Same stargazer, but without std output spam
quiet_stargazer <- function(...) {
    junk <- capture.output(stargazer(...))
}
