PlotLabVal <- function (ptable, pref, pcolname, ptitle) {
  library(orgutils)

  df <- readOrg("525-lab-values.txt", table.name = ptable)
  labinfo <- readOrg("525-lab-values.txt", table.name = pref)
  row.names(labinfo) <- labinfo[, 1]; labinfo[1] <- NULL

  x <- df[, c(pcolname)]
  df$Date <- as.Date(df$Date, "%Y-%m-%d")
  y <- df$Date

  bmax <- as.numeric(labinfo[c("max"), c(pcolname)])
  bmin <- as.numeric(labinfo[c("min"), c(pcolname)])

  plot(y, x, xaxt = "n", pch=19,
       col= ifelse(x > bmax, "red", ifelse(x < bmin, "blue", "green")),
       cex=1.5,
       main=ptitle, las=2,
                                        # NAs not allowed in 'ylim'
       ylim=c(min(bmin, min(x, na.rm=TRUE)),
              max(bmax, max(x, na.rm=TRUE))),
       ylab=labinfo[c("unite"), c(pcolname)])

  abline(h=bmax, lty=2, col="red", lwd=3)
  abline(h=bmin, lty=2, col="blue", lwd=3)
  abline(h=(bmin + bmax) / 2, lty=2, col="green")
  axis(1, df$Date, format(df$Date, "%Y-%m"), cex.axis = 0.7)

  fit <- lm(x ~ y)
  abline(fit, col="gray")
}
