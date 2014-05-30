require("timeSeries")
require("gdata")

#
# Plot the temperatures obtained from an Australian Bureau of meteorology station
# data file.
#
plot.temps <- function(file="IDCJAC0011_066062_1800_Data.csv",
                       title="Observatory Hill Temperature Measurements") {
    temps.ts <- get.temp.ts(file)
    #browser()
    ylab <- attr(temps.ts, "units")
    ylab <- gsub("\\.+", " ", ylab)
    plot(temps.ts, plot.type=c("single"), type=c("l"), col=c("red"), lwd=1, format="%Y",
         xlab="Year", ylab=ylab, main=title)
    return(temps.ts)
}

#
# Plot a rolling mean of the temperatures obtained from an Australian Bureau of
# meteorology station data file.
#
# file:  The name of the file containing the observations.
# k: The number of days over which to compute a rolling mean to be plotted.
# title: The title for the plot.
#
plot.temps.means <- function(file="IDCJAC0011_066062_1800_Data.csv", k=365,
                             title="Observatory Hill Temperature Measurements") {
    temps.ts <- get.temp.ts(file)
    ylab <- attr(temps.ts, "units")
    ylab <- gsub("\\.+", " ", ylab)
    #browser()
    #temps.ts <- interpNA(temps.ts, type=c("linear"))
    temps.ts <- na.omit(temps.ts, method="r")
    rmean <- rollMean(temps.ts, k=k)
    plot(rmean, plot.type=c("single"), type=c("l"), col=c("red"), lwd=1, format="%Y",
         xlab="Year", ylab=ylab, main=title)
    return(temps.ts)
}

#
# Plot a rolling mean of the temperatures obtained from an Australian Bureau of
# meteorology station data file for a given rangeof years.
#
# file:  The name of the file containing the observations.
# k: The number of days over which to compute a rolling mean to be plotted.
# title: The title for the plot.
# year.start: Starting year for the plot.
# year.end: End year for the plot.
#
plot.temps.means.range <- function(file="IDCJAC0011_066062_1800_Data.csv", k=365,
                             title="Observatory Hill Temperature Measurements",
                             year.start=1859, year.end=NA) {
    temps.ts <- get.temp.ts(file)
    ylab <- attr(temps.ts, "units")
    ylab <- gsub("\\.+", " ", ylab)    
    temps.ts <- interpNA(temps.ts, type=c("linear"))
    rmean <- rollMean(temps.ts, k=k, align=c("right"))
    if (!is.na(year.start)) {
        rmean <- getYears(rmean, year.start, year.end)
    }
    plot(rmean, plot.type=c("single"), type=c("l"), col=c("red"), lwd=1, format="%Y",
         xlab="Year", ylab=ylab, main=title)
    return(temps.ts)
}

#
# Return the station data read from the supplied file as a time series, using
# the R timeSeries package.
#
# file:  The name of the file containing the observations.
#
get.temp.ts <- function(file="IDCJAC0011_066062_1800_Data.csv") {
    temp.df <- read.csv(file, header=TRUE,
                        na.strings="", colClasses = "character")
    date.col <- apply(temp.df, 1, function(y) { paste(y[3], "/", y[4], "/", y[5], sep="")})
    date.col <- as.Date(date.col, format="%Y/%m/%d")
    temp.df.new <- subset(temp.df, select=c(6))
    temp.df.new[,1] <- as.numeric(temp.df.new[,1])
    temp.ts <- timeSeries(temp.df.new, date.col, format="%Y-%m-%d")
    return(temp.ts)
}

#
# Plot a histogram of the number of days where temperature in the supplied data
# set is above a certain threshold.
#
# file:  The name of the file containing the observations.
# title: The title for the plot.
# threshold: The threshold above which we are intersted in counting towards our
#             totals.
# year.start: Starting year for the plot.
# year.end: End year for the plot.
#
plot.max.greater <- function(file="IDCJAC0011_066062_1800_Data.csv",
                             title="Observatory Hill Max Temperature Measurements",
                             threshold=35,
                             year.start=1859, year.end=NA) {
    temps.ts <- get.temp.ts(file)
    temps.ts <- na.omit(temps.ts, method=c("r"))
    title.2 <- "Number of days greater than "
    title.2 <- paste(title.2, as.character(threshold), " degrees C")
     
    ylab <- title.2
    if (!is.na(year.start)) {
        temps.ts <- getYears(temps.ts, year.start, year.end)
    }
    
    histo <- get.above.thresh(temps.ts, threshold)
    plot(histo, plot.type=c("single"), type=c("h"), col=c("red"), lwd=3, format="%Y",
         xlab="Year", ylab=ylab, main=title)
    return(temps.ts)
}

#
# Return a timeseries containing the number of days in each year that have
# T >= t, computed from the supplied time series.
#
# time.s: The time series containing temperature data.
# t: The threshold we use to determine which days will be counted.
#
get.above.thresh <- function(time.s, t=35) {
    first.d <- rownames(time.s)[1]
    last.d <- rownames(time.s)[nrow(time.s)]
    first.y <- as.numeric(substr(first.d, 1, 4))
    last.y <- as.numeric(substr(last.d, 1, 4))
    years <- first.y:(last.y - 1)

    # Get the positions vector
    pos <- attr(time.s, "positions")

    year.totals <- c()
    for (i in years) {
        i.start <- as.character(i)
        start.year <- paste(i.start, "-01-01", sep="")
        start.year <- as.numeric(as.POSIXct(as.Date(start.year, format="%Y-%m-%d"), tz="GMT"))
        i.end <- as.character(i+1)
        end.year <-  paste(i.end, "-01-01", sep="")
        end.year <- as.numeric(as.POSIXct(as.Date(end.year, format="%Y-%m-%d"), tz="GMT"))

        # FIXME: This is different from the get years version that uses &.
        #browser()
        tsp.index <- which(pos >= start.year & pos <= end.year)
        total <- 0
        for (j in tsp.index) {
            if (time.s[j,1] >= t) {
                total = total + 1
            }
        }
        year.totals <- c(year.totals, total)
    }

    dates <- as.Date(as.character(years), format="%Y")
            
    above.thresh.ts <- timeSeries(year.totals, dates)
    return(above.thresh.ts)
}
#
# Return a subset of the supplied timeseries containing data between the
# supplied f and t parameters.
#
# t.s: The time series we are extracting certain years from.
# f: The year "from" which we want to start extracting our subset.
# t: The year "to" which we want to extract data.
#
getYears <- function(t.s, f, t) {
    if (is.na(f)) {
        stop("Invalid starting date supplied.")
    }
    f <- as.character(f)
    f <- as.numeric(as.POSIXct(as.Date(f, format="%Y"), tz="GMT"))
    pos <- attr(t.s, "positions")
    if (is.na(t)) {
        t <- pos[length(pos)]
    }
    else {
        t <- as.character(t)
        t <- as.numeric(as.POSIXct(as.Date(t, format="%Y"), tz="GMT"))
    }

    tsp.index <- which(pos >= f & pos <= t)
    tsp.seq <- pos[tsp.index]
    f <- tsp.seq[1]
    t <- tsp.seq[length(tsp.seq)]
    return(window(t.s, f, t))
}

