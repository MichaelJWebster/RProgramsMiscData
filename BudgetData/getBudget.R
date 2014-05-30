require("timeSeries")
require("gdata")

#
# Read in the yearly budget excel file, and return a timeseries containing
# Expenditure, Revenue, and Expenditure - Revenue columns.
#
# file: The reserve bank historical yearly budget data spreadsheet
#
getYearlyBudgetTS <- function(file = "e01bhist.xls") {
    x <- read.xls(file)
    y <- x[,c(1, 16, 28)]
    y <- y[c(10:26,28),]
    y[,2] <- as.numeric(as.character(y[,2]))
    y[,3] <- as.numeric(as.character(y[,3]))
    y[,1] <- as.character(y[,1])
    y4 <- y[,3] - y[,2]
    colnames <- c("Revenue", "Expenditure", "Expenditure - Revenue")
    #colnames <- c("Expenditure - Revenue")
    dims <- list(y[,1], colnames)
    m <- matrix(c(y[,2], y[,3], y4), nrow=18, ncol=3)
    #m <- matrix(c(y4), nrow=18, ncol=1)
    dimnames(m) <- dims
    tsb <- ts(m, start=c(1996))
    return(tsb)
}

#
# Read in the monthly budget excel file, and return a timeseries containing
# Expenditure, Revenue, surplus, and calculated surplus columns.
#
# file: The reserve bank historical monthly budget data spreadsheet
#
getMonthlyBudgetTS <- function(file = "e01ahist.xls") {
    x <- read.xls(file, na.strings="", colClasses="character")

    year.col = 1
    total.revenues.col <- which(colnames(x) == "X.14")
    total.expenses.col <- which(colnames(x) == "X.26")
    surplus.col <- which(colnames(x) == "X.30")
    which.cols <- c(year.col, total.revenues.col, total.expenses.col, surplus.col)

    y <- x[c(10:length(x[,1])), which.cols]
    md <- make.date()
    new.col <- sapply(y[,1], md)
    new.col <- as.Date(new.col, format="%d-%b-%Y")
    y[,1] <- new.col
    for (i in 1:ncol(y)) {
        y[,i] <- as.numeric(y[,i])
    }

    y5 <- y[,2] - y[,3]
    colnames <- c("Revenue", "Expenditure", "Surplus", "My Surplus")

    #colnames <- c("Date", colnames)
    dims <- list(y[,1], colnames)
    y.final <- cbind(y, y5)
    ylist <- c(y.final[,2], y.final[,3], y.final[,4], y.final[,5])
    m <- matrix(ylist, nrow=length(y[,1]), ncol=length(colnames))
    dimnames(m) <- dims
    #tsb <- ts(m, start=c(1969, 9), frequency=12, deltat = 1/12)
    # new.col
    tsb <- timeSeries(data=m, new.col, format="%d-%b-%Y")
    return(tsb)
}

#
# Plot data from the Reserve bank's monthly budget excel data file.
#
# file: The reserve bank historical monthly budget data spreadsheet
#
plot.monthly <- function(file = "e01ahist.xls") {
    monthly.ts <- getMonthlyBudgetTS(file)
    matplot(monthly.ts, type=c("l"), col=c(1:2, "blue", "green"), lty=1, lwd=c(2,2,2,1))
    legend("topleft", c("Revenue", "Expenditure", "My Surplus", "Surplus"), fill=c(1:2, "blue", "green"))
}

#
# Plot columns selected from Expenditure, Revunue, Surplus, and calculated
# surplus, from the Reserve bank's monthly budget excel data file.
#
# file: The reserve bank historical monthly budget data spreadsheet
# cols: A vector of the column numbers we want to display.
#
plot.monthly.specific <- function(file = "e01ahist.xls", cols=1:4) {
    monthly.ts <- getMonthlyBudgetTS(file)
    monthly.ts <- monthly.ts[,cols]
    colours = c("red", "black", "blue", "green")
    thickness = c(2,2,2,1)
    leg <- c("Revenue", "Expenditure", "Surplus", "My Surplus")
    c <- colours[cols]
    t <- thickness[cols]
    l <- leg[cols]
    ts.plot(monthly.ts, type=c("l"), col=c, lty=1, lwd=t, format="%Y")
    legend("topleft", l, fill=c)
}

#
# Plot columns selected from Expenditure, Revunue, Surplus, and calculated
# surplus, from the Reserve bank's monthly budget excel data file, for a range
# of years.
#
# file: The reserve bank historical monthly budget data spreadsheet
# cols: A vector of the column numbers we want to display.
# from: The starting year we're interested in.
# to: The last year we're interested in.
#
plot.monthly.specific.years <- function(file = "e01ahist.xls", cols=1:4, from=NA, to=NA) {
    monthly.ts <- getMonthlyBudgetTS(file)
    monthly.ts <- monthly.ts[,cols]
    if (!is.na(from)) {
        monthly.ts <- getYears(monthly.ts, from, to)
    }
    colours = c("red", "black", "blue", "green")
    thickness = c(2,2,2,1)
    leg <- c("Revenue", "Expenditure", "Surplus", "My Surplus")
    c <- colours[cols]
    t <- thickness[cols]
    l <- leg[cols]
    rmean <- rollMean(monthly.ts, k=6)
    
    plot(monthly.ts, plot.type=c("single"), type=c("l"), col=c, lty=1, lwd=t, format="%Y",
         xlab="Year", ylab="$ Million")
    legend("topleft", l, fill=c)
    return(rmean)
}

#
# Plot means of columns selected from Expenditure, Revunue, Surplus, and
# calculated surplus, from the Reserve bank's monthly budget excel data file,
# for a range of years.
#
# file: The reserve bank historical monthly budget data spreadsheet
# cols: A vector of the column numbers we want to display.
# from: The starting year we're interested in.
# to: The last year we're interested in.
# mean.samples: How many months to include in our rolling average.
#
plot.monthly.specific.years.means <- function(file = "e01ahist.xls", cols=1:4, from=NA, to=NA, mean.samples=6) {
    monthly.ts <- getMonthlyBudgetTS(file)
    monthly.ts <- monthly.ts[,cols]
    if (!is.na(from)) {
        monthly.ts <- getYears(monthly.ts, from, to)
    }
    monthly.ts <- substituteNA(monthly.ts, type=c("zeros"))
    colours = c("red", "black", "blue", "green")
    thickness = c(2,2,2,1)
    leg <- c("Revenue", "Expenditure", "Surplus", "My Surplus")
    c <- colours[cols]
    t <- thickness[cols]
    l <- leg[cols]
    rmean <- rollMean(monthly.ts, k=mean.samples)
    
    plot(rmean, plot.type=c("single"), type=c("l"), col=c, lty=1, lwd=t, format="%Y",
         xlab="Year", ylab="$ Million")
    legend("topleft", l, fill=c)
    return(rmean)
}

#
# Split the date fields to get the year, convert to numeric, and then to the
# check.
#
getYears <- function(t.s, f, t) {
    #tsp <- attr(t.s, "tsp")
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

#
# Untility function.
#
make.date <- function(x="1") {
    paste.func <- function(y) {
        y1 <- paste(x, "-", y, sep="")
        return(y1)
    }
    return(paste.func)
 }
