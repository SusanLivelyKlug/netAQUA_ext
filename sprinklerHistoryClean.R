# url of netaqua history
# netaqua uses
#    var oXmlHttp = zXmlHttp.createRequest();
#    var command_line="get_history_info.cgi?";

library(XML)
library(plyr)
library(dplyr)

fileUrl <- "http://192.168.0.139/get_history_info.cgi?"
# from VALVE to ]. is one line of output, but the cgi call gives
# one giant line of all the data.

doc <- xmlTreeParse(fileUrl, useInternal=TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)

mdata = ldply(xmlToList(rootNode), data.frame)
# leaves us with three columns
# X..i.. | text | .attrs
# rename the X..i..
names(mdata) <- c("reading", "date", "attrs")

# We can skip the first line "history |"
d <- dim(mdata)
mdata2 = slice(mdata, 2:d[1])

# The rest of the data needs to be reworked.
# if text == VALVE the row can be ignored
# if text == SYS we can also ignore that row

# SLK - need to add the SYS row removal
# also check the data along the way it seems like we lost some rows :/

# intermediate data desired format is:
# date | start | complete
df <- mutate( mdata2,
        start = gsub("started", reading, reading),
        complete = gsub("completed", reading, reading ))

# for each date[x] if date[x] == NA then date[x] = date[x-1]
# recalling that d[1] is the number of rows
d <- dim(df)
d <- d[1]
for (i in 2:d[1]) {
     if ( is.na(df$date[i]) )
          df$date[i] = df$date[i-1]; # why we start at 2
}

# now if the 'reading' is NA then we don't need that row anymore
df2 <- filter(df, !is.na(reading))

# Really only need the date | start | complete columns now
keeps <- c("date", "start", "complete")
df3 <- df2[keeps]

# will be using tidyr to extract the readings into even more columns
library(tidyr)
library(lubridate)

# remove the day of the week - for some reason it's really messing me up.
df3$date <- sub("^[A-z]*\ ", "", df3$date)
### this works if you get rid of the %a at the start
df3$date <- parse_date_time(df3$date, "H:M:S p, b d Y;")

# sort by time
df4 <- arrange(df3, date)
names(df4) <- c("date.time", "start", "complete")


# df5 <- df4 %>% extract(start, new, "(.*started)\\s(.*)")
# extract isn't working because of the NA's in half the start cells

### ran by hand to clean up some chaff
# > df4 <- arrange(df3, date)
# > View(df4)
# > View(df4)
# > dim(df4)
# [1] 71  3
# > df4 <- slice(df4, 2:71)
# > dt1 <- slice(df4, 1:31)
# > dt1 <- slice(df4, 1:32)
# > dt2 <- slice(df4, 35:70)
# > df5 <- rbind(dt1, dt2)
# > View(df5)
###

new <- c("action", "zone.cycle.timer")
df5 <- mutate(df5, start=ifelse(is.na(start), "none [none]", start))
df6 <- extract(df5, start, new, "([^[]+)([^]]+)" )
df6$zone.cycle.timer <- sub("^[[]", "", df6$zone.cycle.timer)

new <- c("action2", "flow.duration")
df6 <- mutate(df6, complete=ifelse(is.na(complete), "none [none]", complete))
df7 <- extract(df6, complete, new, "([^[]+)([^]]+)" )
# Throw out all the "["
df7$flow.duration<- sub("^[[]", "", df7$flow.duration)

# move the date.time to the column complete.dtime if complete.dtime !is.na
names(df7) <- c("date.time", "start.type", "zone.cycle.timer", "complete", "flow.duration")
df7 <- mutate(df7, complete.dtime = ymd_hms(date.time))
df7 <- mutate(df7, start.dtime = ymd_hms(date.time))

# this mucks things up.
### df7$complete.dtime = ifelse( df7$complete == "none ", NA, df7$complete.dtime)
### df7$start.type <- sub("^[.]*\ .*", "\1", df7$start.type )
df7$flow.duration <- sub("none", "none; none", df7$flow.duration)
df8 <- extract(df7, flow.duration, c("flow", "duration"), "(^.*);\\s(.*)" )

# make numeric the flow column
df8$flow <- extract_numeric(df8$flow)

# make duration into just H:M:S
df8$duration <- sub("duration ", "", df8$duration)
df8$duration <- sub("none", "00:00:00", df8$duration)
df8$duration <- hms(df8$duration)

# make the start.type into just automatic or manual
df8$start.type <- sub(" watering started", "", df8$start.type)

# make the watering completed into just 'y'
df8$complete <- sub("Watering completed", "Y", df8$complete)
df8$complete <- sub("none", "N", df8$complete)

#
# fix the zone.cycle.timer
#
df8$zone.cycle.timer <- sub("none", "0; 0; 0", df8$zone.cycle.timer)
df8$zone.cycle.timer <- sub("(zone.*)", "\\1; 0; 0", df8$zone.cycle.timer)
df8$zone.cycle.timer <- sub("(cycle.*)", "zone 0; \\1", df8$zone.cycle.timer)
df8$zone.cycle.timer <- sub("(.*cycle [1-9])$", "\\1; timer 0", df8$zone.cycle.timer)
     # now the zone.cycle.timer is "z; c; t" format
into <- c("zone", "cycle", "timer")
df9 <- extract(df8, zone.cycle.timer, into, "(^.*);\\s(.*);\\s(.*)" )
df9$zone <- extract_numeric(df9$zone)
df9$cycle <- extract_numeric(df9$cycle)
df9$timer <- extract_numeric(df9$timer)

#
# make 2 new data frames, one with rows from starts and the other completes
# since they are in time order and events are sequential only they can be
# Put back together after more tidy work
#
list_df <- split(df9, df9$complete)
df_starts <- list_df[[1]]
df_compls <- list_df[[2]]
keeps <- c("start.dtime", "start.type", "zone", "cycle", "timer")
df_starts <- df_starts[keeps]
keeps <- c("complete.dtime", "flow", "duration")
df_compls <- df_compls[keeps]
tidydf <- rbind(df_starts, df_compls)
