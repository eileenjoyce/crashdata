# UI 
library(shiny)
library(jsonlite)
library(reshape2)
library(ggplot2)
library(dplyr)
library(data.table)

motorcycle_data<-fread('cycle_flag.csv', data.table=FALSE)

df1 <- motorcycle_data[c('CRASH_YEAR','MCYCLE_DEATH_COUNT')]

df1$metric1997 <- sum(df1$MCYCLE_DEATH_COUNT[df1$CRASH_YEAR==1997])
df1$metric1998 <- sum(df1$MCYCLE_DEATH_COUNT[df1$CRASH_YEAR==1998])
df1$metric1999 <- sum(df1$MCYCLE_DEATH_COUNT[df1$CRASH_YEAR==1999])
df1$metric2000 <- sum(df1$MCYCLE_DEATH_COUNT[df1$CRASH_YEAR==2000])
df1$metric2001 <- sum(df1$MCYCLE_DEATH_COUNT[df1$CRASH_YEAR==2001])
df1$metric2002 <- sum(df1$MCYCLE_DEATH_COUNT[df1$CRASH_YEAR==2002])
df1$metric2003 <- sum(df1$MCYCLE_DEATH_COUNT[df1$CRASH_YEAR==2003])
df1$metric2004 <- sum(df1$MCYCLE_DEATH_COUNT[df1$CRASH_YEAR==2004])
df1$metric2005 <- sum(df1$MCYCLE_DEATH_COUNT[df1$CRASH_YEAR==2005])
df1$metric2006 <- sum(df1$MCYCLE_DEATH_COUNT[df1$CRASH_YEAR==2007])
df1$metric2008 <- sum(df1$MCYCLE_DEATH_COUNT[df1$CRASH_YEAR==2008])
df1$metric2009 <- sum(df1$MCYCLE_DEATH_COUNT[df1$CRASH_YEAR==2009])
df1$metric2010 <- sum(df1$MCYCLE_DEATH_COUNT[df1$CRASH_YEAR==2010])
df1$metric2011 <- sum(df1$MCYCLE_DEATH_COUNT[df1$CRASH_YEAR==2011])
df1$metric2012 <- sum(df1$MCYCLE_DEATH_COUNT[df1$CRASH_YEAR==2012])
df1$metric2013 <- sum(df1$MCYCLE_DEATH_COUNT[df1$CRASH_YEAR==2013])
df1$metric2014 <- sum(df1$MCYCLE_DEATH_COUNT[df1$CRASH_YEAR==2014])
df1$metric2015 <- sum(df1$MCYCLE_DEATH_COUNT[df1$CRASH_YEAR==2015])
df1$metric2016 <- sum(df1$MCYCLE_DEATH_COUNT[df1$CRASH_YEAR==2016])

df1$CRASH_YEAR <- NULL
df1$MCYCLE_DEATH_COUNT <- NULL
df1<-df1[1,]
df1$label = "Total motorcycle deaths"
df1 = df1[,c(20,1:19)]

df2<-rbind(df1, c('Deaths per 100,000 registered motorcycles' ,51.4, 59.7, 56.6,71.2,56.6,54.8,59.2,55.1,65.5,55.7,63.0,61.4,52.5,56.7,49.9,51.9,45.2,46.9,45.1,49.0))

helmet_unknown <- motorcycle_data[c('CRASH_YEAR','MC_DVR_HLMTON_IND','MCYCLE_DEATH_COUNT')]
helmet_unknown <- subset(helmet_unknown, MCYCLE_DEATH_COUNT>=1,drop=FALSE)
for(unique_value in unique(helmet_unknown$MC_DVR_HLMTON_IND)){
helmet_unknown[paste("MC_DVR_HLMTON_IND", unique_value, sep = ".")] <- ifelse(helmet_unknown$MC_DVR_HLMTON_IND == unique_value, 1, 0)
}
helmet_unknown$metric1997 <- sum(helmet_unknown$MC_DVR_HLMTON_IND.U[helmet_unknown$CRASH_YEAR==1997])
helmet_unknown$metric1998 <- sum(helmet_unknown$MC_DVR_HLMTON_IND.U[helmet_unknown$CRASH_YEAR==1998])
helmet_unknown$metric1999 <- sum(helmet_unknown$MC_DVR_HLMTON_IND.U[helmet_unknown$CRASH_YEAR==1999])
helmet_unknown$metric2000 <- sum(helmet_unknown$MC_DVR_HLMTON_IND.U[helmet_unknown$CRASH_YEAR==2000])
helmet_unknown$metric2001 <- sum(helmet_unknown$MC_DVR_HLMTON_IND.U[helmet_unknown$CRASH_YEAR==2001])
helmet_unknown$metric2002 <- sum(helmet_unknown$MC_DVR_HLMTON_IND.U[helmet_unknown$CRASH_YEAR==2002])
helmet_unknown$metric2003 <- sum(helmet_unknown$MC_DVR_HLMTON_IND.U[helmet_unknown$CRASH_YEAR==2003])
helmet_unknown$metric2004 <- sum(helmet_unknown$MC_DVR_HLMTON_IND.U[helmet_unknown$CRASH_YEAR==2004])
helmet_unknown$metric2005 <- sum(helmet_unknown$MC_DVR_HLMTON_IND.U[helmet_unknown$CRASH_YEAR==2005])
helmet_unknown$metric2006 <- sum(helmet_unknown$MC_DVR_HLMTON_IND.U[helmet_unknown$CRASH_YEAR==2007])
helmet_unknown$metric2008 <- sum(helmet_unknown$MC_DVR_HLMTON_IND.U[helmet_unknown$CRASH_YEAR==2008])
helmet_unknown$metric2009 <- sum(helmet_unknown$MC_DVR_HLMTON_IND.U[helmet_unknown$CRASH_YEAR==2009])
helmet_unknown$metric2010 <- sum(helmet_unknown$MC_DVR_HLMTON_IND.U[helmet_unknown$CRASH_YEAR==2010])
helmet_unknown$metric2011 <- sum(helmet_unknown$MC_DVR_HLMTON_IND.U[helmet_unknown$CRASH_YEAR==2011])
helmet_unknown$metric2012 <- sum(helmet_unknown$MC_DVR_HLMTON_IND.U[helmet_unknown$CRASH_YEAR==2012])
helmet_unknown$metric2013 <- sum(helmet_unknown$MC_DVR_HLMTON_IND.U[helmet_unknown$CRASH_YEAR==2013])
helmet_unknown$metric2014 <- sum(helmet_unknown$MC_DVR_HLMTON_IND.U[helmet_unknown$CRASH_YEAR==2014])
helmet_unknown$metric2015 <- sum(helmet_unknown$MC_DVR_HLMTON_IND.U[helmet_unknown$CRASH_YEAR==2015])
helmet_unknown$metric2016 <- sum(helmet_unknown$MC_DVR_HLMTON_IND.U[helmet_unknown$CRASH_YEAR==2016])
helmet_unknown<-helmet_unknown[1,]
helmet_unknown$label = "Unknown Helmet"
helmet_unknown <- helmet_unknown[ -c(1:7) ]
helmet_unknown = helmet_unknown[,c(20,1:19)]

helmet_yes <- motorcycle_data[c('CRASH_YEAR','MC_DVR_HLMTON_IND','MCYCLE_DEATH_COUNT')]
helmet_yes <- subset(helmet_yes, MCYCLE_DEATH_COUNT>=1)
for(unique_value in unique(helmet_yes$MC_DVR_HLMTON_IND)){
helmet_yes[paste("MC_DVR_HLMTON_IND", unique_value, sep = ".")] <- ifelse(helmet_yes$MC_DVR_HLMTON_IND == unique_value, 1, 0)
}
helmet_yes$metric1997 <- sum(helmet_yes$MC_DVR_HLMTON_IND.Y[helmet_yes$CRASH_YEAR==1997])
helmet_yes$metric1998 <- sum(helmet_yes$MC_DVR_HLMTON_IND.Y[helmet_yes$CRASH_YEAR==1998])
helmet_yes$metric1999 <- sum(helmet_yes$MC_DVR_HLMTON_IND.Y[helmet_yes$CRASH_YEAR==1999])
helmet_yes$metric2000 <- sum(helmet_yes$MC_DVR_HLMTON_IND.Y[helmet_yes$CRASH_YEAR==2000])
helmet_yes$metric2001 <- sum(helmet_yes$MC_DVR_HLMTON_IND.Y[helmet_yes$CRASH_YEAR==2001])
helmet_yes$metric2002 <- sum(helmet_yes$MC_DVR_HLMTON_IND.Y[helmet_yes$CRASH_YEAR==2002])
helmet_yes$metric2003 <- sum(helmet_yes$MC_DVR_HLMTON_IND.Y[helmet_yes$CRASH_YEAR==2003])
helmet_yes$metric2004 <- sum(helmet_yes$MC_DVR_HLMTON_IND.Y[helmet_yes$CRASH_YEAR==2004])
helmet_yes$metric2005 <- sum(helmet_yes$MC_DVR_HLMTON_IND.Y[helmet_yes$CRASH_YEAR==2005])
helmet_yes$metric2006 <- sum(helmet_yes$MC_DVR_HLMTON_IND.Y[helmet_yes$CRASH_YEAR==2007])
helmet_yes$metric2008 <- sum(helmet_yes$MC_DVR_HLMTON_IND.Y[helmet_yes$CRASH_YEAR==2008])
helmet_yes$metric2009 <- sum(helmet_yes$MC_DVR_HLMTON_IND.Y[helmet_yes$CRASH_YEAR==2009])
helmet_yes$metric2010 <- sum(helmet_yes$MC_DVR_HLMTON_IND.Y[helmet_yes$CRASH_YEAR==2010])
helmet_yes$metric2011 <- sum(helmet_yes$MC_DVR_HLMTON_IND.Y[helmet_yes$CRASH_YEAR==2011])
helmet_yes$metric2012 <- sum(helmet_yes$MC_DVR_HLMTON_IND.Y[helmet_yes$CRASH_YEAR==2012])
helmet_yes$metric2013 <- sum(helmet_yes$MC_DVR_HLMTON_IND.Y[helmet_yes$CRASH_YEAR==2013])
helmet_yes$metric2014 <- sum(helmet_yes$MC_DVR_HLMTON_IND.Y[helmet_yes$CRASH_YEAR==2014])
helmet_yes$metric2015 <- sum(helmet_yes$MC_DVR_HLMTON_IND.Y[helmet_yes$CRASH_YEAR==2015])
helmet_yes$metric2016 <- sum(helmet_yes$MC_DVR_HLMTON_IND.Y[helmet_yes$CRASH_YEAR==2016])
helmet_yes<-helmet_yes[1,]
helmet_yes$label = "Helmet"
helmet_yes <- helmet_yes[ -c(1:7) ]
helmet_yes = helmet_yes[,c(20,1:19)]

helmet_no <- motorcycle_data[c('CRASH_YEAR','MC_DVR_HLMTON_IND','MCYCLE_DEATH_COUNT')]
helmet_no <- subset(helmet_no, MCYCLE_DEATH_COUNT>=1)
for(unique_value in unique(helmet_no$MC_DVR_HLMTON_IND)){
helmet_no[paste("MC_DVR_HLMTON_IND", unique_value, sep = ".")] <- ifelse(helmet_no$MC_DVR_HLMTON_IND == unique_value, 1, 0)
}
helmet_no$metric1997 <- sum(helmet_no$MC_DVR_HLMTON_IND.N[helmet_no$CRASH_YEAR==1997])
helmet_no$metric1998 <- sum(helmet_no$MC_DVR_HLMTON_IND.N[helmet_no$CRASH_YEAR==1998])
helmet_no$metric1999 <- sum(helmet_no$MC_DVR_HLMTON_IND.N[helmet_no$CRASH_YEAR==1999])
helmet_no$metric2000 <- sum(helmet_no$MC_DVR_HLMTON_IND.N[helmet_no$CRASH_YEAR==2000])
helmet_no$metric2001 <- sum(helmet_no$MC_DVR_HLMTON_IND.N[helmet_no$CRASH_YEAR==2001])
helmet_no$metric2002 <- sum(helmet_no$MC_DVR_HLMTON_IND.N[helmet_no$CRASH_YEAR==2002])
helmet_no$metric2003 <- sum(helmet_no$MC_DVR_HLMTON_IND.N[helmet_no$CRASH_YEAR==2003])
helmet_no$metric2004 <- sum(helmet_no$MC_DVR_HLMTON_IND.N[helmet_no$CRASH_YEAR==2004])
helmet_no$metric2005 <- sum(helmet_no$MC_DVR_HLMTON_IND.N[helmet_no$CRASH_YEAR==2005])
helmet_no$metric2006 <- sum(helmet_no$MC_DVR_HLMTON_IND.N[helmet_no$CRASH_YEAR==2007])
helmet_no$metric2008 <- sum(helmet_no$MC_DVR_HLMTON_IND.N[helmet_no$CRASH_YEAR==2008])
helmet_no$metric2009 <- sum(helmet_no$MC_DVR_HLMTON_IND.N[helmet_no$CRASH_YEAR==2009])
helmet_no$metric2010 <- sum(helmet_no$MC_DVR_HLMTON_IND.N[helmet_no$CRASH_YEAR==2010])
helmet_no$metric2011 <- sum(helmet_no$MC_DVR_HLMTON_IND.N[helmet_no$CRASH_YEAR==2011])
helmet_no$metric2012 <- sum(helmet_no$MC_DVR_HLMTON_IND.N[helmet_no$CRASH_YEAR==2012])
helmet_no$metric2013 <- sum(helmet_no$MC_DVR_HLMTON_IND.N[helmet_no$CRASH_YEAR==2013])
helmet_no$metric2014 <- sum(helmet_no$MC_DVR_HLMTON_IND.N[helmet_no$CRASH_YEAR==2014])
helmet_no$metric2015 <- sum(helmet_no$MC_DVR_HLMTON_IND.N[helmet_no$CRASH_YEAR==2015])
helmet_no$metric2016 <- sum(helmet_no$MC_DVR_HLMTON_IND.N[helmet_no$CRASH_YEAR==2016])
helmet_no<-helmet_no[1,]
helmet_no$label = "No Helmet"
helmet_no <- helmet_no[ -c(1:7) ]
helmet_no = helmet_no[,c(20,1:19)]

helmets<-rbind(helmet_unknown,helmet_yes,helmet_no)
df3<-rbind(df2, helmets)

myf1 <- function(x) { sprintf('["%s",%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s]', 
                              x[1], x[2],x[3],x[4],x[5],x[6],x[7],x[8],x[9],x[10],x[11],x[12],x[13],x[13],x[14]
                             ,x[15],x[16],x[17],x[18],x[19],x[20])}
data <- apply(df3,1,myf1)
msg_str1 <- paste0('[',toString(data),']')


ui <- fluidPage(
        titlePanel("Fatalities and Registered Motorcycles"),

            mainPanel(
                       # make sure your billboard.min.css and billboard.min.js files are in the www directory
                        tags$script(src="//d3js.org/d3.v4.min.js"),
                        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "billboard.min.css")),
                        tags$script(src="billboard.min.js"),
 
                        # the id here is the same as the id in the first line of the js code
                        tags$div(id="CombinationChart"),

                        # load your javascript code
                        tags$script(src="my_script.js")
                    )
    )

server <- function(input, output, session) {
    
    #sends your msg_str you created above to the js code
    session$sendCustomMessage(type="get_data_from_shiny", msg_str1) 
}

shinyApp(ui=ui, server=server)