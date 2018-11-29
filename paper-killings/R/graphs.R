################
# EBA Graphs ###
################

# Main models
hist(m1, variables = c("logrgdppc", "polity2", "polity2sq", "uamkyr",
                       "UCDPcivilwarongoing",
                       "UCDPcivilwarstart", "COWcivilwarongoing",
                       "COWcivilwarstart", "ethnowarongoing", "ethnowarstart",
                       "assdummy", "totaltrade", "tradedependence", "milper",
                       "milex","pop", "totalbeaths", "guerrilladummy", "regtrans",
                       "riotdummy", "territoryaims", "militias", "physint",
                       "percentpopurban", "coupdummy", "postcoldwar",
                       "lmtnest", "realgdp", "discrim", "exclpop", "discpop",
                       "elf", "polrqnew", "egippolrqnew", "poltrqnew",
                       "egiptpolrqnew"),
     main = c("Log GDP capita", "Polity IV", "Polity IV^2", "Years last genocide",
              "UCDP ongoing", "UCDP onset", "COW ongoing", "COW onset", 
              "Ethnic ongoing", "Ethnic onset", "Assassination", "Total trade", 
              "Trade dependence", "Military personnel", "Military expenditure", "Population", 
              "Total deaths", "Guerrilla", "Regime transition", "Riots",
              "Territory Aims", "Militias", "Physical integrity", "% Urban",
              "Coups", "Post-Cold War", "Mountainous terrain", "Real GDP",
              "Discrimination", "Excl pop", "Discrim pop", "ELF", "Groups/Eth relevant", 
              "Group/Tot pop", "Inc groups/Eth relevant", "Inc groups/Tot pop"),
     density.col = "black", mu.col = "red3")

# Round
m1$coefficients$mean$beta2 <- round(as.numeric(m1$coefficients$mean$beta),4)
m1$coefficients$mean$se2 <- round(as.numeric(m1$coefficients$mean$se),4)
m1$coefficients$mean

## Models including only mass killings during civil wars
hist(m1, variables = c("logrgdppc", "polity2", "polity2sq", "uamkyr",
                       "assdummy", "totaltrade", "tradedependence", "milper",
                       "milex","pop", "totalbeaths", "guerrilladummy", "regtrans",
                       "riotdummy", "territoryaims", "militias", "physint",
                       "percentpopurban", "coupdummy", "postcoldwar",
                       "lmtnest", "realgdp", "discrim", "exclpop", "discpop",
                       "elf", "polrqnew", "egippolrqnew", "poltrqnew",
                       "egiptpolrqnew"),
     main = c("Log GDP capita", "Polity IV", "Polity IV^2", "Years last genocide",
              "Assassination", "Total trade", 
              "Trade dependence", "Military personnel", "Military expenditure", "Population", 
              "Total deaths", "Guerrilla", "Regime transition", "Riots",
              "Territory Aims", "Militias", "Physical integrity", "% Urban",
              "Coups", "Post-Cold War", "Mountainous terrain", "Real GDP",
              "Discrimination", "Excl pop", "Discrim pop", "ELF", "Groups/Eth relevant", 
              "Groups/Tot pop", "Inc groups/Eth relevant", "Inc groups/Tot pop"),
     density.col = "black", mu.col = "red3")

######################
### Random forests ###
######################

# Main model
a <- h2o.loadModel("gridrf01c_model_58")
print(va <- a %>% h2o.varimp() %>% as.data.frame() %>% head(., 6))

df2a <- as.h2o(df2)

df2a$MKstart <- as.factor(df2a$MKstart)  #encode the binary repsonse as a factor
h2o.levels(df2a$MKstart)

# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = df2a, 
                         ratios = c(0.7, 0.15),  # 70%, 15%, 15%
                         seed = 42)  # reproducibility


train <- h2o.assign(splits[[1]], "train.hex")   
valid <- h2o.assign(splits[[2]], "valid.hex") 
test <- h2o.assign(splits[[3]], "test.hex")

y <- "MKstart"
x <- setdiff(names(df2), c(y, "ccode", "year", "rgdppc",
                           "mksyr2", "mksyr3", "sf", "country",
                           "elf2", "polity2sq"))  

par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(2,7.5,1,1))
barplot(va$scaled_importance[6:1],
        horiz = TRUE, las = 1, cex.names=0.9,
        names.arg = c("Population", 
                      "Military personnel",
                      "Trade dependence", 
                      "Log GDP per capita",
                      "% Urban pop.",
                      "Years mass killing"),
        main = "")


# Partial dependence plots
par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(2,7.5,1,1))
barplot(va$scaled_importance[6:1],
        horiz = TRUE, las = 1, cex.names=0.9,
        names.arg = c("Polity IV", 
                      "Military personnel",
                      "Ethnic polarisation", 
                      "% Urban pop.",
                      "Years mass killing",
                      "Log GDP per capita"),
        main = "")


logrgdppc <- h2o.partialPlot(object = a, data = train, cols = c("logrgdppc"))
p3 <- qplot(logrgdppc$logrgdppc, logrgdppc$mean_response) + geom_line() + theme_classic() + ylim(0, 0.05) +
        xlab("Log GDP per capita") + ylab("Mean response")

mksyr <- h2o.partialPlot(object = a, data = train, cols = c("mksyr"))
p1 <- qplot(mksyr$mksyr, mksyr$mean_response) + geom_line() + theme_classic() + ylim(0, 0.05) +
        xlab("Years since mass killing") +  ylab("Mean response")

percentpopurban <- h2o.partialPlot(object = a, data = train, cols = c("percentpopurban"))
p2 <- qplot(percentpopurban$percentpopurban, percentpopurban$mean_response) + geom_line() +
        theme_classic() + ylim(0, 0.05) + xlab("% Urban") + ylab("Mean response")

tradedependence <- h2o.partialPlot(object = a, data = train, cols = c("tradedependence"))
p4 <- qplot(tradedependence$tradedependence, tradedependence$mean_response) + geom_line() +
        theme_classic() + ylim(0, 0.05) + xlab("Trade dependence") + ylab("Mean response")

milper <- h2o.partialPlot(object = a, data = train, cols = c("milper"))
p5 <- qplot(milper$milper, milper$mean_response) + geom_line() + theme_classic() + ylim(0, 0.05) +
        xlab("Military personnel") + ylab("Mean response")

pop <- h2o.partialPlot(object = a, data = train, cols = c("pop"))
p6 <- qplot(pop$pop, pop$mean_response) + geom_line() + theme_classic() + ylim(0, 0.05) +
        xlab("Population") + ylab("Mean response")


# Multiplot function: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}

multiplot(p1, p4, p2, p5, p3, p6, cols = 3)

#######################################
### Mass killings during civil wars ###
#######################################

# UCDP == 1
a <- h2o.loadModel("gridrf02_model_34")
print(va <- a %>% h2o.varimp() %>% as.data.frame() %>% head(., 6)) 

par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(2,7.5,1,1))
barplot(va$scaled_importance[6:1],
        horiz = TRUE, las = 1, cex.names=0.9,
        names.arg = c("Total battle deaths", 
                      "Log GDP per capita",
                       "Trade dependence",
                      "Military personnel",
                      "% Urban pop.",
                      "Years mass killing"),
        main = "")

df.ucdpa <- as.h2o(df.ucdp)

df.ucdpa$MKstart <- as.factor(df.ucdpa$MKstart)  #encode the binary repsonse as a factor
h2o.levels(df.ucdpa$MKstart)

# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = df.ucdpa, 
                         ratios = c(0.7, 0.15),  # 70%, 15%, 15%
                         seed = 42)  # reproducibility


train <- h2o.assign(splits[[1]], "train.hex")   
valid <- h2o.assign(splits[[2]], "valid.hex") 
test <- h2o.assign(splits[[3]], "test.hex")

y <- "MKstart"
x <- setdiff(names(df.ucdp), c(y, "ccode", "year", "rgdppc",
                               "mksyr2", "mksyr3", "sf", "country",
                               "elf2", "polity2sq")) 

mksyr <- h2o.partialPlot(object = a, data = train, cols = c("mksyr"))
p1 <- qplot(mksyr$mksyr, mksyr$mean_response) + geom_line() + theme_classic() + 
        ylim(0, 0.1) + xlab("Years since mass killing") +  ylab("Mean response")

percentpopurban <- h2o.partialPlot(object = a, data = train, cols = c("percentpopurban"))
p2 <- qplot(percentpopurban$percentpopurban, percentpopurban$mean_response) + geom_line() +
        ylim(0, 0.1) + theme_classic() + xlab("% Urban") + ylab("Mean response")

milper <- h2o.partialPlot(object = a, data = train, cols = c("milper"))
p3 <- qplot(milper$milper, milper$mean_response) + geom_line() + theme_classic() +
        ylim(0, 0.25) + xlab("Military personnel") + ylab("Mean response")

tradedependence <- h2o.partialPlot(object = a, data = train, cols = c("tradedependence"))
p4 <- qplot(tradedependence$tradedependence, tradedependence$mean_response) + geom_line() + theme_classic() +
        ylim(0, 0.1) + xlab("Trade dependence") + ylab("Mean response")

logrgdppc <- h2o.partialPlot(object = a, data = train, cols = c("logrgdppc"))
p5 <- qplot(logrgdppc$logrgdppc, logrgdppc$mean_response) + geom_line() + theme_classic() +
        ylim(0, 0.1) + xlab("Log GDP per capita") + ylab("Mean response")

totalbeaths <- h2o.partialPlot(object = a, data = train, cols = c("totalbeaths"))
p6 <- qplot(totalbeaths$totalbeaths, totalbeaths$mean_response) + geom_line() + theme_classic() +
        ylim(0, 0.35) + xlab("Total battle deaths") + ylab("Mean response")

multiplot(p1, p4, p2, p5, p3, p6, cols = 3)


# COW == 1
a <- h2o.loadModel("gridrf03_model_3")
print(va <- a %>% h2o.varimp() %>% as.data.frame() %>% head(., 6)) 

par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(2,7.5,1,1))
barplot(va$scaled_importance[6:1],
        horiz = TRUE, las = 1, cex.names=0.9,
        names.arg = c("Total battle deaths", 
                      "Excluded population",
                      "Yrs since mass killing",
                      "Log GDP per capita",
                      "Ethnic polarisation",
                      "Physical integrity"),
        main = "")

df.cowa <- as.h2o(df.cow)

df.cowa$MKstart <- as.factor(df.cowa$MKstart)  #encode the binary repsonse as a factor
h2o.levels(df.cowa$MKstart)

# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = df.cowa, 
                         ratios = c(0.7, 0.15),  # 70%, 15%, 15%
                         seed = 42)  # reproducibility


train <- h2o.assign(splits[[1]], "train.hex")   
valid <- h2o.assign(splits[[2]], "valid.hex") 
test <- h2o.assign(splits[[3]], "test.hex")

y <- "MKstart"
x <- setdiff(names(df.ucdp), c(y, "ccode", "year", "rgdppc",
                               "mksyr2", "mksyr3", "sf", "country",
                               "elf2", "polity2sq")) 

physint <- h2o.partialPlot(object = a, data = train, cols = c("physint"))
p1 <- qplot(physint$physint, physint$mean_response) + geom_line() + theme_classic() + 
        xlab("Physical integrity") +  ylab("Mean response")

egiptpolrqnew <- h2o.partialPlot(object = a, data = train, cols = c("egiptpolrqnew"))
p2 <- qplot(egiptpolrqnew$egiptpolrqnew, egiptpolrqnew$mean_response) + geom_line() +
        theme_classic() + xlab("Ethnic polarisation") + ylab("Mean response")

logrgdppc <- h2o.partialPlot(object = a, data = train, cols = c("logrgdppc"))
p3 <- qplot(logrgdppc$logrgdppc, logrgdppc$mean_response) + geom_line() + theme_classic() +
        ylim(0, 0.1) + xlab("Log GDP per capita") + ylab("Mean response")

mksyr <- h2o.partialPlot(object = a, data = train, cols = c("mksyr"))
p4 <- qplot(mksyr$mksyr, mksyr$mean_response) + geom_line() + theme_classic() + 
        ylim(0, 0.1) + xlab("Years since mass killing") +  ylab("Mean response")

exclpop <- h2o.partialPlot(object = a, data = train, cols = c("exclpop"))
p5 <- qplot(exclpop$exclpop, exclpop$mean_response) + geom_line() + theme_classic() +
        ylim(0, 0.1) + xlab("Excluded population") + ylab("Mean response")

totalbeaths <- h2o.partialPlot(object = a, data = train, cols = c("totalbeaths"))
p6 <- qplot(totalbeaths$totalbeaths, totalbeaths$mean_response) + geom_line() + theme_classic() +
        ylim(0, 0.1) + xlab("Total battle deaths") + ylab("Mean response")

multiplot(p1, p4, p2, p5, p3, p6, cols = 3)

# Ethnic conflict == 1
a <- h2o.loadModel("gridrf04_model_14")
print(va <- a %>% h2o.varimp() %>% as.data.frame() %>% head(., 6)) 

par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(2,7.5,1,1))
barplot(va$scaled_importance[6:1],
        horiz = TRUE, las = 1, cex.names=0.9,
        names.arg = c("Military expenditure",
                      "Total trade", 
                      "% Urban",
                      "Trade dependence",
                      "Military personnel",
                      "CINC"),
        main = "")

df.etha <- as.h2o(df.eth)

df.etha$MKstart <- as.factor(df.etha$MKstart)  #encode the binary repsonse as a factor
h2o.levels(df.etha$MKstart)

# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = df.etha, 
                         ratios = c(0.7, 0.15),  # 70%, 15%, 15%
                         seed = 42)  # reproducibility


train <- h2o.assign(splits[[1]], "train.hex")   
valid <- h2o.assign(splits[[2]], "valid.hex") 
test <- h2o.assign(splits[[3]], "test.hex")

y <- "MKstart"
x <- setdiff(names(df.ucdp), c(y, "ccode", "year", "rgdppc",
                               "mksyr2", "mksyr3", "sf", "country",
                               "elf2", "polity2sq")) 

cinc <- h2o.partialPlot(object = a, data = train, cols = c("cinc"))
p1 <- qplot(cinc$cinc, cinc$mean_response) + geom_line() + theme_classic() + 
        xlab("CINC") +  ylab("Mean response")

milper <- h2o.partialPlot(object = a, data = train, cols = c("milper"))
p2 <- qplot(milper$milper, milper$mean_response) + geom_line() +
        theme_classic() + xlab("Military personnel") + ylab("Mean response")

tradedependence <- h2o.partialPlot(object = a, data = train, cols = c("tradedependence"))
p3 <- qplot(tradedependence$tradedependence, tradedependence$mean_response) + geom_line() + theme_classic() +
        xlab("Trade dependence") + ylab("Mean response")

percentpopurban <- h2o.partialPlot(object = a, data = train, cols = c("percentpopurban"))
p4 <- qplot(percentpopurban$percentpopurban, percentpopurban$mean_response) + geom_line() +
       theme_classic() + xlab("% Urban") + ylab("Mean response")

totaltrade <- h2o.partialPlot(object = a, data = train, cols = c("totaltrade"))
p5 <- qplot(totaltrade$totaltrade, totaltrade$mean_response) + geom_line() +
        theme_classic() + xlab("Total trade") + ylab("Mean response")

milex <- h2o.partialPlot(object = a, data = train, cols = c("milex"))
p6 <- qplot(milex$milex, milex$mean_response) + geom_line() +
        theme_classic() + xlab("Military expenditure") + ylab("Mean response")

multiplot(p1, p4, p2, p5, p3, p6, cols = 3)

####################################################
### Same models with Genocide/Politicide (Harff) ###
####################################################

# Main model
a <- h2o.loadModel("gridrf05_model_27")
print(va <- a %>% h2o.varimp() %>% as.data.frame() %>% head(., 6)) 

par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(2,7.5,1,1))
barplot(va$scaled_importance[6:1],
        horiz = TRUE, las = 1, cex.names=0.9,
        names.arg = c("Real GDP", 
                      "Military expenditure",
                      "Total trade", 
                      "Military personnel",
                      "CINC", 
                      "% Urban"),
        main = "")

df5a <- as.h2o(df5)

df5a$uamkstart <- as.factor(df5a$uamkstart)  #encode the binary repsonse as a factor
h2o.levels(df5a$uamkstart)

# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = df5a, 
                         ratios = c(0.7, 0.15),  # 70%, 15%, 15%
                         seed = 42)  # reproducibility


train <- h2o.assign(splits[[1]], "train.hex")   
valid <- h2o.assign(splits[[2]], "valid.hex") 
test <- h2o.assign(splits[[3]], "test.hex")

y <- "uamkstart"
x <- setdiff(names(df5), c(y, "ccode", "year", "rgdppc",
                           "uamkyr2", "uamkyr3", "sf", "country",
                           "elf2", "polity2sq")) 


cinc <- h2o.partialPlot(object = a, data = train, cols = c("cinc"))
p2 <- qplot(cinc$cinc, cinc$mean_response) + geom_line() + theme_classic() + 
        xlab("CINC") +  ylab("Mean response")

milper <- h2o.partialPlot(object = a, data = train, cols = c("milper"))
p3 <- qplot(milper$milper, milper$mean_response) + geom_line() +
        theme_classic() + xlab("Military personnel") + ylab("Mean response")

totaltrade <- h2o.partialPlot(object = a, data = train, cols = c("totaltrade"))
p4 <- qplot(totaltrade$totaltrade, totaltrade$mean_response) + geom_line() + theme_classic() +
        xlab("Total trade") + ylab("Mean response")

percentpopurban <- h2o.partialPlot(object = a, data = train, cols = c("percentpopurban"))
p1 <- qplot(percentpopurban$percentpopurban, percentpopurban$mean_response) + geom_line() +
        theme_classic() + xlab("% Urban") + ylab("Mean response")

realgdp <- h2o.partialPlot(object = a, data = train, cols = c("realgdp"))
p6 <- qplot(realgdp$realgdp, realgdp$mean_response) + geom_line() +
        theme_classic() + xlab("Real GDP") + ylab("Mean response")

milex <- h2o.partialPlot(object = a, data = train, cols = c("milex"))
p5 <- qplot(milex$milex, milex$mean_response) + geom_line() +
        theme_classic() + xlab("Military expenditure") + ylab("Mean response")

multiplot(p1, p4, p2, p5, p3, p6, cols = 3)

# UCDP == 1
df.ucdp2 <- df5 %>% filter(UCDPcivilwarongoing == 1)
df.ucdp2a <- as.h2o(df.ucdp2)

df.ucdp2a$uamkstart <- as.factor(df.ucdp2a$uamkstart)  #encode the binary repsonse as a factor
h2o.levels(df.ucdp2a$uamkstart)

# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = df.ucdp2a, 
                         ratios = c(0.7, 0.15),  # 70%, 15%, 15%
                         seed = 42)  # reproducibility


train <- h2o.assign(splits[[1]], "train.hex")   
valid <- h2o.assign(splits[[2]], "valid.hex") 
test <- h2o.assign(splits[[3]], "test.hex")

y <- "uamkstart"
x <- setdiff(names(df.ucdp2), c(y, "ccode", "year", "rgdppc",
                                "uamkyr2", "uamkyr3", "sf", "country",
                                "elf2", "polity2sq")) 

a <- h2o.loadModel("gridrf06_model_43")
print(va <- a %>% h2o.varimp() %>% as.data.frame() %>% head(., 6)) 

par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(2,7.5,1,1))
barplot(va$scaled_importance[6:1],
        horiz = TRUE, las = 1, cex.names=0.9,
        names.arg = c("Excluded population",
                      "% Urban pop.", 
                      "Trade dependence",
                      "Log GDP per capita",
                      "Years since genocide", 
                      "Military personnel"),
        main = "")

uamkyr <- h2o.partialPlot(object = a, data = train, cols = c("uamkyr"))
p2 <- qplot(uamkyr$uamkyr, uamkyr$mean_response) + geom_line() + theme_classic() + 
        xlab("Years since genocide") +  ylab("Mean response")

milper <- h2o.partialPlot(object = a, data = train, cols = c("milper"))
p1 <- qplot(milper$milper, milper$mean_response) + geom_line() +
        theme_classic() + xlab("Military personnel") + ylab("Mean response")

logrgdppc <- h2o.partialPlot(object = a, data = train, cols = c("logrgdppc"))
p3 <- qplot(logrgdppc$logrgdppc, logrgdppc$mean_response) + geom_line() + theme_classic() +
        xlab("Log GDP per capita") + ylab("Mean response")

percentpopurban <- h2o.partialPlot(object = a, data = train, cols = c("percentpopurban"))
p5 <- qplot(percentpopurban$percentpopurban, percentpopurban$mean_response) + geom_line() +
        theme_classic() + xlab("% Urban") + ylab("Mean response")

tradedependence <- h2o.partialPlot(object = a, data = train, cols = c("tradedependence"))
p4 <- qplot(tradedependence$tradedependence, tradedependence$mean_response) + geom_line() +
        theme_classic() + xlab("Trade dependence") + ylab("Mean response")

exclpop <- h2o.partialPlot(object = a, data = train, cols = c("exclpop"))
p6 <- qplot(exclpop$exclpop, exclpop$mean_response) + geom_line() +
        theme_classic() + xlab("Excluded population") + ylab("Mean response")

multiplot(p1, p4, p2, p5, p3, p6, cols = 3)

# COW == 1
df.cow2 <- df5 %>% filter(COWcivilwarongoing == 1)

df.cow2a$uamkstart <- as.factor(df.cow2a$uamkstart)  #encode the binary repsonse as a factor
h2o.levels(df.cow2a$uamkstart)

# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = df.cow2a, 
                         ratios = c(0.7, 0.15),  # 70%, 15%, 15%
                         seed = 42)  # reproducibility


train <- h2o.assign(splits[[1]], "train.hex")   
valid <- h2o.assign(splits[[2]], "valid.hex") 
test <- h2o.assign(splits[[3]], "test.hex")

y <- "uamkstart"
x <- setdiff(names(df.cow2), c(y, "ccode", "year", "rgdppc",
                               "uamkyr2", "uamkyr3", "sf", "country",
                               "elf2", "polity2sq")) 

a <- h2o.loadModel("gridrf07_model_27")
print(va <- a %>% h2o.varimp() %>% as.data.frame() %>% head(., 6)) 

par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(2,7.5,1,1))
barplot(va$scaled_importance[6:1],
        horiz = TRUE, las = 1, cex.names=0.9,
        names.arg = c("Military expenditure",
                      "% Urban pop.", 
                      "Total trade",
                      "Years since genocide", 
                      "Military personnel", 
                      "Trade dependence"),
        main = "")

uamkyr <- h2o.partialPlot(object = a, data = train, cols = c("uamkyr"))
p3 <- qplot(uamkyr$uamkyr, uamkyr$mean_response) + geom_line() + theme_classic() + 
        xlab("Years since genocide") +  ylab("Mean response")

milper <- h2o.partialPlot(object = a, data = train, cols = c("milper"))
p2 <- qplot(milper$milper, milper$mean_response) + geom_line() +
        theme_classic() + xlab("Military personnel") + ylab("Mean response")

totaltrade <- h2o.partialPlot(object = a, data = train, cols = c("totaltrade"))
p4 <- qplot(totaltrade$totaltrade, totaltrade$mean_response) + geom_line() + theme_classic() +
        xlab("Total trade") + ylab("Mean response")

percentpopurban <- h2o.partialPlot(object = a, data = train, cols = c("percentpopurban"))
p5 <- qplot(percentpopurban$percentpopurban, percentpopurban$mean_response) + geom_line() +
        theme_classic() + xlab("% Urban") + ylab("Mean response")

tradedependence <- h2o.partialPlot(object = a, data = train, cols = c("tradedependence"))
p1 <- qplot(tradedependence$tradedependence, tradedependence$mean_response) + geom_line() +
        theme_classic() + xlab("Trade dependence") + ylab("Mean response")

milex <- h2o.partialPlot(object = a, data = train, cols = c("milex"))
p6 <- qplot(milex$milex, milex$mean_response) + geom_line() +
        theme_classic() + xlab("Military expenditure") + ylab("Mean response")

multiplot(p1, p4, p2, p5, p3, p6, cols = 3)

# Ethnic conflict == 1
df.eth2 <- df5 %>% filter(ethnowarongoing == 1)

df.eth2a <- as.h2o(df.eth2)

df.eth2a$uamkstart <- as.factor(df.eth2a$uamkstart)  #encode the binary repsonse as a factor
h2o.levels(df.eth2a$uamkstart)

# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = df.eth2a, 
                         ratios = c(0.7, 0.15),  # 70%, 15%, 15%
                         seed = 42)  # reproducibility


train <- h2o.assign(splits[[1]], "train.hex")   
valid <- h2o.assign(splits[[2]], "valid.hex") 
test <- h2o.assign(splits[[3]], "test.hex")

y <- "uamkstart"
x <- setdiff(names(df.eth2), c(y, "ccode", "year", "rgdppc",
                               "uamkyr2", "uamkyr3", "sf", "country",
                               "elf2", "polity2sq")) 

# Running the model
rf <- h2o.grid("randomForest", x = x, y = y, training_frame = train, 
               validation_frame = valid, nfolds = 5, grid_id = "gridrf08",
               fold_assignment = "Stratified",
               hyper_params = list(ntrees = c(256, 512, 1024),
                                   max_depth = c(10, 20, 40),
                                   mtries = c(5, 6, 7),
                                   balance_classes = c(TRUE, FALSE),
                                   sample_rate = c(0.5, 0.632, 0.95),
                                   col_sample_rate_per_tree = c(0.5, 0.9, 1.0),
                                   histogram_type = c("UniformAdaptive",
                                                      "Random",
                                                      "QuantilesGlobal",
                                                      "RoundRobin")),
               search_criteria = list(strategy = "RandomDiscrete", 
                                      max_models = 100, 
                                      stopping_metric = "auc", 
                                      stopping_tolerance = 0.01, 
                                      stopping_rounds = 5, 
                                      seed = 26227709)) 

rf.grid <- h2o.getGrid(grid_id = "gridrf08",
                       sort_by = "auc",
                       decreasing = TRUE)
rf2 <- h2o.getModel(rf.grid@model_ids[[1]])
h2o.saveModel(rf2, path = "/root/Documents/mk/")
summary(rf2)
varimp <- as.data.frame(h2o.varimp(rf2))
h2o.varimp_plot(rf2)
h2o.performance(rf2, newdata = test)

## Graphs
a <- h2o.loadModel("/home/sussa/Documents/GitHub/mk/gridrf08_model_55")
print(va <- a %>% h2o.varimp() %>% as.data.frame() %>% head(., 6)) 

par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(2,7.5,1,1))
barplot(va$scaled_importance[6:1],
        horiz = TRUE, las = 1, cex.names=0.9,
        names.arg = c("Total battle deaths", 
                      "Military personnel",
                      "Log GDP per capita", 
                      "CINC",
                      "% Urban",
                      "Military expenditure"),
        main = "")

cinc <- h2o.partialPlot(object = a, data = train, cols = c("cinc"))
p3 <- qplot(cinc$cinc, cinc$mean_response) + geom_line() + theme_classic() + 
        xlab("CINC") +  ylab("Mean response")

milper <- h2o.partialPlot(object = a, data = train, cols = c("milper"))
p5 <- qplot(milper$milper, milper$mean_response) + geom_line() +
        theme_classic() + xlab("Military personnel") + ylab("Mean response")

logrgdppc <- h2o.partialPlot(object = a, data = train, cols = c("logrgdppc"))
p4 <- qplot(logrgdppc$logrgdppc, logrgdppc$mean_response) + geom_line() + theme_classic() +
        xlab("Log GDP per capita") + ylab("Mean response")

percentpopurban <- h2o.partialPlot(object = a, data = train, cols = c("percentpopurban"))
p2 <- qplot(percentpopurban$percentpopurban, percentpopurban$mean_response) + geom_line() +
        theme_classic() + xlab("% Urban") + ylab("Mean response")

totalbeaths <- h2o.partialPlot(object = a, data = train, cols = c("totalbeaths"))
p6 <- qplot(totalbeaths$totalbeaths, totalbeaths$mean_response) + geom_line() +
        theme_classic() + xlab("Total battle deaths") + ylab("Mean response")

milex <- h2o.partialPlot(object = a, data = train, cols = c("milex"))
p1 <- qplot(milex$milex, milex$mean_response) + geom_line() +
        theme_classic() + xlab("Military expenditure") + ylab("Mean response")

multiplot(p1, p4, p2, p5, p3, p6, cols = 3)