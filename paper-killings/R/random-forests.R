######################
### Random forests ###
######################

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
        if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-wolpert/4/R")


# Load package
library(h2o)
h2o.init(nthreads = -1)

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

##########################
### Running the models ###
##########################

rf <- h2o.grid("randomForest", x = x, y = y, training_frame = train, 
               validation_frame = valid, nfolds = 5, grid_id = "gridrf01",
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
                                      max_models = 500, 
                                      stopping_metric = "auc", 
                                      stopping_tolerance = 0.01, 
                                      stopping_rounds = 5, 
                                      seed = 26227709)) 

# Saving the most accurate model
rf.grid <- h2o.getGrid(grid_id = "gridrf01",
                       sort_by = "auc",
                       decreasing = TRUE)

rf2 <- h2o.getModel(rf.grid@model_ids[[1]])
h2o.saveModel(rf2, path = "/root/Documents/mk/")
summary(rf2)
varimp <- as.data.frame(h2o.varimp(rf2))
h2o.varimp_plot(rf2)
h2o.performance(rf2, newdata = test)

# Second model
rf <- h2o.grid("randomForest", x = x, y = y, training_frame = train, 
               validation_frame = valid, nfolds = 5, grid_id = "gridrf01b",
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
                                      max_models = 500, 
                                      stopping_metric = "auc", 
                                      stopping_tolerance = 0.01, 
                                      stopping_rounds = 5, 
                                      seed = 44849999))

# Saving the most accurate model
rf.grid <- h2o.getGrid(grid_id = "gridrf01b",
                       sort_by = "auc",
                       decreasing = TRUE)

rf2 <- h2o.getModel(rf.grid@model_ids[[1]])
h2o.saveModel(rf2, path = "/root/Documents/mk/")
summary(rf2)
varimp <- as.data.frame(h2o.varimp(rf2))
h2o.varimp_plot(rf2)
h2o.performance(rf2, newdata = test) 

# Third model
rf <- h2o.grid("randomForest", x = x, y = y, training_frame = train, 
               validation_frame = valid, nfolds = 5, grid_id = "gridrf01c",
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
                                      max_models = 500, 
                                      stopping_metric = "auc", 
                                      stopping_tolerance = 0.01, 
                                      stopping_rounds = 5, 
                                      seed = 1502436)) 

# Saving the most accurate model
rf.grid <- h2o.getGrid(grid_id = "gridrf01c",
                       sort_by = "auc",
                       decreasing = TRUE)

rf2 <- h2o.getModel(rf.grid@model_ids[[1]])
h2o.saveModel(rf2, path = "/root/Documents/mk/")
summary(rf2)
varimp <- as.data.frame(h2o.varimp(rf2))
h2o.varimp_plot(rf2)
h2o.performance(rf2, newdata = test) 

##########################
### Ongoing civil wars ###
##########################

# UCDP == 1
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

# Running the model
rf <- h2o.grid("randomForest", x = x, y = y, training_frame = train, 
               validation_frame = valid, nfolds = 5, grid_id = "gridrf02",
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
                                      max_models = 1000, 
                                      stopping_metric = "auc", 
                                      stopping_tolerance = 0.01, 
                                      stopping_rounds = 5, 
                                      seed = 26227709)) 

rf.grid <- h2o.getGrid(grid_id = "gridrf02",
                       sort_by = "auc",
                       decreasing = TRUE)
rf2 <- h2o.getModel(rf.grid@model_ids[[1]])
h2o.saveModel(rf2, path = "/root/Documents/mk/")
summary(rf2)
varimp <- as.data.frame(h2o.varimp(rf2))
h2o.varimp_plot(rf2)
h2o.performance(rf2, newdata = test)

# COW == 1
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

# Running the model
rf <- h2o.grid("randomForest", x = x, y = y, training_frame = train, 
               validation_frame = valid, nfolds = 5, grid_id = "gridrf03",
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
                                      max_models = 500, 
                                      stopping_metric = "auc", 
                                      stopping_tolerance = 0.01, 
                                      stopping_rounds = 5, 
                                      seed = 26227709)) 

rf.grid <- h2o.getGrid(grid_id = "gridrf03",
                       sort_by = "auc",
                       decreasing = TRUE)
rf2 <- h2o.getModel(rf.grid@model_ids[[1]])
h2o.saveModel(rf2, path = "/root/Documents/mk/")
summary(rf2)
varimp <- as.data.frame(h2o.varimp(rf2))
h2o.varimp_plot(rf2)
h2o.performance(rf2, newdata = test)

# Ethnic conflict == 1
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
x <- setdiff(names(df.eth), c(y, "ccode", "year", "rgdppc",
                               "mksyr2", "mksyr3", "sf", "country",
                               "elf2", "polity2sq")) 

# Running the model
rf <- h2o.grid("randomForest", x = x, y = y, training_frame = train, 
               validation_frame = valid, nfolds = 5, grid_id = "gridrf04",
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
                                      max_models = 500, 
                                      stopping_metric = "auc", 
                                      stopping_tolerance = 0.01, 
                                      stopping_rounds = 5, 
                                      seed = 26227709)) 

rf.grid <- h2o.getGrid(grid_id = "gridrf04",
                       sort_by = "auc",
                       decreasing = TRUE)
rf2 <- h2o.getModel(rf.grid@model_ids[[1]])
h2o.saveModel(rf2, path = "/root/Documents/mk/")
summary(rf2)
varimp <- as.data.frame(h2o.varimp(rf2))
h2o.varimp_plot(rf2)
h2o.performance(rf2, newdata = test)

###########################################################
## Same models with Genocide/Politicide variable (Harf) ###
###########################################################
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

# Main model
rf <- h2o.grid("randomForest", x = x, y = y, training_frame = train, 
               validation_frame = valid, nfolds = 5, 
               grid_id = "gridrf05",
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
                                      max_models = 500, 
                                      stopping_metric = "auc", 
                                      stopping_tolerance = 0.01, 
                                      stopping_rounds = 5, 
                                      seed = 26227709)) 

# Saving the most accurate model
rf.grid <- h2o.getGrid(grid_id = "gridrf05",
                       sort_by = "auc",
                       decreasing = TRUE)

rf2 <- h2o.getModel(rf.grid@model_ids[[1]])
h2o.saveModel(rf2, path = "/root/Documents/mk/")
summary(rf2)
varimp <- as.data.frame(h2o.varimp(rf2))
h2o.varimp_plot(rf2)
h2o.performance(rf2, newdata = test)

# UCDP == 1
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

# Running the model
rf <- h2o.grid("randomForest", x = x, y = y, training_frame = train, 
               validation_frame = valid, nfolds = 5, grid_id = "gridrf06",
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

rf.grid <- h2o.getGrid(grid_id = "gridrf06",
                       sort_by = "auc",
                       decreasing = TRUE)
rf2 <- h2o.getModel(rf.grid@model_ids[[1]])
h2o.saveModel(rf2, path = "/root/Documents/mk/")
summary(rf2)
varimp <- as.data.frame(h2o.varimp(rf2))
h2o.varimp_plot(rf2)
h2o.performance(rf2, newdata = test)

# COW == 1
df.cow2a <- as.h2o(df.cow2)

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

# Running the model
rf <- h2o.grid("randomForest", x = x, y = y, training_frame = train, 
               validation_frame = valid, nfolds = 5, grid_id = "gridrf07",
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

rf.grid <- h2o.getGrid(grid_id = "gridrf07",
                       sort_by = "auc",
                       decreasing = TRUE)
rf2 <- h2o.getModel(rf.grid@model_ids[[1]])
h2o.saveModel(rf2, path = "/root/Documents/mk/")
summary(rf2)
varimp <- as.data.frame(h2o.varimp(rf2))
h2o.varimp_plot(rf2)
h2o.performance(rf2, newdata = test)

# Ethnic conflict == 1
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