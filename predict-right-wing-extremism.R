## Convert R Markdown to R script
## knitr::purl("predict-right-wing-extremism.Rmd")



## ---------------------------------------------------------------------------------------------
rm(list = ls())


## ----setup, include=F-------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
Packages <- c("tidyverse",     
            "tidymodels",    
            "ranger",        
            "vip",           
            "parallel"       
            )
            
lapply(Packages, require, character.only = T)


## ----eval=F, message=F------------------------------------------------------------------------
## Packages <- c("tidyverse",   # 1.3.0      Data preparation
##             "tidymodels",    # 0.1.3      Modeling, data division
##             "ranger",        # 0.12.1     Random Forest Algorithm
##             "vip",           # 0.3.2      Variable importance
##             "parallel"       # 4.0.2      Parallel calculation
##            )
## 
## lapply(Packages, require, character.only = T)
## rm(Packages)


## ---- message=F-------------------------------------------------------------------------------
Data <- haven::read_sav('ESS1-9.sav')


## ---------------------------------------------------------------------------------------------
Data <- Data %>%
  dplyr::select(lrscale,
                impsafe,
                ipstrgv,
                ipbhprp,
                ipfrule,
                imptrad, 
                imsmetn,
                imdfetn,
                impcntr,
                rlgdgr,
                ppltrst,
                pplfair,
                pplhlp,
                trstprl,
                trstplt,
                trstprt,
                eduyrs,
                gndr,
                agea,
                hincfel,
                hinctnta,
                rlgdgr
               )


## ---------------------------------------------------------------------------------------------
summary(Data)


## ---------------------------------------------------------------------------------------------
anyNA(Data)

apply(Data, 2, function(col)sum(is.na(col))/length(col)) * 100


## ---------------------------------------------------------------------------------------------
naniar::gg_miss_var(Data, show_pct = T) + 
  ylim(0, 50) +
  labs(title = "Overview of missing values",
       x="Variables", 
       y = "Proportion of missing values in %") +
  theme(plot.title = element_text(hjust = 0.5)) 

#ggsave("missing_values.png")


## ---------------------------------------------------------------------------------------------
df <- Data %>% 
  drop_na()

anyNA(df) 

nrow(df) / nrow(Data) * 100 # Ratio of data set sizes

#rm(Data)


## ----message=FALSE, warning=FALSE-------------------------------------------------------------
ggplot2::ggplot(df, aes(x=lrscale, y = (..count..)/sum(..count..)* 100)) + 
  geom_histogram(fill = "#00326d") +
  labs(title = "Self-assessment on left-right scale (0 to 10)",
       x="Question: 'place yourself on this scale, where 0 means the left and 10 means the right?'", 
       y = "Proportion in %") +
  theme(plot.title = element_text(hjust = 0.5)) 

#ggsave("histogram_lrscale.png")


## ---------------------------------------------------------------------------------------------
df <- df %>%
  dplyr::mutate(right_wing = car::recode(df$lrscale,
                            "0:8 = 0; 9:10 = 1; else = NA", as.factor = T))

df <- df %>% 
  purrr::modify_at("lrscale", ~ NULL)


## ---------------------------------------------------------------------------------------------
df$gndr <- as.factor(df$gndr)

is.factor(df$gndr)


## ---------------------------------------------------------------------------------------------
df %>% 
  count(right_wing) %>% 
  mutate(prop = n/sum(n) * 100)


## ---------------------------------------------------------------------------------------------
set.seed(42)

# 1. Data division (75%/25%)
splits   <- initial_split(df, prop = 3/4, strata = right_wing)

df_other <- training(splits) # not testing
df_test  <- testing(splits)  # testing

# Distribution target variable in not testing set
df_other %>% 
  count(right_wing) %>% 
  mutate(prop = n/sum(n) * 100)

# Distribution of target variable in test set
df_test  %>% 
  count(right_wing) %>% 
  mutate(prop = n/sum(n) * 100)


## ---------------------------------------------------------------------------------------------
set.seed(42)

# 2. Data division (80%/20%)
val_set <- validation_split(df_other, 
                            strata = right_wing, 
                            prop = 0.8)
val_set


## ---------------------------------------------------------------------------------------------
cores <- parallel::detectCores() - 2

cores


## ---------------------------------------------------------------------------------------------
rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 64) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("classification")


## ---------------------------------------------------------------------------------------------
rf_recipe <- recipe(right_wing ~ ., data = df_other) #%>%
   #step_downsample(right_wing, under_ratio = 1) #%>% # optional, geht schneller!
   #step_dummy(all_nominal(), -all_outcomes()) %>% # One-Hot Encoding
   #step_normalize(all_numeric()) # Normalisierung



## ---------------------------------------------------------------------------------------------
rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe)


## ----warning = F------------------------------------------------------------------------------
set.seed(42)

intervalStart <- Sys.time()

rf_res <- 
  rf_workflow %>% 
  tune_grid(val_set,
            grid = 20,
            control = control_grid(save_pred = T),
            metrics = metric_set(roc_auc))

intervalEnd <- Sys.time()

# time measurement
paste("Duration:",intervalEnd - intervalStart, attr(intervalEnd - intervalStart,"units"))


## ---------------------------------------------------------------------------------------------
rf_res %>% 
  show_best(metric = "roc_auc")


## ---------------------------------------------------------------------------------------------
autoplot(rf_res) + 
  labs(title = "Number of predictors and datapoints at each node") +
  theme(plot.title = element_text(hjust = 0.5))

#ggsave("predictors_nodesize.png")


## ---------------------------------------------------------------------------------------------
rf_best <- 
  rf_res %>% 
  select_best(metric = "roc_auc")

rf_best


## ---------------------------------------------------------------------------------------------
rf_res %>% 
  collect_predictions()


## ---------------------------------------------------------------------------------------------
rf_auc <- 
  rf_res %>% 
  collect_predictions(parameters = rf_best) %>% 
  roc_curve(right_wing, .pred_0) %>% 
  mutate(model = "Random Forest")


## ---------------------------------------------------------------------------------------------
rf_auc %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) + 
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) + 
  coord_equal() + 
  scale_color_viridis_d(option = "plasma", end = .6) +
  labs(title = "Receiver Operating Characteristic (ROC) Curve (validation set)") +
  theme(plot.title = element_text(hjust = 0.5))

#ggsave("roc_curve_validation.png")


## ---------------------------------------------------------------------------------------------
last_rf_mod <- 
  rand_forest(mtry = 1, min_n = 17, trees = 64) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
  set_mode("classification")

# the last workflow
last_rf_workflow <- 
  rf_workflow %>% 
  update_model(last_rf_mod)

# the last fit
set.seed(42)

intervalStart <- Sys.time()

last_rf_fit <- 
  last_rf_workflow %>% 
  last_fit(splits)

intervalEnd <- Sys.time()

# time measurement
paste("Duration:",intervalEnd - intervalStart, attr(intervalEnd - intervalStart,"units"))


## ---------------------------------------------------------------------------------------------
last_rf_fit %>% 
  collect_metrics()


## ----warning = F------------------------------------------------------------------------------
last_rf_fit %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 20, aesthetics = list(fill = "#00326d")) +
  labs(title = "Importance of individual predictors in the random forest model") +
  theme(plot.title = element_text(hjust = 0.5))

#ggsave("variable_importance.png")


## ---------------------------------------------------------------------------------------------
last_rf_auc <- 
  last_rf_fit %>% 
  collect_predictions(parameters = rf_best) %>% 
  roc_curve(right_wing, .pred_0) %>% 
  mutate(model = "Random Forest")


## ---------------------------------------------------------------------------------------------
last_rf_auc %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) + 
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) + 
  coord_equal() + 
  scale_color_viridis_d(option = "plasma", end = .6) +
  labs(title = "Receiver Operating Characteristic (ROC) Curve (test set)") +
  theme(plot.title = element_text(hjust = 0.5))

#ggsave("roc_curve_test.png")

