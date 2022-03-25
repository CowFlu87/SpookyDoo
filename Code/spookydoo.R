library(tidyverse)
library(tidymodels)
library(lubridate)
library(glue)
library(ggtext)
library(showtext)
library(patchwork)

# Love me some creepy font
font_add_google("Creepster", "Creepster")
font_add_google("Roboto", "Roboto")
showtext_auto()


# Load the file 
spookydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

# Styling the plot
theme_update(panel.background   = element_rect(fill='#242424', color='#242424'),
             plot.background    = element_rect(fill='#242424', color= '#242424'),
             panel.border       = element_rect(fill = NA, color = NA),
             panel.grid.major.x = element_line(color = "#F2F2F2",
                                               size = 0.4,
                                               linetype = "dotted"),
             panel.grid.major.y = element_line(color = "#F2F2F2",
                                               size = 0.4,
                                               linetype = "dotted"),
             panel.grid.minor.y  = element_blank(),
             panel.grid.minor.x  = element_blank(),
             axis.ticks.y        = element_blank(),
             axis.ticks.x        = element_blank(),
             plot.margin         = unit(c(1,1,1.5,1.2),"cm"))



# Part 1 ------------------------------
# Excluding the "Unmask" variables

spookydoo_vanilla <- spookydoo %>% 
  filter(
    format != "TV Series (segmented)",
    imdb != "NULL") %>% 
  select(c(title, 
           date_aired, 
           season, 
           format, 
           imdb, 
           monster_amount, 
           motive, 
           setting_terrain)) %>% 
  mutate(
    date_aired = round(as.numeric(format(date_aired, "%Y")),digits = -1),
    date_aired = as.factor(date_aired),
    format = as.factor(format),
    imdb = parse_number(imdb),
    monster_amount = as.factor(monster_amount)
  ) %>% 
  filter(
    !is.na(imdb)
  ) %>% 
  mutate_if(
    is.logical, as.factor
  ) 

# Creating the first model
set.seed(123)

# Splitting the data in a training set and a testing set
spooky_split <- initial_split(spookydoo_vanilla, strata = "imdb", prop = .75) 

spooky_train <- spooky_split %>% training()
spooky_test <- spooky_split %>% testing()

# Adding a repice
spooky_recipe <- recipe(imdb ~ ., data = spooky_train) %>% 
  update_role(title, new_role = "ID") %>% 
  step_other(motive, threshold = .05) %>% 
  step_other(monster_amount, threshold = .05) %>%
  step_other(setting_terrain, threshold = .05) %>% 
  step_other(season, threshold = .1)

# Creating the model, then wrap it all up with a workflow
lm_model <- linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression')

spooky_workflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(spooky_recipe)


# Fitting the model 
spooky_fit <- fit(spooky_workflow, spooky_train)

spooky_final <- spooky_test %>% 
  bind_cols(predict(spooky_fit, spooky_test))

# Collecting the metrics
spooky_lastfit <- spooky_workflow %>% 
  last_fit(split = spooky_split)

rsq <- spooky_lastfit$.metrics %>% 
  as.data.frame %>% 
  select(-.config) %>% 
  filter(.metric == "rsq") %>% 
  mutate(.estimate = round(.estimate, 2)) %>% 
  pull(.estimate)

# Plotting the results 
spooky_final %>% 
  ggplot(aes(x=.pred, y=imdb))+
  geom_abline(col = "#8fce00", 
              lty = 2,
              size=1.2)+
  geom_point(shape = 21,
             size  = 6,
             color = "#38761d",
             fill  = "#8fce00")+
  annotate(
    "text",
    x = 9, 
    y = 5.7,
    family = "Roboto",
    color = "#8fce00",
    size = 9.7,
    label = glue("italic(R^2): {rsq}"),
    parse = TRUE,
    hjust = 1, vjust = 1)+
  coord_cartesian(xlim=c(5, 9),
                  ylim=c(5, 10)) +
  labs(title = "Is Scoobydoo unpredictable?",
       subtitle = glue("Some peculiar features of the show<br>can almost accurately predict the ratings"),
       x = "Prediction",
       y = "IMDB score",
       caption = "<b>Source:</b> TidyTuesday") +
  theme(plot.title = element_text(size = 36,
                                  family = "Creepster",
                                  color = "#8fce00",
                                  face = "bold"),
        plot.subtitle = element_markdown(size = 30,
                                         family = "Creepster",
                                         color = "#F2F2F2"),
        plot.caption = element_markdown(size = 14,
                                        family = "Roboto",
                                        color = "#F2F2F2"),
        axis.title = element_text(size = 16,
                                  family = "Roboto",
                                  color = "#F2F2F2"),
        axis.text           = element_text(size=14,
                                           family = "Roboto",
                                           color = "#F2F2F2"))



# Part 2 ---------------------
# Unmasking Velma and Shaggy

# Velma
spookydoo_velma <- spookydoo %>% 
  filter(
    format != "TV Series (segmented)",
    imdb != "NULL") %>% 
  select(c(title, 
           date_aired, 
           season, 
           format, 
           imdb, 
           monster_amount, 
           motive, 
           setting_terrain,
           unmask_velma)) %>% 
  mutate(
    date_aired = round(as.numeric(format(date_aired, "%Y")),digits = -1),
    date_aired = as.factor(date_aired),
    format = as.factor(format),
    imdb = parse_number(imdb),
    monster_amount = as.factor(monster_amount)
  ) %>% 
  filter(
    !is.na(imdb)
  ) %>% 
  mutate_if(
    is.logical, as.factor
  ) 

# Shaggy
spookydoo_shaggy <- spookydoo %>% 
  filter(
    format != "TV Series (segmented)",
    imdb != "NULL") %>% 
  select(c(title, 
           date_aired, 
           season, 
           format, 
           imdb, 
           monster_amount, 
           motive, 
           setting_terrain,
           unmask_shaggy)) %>% 
  mutate(
    date_aired = round(as.numeric(format(date_aired, "%Y")),digits = -1),
    date_aired = as.factor(date_aired),
    format = as.factor(format),
    imdb = parse_number(imdb),
    monster_amount = as.factor(monster_amount)
  ) %>% 
  filter(
    !is.na(imdb)
  ) %>% 
  mutate_if(
    is.logical, as.factor
  ) 



# Creating the model
set.seed(123)

# Velma model
spooky_split_velma <- initial_split(spookydoo_velma, strata = "imdb", prop = .75) 
spooky_train_velma <- spooky_split_velma %>% training()
spooky_test_velma <- spooky_split_velma %>% testing()

# Adding a repice and the workflow
spooky_recipe_velma <- recipe(imdb ~ ., data = spooky_train_velma) %>% 
  update_role(title, new_role = "ID") %>% 
  step_other(motive, threshold = .05) %>% 
  step_other(monster_amount, threshold = .05) %>%
  step_other(setting_terrain, threshold = .05) %>% 
  step_other(season, threshold = .1)


spooky_velma_workflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(spooky_recipe_velma)


# Is the model robust? Using crossvalidation folds to assess
# if the performance are due to the rng or not.
spooky_velma_fold <- vfold_cv(spooky_train_velma, times = 10) #bootstrap for bootstrapping

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

set.seed(130)
spooky_velma_results  <- 
  spooky_velma_workflow %>% 
  fit_resamples(resamples = spooky_velma_fold, control = keep_pred)

rsq_velma_cv <- spooky_velma_results %>% 
  collect_metrics() %>% 
  filter(.metric == "rsq") %>% 
  mutate(mean = round(mean,2) ) %>% 
  pull(mean)

# Fitting the model
spooky_velma_fit <- fit(spooky_velma_workflow, spooky_train_velma)

spooky_velma_final <- spooky_test %>% 
  bind_cols(predict(spooky_velma_fit, spooky_test_velma))


# Shaggy model
spooky_split_shaggy <- initial_split(spookydoo_shaggy, strata = "imdb", prop = .75) 
spooky_train_shaggy <- spooky_split_shaggy %>% training()
spooky_test_shaggy <- spooky_split_shaggy %>% testing()


# Adding a repice and the workflow
spooky_recipe_shaggy <- recipe(imdb ~ ., data = spooky_train_shaggy) %>% 
  update_role(title, new_role = "ID") %>% 
  step_other(motive, threshold = .05) %>% 
  step_other(monster_amount, threshold = .05) %>%
  step_other(setting_terrain, threshold = .05) %>% 
  step_other(season, threshold = .1)

spooky_shaggy_workflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(spooky_recipe_shaggy)

# Is the model robust? Using crossvalidation folds to assess
# if the performance are due to the rng or not.

spooky_shaggy_fold <- vfold_cv(spooky_train_shaggy, times = 10) #bootstrap for bootstrapping

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

set.seed(130)
spooky_shaggy_results  <- 
  spooky_shaggy_workflow %>% 
  fit_resamples(resamples = spooky_shaggy_fold, control = keep_pred)

rsq_shaggy_cv <- spooky_shaggy_results %>% 
  collect_metrics() %>% 
  filter(.metric == "rsq") %>% 
  mutate(mean = round(mean,2) ) %>% 
  pull(mean)

# Fitting the model
spooky_shaggy_fit <- fit(spooky_shaggy_workflow, spooky_train_shaggy)

spooky_shaggy_final <- spooky_test_shaggy %>% 
  bind_cols(predict(spooky_shaggy_fit, spooky_test_shaggy))


# Plotting the results
spooky_velma_final %>% 
  ggplot(aes(x=.pred, y=imdb))+
  geom_abline(col = "#8fce00", 
              lty = 2,
              size=1.2)+
  geom_point(shape = 21,
             size  = 6,
             color = "#38761d",
             fill  = "#8fce00")+
  annotate(
    "text",
    x = 9, 
    y = 5.7,
    family = "Roboto",
    color = "#8fce00",
    size = 5.7,
    label = glue("italic(R^2): {rsq_velma_cv}"),
    parse = TRUE,
    hjust = 1, vjust = 1)+
  coord_cartesian(xlim=c(5, 9),
                  ylim=c(5, 10)) +
  labs(title = "Velma",
       subtitle = "The geek",
       x = "Prediction",
       y = "IMDB score") +
  theme(plot.title = element_text(size = 30,
                                  family = "Creepster",
                                  color = "#8fce00",
                                  face = "bold"),
        plot.subtitle = element_text(size = 24,
                                     family = "Creepster",
                                     color = "#F2F2F2"),
        axis.title = element_text(size = 16,
                                  family = "Roboto",
                                  color = "#F2F2F2"),
        axis.text           = element_text(size=14,
                                           family = "Roboto",
                                           color = "#F2F2F2")) +
  {
    spooky_shaggy_final %>% 
      ggplot(aes(x=.pred, y=imdb))+
      geom_abline(col = "#8fce00", 
                  lty = 2,
                  size=1.2)+
      geom_point(shape = 21,
                 size  = 6,
                 color = "#38761d",
                 fill  = "#8fce00")+
      annotate(
        "text",
        x = 9, 
        y = 5.7,
        family = "Roboto",
        color = "#8fce00",
        size = 5.7,
        label = glue("italic(R^2): {rsq_shaggy_cv}"),
        parse = TRUE,
        hjust = 1, vjust = 1)+
      coord_cartesian(xlim=c(5, 9),
                      ylim=c(5, 10)) +
      labs(title = "Shaggy",
           subtitle = "The sissy",
           x = "Prediction",
           y = "IMDB score") +
      theme(plot.title = element_text(size = 30,
                                      family = "Creepster",
                                      color = "#8fce00",
                                      face = "bold"),
            plot.subtitle = element_text(size = 24,
                                         family = "Creepster",
                                         color = "#F2F2F2"),
            axis.title = element_text(size = 16,
                                      family = "Roboto",
                                      color = "#F2F2F2"),
            axis.text           = element_text(size=14,
                                               family = "Roboto",
                                               color = "#F2F2F2"))
  } +
  plot_annotation(title = "Does the unmasker count?",
                  subtitle = "The plot thickens. If Velma or Shaggy unmask, the model improves.",
                  caption = "<b>Source:</b> TidyTuesday",
                  theme = theme(plot.title = element_text(size = 36,
                                                          family = "Creepster",
                                                          color = "#8fce00",
                                                          face = "bold"),
                                plot.subtitle = element_text(size = 30,
                                                             family = "Creepster",
                                                             color = "#F2F2F2",
                                                             face = "bold"),
                                plot.caption = element_markdown(size = 14,
                                                            family = "Roboto",
                                                            color = "#F2F2F2")))


# Part 3 -------------
# Full analysis

spookydoo_complete <- spookydoo %>% 
  filter(format != "TV Series (segmented)",
         imdb != "NULL") %>% 
  select(c(title, 
           date_aired, 
           season, 
           format, 
           imdb, 
           monster_amount, 
           motive, 
           setting_terrain,
           unmask_fred,
           unmask_daphnie,
           unmask_velma,
           unmask_scooby)) %>% 
  mutate(date_aired = round(as.numeric(format(date_aired, "%Y")),digits = -1),
         date_aired = as.factor(date_aired),
         format = as.factor(format),
         imdb = parse_number(imdb),
         monster_amount = as.factor(monster_amount)) %>% 
  filter(!is.na(imdb)) %>% 
  mutate_if(is.logical, as.factor)


# Create the model 
set.seed(234)
spooky_complete_split <- initial_split(spookydoo_complete, strata = "imdb", prop = .75) 

spooky_complete_train <- spooky_complete_split %>% training()
spooky_complete_test <- spooky_complete_split %>% testing()

# Adding a recipe
spooky_complete_recipe <- recipe(imdb ~ ., data = spooky_complete_train) %>% 
  update_role(title, new_role = "ID") %>% 
  step_other(motive, threshold = .05) %>% 
  step_other(monster_amount, threshold = .05) %>%
  step_other(setting_terrain, threshold = .05) %>% 
  step_other(season, threshold = .1)

# Adding the workflow
spooky_complete_workflow <- workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(spooky_complete_recipe)

# Is the model robust? Using crossvalidation folds to assess
# if the performance are due to the rng or not.

# Using some folds to increase the performance of the model
spooky_fold <- vfold_cv(spooky_complete_train, times = 10) #bootstrap for bootstrapping

keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

set.seed(130)
spooky_results  <- 
  spooky_complete_workflow %>% 
  fit_resamples(resamples = spooky_fold, control = keep_pred)

rsq_cv <- spooky_results %>% 
  collect_metrics() %>% 
  filter(.metric == "rsq") %>% 
  mutate(mean = round(mean,2)) %>% 
  pull(mean)

# Prediction 
spooky_complete_fit <- fit(spooky_complete_workflow, spooky_complete_train)

spooky_complete_final <- spooky_complete_test %>% 
  bind_cols(predict(spooky_complete_fit, spooky_complete_test))


spooky_complete_lastfit <- spooky_complete_workflow %>% 
  last_fit(split = spooky_complete_split)


# Plotting the final results
spooky_complete_final %>% 
  ggplot(aes(x=.pred, y=imdb))+
  geom_abline(col = "#8fce00", 
              lty = 2,
              size=1.2)+
  geom_point(shape = 21,
             size  = 6,
             color = "#38761d",
             fill  = "#8fce00")+
  annotate(
    "text",
    x = 9, 
    y = 5.7,
    family = "Roboto",
    color = "#8fce00",
    size = 9.7,
    label = glue("italic(R^2): {rsq_cv}"),
    parse = TRUE,
    hjust = 1, vjust = 1)+
  coord_cartesian(xlim=c(5, 9),
                  ylim=c(5, 10)) +
  labs(title = "Unmasking the truth",
       subtitle = glue("The audience seems to care about who takes the mask off"),
       x = "Prediction",
       y = "IMDB score",
       caption = "<b>Source:</b> TidyTuesday") +
  theme(plot.title = element_text(size = 36,
                                  family = "Creepster",
                                  color = "#8fce00",
                                  face = "bold"),
        plot.subtitle = element_text(size = 30,
                                     family = "Creepster",
                                     color = "#F2F2F2"),
        plot.caption = element_markdown(size = 14,
                                    family = "Roboto",
                                    color = "#F2F2F2"),
        axis.title = element_text(size = 16,
                                  family = "Roboto",
                                  color = "#F2F2F2"),
        axis.text           = element_text(size=14,
                                           family = "Roboto",
                                           color = "#F2F2F2"))
