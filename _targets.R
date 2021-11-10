library(targets)
tar_option_set(packages = c("tidyverse", "tidymodels"))

validate <- function(pokemon_data) {
  total_mismatch <- with(
    pokemon_data, 
    total != hp + atk + def + sp_atk + sp_def + spd
  )
  
  important_columns <- pokemon_data %>% select(type1, hp:atk)
  
  valid <- !any(total_mismatch) && !any(is.na(important_columns))
  
  if (!valid) {
    stop("Data is invalid")
  }
  
  valid
}

split_data <- function(data, validated = FALSE) {
  if (!validated) {
    stop("Data has not been validated")
  }
  train_families <- pokemon %>% distinct(family) %>% 
    sample_frac(0.7) %>% pull(family)
  
  pokemon_train <- pokemon %>% filter(family %in% train_families)
  pokemon_test <- pokemon %>% filter(!(family %in% train_families))
  list(train = pokemon_train, test = pokemon_test)
}

define_model <- function() {
  rand_forest(
    trees = 200,
    mtry = 3
  ) %>%
    set_engine("ranger") %>% 
    set_mode("classification")
}

define_preprocessing_recipe <- function(train_data) {
  recipe(
    type1 ~ hp + atk + def + sp_atk + sp_def + spd + total + color,
    data = train_data
  ) %>%
    step_mutate(
      hp = hp / total,
      atk = atk / total,
      def = def / total,
      sp_atk = sp_atk / total,
      sp_def = sp_def / total,
      spd = spd / total
    ) %>% 
    step_normalize(total)
}

define_workflow <- function(model, recipe) {
  workflow() %>% add_recipe(recipe) %>% add_model(model)
}

evaluate <- function(model, test_data) {
  test_data %>% 
    mutate(
      predicted_type1 = predict(model,test_data)$.pred_class
    ) %>%
    accuracy(type1, predicted_type1) %>% 
    pull(.estimate)
}

list(
  tar_target(
    pokemon_data_file,
    "data/pokemon.csv",
    format = "file"
  ),
  tar_target(
    pokemon,
    read_csv(pokemon_data_file)
  ),
  tar_target(
    validation,
    validate(pokemon)
  ),
  tar_target(
    data_split,
    split_data(pokemon, validated = validation),
    cue = tar_cue()
  ),
  tar_target(
    train,
    data_split$train
  ),
  tar_target(
    test,
    data_split$test
  ),
  tar_target(
    model,
    define_model()
  ),
  tar_target(
    preprocessing,
    define_preprocessing_recipe(train)
  ),
  tar_target(
    workflow,
    define_workflow(model, preprocessing)
  ),
  tar_target(
    fitted_workflow,
    fit(workflow, train)
  ),
  tar_target(
    accuracy,
    evaluate(fitted_workflow, test)
  )
)
