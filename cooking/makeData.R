##### Library #####
library(rjson)
library(data.table)
library(tm)
library(gmodels)
library(ggplot2); theme_set(theme_bw())

##### Load data #####
dishes = fromJSON(file = "~/OneDrive/kaggle/cooking/all/train.json")

##### Executed Statements #####
out = list()
for(i in 1:length(dishes)) {
  print(i)
  add = as.data.table(dishes[[i]]$ingredients)
  add$cuisine = dishes[[i]]$cuisine
  add$id = dishes[[i]]$id
  out[[i]] = add
}

out = rbindlist(out)
names(out)[1] = "ingredient"
out$count = 1

dishes_count_by_cuisine = out[, list(ingredient_apps = sum(count)), by = c("cuisine", "id")]
dishes_count_by_cuisine$count = 1
dishes_count_by_cuisine = dishes_count_by_cuisine[, list(n_dishes = sum(count)), by = "cuisine"]

ingredient_freq =  out[, list(overall_ing_freq = sum(count)), by = c("ingredient")]
ingredient_freq$overall_ing_freq = ingredient_freq$overall_ing_freq/length(unique(out$id))

saveRDS(list(out = out, cuisine = dishes_count_by_cuisine, ingredients = ingredient_freq), "data.rds")
