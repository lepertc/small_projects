##### Library #####
library(shiny)
library(data.table)
library(ggplot2); theme_set(theme_bw())

##### Get data #####
data = readRDS("/Users/chloelepert/OneDrive/kaggle/cooking/data.rds")
df = data$out
ingredients = data$ingredients
ingredient_options = ingredients[overall_ing_freq * length(unique(df$id)) > 100, ]$ingredient
cuisine_dist = data$cuisine

##### Functions #####
filter_on_ing = function(df, ing) {
  df_has_one = df[ingredient %in% ing, ]
  df_has_one$one = 1
  df_matches = df_has_one[, list(count = sum(one)), by = c("id", "cuisine")]
  df_matches = df_matches[count == length(ing), ]
  cuisines = as.data.table(table(df_matches$cuisine))
  names(cuisines) = c("cuisine", "freq")
  print(cuisines)
  df_ing = df[id %in% df_matches$id, ]
  recipes = unique(df_matches$id)
  return(list(df_ing = df_ing, cuisines = cuisines, recipes = recipes))
}

suggest_ing = function(df_ing, ings, ingredient_freq) {
  n_recipe = length(unique(df_ing$id))
  df_ing = df_ing[!(ingredient %in% ings), ]
  ing_freq = as.data.table(table(df_ing$ingredient))
  names(ing_freq) = c("ingredient", "filtered_count")
  ing_freq$filtered_freq = ing_freq$filtered_count/n_recipe
  ing_freq = merge(ing_freq, ingredient_freq, by = "ingredient")
  ing_freq$index = (0.1 + ing_freq$filtered_freq)/(0.1 + ing_freq$overall_ing_freq)
  ing_freq = ing_freq[order(-index), ]
  return(ing_freq[c(1:10), ])
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Ingredient Suggestions"),
   selectizeInput("current_ingredients", "Select Ingredients", ingredient_options, multiple = TRUE),
   verbatimTextOutput("number_recipes"),
   plotOutput("cuisines"),
   plotOutput("suggested_additions"),
   tableOutput("recipes")
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  df_ings <- reactive({
    t1 = filter_on_ing(df, input$current_ingredients)
    suggest = suggest_ing(t1$df_ing, input$current_ingredients, ingredients)
    list(suggest = suggest, cuisines = t1$cuisines, recipes = t1$recipes)
  })
  
  output$suggested_additions  <- renderPlot(
    ggplot(df_ings()$suggest, aes(x = reorder(ingredient, index), y = index)) + geom_col() + coord_flip() +
      labs(x = "Ingredient", y = "index", title = "Ingredient over expressed with ingredient selection")
  )
  
  output$cuisines <- renderPlot({
    ggplot(df_ings()$cuisines, aes(x = "", y = freq, fill = cuisine)) + coord_polar("y") + geom_col() +
      labs(title = "Distribution of cuisines with these ingredients")
  })
  
  output$number_recipes <- renderText({
    paste("There are", as.character(length(df_ings()$recipes)), "recipes with these ingredients.", sep = " ")
  })
  
  output$recipes <- renderTable({
    recette = df[id %in% df_ings()$recipes, ]
    recette = recette[, .(ingredients = paste(ingredient, collapse = ", ")), by = c("cuisine", "id")]
    recette[1:10, ]
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

