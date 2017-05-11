## ----tree definition-----------------------------------------------------
library(datree)

# Create outcomes
outdoor_sun <- o_node(~ v_os, "Outdoor-Sun")
porch_sun <- o_node(~ v_ps, "Porch-Sun")
indoor_rain <- o_node(~ v_ir, "Indoor-Rain")
indoor_sun <- o_node(~ v_is, "Indoor-Sun")
porch_rain <- o_node(~ v_pr, "Porch-Rain")
outdoor_rain <- o_node(~ v_or, "Outdoor-Rain")

# Uncertainty nodes at each alternative
outdoor <- u_node(list(outdoor_sun, outdoor_rain),
                  probs = c(~ p_sun, ~ 1- p_sun),
                  name = "Outdoor")
porch <- u_node(list(porch_sun, porch_rain),
                  probs = c(~ p_sun, ~ 1- p_sun),
                  name = "Porch")
indoor <- u_node(list(indoor_sun, indoor_rain),
                  probs = c(~ p_sun, ~ 1- p_sun),
                  name = "Indoor")

# Decision of party location
location <- d_node(list(outdoor, porch, indoor),
                   "Location")

## ----initial values------------------------------------------------------

# Load initial values
v_os <- 1
v_ps <- .95
v_ir <- .67
v_is <- .57
v_pr <- .32
v_or <- 0
p_sun <- .4

## ------------------------------------------------------------------------
evaluate(outdoor)
evaluate(indoor)
evaluate(porch)

evaluate(location)

## ------------------------------------------------------------------------
evaluate(outdoor, list(p_sun = .5))

## ------------------------------------------------------------------------
# This makes no sense, but still works ... (unfortunately)
evaluate(outdoor, list(p_sun = 500))

## ------------------------------------------------------------------------
decide(location)

## ------------------------------------------------------------------------
decide(location, list(p_sun = .8))

## ------------------------------------------------------------------------
library(tidyverse)
map_df(c(0, .5, 1), ~ cbind(
  decide(location, list(p_sun = .x)), p_sun = .x)) %>% 
  ggplot(aes(x = p_sun, y = evalue)) +
  geom_line(aes(color = alternative))

## ------------------------------------------------------------------------
# Decision of party location
location <- d_node(list( # Alternatives
  # Outdoor
  u_node(list(  # Outcomes for Outdoor
    o_node(~ v_os, "Outdoor-Sun"),
    o_node(~ v_or, "Outdoor-Rain")), 
    probs = c(~ p_sun, ~ 1 - p_sun),  
    name = "Outdoor"),
  # Porch
  u_node(list(  # Outcomes for Porch
    o_node(~ v_ps, "Porch-Sun"),
    o_node(~ v_pr, "Porch-Rain")), 
    probs = c(~ p_sun, ~ 1 - p_sun),  
    name = "Porch"),
  # Indoor
  u_node(list(  # Outcomes for Indoor
    o_node(~ v_is, "Indoor-Sun"),
    o_node(~ v_ir, "Indoor-Rain")), 
    probs = c(~ p_sun, ~ 1 - p_sun),  
    name = "Indoor")
  ), "Location")

decide(location)

