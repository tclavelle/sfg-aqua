##################################################
## Project: Aquaculture Interactions
## Script purpose: Build flowchart / network diagrams
## Date: 05/9/2017
## Author: Tyler Clavelle
##################################################

library(DiagrammeR)

### Interaction network with interaction types as sub-nodes
grViz("
digraph dot {
      compound = true;
      graph [fontsize = 10, layout = dot, rankdir = LR]
      # graph [fontsize = 10, layout = dot]

      node [shape = oval,
      style = filled,
      color = grey,
      label = '']

## SHAPES FOR STAKEHOLDERS      
      node [fillcolor = DarkSeaGreen, label = 'Capture\nfisheries']
      a
      
subgraph cluster0 {
      node [fillcolor = LightSeaGreen, label = 'Fish ponds']
      b
      node [fillcolor = LightSeaGreen, label = 'Fish cages']
      c
      node [fillcolor = LightSeaGreen, label = 'Shellfish']
      d
}

      node [fillcolor = orange, label = 'Wild\npopulation', shape = square]
      f
      node [fillcolor = orange, label = 'Habitat /\necosystem', shape = square]
      g

#### ARROWS FOR INTERACTIONS
## Ecological Interactions
      edge [color = Salmon, label = 'fishmeal', fontcolor = Salmon]
      a -> {b c};
      edge [color = Salmon, label = 'seed/fry', fontcolor = Salmon]
      f -> c [lhead = cluster0];
      edge [color = Salmon, label = 'disease']
      {c d} -> f
      edge [color = Salmon, label = 'habitat modification', fontcolor = Salmon]
      {a b c d} -> g
      edge [color = Salmon, label = 'water quality (-)', fontcolor = Salmon]
      {b c} -> g
      edge [color = Salmon, label = 'water quality (+)', fontcolor = Salmon]
      {d} -> g
      edge [color = Salmon, label = 'genetic fitness', fontcolor = Salmon]
      {c d} -> f
      edge [color = Salmon, label = 'invasive species', fontcolor = Salmon]
      {c d} -> g
      edge [color = Salmon, label = 'stock enhancement', fontcolor = Salmon]
      {c d} -> f
      edge [color = Salmon, label = 'harvest', fontcolor = Salmon]
      f -> {a}
## Spatial Interactions
      edge [color = SteelBlue, label = 'exclusion', fontcolor = SteelBlue]
      c -> a [ltail = cluster0]
      edge [color = SteelBlue, label = 'de facto MPA', fontcolor = SteelBlue]
      {c d} -> f [ltail = cluster0]
      edge [color = SteelBlue, label = 'FAD', fontcolor = SteelBlue]
      {c d} -> a [ltail = cluster0]
## Market Interaction Edges
      edge [color = DarkMagenta, label = 'price competition', fontcolor = DarkMagenta]
      c -> a [ltail = cluster0]
      edge [color = DarkMagenta, label = 'increased market', fontcolor = DarkMagenta]
      c -> a [ltail = cluster0]
      edge [color = DarkMagenta, label = 'increased demand', fontcolor = DarkMagenta]
      c -> a [ltail = cluster0]

      }")