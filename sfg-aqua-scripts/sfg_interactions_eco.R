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
rank = same
label = 'Fed mariculture'
      node [fillcolor = LightSeaGreen, label = 'Fish/shrimp ponds']
      b
      node [fillcolor = LightSeaGreen, label = 'Fish/shrimp cages']
      c
}

subgraph cluster1 {
rank = same
label = 'Unfed mariculture'
      node [fillcolor = LightSeaGreen, label = 'Bivalves']
      d 
      node [fillcolor = LightSeaGreen, label = 'Seaweed']
      h
}

      node [fillcolor = orange, label = 'Wild\npopulation', shape = square]
      f
      node [fillcolor = orange, label = 'Habitat /\necosystem', shape = square]
      g
      
      #### ARROWS FOR INTERACTIONS
      ## Ecological Interactions
      edge [color = Black, label = 'feed', fontcolor = Black]
      a -> b [lhead = cluster0]
      edge [color = Black, label = 'seed/fry', fontcolor = Black]
      f -> c [lhead = cluster0]
      edge [color = Black, label = 'seed/fry', fontcolor = Black]
      f -> h [lhead = cluster1]
      edge [color = Salmon, label = 'disease', fontcolor = Salmon]
      c -> f [dir = both]
      edge [color = Salmon, label = 'disease', fontcolor = Salmon]
      {d h} -> f [dir = both, tailport = cluster1]
      edge [color = Salmon, label = 'habitat modification', fontcolor = Salmon]
      c -> g [ltail = cluster0]
      edge [color = Salmon, label = 'habitat modification', fontcolor = Salmon]
      h -> g [ltail = cluster1]
      edge [color = Salmon, label = 'water quality', fontcolor = Salmon]
      c -> g [ltail = cluster0]
      edge [color = Green, label = 'water quality', fontcolor = Green]
      {d} -> g [ltail = cluster1]
      edge [color = Salmon, label = 'genetic fitness', fontcolor = Salmon]
      {c} -> f
      edge [color = Salmon, label = 'invasive species', fontcolor = Salmon]
      {c} -> g
      edge [color = Salmon, label = 'invasive species', fontcolor = Salmon]
      d -> g [ltail = cluster1]
      edge [color = Green, label = 'stock enhancement', fontcolor = Green]
      {c} -> f [ltail = cluster0]
      edge [color = Green, label = 'stock enhancement', fontcolor = Green]
      d -> f
      edge [color = Black, label = 'harvest', fontcolor = Black]
      f -> {a}
      
      }")