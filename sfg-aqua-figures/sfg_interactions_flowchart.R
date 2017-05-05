library(DiagrammeR)
library(tidyverse)

### Sequence diagram using mermaid in DiagrammeR
mermaid("
graph TB
        A[Aquatic base production]-->B(Capture fisheries)
        A-->D(Shellfish aquaculture)
        A-->C(Seaweed aquaculture)
        B-->E[Fishmeal]
        E-->H[Freshwater aquaculture]
        E-->F(Fed marine aquaculture)
        B-->G[Human consumption]
        D-->G
        F-->G
        style A fill:DeepSkyBlue  
        style B fill:DarkSeaGreen
        style C fill:DarkSeaGreen
        style D fill:DarkSeaGreen
        style E fill:Peru
        style F fill:DarkSeaGreen
        style H fill:White"
        )

### Sequence diagram using mermaid in DiagrammeR
mermaid("
        graph LR
        A[Aquatic base production]-->B(Capture fisheries)
        A-->C(Recreational fisheries)
        A-->D(Shellfish aquaculture)
        A-->E(Seaweed aquaculture)
        B-->F[Fishmeal]
        F-->G(Fed marine aquaculture)
        G-->H[Human consumption]
        B-->H
        C-->H
        D-->H
        style A fill:DeepSkyBlue  
        style B fill:DarkSeaGreen
        style C fill:DarkSeaGreen
        style D fill:DarkSeaGreen
        style E fill:Peru
        style F fill:DarkSeaGreen
        style H fill:White"
)

### Interaction network interactions as edges
grViz("
digraph dot {
      
      graph [fontsize = 10]
      
      node [shape = oval,
      style = filled,
      color = grey,
      label = '']
      
      node [fillcolor = DarkSeaGreen, label = 'Capture fisheries']
      a
      
      node [fillcolor = DarkSeaGreen, label = 'Recreational fisheries']
      b
      
      node [fillcolor = LightSeaGreen, label = 'Fish cages']
      c
      
      node [fillcolor = LightSeaGreen, label = 'Shellfish aquaculture']
      d
      
      node [fillcolor = LightSeaGreen, label = 'Seaweed aquaculture']
      e

      node [fillcolor = orange, shape = 'square', label = 'Wild populations']
      f
      
      node [fillcolor = orange, shape = 'square', label = 'Habitat/ecosystem']
      g
      
      edge [color = red]
      a -> f [label = 'harvest', color = red]
      b -> f [label = 'harvest', color = red]
      }")


### Interaction network with interaction types as sub-nodes
grViz("
digraph dot {
      
      graph [fontsize = 10, layout = dot, rankdir = LR]
      
      node [shape = oval,
      style = filled,
      color = grey,
      label = '']
      
      node [fillcolor = DarkSeaGreen, label = 'Capture fisheries']
      a
      
      node [fillcolor = DarkSeaGreen, label = 'Recreational fisheries']
      b
      
      node [fillcolor = LightSeaGreen, label = 'Fish cages']
      c
      
      node [fillcolor = LightSeaGreen, label = 'Shellfish aquaculture']
      d
      
      node [fillcolor = LightSeaGreen, label = 'Seaweed aquaculture']
      e
      
      node [fillcolor = orange, label = 'Ecological', shape = square]
      f
      
      node [fillcolor = orange, label = 'Spatial', shape = square]
      g
      
      node [fillcolor = orange, label = 'Market', shape = square]
      h
      
      edge [color = grey]
      {a b} -> {f g}
      {a} -> {h}
      c -> {f g h}
      d -> {f g h}
      e -> {f g}
      f -> {a b c d e}
      g -> {a b c d e}
      h -> {a c d e}
      }")