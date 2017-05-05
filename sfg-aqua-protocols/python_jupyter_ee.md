---
title: 'Setting up the Earth Engine Python API for use with Docker and Jupyter Notebooks'
author: Tyler Clavelle
output: html_document
---

## Overview
This tutorial explains how to install, configure, and start using Google Earth Engine's Python API with Docker and Jupyter Notebooks.

The setup process has the following main components:  
  1. Docker installation
  2. Jupyter installation

 ## Docker Installation
One of the tricky things with Python is that everyone's Python environment is different and many Python libraries also depend on non-Python code that is different on each machine. This makes manually managing your Python environment rather tricky and there are multiple Python packaging solutions designed to assist with this, however no community standard exists.

Enter Docker. Docker Containers package up everything needed to run an analysis, including the OS, Python code, and dependencies. A user can *pull* a Docker Container much like a GitHub repo and have everything needed to run a certain analysis without manually installing all components and messing with your local system's configuration.

Docker vs Docker Machine

You can create a Google Compute Engine VM machine using `$ docker-machine create`

Docker command for launching Earth Engine container with local volume mounted:

```
Tyler-SFG$ docker run -d -p 8888:8888 -v /Users/Tyler-SFG/Desktop/notebooks:/home/jovyan/work tylere/docker-debian-python3-scipy-notebook
```

**EE Authorization Code** - 4/tpXn4CQN9yYWEJ4GbOgsPInAyafyYyk470iJ7OvStGA
