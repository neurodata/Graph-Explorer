# NeuroData's Graph Explorer
Web-based graph analytics and some visualization

To run:

1. go to the shiny dir
2. start R
3. type `library('shiny')`
5. type `runApp()`

# Deploy Updates

1. log in to brainapps1.neurodata.io
2. ```cd /srv/shinyapps/Graph-Explorer```
3. ```git pull deploy```
4. (maybe restart shiny)

# If docker container goes down

1. ```./graphexplorerstart.sh```

# If we want to add more dependencies to installation

- Update dockerfile
- Run ```sudo docker build -t graph_explorer .``` in the same directory as the Dockerfile 

# To get a terminal on the docker container

- ```docker exec -it <<container name>> /bin/bash```
- Find the container name by running ```docker ps```
