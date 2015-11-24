from rocker/shiny

RUN apt-get update
RUN apt-get install -y \
	mesa-common-dev \
	libglu1-mesa-dev \
	libxml2 libxml2-dev \
	python-igraph 

# install shiny packages
RUN R -e "install.packages(c('data.table', 'FNN', 'igraph', 'rgl', 'shinyBS', 'shinyRGL', 'scrapeR', 'rARPACK', 'DT', 'mclust', 'Rtsne'), repos='https://cran.rstudio.com/')"
