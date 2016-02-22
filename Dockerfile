from rocker/shiny

RUN apt-get clean
RUN apt-get update
RUN apt-get install -y \
	mesa-common-dev \
	libglu1-mesa-dev \
	libxml2 libxml2-dev \
	python-igraph \ 
	libssl-dev 

# install shiny packages
RUN R -e "install.packages(c('data.table', 'FNN', 'igraph', 'rgl', 'shinyBS', 'shinyRGL', 'scrapeR', 'irlba', 'Matrix', 'DT', 'scales', 'rARPACK', 'mclust', 'Rtsne', 'devtools', 'rvest'), repos='https://cran.rstudio.com/')"

# install data analytics website packages
RUN R -e "install.packages(c('robustbase', 'reshape', 'fastcluster', 'ggdendro', 'tsne','htmltools','grid','gtable'),  repos='https://cran.rstudio.com/')"

RUN R -e "devtools::install_github('rstudio/DT')"
