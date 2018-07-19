FROM rocker/verse

RUN apt-get update
RUN apt-get -y upgrade
RUN apt-get -y install libfftw3-dev

RUN Rscript -e "install.packages('imager', quiet = TRUE)"
RUN Rscript -e "install.packages('shinytest', quiet = TRUE)"
RUN Rscript -e "devtools::install_github('MahShaaban/colocr)"
