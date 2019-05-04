FROM rocker/verse

RUN apt-get update
RUN apt-get -y upgrade
RUN apt-get -y install libfftw3-dev bzip2

RUN Rscript -e "install.packages('imager', quiet = TRUE)"
RUN Rscript -e "install.packages('shinytest', quiet = TRUE)"
RUN Rscript -e "install.packages('shinyBS', quiet = TRUE)"
RUN Rscript -e "devtools::install_github('ropensci/colocr')"
RUN cd ~
RUN wget https://github.com/Medium/phantomjs/releases/download/v2.1.1/phantomjs-2.1.1-linux-x86_64.tar.bz2
RUN tar xvjf phantomjs-2.1.1-linux-x86_64.tar.bz2
RUN mv phantomjs-2.1.1-linux-x86_64 /usr/local/share
RUN ln -sf /usr/local/share/phantomjs-2.1.1-linux-x86_64/bin/phantomjs /usr/local/bin
