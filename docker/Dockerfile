FROM rocker/r-ver:3.6.1

# Install Ubuntu packages
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcairo2-dev \
    libjpeg-dev \
    libxt-dev \
    libssl-dev \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libssl-dev

#install necessary libraries
RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_github('LUMC/dgeAnalysis')"

EXPOSE 1402

CMD R -e "library(dgeAnalysis); dgeAnalysis::startApp()"
