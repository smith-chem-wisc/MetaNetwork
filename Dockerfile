FROM rocker/verse:4.0.5

RUN apt-get update && apt-get install -y \
	libcurl4-gnutls-dev \ 
	libssl-dev

COPY . ./app
COPY ./renv.lock ./renv.lock

RUN apt-get -y install libxml2-dev 
RUN R -e 'install.packages("renv")'
RUN R -e 'renv::restore()' 

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', port = 3838, host = '0.0.0.0')"]


