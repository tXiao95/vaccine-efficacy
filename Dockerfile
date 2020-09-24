FROM rocker/shiny-verse:latest

RUN apt-get update 
RUN apt-get install -y libv8-dev

RUN Rscript -e "install.packages('LaplacesDemon', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('rstan', repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages('shinyjs', repos='http://cran.rstudio.com/')"
# looks like some neat options here
# RUN R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com/')"

#! if you add lines do it after the rstan install so that it caches !#

COPY vaccine-efficacy.Rproj /srv/shiny-server/
COPY *.R /srv/shiny-server/
COPY simulation.stan /srv/shiny-server/

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app as entry point
CMD ["/usr/bin/shiny-server"]