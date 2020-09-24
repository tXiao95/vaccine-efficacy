FROM rocker/shiny-verse:latest

RUN apt-get update 
RUN apt-get install -y libv8-dev

RUN install2.r -e rstan LaplacesDemon shinyjs plotly

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

# write .rds stanmodelfit
RUN R -e "model <- rstan::stan_model('/srv/shiny-server/simulation.stan', auto_write = TRUE)"

# run app as entry point
CMD ["/usr/bin/shiny-server"]
