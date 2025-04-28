FROM rocker/shiny-verse

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy natureProp files and restore its renv environment
COPY ./natureprops /srv/shiny-server/natureprops
# Change working directory
WORKDIR /srv/shiny-server/natureprops
# Restore environment
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

# Copy app2 files and restore its renv environment
# COPY ./app2 /srv/shiny-server/app2
# WORKDIR /srv/shiny-server/app2
# RUN Rscript -e 'install.packages("renv")'
# RUN Rscript -e 'renv::consent(provided = TRUE)'
# RUN Rscript -e 'renv::restore()'

# Expose Shiny Server port
EXPOSE 3838

# Start Shiny Server
CMD ["/usr/bin/shiny-server"]