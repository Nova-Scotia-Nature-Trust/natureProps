# Install system dependencies
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
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    cmake \
    libabsl-dev \
    && rm -rf /var/lib/apt/lists/*

RUN echo 'preserve_logs true;' >> /etc/shiny-server/shiny-server.conf
RUN echo 'sanitize_errors false;' >> /etc/shiny-server/shiny-server.conf

# Copy natureProp files and restore its renv environment
COPY . /srv/shiny-server/natureprops
# Change working directory
WORKDIR /srv/shiny-server/natureprops
# Restore environment
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

# Give Shiny user permission to renv library
RUN chown -R shiny:shiny /srv/shiny-server/natureprops/renv

# Set working directory back to Shiny Server root
WORKDIR /srv/shiny-server

# Expose Shiny Server port
EXPOSE 3838

# Start Shiny Server
CMD ["/usr/bin/shiny-server"]