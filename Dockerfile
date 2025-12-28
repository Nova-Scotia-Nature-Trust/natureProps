# ----------------------------------------
# Base image
# ----------------------------------------
FROM rocker/shiny-verse:latest

# ----------------------------------------
# System dependencies
# ----------------------------------------
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

# ----------------------------------------
# Shiny Server config tweaks
# ----------------------------------------
RUN echo 'preserve_logs true;' >> /etc/shiny-server/shiny-server.conf \
 && echo 'sanitize_errors false;' >> /etc/shiny-server/shiny-server.conf

# ----------------------------------------
# Environment variables for renv + CRAN
# ----------------------------------------
ENV RENV_CONFIG_CACHE_ENABLED=FALSE
ENV R_LIBS_USER=/srv/shiny-server/natureprops/renv/library
ENV R_REPOS=https://cloud.r-project.org

# ----------------------------------------
# Set app directory
# ----------------------------------------
WORKDIR /srv/shiny-server/natureprops

# ----------------------------------------
# Copy full app
# ----------------------------------------
COPY . /srv/shiny-server/natureprops

# ----------------------------------------
# Ensure renv library path exists and is writable
# ----------------------------------------
RUN mkdir -p /srv/shiny-server/natureprops/renv/library \
 && chown -R shiny:shiny /srv/shiny-server/natureprops/renv

# ----------------------------------------
# Install renv and restore packages
# ----------------------------------------
RUN Rscript -e 'install.packages("renv")' \
 && Rscript -e 'renv::consent(provided = TRUE)' \
 && Rscript -e 'renv::restore(project="/srv/shiny-server/natureprops", library="/srv/shiny-server/natureprops/renv/library", prompt=FALSE)'

# ----------------------------------------
# Set working directory back to Shiny Server root
# ----------------------------------------
WORKDIR /srv/shiny-server

# ----------------------------------------
# Expose Shiny Server port
# ----------------------------------------
EXPOSE 3838

# ----------------------------------------
# Start Shiny Server
# ----------------------------------------
CMD ["/usr/bin/shiny-server"]
