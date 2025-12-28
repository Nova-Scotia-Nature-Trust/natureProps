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
# Set app directory
# ----------------------------------------
WORKDIR /srv/shiny-server/natureprops

# ----------------------------------------
# Copy renv files first (for caching)
# ----------------------------------------
COPY ./renv.lock ./natureprops/renv.lock

# ----------------------------------------
# Install renv & restore packages
# ----------------------------------------
RUN Rscript -e 'install.packages("renv")' \
 && Rscript -e 'renv::consent(provided = TRUE)' \
 && Rscript -e 'renv::restore(library="/srv/shiny-server/natureprops/renv/library", prompt = FALSE)'

# ----------------------------------------
# Copy the rest of the app
# ----------------------------------------
COPY . /srv/shiny-server/natureprops

# ----------------------------------------
# Fix ownership so Shiny can access packages
# ----------------------------------------
RUN chown -R shiny:shiny /srv/shiny-server/natureprops/renv

# ----------------------------------------
# Ensure R sees the renv library in all sessions
# ----------------------------------------
ENV R_LIBS_USER=/srv/shiny-server/natureprops/renv/library

# Also write it to Renviron.site for Shiny Server sessions
RUN echo "R_LIBS_USER=/srv/shiny-server/natureprops/renv/library" >> /usr/lib/R/etc/Renviron.site

# ----------------------------------------
# Expose port
# ----------------------------------------
EXPOSE 3838

# ----------------------------------------
# Start Shiny Server
# ----------------------------------------
CMD ["/usr/bin/shiny-server"]
