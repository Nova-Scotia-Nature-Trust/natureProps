# Notes on deploying app with Docker

The Docker build needs to be done in a temporary folder that isn't synced with OneDrive. This is currently set to `C:\Users\dominic\deploy_docker_apps`. Apps are deployed using Shiny server, which allows for multiple apps to be deployed in a single container. 

Follow these steps to deploy the app on Docker. 

1. Setup Docker file as follows. Note that a dockerignore file can be used if necessary. 

```
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
COPY ./natureprop /srv/shiny-server/natureprop
# Change working directory
WORKDIR /srv/shiny-server/natureprop
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
```


2. Copy app code files, Docker config file, Docker ignore file, renv.lock file, and data files to the `natureprops` folder within the deploy apps folder. To do this run the `copy_docker_files.ps1` script. Run command is `.\copy_docker_files.ps1`. Note the `F8` can be used to run a single/multiple selected lines in the terminal. 

3. Open terminal in `docker_deploy_apps` directory and build the image by running `docker build -t nsnt-apps .`. 
   
4. Once the image is built, test it locally by running `docker run -d --name nsnt-apps -p 3838:3838 nsnt-apps`. Access to the app will be through `localhost:3838/natureprops`. Note that the name of the container and image are the same (although they needn't be).
   
5. The app files can be updated by copying local files to the running Docker container. The app will automatically restart when it detects new code files. To replace the `app.R` file for example, use `docker cp app.R nsnt-apps:/srv/shiny-server/natureprops/`.
   
6. To view files within the container open a bash terminal using `docker exec -it nsnt-apps bash`. The list files using `ls -al` in which folder you've navigated to. Use `exit` to exit the bash terminal. To view logs of the shiny server run `docker logs nsnt-apps`. The view logs of the app go into the bash terminal and navigate to the log directory using `cd /var/log/shiny-server`. Then view the logs for natureprops by using `cat natureprops.log` or `tail -f natureprops.log`  
   
7. Once the app has been verified to be working, it can be pushed to DockerHub. First tag the version of the image using `docker tag nsnt-apps domhenrynsnt/nsnt-apps:latest`. Then push using `docker push domhenrynsnt/nsnt-apps:latest`.
   
8. The app container can then be started on the server using `ssh nsnt_admin@192.168.1.51` to get in, then `docker pull domhenrynsnt/nsnt-apps:latest` to pull latest version. Run container using `docker run -d --name nsnt-apps -p 3838:3838 domhenrynsnt/nsnt-apps:latest`.
9. App can be accessed using `192.168.1.51:3838/natureprops`
   
10. Docker maintenance on server (due to limited space). Get images using `docker images -a`. Delete image using by first removing container `docker rm [CONTAINER NAME]`, then `docker rmi [IMAGE ID]`. Then look for dangling images using `docker images -f dangling=true`, then prune them using `docker image prune`