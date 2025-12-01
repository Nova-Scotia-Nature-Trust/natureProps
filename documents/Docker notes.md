# Notes on deploying app with Docker

Render html file in the terminal: `quarto render "Docker notes.md"`

Make sure the repository exisits on Docker Hub Run `docker login` to double check you can push/pull to DockerHub

## Deployment

The Docker build needs to be done in a temporary folder that isn't synced with OneDrive. This is currently set to `C:\Users\dominic\deploy_docker_apps`. Apps are deployed using Shiny server, which allows for multiple apps to be deployed in a single container.

Follow these steps to deploy the app on Docker.

1.  Setup Docker file as follows. Note that a dockerignore file can be used if necessary.

```         
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
    && rm -rf /var/lib/apt/lists/*

RUN echo 'preserve_logs true;' >> /etc/shiny-server/shiny-server.conf
RUN echo 'sanitize_errors false;' >> /etc/shiny-server/shiny-server.conf

# Copy natureProp files and restore its renv environment
COPY ./natureprops /srv/shiny-server/natureprops
# Change working directory
WORKDIR /srv/shiny-server/natureprops
# Restore environment
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

# Copy cons-lands-map files and restore its renv environment
COPY ./cons-lands-map /srv/shiny-server/cons-lands-map
WORKDIR /srv/shiny-server/cons-lands-map
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

# Set working directory back to Shiny Server root
WORKDIR /srv/shiny-server

# Expose Shiny Server port
EXPOSE 3838

# Start Shiny Server
CMD ["/usr/bin/shiny-server"]
```

2.  Copy app code files, Docker config file, Docker ignore file, renv.lock file, and data files to the `natureprops` folder within the deploy apps folder. To do this run the `copy_docker_files.ps1` script. Run command is `.\copy_docker_files.ps1`. Note the `F8` can be used to run a single/multiple selected lines in the terminal.

3.  Open terminal in `docker_deploy_apps` directory.

4.  If `nsnt-apps` image exists (and is running), first stop the container and remove it before building new testing version using `docker stop nsnt-apps-test`, `docker rm nsnt-apps-test` and `docker rmi nsnt-apps`.

5.  Build the image by running `docker build -t nsnt-apps .`.

6.  Once the image is built, test it locally by running `docker run -d --name nsnt-apps-test -p 3838:3838 nsnt-apps`. Access to the app will be through `localhost:3838/natureprops`. Note that the name of the container and image can the same (although they needn't be).

7.  The app files can be updated by copying local files to the running Docker container. The app will automatically restart when it detects new code files. To replace the `app.R` file for example, use `docker cp app.R nsnt-apps:/srv/shiny-server/natureprops/`. To replace all files in the `natureprops` folder, use `docker cp ./natureprops/. nsnt-apps:/srv/shiny-server/natureprops/` (note the trailing `.` indicates copy all files in the folder, not the folder itself). Note that the container should be stopped, then copy files, then start again to work reliably (it seems that files are not overwritten when the container is running). If files are copied to an existing container then the files need to be committed to the tagged version and then pushed to Docker Hub. Commit using `docker commit nsnt-apps domhenrynsnt/nsnt-apps:v1.0` after copying over files.

8.  To view files within the container open a bash terminal using `docker exec -it nsnt-apps bash`. The list files using `ls -al` in which folder you've navigated to. Use `exit` to exit the bash terminal. To view logs of the shiny server run `docker logs nsnt-apps`. The view logs of the app go into the bash terminal and navigate to the log directory using `cd /var/log/shiny-server`. Then view the logs for `natureprops` by using `cat natureprops.log` or `tail -f natureprops.log`

9.  Once the app has been verified to be working, it can be pushed to DockerHub. First tag with the version of the image and then tag latest using `docker tag nsnt-apps domhenrynsnt/nsnt-apps:v2.2`and `docker tag nsnt-apps domhenrynsnt/nsnt-apps:latest`.

10. Push to DockerHub using both tags `docker push domhenrynsnt/nsnt-apps:v2.2` and `docker push domhenrynsnt/nsnt-apps:latest`.

11. The app container can then be started on the server using `ssh nsnt_admin@192.168.1.51` to get in.

12. Stop and remove existing containers `docker stop nsnt-apps-prod` and `docker rm nsnt-apps-prod`.

13. Then `docker pull domhenrynsnt/nsnt-apps:v2.2` to pull preferred version. Run container using `docker run -d --name nsnt-apps-prod -p 3030:3838 domhenrynsnt/nsnt-apps:v2.2`. Note that the port has to be changed to `3030` because there are conflicts if we're using `3838` for multiple apps.

14. Note on ports. The format is **:** `-p [HOST_PORT]:[CONTAINER_PORT]`

    -   **3030** = Port on the host machine (your server at 192.168.1.51)

    -   **3838** = Port inside the container where Shiny Server is listening

    -   Inside the container, Shiny Server runs on port 3838 (standard Shiny Server port). When you access `192.168.1.51:3030`, Docker forwards that traffic to port 3838 inside the container. Here we're using 3030 on the host to avoid conflicts with other apps using 3838.

15. App can be accessed using `192.168.1.51:3030/natureprops`

16. Docker maintenance on server (due to limited space). Get images using `docker images -a`. Delete image using by first removing container `docker rm [CONTAINER NAME]`, then `docker rmi [IMAGE ID]`. Then look for dangling images using `docker images -f dangling=true`, then prune them using `docker image prune`

## Debugging

Use the following commands to view the logs within the Shiny Server

View the available logs at a higher level: `docker logs -f nsnt-apps`. View the available logs for the shiny server: `docker exec -it nsnt-apps ls -la /var/log/shiny-server/`.

This will then provide a list of logs for each app (these log files will change, so this is just an example). To open a log run: `docker exec -it nsnt-apps cat /var/log/shiny-server/natureprops-shiny-20250813-132812-39227.log`

The generic command is `docker exec -it nsnt-apps cat /var/log/shiny-server/{LOG FILE NAME}.log`

If logs aren't available try running the app directly using `docker exec -it nsnt-apps-test R -e "setwd('/srv/shiny-server/natureprops'); source('app.R')"` which will print outputs directly to the terminal.