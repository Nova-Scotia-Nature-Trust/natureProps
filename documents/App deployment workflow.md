---
title: "Automating app deployment with Docker and GitHub actions"
format:
  html:
    toc: true
    toc-location: left
    toc-float: true
    toc-depth: 4
    toc-expand: 3
    smooth-scroll: true
    theme: cosmo
---

An alternative to building docker images locally, testing them locally, pushing to DockerHub, then pulling them on the server, is to automate the build and deployment process. This is done using Docker and Github actions. The steps to set this up are outlined below.

Note that render this html file can be done in the terminal: `quarto render "App deployment workflow.md"`

## Build Dockerfile and test locally

### Dockerfile

The [`{shiny2docker}`](https://github.com/VincentGuyader/shiny2docker) package provides a set of functions to build a Dockerfile by analysing the code repository and detecting the dependencies and appropriate container to build upon. Simply run `shiny2docker::shiny2docker(path = ".")` to generate the Dockerfile in the root directory. The generated Dockerfile also has instructions for dealing with `renv` cache so that the full R package library is not installed on every build, which saves time during the build.

Some explantions of commands in the Dockerfile:

1.  `RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/`: creates two directories where R will look for global configuration files. These directories are needed so that R can read a system-wide `Rprofile.site` file in the next step

2.  `RUN --mount=type=cache,id=renv-cache,target=/root/.cache/R/renv \ R -e 'renv::restore()'`: This uses Docker BuildKit’s cache feature and creates persistent cache. Cached between builds on the same machine or CI runner. The `restore()` then installs required packages but uses cache where possible.

It's a good idea to locally test that the build is successful before going further. The Docker build needs to be done in a temporary folder that isn't synced with OneDrive. This is currently set to `C:\Users\dominic\deploy_docker_apps`. Apps are deployed using Shiny Server. The process for doing this is as follows:

Copy app code files, Docker config file, Docker ignore file, renv.lock file, and data files to the `natureprops` folder within the deploy apps folder. To do this run the `copy_docker_files.ps1` script. Run command is `.\copy_docker_files.ps1`. Note the `F8` can be used to run a single/multiple selected lines in the terminal.

Note that the `/renv` should be added to the `.dockerignore` file. It's not an issue when building locally (because that folder is not copied to the deploy apps directory), but it is tracked on the GitHub repo so when then image is built using GitHub actions then it will by default copy the `/renv` folder. This causes issues with renv recognising where the local library is. It's also important not to track the `.Rprofile` file. 

### Test Locally

1.  Open terminal in `docker_deploy_apps` directory. Tip: use `CTRL + R` to search for terminal commands. Use same shortcut to cycle through search results.

2.  If `natureprops-dev` container is running, first stop the container and remove it before building new testing version using `docker stop natureprops-dev`, `docker rm natureprops-dev` and `docker rmi natureprops`.

3.  Build the image by running `docker build -t natureprops .`. Note that building in this way will make use of Docker's local image cache. In order to replicate what a build using GitHub actions would look like use: `docker buildx build --no-cache --progress=plain --load -t natureprops .`. To build locally without `buildx`, use: `docker build --no-cache -t natureprops .`.

4.  Once the image is built, test it locally by running `docker run -d --name natureprops-dev -p 3838:3838 natureprops`. Access to the app will be through `localhost:3838`. The name of the container and image can the same (although they needn't be). Note that the .Renviron file is available when building locally but not when building in GitHub Actions. This is because it is not tracked with version control because it contains server login information. That means `env` parameters don't need to be specified when using `docker run`.

5.  To view files within the container open a bash terminal using `docker exec -it natureprops bash`. The list files using `ls -al` in which folder you've navigated to. Use `exit` to exit the bash terminal. To view logs of the shiny server run `docker logs natureprops`. The view logs of the app go into the bash terminal and navigate to the log directory using `cd /var/log/shiny-server`. Then view the logs for `natureprops` by using `cat natureprops.log` or `tail -f natureprops.log`

Note that the .Renviron file is available when building locally but not when building in GitHub Actions. This is because it is not tracked with version control because it contains server login information. That means `env` parameters don't need to be specified when using `docker run`.

A note on Docker maintenance: Get images using `docker images -a`. Delete image using by first removing container `docker rm [CONTAINER NAME]`, then `docker rmi [IMAGE ID]`. Then look for dangling images using `docker images -f dangling=true`, then prune them using `docker image prune`.

### Debugging

Use the following commands to view the logs within the Shiny Server

View the available logs at a higher level: `docker logs -f natureprops`. View the available logs for the shiny server: `docker exec -it natureprops ls -la /var/log/shiny-server/`.

This will then provide a list of logs for each app (these log files will change, so this is just an example). To open a log run: `docker exec -it natureprops cat /var/log/shiny-server/natureprops-shiny-20250813-132812-39227.log`

The generic command is `docker exec -it natureprops cat /var/log/shiny-server/{LOG FILE NAME}.log`

If logs aren't available try running the app directly using `docker exec -it natureprops-dev R -e "setwd('/srv/shiny-server/natureprops'); source('app.R')"` which will print outputs directly to the terminal.

## Create GitHub Action

The action instructions are written as a `yml` file in the `./.gitub/workflows` folder. This is the `docker.yml` file used to create an action to deploy the natureProps app (GitHb automatically detects the action when the file is created). This GitHub Actions workflow automatically builds and pushes a Docker image for the Shiny app whenever a repository version tag is created during a release. The last two lines handle caching in order to speed up the build (see [here](https://www.blacksmith.sh/blog/cache-is-king-a-guide-for-docker-layer-caching-in-github-actions) for more information about the cache setup).

``` yml
name: Build and Push Docker image

on:
  push:
    tags:
      - "v*"

jobs:
  build:
    runs-on: ubuntu-latest

    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Log in to Docker Hub
        uses: docker/login-action@v3
        with:
          username: ${{ secrets.DOCKERHUB_USERNAME }}
          password: ${{ secrets.DOCKERHUB_TOKEN }}

      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v3

      - name: Build and push image
        uses: docker/build-push-action@v5
        with:
          context: .
          push: true
          tags: |
            domhenrynsnt/natureprops:latest
            domhenrynsnt/natureprops:${{ github.ref_name }}
          cache-from: type=registry,ref=domhenrynsnt/natureprops:latest
          cache-to: type=inline,mode=min
```

Below is an annotated explanation of what the commands in the file are doing.

### Workflow name

``` yml
name: Build and Push Docker image 
```

-   This is the human-readable name of the workflow and it appears in the Actions tab in GitHub.

### Trigger conditions (`on`)

``` yml
on:   push:     branches: [ "main" ]     tags:       - "v*" 
```

This section defines when the workflow runs.

-   The workflow runs on:

    -   Any push to the `main` branch (this has been disabled but kept here for full explanation of how it can work).

    -   Any push of a Git tag that starts with `v` (e.g. `v1.0.0`, `v2.3.1`).

This setup allows the `latest` images to be built from `main` and versioned Docker images to be built from GitHub release tags.

### Jobs

``` yml
jobs:   build:     runs-on: ubuntu-latest 
```

-   A workflow consists of one or more jobs. This workflow has a single job named `build.` The job runs on GitHub’s hosted Ubuntu Linux runner.

### Job steps

Each job is made up of steps, which are executed in order.

#### 1. Check out the repository

``` yml
- name: Checkout code   uses: actions/checkout@v4 
```

-   Downloads the repository contents into the runner

-   Required so Docker can access:

    -   The `Dockerfile`

    -   The Shiny app source code

#### 2. Log in to Docker Hub

``` yml
- name: Log in to Docker Hub   uses: docker/login-action@v3   with:     username: ${{ secrets.DOCKERHUB_USERNAME }}     password: ${{ secrets.DOCKERHUB_TOKEN }} 
```

-   Authenticates GitHub Actions with Docker Hub

-   Uses GitHub Secrets to keep credentials secure (required in order to push images to Docker Hub)

Secrets used:

-   `DOCKERHUB_USERNAME` – your Docker Hub username

-   `DOCKERHUB_TOKEN` – a Docker Hub access token

These are set and stored in the GitHub repository under Settings -\> Secrets and variables -\> Actions -\> Repository secrets.

#### 3. Set up Docker Buildx

``` yml
- name: Set up Docker Buildx   uses: docker/setup-buildx-action@v3 
```

-   Enables Docker Buildx, an extended build tool (standard for modern Docker GitHub workflows).

#### 4. Build and push the Docker image

``` yml
- name: Build and push image   uses: docker/build-push-action@v5   with:     context: .     push: true     tags: |       domhenrynsnt/natureprops:latest       domhenrynsnt/natureprops:${{ github.ref_name }} 
```

-   `context: .`

    -   Uses the repository root as the Docker build context (this is where the `Dockerfile` is located)/

-   `push: true`

    -   Pushes the built image to Docker Hub

    -   Without this, the image would only be built locally on the runner

Two tags are applied to each build:

1.  `latest`

    ``` yml
    domhenrynsnt/natureprops:latest 
    ```

    -   Always points to the most recent successful build from `main`

2.  Version tag

    ``` yml
    domhenrynsnt/natureprops:${{ github.ref_name }} 
    ```

    -   Uses the Git reference name:

        -   `v1.2.0` if the workflow was triggered by a tag

        -   `main` if triggered by a branch push

    -   This allows versioned Docker images on Dockerhub to match GitHub releases.

## Server setup & testing

### Server setup
An alternative to specifying the details of each pull and run command is to use a docker compose yml file, which starts a docker service. This makes it easy to stop, start, restart, and update containers after pulling new/updated images. Added bonus is that environmental variables can be set in the compose file. Start by writing the docker compose file (below is an example for the natureprops app). Note that the Postgres environement details will need to be replaced with actual values. If there are multiple apps running then the ports will need to be different for each so that there are no conflicts. Set a unique container name for each service.

Note on ports (when using more than one app): The format is : `-p [HOST_PORT]:[CONTAINER_PORT]`

-   3030 = Port on the host machine (your server at 192.168.1.51)
-   3838 = Port inside the container where Shiny Server is listening
-   Inside the container, Shiny Server runs on port 3838 (standard Shiny Server port). When you access `192.168.1.51:3030`, Docker forwards that traffic to port 3838 inside the container.

``` yml
services:
  natureprops-app:
    image: domhenrynsnt/natureprops:latest
    container_name: natureprops-prod
    restart: unless-stopped
    ports:
      - "3838:3838"
    environment:
      POSTGRES_HOST: hostaddress
      POSTGRES_USER: username
      POSTGRES_PASSWORD: userpassword
```

To get the compose file onto the server follow these steps:

1.  `sudo mkdir -p /srv/shinyapps/natureprops`. This creates an empty folder for the docker compose setup.
2.  `sudo chown -R nsnt_admin:nsnt_admin /srv/shinyapps/natureprops`. This changes ownership so that nsnt_admin (non-root) can edit files.
3.  `cd /srv/shinyapps/natureprops ls -l`. Check folder exists.
4.  `scp docker-compose.yml nsnt_admin@SERVER_ADDRESS:/srv/shinyapps/natureprops/`. Copy the `docker-compose.yml` file from local machine to server folder.

The following commands can be used to control the natureprops-app service. First need to navigate to the folder in which the compose file is stored.


``` bash
# Navigate to the folder with the compose file
`cd /srv/shinyapps/natureprops`  

# Check folder contents
`ls`

# Pull the latest version of the image specified in the compose file
docker compose pull

# Start the natureprops-app service
docker compose up -d 

# View logs for natureprops-app (container must be running)
docker compose logs

# Stop natureprops-app
docker compose stop 

# Restart natureprops-app
docker compose restart
```

If for some reason there is a need to rollback and run a previously tagged version of the image from DockerHub use the following commands (after stopping the service):

Pull the image: `docker pull domhenrynsnt/natureprops:v1.0.1`

Run the image: `docker run -d --name natureprops-prod -p 3838:3838 -e POSTGRES_HOST=[HOSTNAME] -e POSTGRES_USER=[USER] -e POSTGRES_PASSWORD=[PASSWORD] domhenrynsnt/natureprops:v1.0.1`

A note on Docker maintenance: Get images using `docker images -a`. Delete image using by first removing container `docker rm [CONTAINER NAME]`, then `docker rmi [IMAGE ID]`. Then look for dangling images using `docker images -f dangling=true`, then prune them using `docker image prune`.

### Testing remote images locally

Once images have been built via GitHub actions they are available on DockerHub for testing before `docker compose pull` and `docker compose up -d`. The process is similar to that of running on the server in that a local docker compose file is created which can be used to quickly pull and start containers, and store the environment variables.

Begin by stopping and removing all containers with `docker rm -f $(docker ps -aq)`.

Then navigate to `C:\Users\dominic\deploy_docker_apps`.

Pull latest image with `docker compose -f docker-compose-np pull` (note that the `-f` flag is used to specify a custome file name).

Then start the service with `docker compose -f docker-compose-np up -d`.

## Code development

The preferred approach would be to create a branch from main (i.e. `dev`). Then add code to dev and create a pull request. Review and merge the PR. Then at that point create a new tag and release. This is the stage at which the GitHub Action will run.