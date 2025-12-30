# PowerShell script to copy necessary files for Docker deployment

# Define source and destination paths
$source_path = "C:\Users\dominic\OneDrive - Nova Scotia Nature Trust\Documents\R Shiny\natureProps"
$destination_path_01 = "C:\Users\dominic\deploy_docker_apps\natureprops"

# Create the destination folder if it doesn't exist
if (!(Test-Path -Path $destination_path_01)) {
    New-Item -ItemType Directory -Path $destination_path_01
}

# Copy necessary files

Copy-Item -Path "$source_path\global.R" -Destination $destination_path_01 -Recurse -Force
Copy-Item -Path "$source_path\ui.R" -Destination $destination_path_01 -Recurse -Force
Copy-Item -Path "$source_path\server.R" -Destination $destination_path_01 -Recurse -Force

Copy-Item -Path "$source_path\.Renviron" -Destination $destination_path_01 -Recurse -Force
Copy-Item -Path "$source_path\postgres_auth_config.yml" -Destination $destination_path_01 -Recurse -Force

Copy-Item -Path "$source_path\.dockerignore" -Destination $destination_path_01 -Force
Copy-Item -Path "$source_path\renv.lock" -Destination $destination_path_01 -Force

Copy-Item -Path "$source_path\help" -Destination $destination_path_01 -Recurse -Force
Copy-Item -Path "$source_path\app_data" -Destination $destination_path_01 -Recurse -Force
Copy-Item -Path "$source_path\R" -Destination $destination_path_01 -Recurse -Force

Copy-Item -Path "$source_path\.github" -Destination $destination_path_01 -Recurse -Force

Copy-Item -Path "$source_path\Dockerfile" -Destination $destination_path_01 -Recurse -Force

Write-Host "Files have been successfully copied to $destination_path_01"