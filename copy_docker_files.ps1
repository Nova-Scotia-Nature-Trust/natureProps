# PowerShell script to copy necessary files for Docker deployment

# Define source and destination paths
$source_path = "C:\Users\dominic\OneDrive - Nova Scotia Nature Trust\Documents\R Shiny\natureProps"
$destination_path_01 = "C:\Users\dominic\deploy_docker_apps\natureprops"
$destination_path_02 = "C:\Users\dominic\deploy_docker_apps"

# Create the destination folder if it doesn't exist
if (!(Test-Path -Path $destination_path_01)) {
    New-Item -ItemType Directory -Path $destination_path_01
}

# Copy necessary files
Copy-Item -Path "$source_path\app.R" -Destination $destination_path_01 -Recurse -Force
Copy-Item -Path "$source_path\.dockerignore" -Destination $destination_path_02 -Force
Copy-Item -Path "$source_path\renv.lock" -Destination $destination_path_01 -Force
Copy-Item -Path "$source_path\inputs" -Destination $destination_path_01 -Recurse -Force
Copy-Item -Path "$source_path\R" -Destination $destination_path_01 -Recurse -Force

Write-Host "Files have been successfully copied to $destination_path"