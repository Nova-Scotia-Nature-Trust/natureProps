# natureProps
This is an R Shiny app for managing the property database for the Nova Scotia Nature Trust. It includes several modules, each serving a specific purpose.

## Overview
The app provides a user-friendly interface for managing property-related data, including landowner communication, property intake, outreach queries, and action item tracking. It leverages the `bslib` package for a modern UI and integrates with a database for data storage and retrieval.

## Modules
1. **Property Intake Module**:
   - Allows users to add new properties and landowner contact details.
   - Includes input validation for fields like property name, phase, and acquisition type.
   - Supports linking landowners to properties using Parcel Identification Numbers (PIDs).

2. **Landowner Communication Module**:
   - Facilitates recording communication with landowners.
   - Supports different communication types (e.g., outreach, direct communication).
   - Tracks details like communication purpose, method, and follow-up dates.

3. **Outreach Queries Module**:
   - Enables users to run queries related to outreach priorities.
   - Provides a data viewer for displaying query results.
   - Includes options for filtering data by focal areas and other criteria.

4. **Action Item Tracking Module**:
   - Tracks action items for properties.
   - Allows users to assign and update checklist items for specific PIDs.
   - Displays success messages upon successful updates.

5. **Data Viewer Module**:
   - Displays various data views, such as PIDs, landowner details, and communication history.
   - Supports exporting data to formats like Excel and PDF.

6. **Value Boxes Module**:
   - Displays key indicators, such as the number of high-priority properties and board approvals.
   - Provides a quick overview of important metrics.

## Functions

Here is a list of all the functions found in the `./R/functions` directory along with a brief description of each:

1. **`append_db_data`**: Appends data to a specified database table within a transaction. Ensures data integrity by rolling back on errors.
2. **`create_db_con`**: Establishes a connection to a PostgreSQL database using predefined parameters.
3. **`join_lookup_fn`**: Joins a lookup table with a data frame based on specified keys and renames the resulting column.
4. **`populate_nsprd_tables`**: Populates various tables in the NS Property Database by extracting and transforming data from a source database.
5. **`prep_view_communications`**: Prepares data for the communication view by fetching and transforming landowner communication data.
6. **`prep_view_landowners`**: Prepares data for the landowner details view by combining landowner and parcel information.
7. **`prep_view_outreach`**: Prepares data for the outreach view by fetching and transforming outreach-related data.
8. **`prep_view_pid`**: Prepares data for parcel views by fetching and transforming parcel-related data.
9. **`validate_pid_input`**: Validates Parcel Identification Numbers (PIDs) against a list of valid PIDs.

## Deployment
The app is containerized using Docker, making it easy to deploy and manage. It uses the `renv` package for dependency management, ensuring consistent environments across deployments.

