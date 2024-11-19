# SCC-II Map Customization Script and Shiny App

This Shiny application is designed to create highly customizable maps of the **SCC-II Swiss Canopy Crane II** research site, located in Hoelstein BL, Switzerland. The SCC-II is part of the University of Basel's research infrastructure to investigate the repsonse of temperate forest ecosystems to drought.

## Features

- **Interactive Map Customization**: Generate maps for various kinds of measurements and research purposes.
- **Customizable Layers**: Add information about trees, infrastructure, and environmental measurements to the map.
- **Dynamic Filters**: Use filters to control and highlight specific data points based on user-defined criteria.
- **Preconfigured Maps**: Load predefined map templates for common use cases or start with a blank canvas.
- **Export Options**: Download maps in PDF or JPEG formats for presentations and publications.
- **Store own presets**: Download own presents to (re)generate maps for specific purposes

## Hosted Application

The application is hosted online for easy access:  
[**Access the SCC-II Map Customization App**](#)

## Local Usage

For advanced customization beyond the capabilities of the UI, the `plot_scciimap_srv.R` script can be used directly in an R environment. This script provides additional flexibility for researchers who need to generate tailored maps programmatically.

*Note*: The app requires the SCC-II Metadatafile which is not part of this repository. 

## Repository Contents

- **`app.R`**: Main Shiny application file.
- **`plot_scciimap_srv.R`**: Script to generate SCC-II maps programmatically.
- **`templates/`**: Directory containing preconfigured map templates in `.rds` format.
- **`datasheets/`**: Directory containing nescessary data for the app *Note* Data included here might be outdated. Use the hosted app for current data.
- **`README.md`**: Documentation for the repository.
## License

This project is licensed under the [MIT License](LICENSE).

---