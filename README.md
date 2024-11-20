# SCC-II Map Customization Script and Shiny App

This Shiny application is designed to create highly customizable maps of the [**Swiss Canopy Crane II (SCC-II)**](https://ppe.duw.unibas.ch/en/sccii/) research site, located in Hoelstein BL, Switzerland. The SCC-II is part of the University of Basel's research infrastructure to investigate the response of temperate forest ecosystems to drought.

## Features

- **Interactive Map Customization**: Generate maps for various kinds of measurements and research purposes.
- **Customizable Layers**: Add information about trees, infrastructure, and environmental measurements to the map.
- **Dynamic Filters**: Use filters to control and highlight specific data points based on user-defined criteria.
- **Preconfigured Maps**: Load predefined map templates for common use cases or start with a blank canvas.
- **Export Options**: Download maps in PDF or JPEG formats for presentations and publications.
- **Store own presets**: Download own presents to (re)generate maps for specific purposes

## Hosted Application

The application is hosted online for easy access:  
[**Access the SCC-II Map Customization App**](https://dabascientific.com/hoelstein/map.html)

## Local Usage

For advanced customization beyond the capabilities of the UI, the `plot_scciimap_srv.R` script can be used directly in an R environment. This script provides additional flexibility for researchers who need to generate tailored maps programmatically.

*Note*: The app requires the *Metadata_Trees_SCCII.xlsx* file which is not part of this public repository. PPE team member will have access to the most up to date version

## Repository Contents

- **`app.R`**: Main Shiny application file.
- **`plot_scciimap_srv.R`**: Script to generate SCC-II maps programmatically.
- **`templates/`**: Directory containing preconfigured map templates in `.rds` format.
- **`datasheets/`**: Directory containing nescessary data for the app.  
*Note*: *Metadata_Trees_SCCII.xlsx*  is not part of this repository
- **`README.md`**: Documentation for the repository.
## License

This project is licensed under the [MIT License](LICENSE).

---