# Setup Guide

This guide provides step-by-step instructions for installing and running PlaViSca locally. For online access, visit <a href="https://xxxx.com">PlaViSca Web Application</a>.

## System Requirements

- **Operating System**: macOS, Windows, or Linux
- **R Version**: 4.5 or later ([Download R](https://cran.r-project.org/))
- **RStudio**: 2025 or later (recommended) ([Download RStudio](https://posit.co/download/rstudio-desktop/))
- **Git**: 2.43 or later ([Download Git](https://git-scm.com/downloads))
- **Pixi**: 0.43 or later ([Download pixi](https://pixi.sh/v0.43.1/))
- **RAM**: 16 GB minimum
- **CPU**: 4 cores minimum
- **Disk Space**: 5 GB free

## Installation Steps

PlaViSca is compatible with macOS, Windows, and Linux operating systems. Follow the steps below to set up the application on your system.

### 1. Clone the Repository

```bash
git clone https://github.com/Sopheap15/PlaViSca.git
```

This will create a directory named `PlaViSca` containing the application files.

### 2. Install Dependencies

First you need to have Pixi installed on your system. See the Pixi docs: https://pixi.sh/

```bash
pixi install
```

### 3. Run the Application

- **If using RStudio (desktop):**

  1. Navigate to project folder and open `app.R`
  2. Click the "Run App" button in the top-right corner of the script editor.

Or run the following command in the terminal:

```bash
pixi run PlaViSca
```

The application will launch in your default web browser.

## Troubleshooting

If you encounter issues during installation or running the app, refer to the following common problems and solutions:

- **App does not start**: ensure all dependencies are installed correctly. Check that your R version is 4.5 or later. Try restarting RStudio.
- **Data not loading**: verify that data files are placed in the `data/` directory with the correct names and formats (see User Manual for details).
- **Browser issues**: clear your browser cache or try a different browser. Ensure JavaScript is enabled.
- **Performance issues**: PlaViSca requires at least 16 GB RAM. Close other applications if needed.
- **Package installation errors**: On Linux, you may need to install system libraries (e.g., `sudo apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev`).

For additional help contact the development team.

## Development Team & Contact Information

For questions, feedback, or collaboration inquiries, please contact:

- **Sopheap Oeng**  
  _Bioinformatic System Engineer, Institut Pasteur du Cambodge_  
  📧 [osopheap@pasteur-kh.org](mailto:osopheap@pasteur-kh.org)

- **Brice Feufack Donfack**  
  _Postdoctoral researcher, Malaria Research Unit, Institut Pasteur du Cambodge_  
  📧 [idonfack@pasteur-kh.org](mailto:idonfack@pasteur-kh.org)

- **Anthony Ruberto**  
  _Postdoctoral Researcher, Center for Tropical and Emerging Global Diseases, University of Georgia, USA_  
  📧 [aruberto@uga.edu](mailto:aruberto@uga.edu)

- **Thomas Obadia**  
  _Bioinformatics and Biostatistcs Hub, Institut Pasteur, Paris, France_

  _G5 Infectious Diseases Epidemiology and Analytics, Institut Pasteur, Paris, France_

- **Giorgio Gonnella**  
  _Head, Bioinformatics and Artificial Intelligence Unit (BAIA), Institut Pasteur du Cambodge_  
  📧 [ggonnella@pasteur-kh.org](mailto:ggonnella@pasteur-kh.org)

## Source Code

PlaViSca is freely available under an open-source license. The authors maintain authority to update and improve the application. Developers and contributors are welcome to explore, modify, or extend the app by visiting the official GitHub repository:

🔗 [GitHub Repository – PlaViSca](https://github.com/Sopheap15/PlaViSca)

© 2025 BAIA Team, Institut Pasteur du Cambodge. All rights reserved.
