# Gentle, pre-compiled R environment for FIFA Accuracy analysis
FROM rocker/tidyverse:4.3.3

# Install system libraries required by spatial R packages (terra, sf, rworldmap)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    && rm -rf /var/lib/apt/lists/*

# Set CRAN repository to Posit Package Manager for instant binary package installs (zero source compilation)
RUN echo 'options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"))' >> /usr/local/lib/R/etc/Rprofile.site

# Install required visualization packages using pre-compiled binaries
RUN R -e 'install.packages(c(\
    "scales", \
    "cowplot", \
    "gridExtra", \
    "ggthemes", \
    "hrbrthemes", \
    "viridis", \
    "RColorBrewer", \
    "ggrepel", \
    "gghighlight", \
    "ggridges", \
    "directlabels", \
    "ggtext", \
    "countrycode", \
    "janitor", \
    "reshape2", \
    "igraph", \
    "ggraph", \
    "tm", \
    "wordcloud", \
    "wordcloud2", \
    "SnowballC", \
    "rworldmap", \
    "emojifont", \
    "quantmod", \
    "packcircles", \
    "gifski", \
    "gganimate"\
))'

# Install GitHub packages
RUN R -e 'remotes::install_github("rensa/ggflags", upgrade = "never")'

WORKDIR /workspace

CMD ["Rscript", "scripts/manipulation-viz-script.R"]
