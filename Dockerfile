FROM rocker/tidyverse:4.0.5

LABEL maintainer="Guillem Hurault <guillem.hurault@hotmail.fr>"

# system libraries
RUN apt update && \
    apt install -y --no-install-recommends \
    # For v8
    libv8-dev \
    # Optional
    libxt6
    # for haven
    #zlib1g-dev \
    # for png
    #libpng-dev

# Create a user variable
ENV USER=rstudio

# Create project directory and set it as working directory
WORKDIR /home/$USER/IRR-eczema-images

# Install R packages to local library using renv
COPY [".Rprofile", "renv.lock", "./"]
COPY renv/activate.R ./renv/
RUN chown -R rstudio . \
 && sudo -u rstudio R -e 'renv::restore(confirm = FALSE)'
