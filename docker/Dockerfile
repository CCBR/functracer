FROM nciccbr/ccbr_ubuntu_24.04:v1

# build time variables
ARG BUILD_DATE="000000"
ENV BUILD_DATE=${BUILD_DATE}
ARG BUILD_TAG="000000"
ENV BUILD_TAG=${BUILD_TAG}
ARG REPONAME="000000"
ENV REPONAME=${REPONAME}

ARG R_VERSION=4.5
ENV R_VERSION=${R_VERSION}

# install conda packages
RUN mamba install -y -c conda-forge \
    r-base=${R_VERSION} \
    r-argparse \
    r-covr \
    r-grDevices \
    r-igraph \
    r-jsonlite \
    r-knitr \
    r-lintr \
    r-readr \
    r-rmarkdown \
    r-roxygen2 \
    r-testthat \
    r-treesitter \
    r-treesitter.r \
  && conda clean -afy

# install R package
COPY . /opt2/functracer
RUN R -e "devtools::install_local('/opt2/functracer', dependencies = TRUE, repos='http://cran.rstudio.com', upgrade='never')" && \
  R -e "library(functracer); devtools::test('/opt2/functracer')" && \
  R -s -e "readr::write_csv(tibble::as_tibble(installed.packages()), '/data2/r-packages_functracer.csv')"

# add functracer exec to the path
RUN chmod -R +x /opt2/conda/lib/R/library/functracer/exec
ENV PATH="$PATH:/opt2/conda/lib/R/library/functracer/exec"
RUN functracer --help

# cleanup
WORKDIR /data2
RUN apt-get clean && apt-get purge \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
