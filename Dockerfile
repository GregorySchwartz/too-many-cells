FROM fpco/stack-build:lts-11.4 as build
RUN mkdir /opt/build

# Copy dependencies.
COPY utility/birch-beer /opt/build/birch-beer
COPY utility/differential /opt/build/differential
COPY utility/hierarchical-spectral-clustering /opt/build/hierarchical-spectral-clustering
COPY utility/modularity /opt/build/modularity
COPY utility/spectral-clustering /opt/build/spectral-clustering
COPY utility/eigen-sparse-utils /opt/build/eigen-sparse-utils

# Copy source.
COPY single_cell/too-many-cells /opt/build

# Replace dependency location and install.
RUN cd /opt/build \
    && rm -rf .stack-work \
    && sed -i "s/\/home\/gw\/code\/utility/\./" stack.yaml \
    && stack build --system-ghc

FROM ubuntu:16.04
RUN mkdir -p /opt/too-many-cells
ARG BINARY_PATH
WORKDIR /opt/too-many-cells
RUN apt-get update && apt-get install -y apt-transport-https \
    && echo "deb https://cran.cnr.berkeley.edu/bin/linux/ubuntu trusty/" | tee -a /etc/apt/sources.list \
    && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9 \
    && apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates \
        libgmp-dev \
        build-essential \
        libblas-dev \
        liblapack-dev \
        libgsl-dev \
        libgtk2.0-dev \
        libcairo2-dev \
        libpango1.0-dev \
        graphviz \
        r-base \
        r-base-dev

RUN R -e "install.packages(c('cowplot', 'ggplot2'), repos='http://cran.us.r-project.org/')" \
    && Rscript -e "source('http://bioconductor.org/biocLite.R')" -e "biocLite('edgeR')"
# NOTICE THIS LINE
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-11.4/8.2.2/bin .
#COPY static /opt/too-many-cells/static
#COPY config /opt/too-many-cells/config
ENTRYPOINT ["/opt/too-many-cells/too-many-cells"]