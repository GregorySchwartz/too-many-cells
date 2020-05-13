FROM fpco/stack-build:lts-12.0 as build

# Make sure /tmp has correct permission and upgrade gcc version.
RUN chown root:root /tmp \
    && chmod 1777 /tmp \
    && chown root:root /root/.stack \
    && chmod 1777 /root/.stack \
    && apt-get update \
    && apt-get install -y software-properties-common \
    && add-apt-repository ppa:ubuntu-toolchain-r/test \
    && apt-get update \
    && apt-get install -y gcc-8 g++-8 gfortran-8 \
    && update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-8 60 --slave /usr/bin/g++ g++ /usr/bin/g++-8 --slave /usr/bin/gfortran gfortran /usr/bin/gfortran-8

RUN mkdir /opt/build

# Copy source.
COPY ./ /opt/build

# Replace dependency location and install.
RUN cd /opt/build \
    && rm -rf .stack-work \
    && stack build --system-ghc

FROM ubuntu:16.04
RUN mkdir -p /opt/too-many-cells
ARG BINARY_PATH
WORKDIR /opt/too-many-cells
RUN apt-get update && apt-get install -y apt-transport-https software-properties-common \
    && echo "deb https://cran.cnr.berkeley.edu/bin/linux/ubuntu trusty/" | tee -a /etc/apt/sources.list \
    && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9 \
    && add-apt-repository ppa:ubuntu-toolchain-r/test \
    && apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates \
        locales \
        libgmp-dev \
        build-essential \
        libblas-dev \
        liblapack-dev \
        libgsl-dev \
        libgtk2.0-dev \
        libcairo2-dev \
        libpango1.0-dev \
        graphviz \
        gcc-8 \
        g++-8 \
        gfortran-8 \
        r-base \
        r-base-dev \
    && update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-8 60 --slave /usr/bin/g++ g++ /usr/bin/g++-8 --slave /usr/bin/gfortran gfortran /usr/bin/gfortran-8

# Set the locale
RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

RUN R -e "install.packages(c('cowplot', 'ggplot2', 'jsonlite'), repos='http://cran.us.r-project.org/')" \
    && Rscript -e "source('http://bioconductor.org/biocLite.R')" -e "biocLite('edgeR')"
# NOTICE THESE LINES
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-12.0/8.4.3/bin .
RUN mkdir -p /opt/build/.stack-work/install/x86_64-linux/lts-12.0/8.4.3/share
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-12.0/8.4.3/share /opt/build/.stack-work/install/x86_64-linux/lts-12.0/8.4.3/share
#COPY static /opt/too-many-cells/static
#COPY config /opt/too-many-cells/config

ENTRYPOINT ["/opt/too-many-cells/too-many-cells"]
