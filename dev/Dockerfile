# macnod/file-server

FROM ubuntu:latest

ENV DEBIAN_FRONTEND=noninteractive

#
# Install Roswell
# 
# BEGIN
#
ENV ROSWELL_REPO=https://github.com/roswell/roswell.git
ENV ROSWELL_SRC_DIR=/roswell-src
ENV ROSWELL_PREFIX=/roswell
ENV ROSWELL=/roswell/bin/ros
ENV PATH="/roswell/bin:$PATH"

RUN apt update && apt upgrade -y && apt install -y \
    ack \
    automake \
    build-essential \
    bzip2 \
    curl \
    emacs-nox \
    git \
    gnupg \
    libcurl4-openssl-dev \
    tar \
    tree \
    vim \
    zlib1g-dev

RUN git clone -b release $ROSWELL_REPO $ROSWELL_SRC_DIR
WORKDIR $ROSWELL_SRC_DIR
RUN chmod +x bootstrap
RUN ./bootstrap
RUN ./configure --prefix=$ROSWELL_PREFIX
RUN make
RUN make install
WORKDIR /
RUN rm -rf $ROSWELL_SRC_DIR
RUN $ROSWELL setup

#
# END
#

# 3rd-party packages
RUN $ROSWELL install postmodern && \
    $ROSWELL install uiop && \
    $ROSWELL install cl-ppcre && \
    $ROSWELL install hunchentoot && \
    $ROSWELL install swank && \
    $ROSWELL install spinneret && \
    $ROSWELL install trivial-utf-8 && \
    $ROSWELL install ironclad && \
    $ROSWELL install babel && \
    $ROSWELL install jose && \
    $ROSWELL install cl-csv && \
    $ROSWELL install cl-unicode && \
    $ROSWELL install cl-base64

# Stable macnod packages
RUN $ROSWELL install macnod/dc-ds && \
    $ROSWELL install macnod/dc-dlist

# Changing macnod packages
ARG CACHEBUST=1
RUN $ROSWELL install macnod/dc-eclectic && \
    $ROSWELL install macnod/rbac

# This project
RUN $ROSWELL install macnod/file-server/dev
