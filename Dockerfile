# macnod/file-server

FROM ubuntu:latest

ENV DEBIAN_FRONTEND=noninteractive

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

RUN $ROSWELL install macnod/dc-ds
RUN $ROSWELL install macnod/dc-dlist
RUN $ROSWELL install macnod/dc-eclectic
RUN $ROSWELL install macnod/file-server
RUN $ROSWELL install postmodern
RUN $ROSWELL install uiop
RUN $ROSWELL install cl-ppcre
RUN $ROSWELL install hunchentoot
RUN $ROSWELL install swank
