FROM i386/ubuntu:18.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update
RUN apt-get install -yq \
    build-essential \
    flex-old \
    bison \
    gdb \
    strace \
    ltrace \
    vim \
    csh

RUN mkdir -p /usr/class/cs143
COPY cool.tar.bz2 /tmp/
RUN tar -xjf /tmp/cool.tar.bz2 -C /usr/class/cs143
RUN ln -s /usr/class/cs143/cool/bin/ /usr/class/cs143/
