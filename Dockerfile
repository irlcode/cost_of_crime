FROM ubuntu:20.04
LABEL org.opencontainers.image.authors="ipp@eu.spb.ru"
MAINTAINER "IRL" "ipp@eu.spb.ru"
ENV DEBIAN_FRONTEND noninteractive
ENV CRIMECOST_PATH .
RUN apt-get update && apt-get -y upgrade
RUN apt-get install -y r-base r-base-dev cmake
ADD . /cost_of_crime
WORKDIR /cost_of_crime
RUN Rscript code/helper_functions/install_dependencies.r
