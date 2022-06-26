# Base image
FROM ubuntu:20.04

# Maintainer tag
LABEL org.opencontainers.image.authors="ipp@eu.spb.ru"

# Disable interactions with shell, always accept default option
ENV DEBIAN_FRONTEND noninteractive

# Set project path
ENV CRIMECOST_PATH .

# Install packages
RUN apt-get update && apt-get upgrade && apt-get update
RUN apt-get instal-y l r-base r-base-dev cmake locales

# Set locale, unicode is required for plots with cyrillic lables
RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8

# Copy project direcroty
ADD . /cost_of_crime

# Set working directory
WORKDIR /cost_of_crime

# Install R dependencies
RUN Rscript code/helper_functions/install_dependencies.r
