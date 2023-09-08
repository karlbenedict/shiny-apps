#!/usr/bin/env bash
# shell script for starting a R Shiny docker container that mounts a local
# host folder containing shiny apps under development. 
#
# The source docker image is based upon the Rocker shiny-verse  tagged
# docker image. Documentation for the source image can be found here:
# https://rocker-project.org/images/versioned/shiny.html

# unless the running user is root this script needs to be run using sudo
# by a user with sudo'ers permissions

# To-do:
# 	setup data backup process for mounted data volume. 

# -d \										# run in daemon mode
# --rm \									# remove the image when container stopped
# --platform linux/amd64 \					# work around Apple silicon support issue
# --name shiny \							# name the container 'shiny'
# -p 3838:3838 \							# map standard shiny port
# -v "$(pwd)"/apps:/srv/shiny-server/apps \		# mount the host 'apps' directory to the default applications folder in the shiny container
# -v shiny-data:/srv/shiny-server/data \	# mount persistant 'shiny-data' volume into the /srv/shiny-server/data dir in container
# rocker/shiny-verse						# use the Rocker shiny-verse source image
docker run -d --rm --platform linux/amd64 --name shiny -p 3838:3838 -v $(pwd)/apps:/srv/shiny-server/apps -v shiny-data:/srv/shiny-server/data rocker/shiny-verse
	
