#!/bin/bash

project_name=rasch
file_compose=docker-compose.yaml

# Stop
/usr/bin/docker compose -p $project_name -f /opt/slocal/dkvol1/$project_name/$file_compose stop