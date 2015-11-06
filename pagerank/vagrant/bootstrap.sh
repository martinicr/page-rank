#!/usr/bin/env bash

sudo apt-get update

sudo apt-get install -y git

sudo apt-get install -y wget

## Erlang
wget http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
sudo dpkg -i erlang-solutions_1.0_all.deb
sudo apt-get update
sudo apt-get install -y erlang

git clone https://github.com/martinicr/page-rank.git




