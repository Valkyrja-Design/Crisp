#!/bin/bash

cabal build 
cabal exec Crisp -- -s $@