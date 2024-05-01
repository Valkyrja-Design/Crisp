#!/bin/bash

cabal build Crisp
cabal exec Crisp -- -s $@