#!/bin/bash

BLUE="\033[0;34m"
GREEN="\033[0;32m"
NC="\033[0m"

set -e
echo -e "${GREEN}==> Running make...${NC}"
make
echo -e "\n${GREEN}==> Running tests...${NC}"

for f in test/*; do
  echo -e "\n${BLUE}--- $f ---${NC}"
  ./line "$f"
done
