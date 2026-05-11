#!/bin/bash

BLUE="\033[0;34m"
GREEN="\033[0;32m"
RED="\033[0;31m"
NC="\033[0m"

echo -e "${GREEN}==> Running make...${NC}"
make
echo -e "\n${GREEN}==> Running tests...${NC}"

failures=0
for f in test/*; do
  echo -e "\n${BLUE}--- $f ---${NC}"
  ./line "$f" || { echo -e "${RED}==> exited with $?${NC}"; ((failures++)); }
done

echo -e "\n${GREEN}==> Done. $failures failure(s).${NC}"
