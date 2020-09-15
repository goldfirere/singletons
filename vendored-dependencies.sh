#!/bin/bash

set -e

PATCHES=patches
VENDORED=vendored

function clone() {
  REPO=$1
  COMMIT=$2

  # Clone
  CUR_DIR=$(pwd)
  REPO_PROJECT_NAME=$(basename "${REPO}")
  CHECKOUT_DIR="${VENDORED}/${REPO_PROJECT_NAME}"
  if [ ! -d "${CHECKOUT_DIR}" ]; then
    git clone "git@github.com:${REPO}" "${CHECKOUT_DIR}" && \
      cd "${CHECKOUT_DIR}" && \
      git reset --hard "${COMMIT}" && \
      git apply "../../${PATCHES}/${REPO_PROJECT_NAME}.patch" && \
      cd "${CUR_DIR}"
  fi
}

mkdir -p ${VENDORED}
clone "glguy/th-abstraction"            "82ffd01498c5b43478d2607911ed308686ace218"
clone "DanielSchuessler/th-expand-syns" "5d41fb524a631fa2c087207701822f4e6313f547"
clone "goldfirere/th-desugar"           "79d856015b6125b397809ed0df52a1f6a952bce9"
