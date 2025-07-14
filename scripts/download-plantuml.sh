#!/bin/bash

set -e
set -u

# Get the base directory of
BASE_DATA_DIR="$(dirname $(readlink -f "$0"))/../data"

if [[ -d $BASE_DATA_DIR ]]
then
    echo "Data directory at: $BASE_DATA_DIR"
else
    mkdir -p $BASE_DATA_DIR
fi

SHA256SUM=e34c12bbe9944f1f338ca3d88c9b116b86300cc8e90b35c4086b825b5ae96d24
FILENAME=plantuml.jar
URL=
https://github.com/plantuml/plantuml/releases/download/v1.2024.7/plantuml-1.2024.7.jar

# ======================
# plantuml
# ======================

process() {
    cd $BASE_DATA_DIR
    wget -c $URL -O $FILENAME
}

if [[ -e $BASE_DATA_DIR/$FILENAME ]]
then
    if echo "$SHA256SUM $BASE_DATA_DIR/$FILENAME" | sha256sum --check --status
    then
        echo "Ok, $FILENAME already exists, exit with success"
    else
        echo "$FILENAME already exists, sha256sum check failed, downloading ... "
        process
    fi
else
    echo "File not exist, downloading ... "
    process
fi
