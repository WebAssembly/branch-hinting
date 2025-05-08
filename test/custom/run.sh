#!/usr/bin/env bash

SCRIPT_DIR=$(dirname "$(readlink -f "$0")")

TESTS=$(ls ${SCRIPT_DIR}/*/*.wast)
exec "${SCRIPT_DIR}/../core/run.py" --opts '-ca ' $@ -- ${TESTS}
