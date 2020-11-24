#!/bin/bash
set -e
set -u

ROOT_PATH="`dirname \"\`realpath "$0"\`\"`"
TEST_PATH=$ROOT_PATH/tests

. "${TEST_PATH}/common.sh"

set +e
STTY_CONFIG=`stty -g 2>/dev/null`
$PYTHON_RUNNER -u "`get_path "$TEST_PATH/run_tests.py"`" --exclude "skip_${DETECTED_OS}" --robot-framework-remote-server-full-directory=$ROOT_PATH --robot-framework-remote-server-name=renode --css-file=$TEST_PATH/robot.css --runner=none -r . "$@"
RESULT_CODE=$?
set -e
if [ -n "${STTY_CONFIG:-}" ]
then
    stty "$STTY_CONFIG"
fi
exit $RESULT_CODE
