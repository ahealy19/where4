#!/bin/bash
LOCATION=/usr/local/bin
WHY3=why3

while [[ $# -gt 1 ]]
do
key="$1"

case $key in
    -l|--location)
    LOCATION="$2"
    shift # past argument
    ;;
    -w|--why3name)
    WHY3="$2"
    shift # past argument
    ;;
    --default)
    DEFAULT=YES
    ;;
    *)
            # unknown option
    ;;
esac
shift # past argument or value
done
echo WHERE4	binary location 		= "${LOCATION}"
echo WHY3 	binary location			= "${WHY3}"

echo calling "why3 config --detect-provers (before uninstalling where4)"
${WHY3} config --detect-provers

sudo rm ${LOCATION}/where4

echo calling "why3 config --detect-provers (after uninstalling where4)"
${WHY3} config --detect-provers