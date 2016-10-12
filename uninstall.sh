#!/bin/bash
# Use -gt 1 to consume two arguments per pass in the loop (e.g. each
# argument has a corresponding value to go with it).
# Use -gt 0 to consume one or more arguments per pass in the loop (e.g.
# some arguments don't have a corresponding value to go with it such
# as in the --default example).
# note: if this is set to -gt 0 the /etc/hosts part is not recognized ( may be a bug )
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