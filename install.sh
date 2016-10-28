#!/bin/bash
# Use -gt 1 to consume two arguments per pass in the loop (e.g. each
# argument has a corresponding value to go with it).
# Use -gt 0 to consume one or more arguments per pass in the loop (e.g.
# some arguments don't have a corresponding value to go with it such
# as in the --default example).

LOCATION=/usr/local/bin
WHY3=why3
DETECTION=/usr/local/share/why3/provers-detection-data.conf
DRIVERS=/usr/local/share/why3/drivers
REINSTALL=0
TREE=0
JSONPATH=forest.json

while [[ $# -gt 1 ]]
do
key="$1"

case $key in
    -l|--location)
    LOCATION="$2"
    shift # past argument
    ;;
    -t|--tree)
    TREE=1
    JSONPATH="$2"
    shift # past argument
    ;;
    -f|--forest)
    JSONPATH="$2"
    shift
    ;;
    -w|--why3name)
    WHY3="$2"
    shift # past argument
    ;;
    -p|--prover-detection)
    DETECTION="$2"
    shift # past argument
    ;;
    -d|--driver-location)
    DRIVERS="$2"
    shift # past argument
    ;;
    -r|--reinstall)
    REINSTALL=1
    ;;
    --default)
    DEFAULT=YES
    ;;
    *)        # unknown option
    ;;
esac
shift # past argument or value
done
echo WHERE4	binary location 		= "${LOCATION}"
echo WHY3 	binary location			= "${WHY3}"
echo prover-detect location 		= "${DETECTION}"
echo why3 drivers location			= "${DRIVERS}"
if [ "$TREE" -eq 1 ]; then
    echo tree path = "${JSONPATH}"
else
    echo forest path = "${JSONPATH}"
fi

if [ "$REINSTALL" -eq 1 ]; then
    ./uninstall.sh -l ${LOCATION} -w ${WHY3}
fi


printf "compiling treetypes interface..."

if ocamlfind ocamlc -c treetypes.mli;
    then echo done
    else echo fail; exit 1
fi
#echo done.

printf "compiling print_tree.ml..."
if ocamlfind ocamlc -g -thread -linkpkg -package yojson \
    print_tree.ml -o print_tree.native;
    then echo done. calling it.. 
    else echo fail; exit 1
fi

echo ./print_tree.native ${TREE} ${JSONPATH}
./print_tree.native ${TREE} ${JSONPATH}

printf "compiling tree.ml interface..."
if ocamlfind ocamlc -c tree.mli;
    then printf "done. Now its binary..."
    else echo fail; exit 1
fi

if ocamlfind ocamlc -g -thread tree.ml -o tree.native;
    then echo done
    else echo fail; exit 1
fi

printf "compiling mytermcode interface..."
if ocamlfind ocamlc -c -linkpkg -package why3 mytermcode.mli;
    then printf "done. Now its binary..."
    else echo fail; exit 1
fi

if ocamlfind ocamlc mytermcode.cmo -g -thread -linkpkg -package str \
    -package unix -package num -package dynlink -package menhirLib \
    -package why3 -package ocamlgraph mytermcode.ml -o mytermcode.native;
    then echo done
    else echo fail; exit 1
fi

printf "compiling get_predictions interface..."
if ocamlfind ocamlc -c  get_predictions.mli;
    then printf "done. Now its binary..."
    else echo fail; exit 1
fi

if ocamlfind ocamlc tree.cmo -g -thread get_predictions.ml \
    -o get_predictions.native;
    then echo done.
    else echo fail; exit 1
fi 

printf "compiling make_session interface..."
if ocamlfind ocamlc -c -linkpkg -package why3 make_session.mli;
    then printf "done. Now its binary..."
    else echo fail; exit 1
fi

if ocamlfind ocamlc make_session.cmo -g -thread -linkpkg -package str \
    -package unix -package num -package dynlink -package menhirLib \
    -package why3 -package ocamlgraph make_session.ml -o make_session.native;
    then echo done
    else echo fail; exit 1
fi

printf "compiling final where4 binary..."
if ocamlfind ocamlc mytermcode.cmo tree.cmo get_predictions.cmo \
    make_session.cmo -g -thread -linkpkg -package str -package unix \
    -package num -package dynlink -package menhirLib -package why3 \
    -package ocamlgraph where4.ml -o where4.native;
    then echo done 
    else echo fail; exit 1
fi

echo moving it to where why3 can find it: ${LOCATION}
sudo cp ./where4.native ${LOCATION}/where4

echo calling "why3 config --detect-provers (before adding where4)"
${WHY3} config --detect-provers

echo  calling "where4 --list-provers"
${LOCATION}/where4 --list-provers

echo copying 'where4.drv' to ${DRIVERS}
sudo cp ./where4.drv ${DRIVERS}/where4.drv

OLDCONF=`cat ${DETECTION}`

if grep -q '\[ATP where4\]' "$DETECTION"; then

	echo ${DETECTION} already contains where4 entry

else

	echo modifying ${DETECTION} with new where4 entry
	
	NEWCONF=`cat where4.conf`

	cat where4.conf ${DETECTION} > temp.txt
	sudo cp temp.txt ${DETECTION}
	rm temp.txt
fi

#echo adding where4 manually
#why3 config --detect --add-prover where4 ${LOCATION}/where4

echo calling "why3 config --detect-provers (after adding where4)"
${WHY3} config --detect-provers

