# Where4
## Andrew Healy
### [Principles of Programming](http://www.cs.nuim.ie/research/pop/) Research Group, Dept. of Computer Science, Maynooth University, Ireland

## Requirements

Where4 has been developed using OCaml 4.02.3, version 0.87.1 of Why3, on a machine running Ubuntu 16.04.

Compiling Where4 requires that the Why3 OCaml API is installed.
According to the Why3 installation instructions [here](http://why3.lri.fr/doc-0.87.2/manual006.html#sec40), the user must call
`make byte opt; make install-lib (as super-user)`
after installing the main Why3 tool.

The only other OCaml library we use is [Yojson](http://mjambon.com/yojson.html) for parsing the JSON file.

## Installation

Move the `where4` directory to a writable location and call `./install.sh`. The shell script is self documenting in a way: it contains all the commands needed to the link the Why3 API and build the Where4 binary.

The following are options that can be supplied to this call to control the installation of Where4.

- `--location / -l PATH` By default, where will be copied to the `/usr/local/bin/` directory. If the user wishes, this location can be overridden to be `PATH`. The given directory must be on the user's `$PATH` environment variable to be found by Why3.
- `--why3name / -w PATH` The location of the Why3 binary, if it is not on the user's `$PATH` can be supplied. Where4 is added to Why3's list of provers by calling `why3 config --detect-provers` at the end of the installation process.
- `--prover-detection / -p PATH` By default, Why3's `provers-detection-data.conf` file is assumed to be located in `/usr/local/share/why/`. The location of this file can be specified as `PATH`.
- `--driver-location / -d PATH` The location of the Why3 driver files. Where4 needs to know where to copy `where4.drv` so that Why3 will be able to find it.
- `--reinstall / -r` Delete the installed binary (i.e. execute `uninstall.sh` and repeat the installation process (the Where4 entry in `provers-detection-data.conf` will not be deleted, however). This option can be combined with the above flags.   

By default, Where4 assumes there is a JSON file called `forest.json` in the current directory which is to be used to construct the prediction model.
The user can control this behaviour with the following flags.
As a random forest is just an array of decision trees, a JSON file containing a single tree may be provided instead, if it is specified as such during installation.

- `--forest / -f PATH` Use the JSON file located at `PATH` to construct the prediction model. This file should define a Random Forest using the JSON schema as shown in Fig. 5.1.
- `--tree / -t PATH` Use the JSON file located at `PATH` to construct the prediction model. This file should define a Decision Tree using the JSON schema as shown in Fig. 5.1.

## Usage

The following options can be appended to calls to `where4` on the command-line:

- `--help / -h` Print a concise version of these notes to the console.
- `--version` Print Where4's version number. This command is used by Why3 to determine if a supported version of Where4 is installed.
- `--list-provers / -l` Print each SMT solver known to Where4 and `found` if Where4 has determined it is installed locally; `NOT found` otherwise.

The following two options, `predict` and `prove`, must be followed by `FILENAME`: a __relative__ path to a `.why` (Why logic language) or `.mlw` (WhyML programming language) file.

- `predict FILENAME` Print the predicted ranking of solver utility for each PO in `FILENAME`. The ranking will consist of all 8 solvers known to Where4 whether they are installed locally or not.
- `predict FILENAME` Call the pre-solver, then each __installed__ solver in the predicted ranking sequentially, for each PO in `FILENAME`.
\
If the Why3 configuration file has been moved to a location other than its default (i.e. `\$HOME/.why3.conf`), the following option is necessary in order for `where4 prove` or `where4 predict` to function correctly:

- `--config / -c CONFIGPATH` Specify the path to Why3's `.why.conf` configuration file as `CONFIGPATH`. Use the default location (`$HOME/.why3.conf`) otherwise.

A number of optional parameters can be appended to the `prove FILENAME` command (their order is unimportant):

- `--verbose` Print the result of each prover call and the time it took. The default behaviour is to just print out the best result and the cumulative time (see Alg. 3).
- `--why`	A special flag for use with the Why3 driver which tells Where4 to convert the given absolute filename to a relative path.   
- `--time / -tm TIME` Override the default (5 seconds) timeout value with `TIME` number of seconds. `TIME` must be an integer; an error message will be printed otherwise.
- `--threshold / -ts THRESH` Use a __cost threshold__ (see Sec. 6.1.1) to limit the number of solvers called based on their predicted ranking. We found in Chapter 6 that a `THRESH` value of 7 is optimum for the test set. `THRESH` must be parsable as a floating-point number; an error message will be printed otherwise.
