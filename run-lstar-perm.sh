#!/usr/bin/env bash

# Example usage of how to run lstar against a non-interactive teacher. This
# script will create two fifos for the learner and teacher to communicate over.
# The communication is not visible, only output to stderr will be shown in
# the terminal

# safety flags, remove x if you don't like all the output
set -euxo pipefail

# create temporary directory, and names for the fifo queues (not files)
tempdir=$(mktemp -d run-lstar.temp.XXXXXX)
queryfifo="$tempdir/queries"
answerfifo="$tempdir/answers"

# find the binary for the learner and teacher.
# The haskell project must be built beforehard (cabal build all)
lstar=$(cabal list-bin ons-hs-lstar-perm)
teacher=$(cabal list-bin ons-hs-teacher)

# make the connection for the processes
mkfifo $queryfifo $answerfifo

# run the teacher in the background
$teacher < $queryfifo > $answerfifo &

# run the learning algorithm, measuring its time
time $lstar > $queryfifo < $answerfifo

# clean up
rm -r $tempdir
