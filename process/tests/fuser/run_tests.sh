#!/bin/bash

run_tests() {
  for i in {1..1000}
  do
    echo "Running cargo test: Attempt $i"
    cargo test
    if [ $? -ne 0 ]; then
      echo "Test failed on attempt $i"
      exit 1  # Exit immediately if the tests fail
    fi
    # sleep 1
  done
}

run_tests

echo "All 1000 test runs completed successfully."
