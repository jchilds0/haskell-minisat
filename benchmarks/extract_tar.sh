#!/bin/bash

# Directory containing the tar files
TARGET_DIR=$1

# Check if the directory is provided and exists
if [[ -z "$TARGET_DIR" ]]; then
  echo "Usage: $0 /path/to/target/directory"
  exit 1
fi

if [[ ! -d "$TARGET_DIR" ]]; then
  echo "The specified directory does not exist: $TARGET_DIR"
  exit 1
fi

# Loop through each tar file in the target directory
for tar_file in "$TARGET_DIR"/*.tar.gz; do
  # Check if there are any tar files
  if [[ ! -e "$tar_file" ]]; then
    echo "No tar files found in the directory: $TARGET_DIR"
    exit 0
  fi

  # Extract the base name of the tar file (without the path and extension)
  base_name=$(basename "$tar_file" .tar.gz)
  
  # Create a directory with the base name
  mkdir -p "$TARGET_DIR/$base_name"
  
  # Extract the tar file into the newly created directory
  tar -xf "$tar_file" -C "$TARGET_DIR/$base_name"
  
  echo "Extracted $tar_file into $TARGET_DIR/$base_name"
done

echo "All tar files have been processed."
