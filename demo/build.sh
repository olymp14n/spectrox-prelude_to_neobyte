#!/bin/bash

# Replaces included dependencies in main file and writes to a new output file.

input_file="demo.lua"
output_file="demo_dist.lua"

# Dependency source paths.
core_dir="../_core"
res_dir="./res"

awk -v core_dir="$core_dir" -v res_dir="$res_dir" '
{
    if ($0 ~ /^require "([^"]+)"$/) {
        match($0, /^require "([^"]+)"$/, arr)
        file_base = arr[1]
        # Set source path according to prefix.
        if (file_base ~ /^core/) {
            include_file = core_dir "/" file_base ".lua"
        } else if (file_base ~ /^res/) {
            include_file = res_dir "/" file_base ".lua"
        } else {
            include_file = file_base ".lua"
        }
        # Include file if it exists.
        if (system("[ -f \"" include_file "\" ]") == 0) {
            while ((getline line < include_file) > 0) {
                print line
            }
            close(include_file)
        } else {
            print "[ERROR] File not found: " include_file > "/dev/stderr"
            exit 1
        }
    } else {
        print
    }
}' "$input_file" > "$output_file"

# Remove package inclusions in output file.
sed -i '/package.path.*/d' "$output_file"