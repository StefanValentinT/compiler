import os

# Change this to the directory you want to scan
directory = "./"

# Iterate through all files in the directory and subdirectories
for root, dirs, files in os.walk(directory):
    for file in files:
        if file.endswith(".c"):
            file_path = os.path.join(root, file)
            
            # Read the file
            with open(file_path, "r") as f:
                lines = f.readlines()
            
            # Filter out lines starting with #
            new_lines = [line for line in lines if not line.lstrip().startswith("#")]
            
            # Write the filtered lines back to the file
            with open(file_path, "w") as f:
                f.writelines(new_lines)
            
            print(f"Processed {file_path}")
