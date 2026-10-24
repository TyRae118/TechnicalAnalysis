# Install the packages needed for the assignment
packages <- c("quantmod", "TTR", "zoo")

for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, repos = "https://cloud.r-project.org")
  }
}

# Display installed versions
for (package in packages) {
  cat(package, "version:",
      as.character(packageVersion(package)), "\n")
}
