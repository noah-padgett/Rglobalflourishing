# .onLoad <- function(libname, pkgname)
# {
#   library.dynam("Rglobalflourishing", pkgname, libname)
# }

.onAttach <- function(lib, pkgname)
{
  version <- read.dcf(file = system.file("DESCRIPTION", package = pkgname),  fields = "Version")
  packageStartupMessage("This is ", paste(pkgname, version),
                        "\n", pkgname, " is FREE software! Please report any bugs.")
}
