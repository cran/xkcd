## Package startup hooks
## Provide an opt-in automatic font loader so users can enable automatic
## registration of system fonts via the `extrafont` package.

.onAttach <- function(libname, pkgname) {
	# Option to opt in to automatic font loading. Default is FALSE to avoid
	# side-effects during library() by default.
	opt <- getOption("xkcd.auto_load_fonts", default = FALSE)
	if (isTRUE(opt)) {
		if (requireNamespace("extrafont", quietly = TRUE)) {
			tryCatch({
				# Attempt to load fonts for common devices quietly; users can override
				# by setting the device argument themselves.
				extrafont::loadfonts(device = "win", quiet = TRUE)
				extrafont::loadfonts(device = "pdf", quiet = TRUE)
			}, error = function(e) {
				packageStartupMessage("xkcd: automatic font loading requested but failed: ", conditionMessage(e))
			})
		} else {
			packageStartupMessage("xkcd: automatic font loading requested but 'extrafont' is not installed")
		}
	}
}

## End of zzz.R