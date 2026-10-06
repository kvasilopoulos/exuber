local({
	# Return early if we don't want to activate it, eg a self-contained script.
	if (nzchar(Sys.getenv("RV_NO_ACTIVATE"))) {
		return()
	}
	if (!nzchar(Sys.which("rv.exe"))) {
		warning(
			"rv is not installed! Install rv, then restart your R session",
			call. = FALSE
		)
		return()
	}
	rv_info_args <- c("info", "--library", "--r-version", "--repositories", "--sandbox")
	run_rv_info <- function(args) {
		suppressWarnings(system2("rv.exe", args, stdout = TRUE))
	}
	rv_info <- run_rv_info(rv_info_args)
	# A project using the sandbox config field already requires a sandbox-aware rv.
	# This fallback lets older rv versions keep working when sandboxing is only
	# requested (or left unset) through the environment.
	if (!is.null(attr(rv_info, "status"))) {
		rv_info_help <- suppressWarnings(system2(
			"rv.exe", c("info", "--help"), stdout = TRUE, stderr = TRUE
		))
		if (
			is.null(attr(rv_info_help, "status")) &&
			!any(grepl("--sandbox", rv_info_help, fixed = TRUE))
		) {
			rv_info_args <- rv_info_args[rv_info_args != "--sandbox"]
			rv_info <- run_rv_info(rv_info_args)
		}
	}
	if (!is.null(attr(rv_info, "status"))) {
		# if system2 fails it'll add a status attribute with the error code
		warning(
			paste(c("failed to run rv info:", rv_info), collapse = "\n"),
			call. = FALSE
		)
		return()
	}
	get_val <- function(prefix) {
		line <- grep(paste0("^", prefix, ":"), rv_info, value = TRUE)
		sub(paste0("^", prefix, ":\\s*"), "", line)
	}

	# Set repos option
	repo_str <- get_val("repositories")

	repo_parts <- strsplit(repo_str, "), ", fixed = TRUE)[[1]]
	repo_parts <- gsub("[()]", "", repo_parts)

	repo_urls <- character(length(repo_parts))
	repo_names <- character(length(repo_parts))

	for (i in seq_along(repo_parts)) {
		parts <- strsplit(repo_parts[i], ",", fixed = TRUE)[[1]]
		repo_names[i] <- trimws(parts[1])
		repo_urls[i] <- trimws(parts[2])
	}
	names(repo_urls) <- repo_names
	options(repos = repo_urls)

	# Check R version and set library
	rv_r_ver <- get_val("r-version")
	sys_r <- sprintf("%s.%s", R.version$major, R.version$minor)
	r_match <- grepl(paste0("^", rv_r_ver), sys_r)

	rv_lib <- if (r_match) {
		normalizePath(get_val("library"), mustWork = FALSE)
	} else {
		message(sprintf(
			"WARNING: R version specified in config (%s) does not match session version (%s).
rv library will not be activated until the issue is resolved. Entering safe mode...
			",
			rv_r_ver,
			sys_r
		))
		file.path(tempdir(), "__rv_R_mismatch")
	}

	if (!dir.exists(rv_lib)) {
		if (r_match) {
			message("creating rv library: ", rv_lib, "\n")
		} else {
			message("creating temporary library: ", rv_lib, "\n")
		}
		dir.create(rv_lib, recursive = TRUE)
	}

	# System library sandbox: when the project enables it, rv info returns a path
	# to a library containing only base + recommended packages. Repoint `.Library`
	# (and empty `.Library.site`) so packages installed in the system library
	# cannot leak into the project.
	sandbox <- if (r_match) get_val("sandbox") else character()
	sandbox_active <- length(sandbox) == 1 && nzchar(sandbox)
	if (sandbox_active) {
		sandbox_active <- isTRUE(tryCatch({
			env <- baseenv()
			if (bindingIsLocked(".Library", env)) unlockBinding(".Library", env)
			assign(".Library", sandbox, envir = env)
			lockBinding(".Library", env)
			if (bindingIsLocked(".Library.site", env)) unlockBinding(".Library.site", env)
			assign(".Library.site", character(), envir = env)
			lockBinding(".Library.site", env)
			TRUE
		}, error = function(e) FALSE))
	}

	.libPaths(rv_lib, include.site = FALSE)
	Sys.setenv("R_LIBS_USER" = rv_lib)
	Sys.setenv("R_LIBS_SITE" = if (sandbox_active) {
		paste(rv_lib, sandbox, sep = .Platform$path.sep)
	} else {
		rv_lib
	})

	# Results
	if (interactive()) {
		message(
			"rv repositories active!\nrepositories: \n",
			paste0(
				"  ",
				names(getOption("repos")),
				": ",
				getOption("repos"),
				collapse = "\n"
			),
			"\n"
		)
		if (sandbox_active) {
			message("rv system library sandbox active:\n  ", sandbox, "\n")
		}
		message(
			if (r_match) {
				"rv libpaths active!\nlibrary paths: \n"
			} else {
				"rv libpaths are not active due to R version mismatch. Using temp directory: \n"
			},
			paste0("  ", .libPaths(), collapse = "\n")
		)
	}
})
