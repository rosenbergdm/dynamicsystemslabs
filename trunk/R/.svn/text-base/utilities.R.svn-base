#!/usr/bin/env rr
# encoding: utf-8

.TARGET_HOST <- 'rosenbergdm.uchicago.edu';
.USERNAME <- 'dynamicsystems';
.PASSWORD <- 'ABSLmsdr19&!';

.collectInfo <- function() {
  historyFile <- tempfile();
  workspaceFile <- tempfile();
  savehistory(file=historyFile);
  cat(Sys.info(), file=historyFile, append=TRUE);
  sink(file=historyFile, append=TRUE);
  traceback();
  sink(NULL);
  save.image(file=workspaceFile);
  file.rename(workspaceFile, paste(workspaceFile, "RData", sep="."));
  file.rename(historyFile, paste(historyFile, "txt", sep="."));
  return(c(historyFile, workspaceFile));
}

.transmitFile <- function(local_file, target_file, host_login) {
  target_uri <- paste('ftp://', host_login$username, ":", host_login$password,
                      "@", host_login$hostname, "/", target_file, sep="");
  tryCatch(
    ftpUpload(file.path(local_file), target_uri),
    error=return(-1);
  );
  return(1);
}

.prepareSubmission <- function(file_vector, username=.USERNAME,
                               password=.PASSWORD, host=.TARGET_HOST) {
  student_name <- readline(prompt="Please enter your name: ");
  student_name <- gsub("[^A-Za-z]", "_", student_name);
  current_time <- Sys.time();
  current_time <- gsub("[^A-Za-z0-9]", "_", current_time);
  cat("\nPlease enter a description.  Press C-d (control-'d') when complete.\n");
  student_message <- readLines(con=stdin());
  message_file <- paste(tempfile(), "txt", sep="");
  cat(student_message, con=message_file);
  file_vector <- c(file_vector, message_file);

  host_login <- list(username=username, password=password, host=host);
  target_files <- character(length=length(file_vector));
  for (ii in seq(along=file_vector)) {
    filename <- paste(student_name, current_time, as.character(ii), sep=".");
    file_ext <- gsub(".*(\\.*)", "\\1", file_vector[ii]);
    filename <- paste(filename, file_ext, sep="");
    target_vector[ii] <- filename;
  }

  result <- list(host_login=host_login, local_files=file_vector,
    target_files=target_vector);

  return(result);
}

submitErrorToTa <- function() {
  confirmation <- readline(prompt="\nSubmit an error to your TA? [y/N]: ");
  if (!(confirmation %in% c('y', 'Y', 'yes', 'Yes', 'YES'))) {
    cat('\nSubmission aborted.\n');
    return();
  }

