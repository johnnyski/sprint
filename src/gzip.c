#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

/* Prototype definitions within this file. */
int no_command (char *cmd);
FILE *uncompress_pipe (FILE *fp);
FILE *compress_pipe (FILE *fp);

int no_command (char *cmd)
{
  int rc;
  /* Return 0 if there is the command 'cmd' on the system. */
  /* Return !0 otherwise. */
  rc = system(cmd);
  if (rc == 0) return rc;
  else return !0;
}

FILE *uncompress_pipe (FILE *fp)
{
  /* Pass the file pointed to by 'fp' through the gzip pipe. */

  FILE *fpipe;

  if (no_command("gzip --version > /dev/null 2>&1")) return fp;
  close(0); /* Redirect stdin for gzip. */
  dup(fileno(fp));

  fpipe = popen("gzip -d -f --stdout", "r");
  if (fpipe == NULL) perror("uncompress_pipe");
  return fpipe;
}

FILE *compress_pipe (FILE *fp)
{
  /* Pass the file pointed to by 'fp' through the gzip pipe. */

  FILE *fpipe;

  if (no_command("gzip --version > /dev/null 2>&1")) return fp;
  fflush(NULL); /* Flush all buffered output before opening this pipe. */
  close(1); /* Redirect stdout for gzip. */
  dup(fileno(fp));

  fpipe = popen("gzip -c", "w");
  if (fpipe == NULL) perror("compress_pipe");
  return fpipe;
}


