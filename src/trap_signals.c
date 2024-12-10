/* trap_signal.c
 *
 */
#include <signal.h>
#include <stdio.h>

void doexit_handler(int sig)
{
  if (sig == 2 || sig == 19 || sig == 9) {
	exit (2);
  }
  exit(-1); 
}



#if defined (IBMRISC) || defined (__hpux)
void trap_signals
#elif defined (CRAY)
void TRAP_SIGNALS
#elif defined (linux)
void trap_signals__
#else
void trap_signals_
#endif
()
{
	signal(SIGINT, doexit_handler);
	signal(SIGSTOP, doexit_handler);
	signal(SIGKILL, doexit_handler);
	signal(SIGFPE, doexit_handler);
	signal(SIGILL, doexit_handler);
	signal(SIGSEGV, doexit_handler);
}








