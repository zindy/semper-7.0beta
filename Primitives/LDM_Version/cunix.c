/*
 *    C code for Semper's UNIX command
 *
 *    Written:  Frank Suess  -  7th January 1993.
 *
 *    Modified: Frank Suess  -  22nd April 1994.
 *              Frank Suess  -  16th June 1994.
 *              Frank Suess  -  19th May 1995.
 *
 *    Fortran interface:
 *
 *    LOGICAL FUNCTION CUNIX ( COMMAND )
 *
 *    CHARACTER*(*) COMMAND
 *
 *    where COMMAND = command string executed.
 *
 *    CUNIX returns .FALSE. if the shell/command returns without error.
 */

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <signal.h>
#include <stdlib.h>
#include <fcntl.h>
#include <inttypes.h>

#include "ftypes.h"

#ifndef NULL
#define NULL 0
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef RECLEN
#define RECLEN 200
#endif

#define sembrk_F77 F77_FUNC(sembrk,SEMBRK)
#define semcon_F77 F77_FUNC(semcon,SEMCON)
#define waits_F77  F77_FUNC(waits,WAITS)

#ifdef ANSI_C
F_logical semcon_F77 ( char *, int32_t );
F_logical sembrk_F77 ( void );
void waits_F77 ( F_real * );
#else
F_logical semcon_F77 ( );
F_logical sembrk_F77 ( );
void waits_F77 ( );
#endif

/* Patch for SIGCLD not being present in all systems */
#ifndef SIGCLD
#define SIGCLD SIGCHLD
#endif


static int child_alive;
static int child_status;

/*  Signal handler for death of child  */

void sig_child ( )
{
    signal ( SIGCLD, SIG_IGN );

    wait ( &child_status );

    signal ( SIGCLD, sig_child );

    child_alive = FALSE;
}

logicalfunction cunix_ ( COMMAND, string_length )
/*===============================================*/

char* COMMAND;
int32_t  string_length;
{
    int    pid;
    int    i;
    int    fd;
    int    nfds;
    int    nb;
    char  *command;
    void (*sig_status) ( );
    F_real wait_time = 0.1;
    int    pfd [ 2 ];
    char   buffer [ RECLEN ];
    int32_t   nbuf = 0;
    int    error_flag = FALSE;

/*  Create a pipe  */

    if ( pipe ( pfd ) ) return ( F_TRUE );

/*  Fork into separate parent and child processes  */

    pid = fork ( );

/*  Parent section of fork() call  */

    if ( pid )
    {

/*  Check for failure of fork() call  */

        if ( pid < 0 ) return ( F_TRUE );

/*  Set read-only end of pipe for non-blocking i/o  */

        fcntl ( pfd [ 0 ], F_SETFL, O_NDELAY );

/*  Close write-only end of pipe  */

        close ( pfd [ 1 ] );

/*  Flag existence of child  */

        child_alive = TRUE;

/*  Set up signal handler for death of child  */

        sig_status = signal ( SIGCLD, sig_child );

/*  Wait for termination of child process  */

        while ( child_alive )
        {

/*  Transfer contents of pipe to Semper's console output stream  */

            if ( flush_pipe ( pfd [ 0 ], buffer, &nbuf, &error_flag ) )
            {

/*  Send signal to kill child if break or error detected  */

                kill ( -pid, SIGKILL );
            }

/*  Sleep for a while  */

            waits_F77 ( &wait_time );
        }

/*  Restore signalling  */

        signal ( SIGCLD, sig_status );

/*  Transfer anything left in pipe  */

        flush_pipe ( pfd [ 0 ], buffer, &nbuf, &error_flag );

/*  Return with exit status of child  */

        if ( WIFEXITED ( child_status ) )
        {
            if ( ! WEXITSTATUS ( child_status ) )
            {

/*  Return status = .FALSE. only if child process exits with zero status  */

                return ( F_FALSE );
            }
        }

        return ( F_TRUE );
    }

/*  Child section of fork() call  */

    else
    {

/*  Make child a new process group leader  */

        setsid ( );

/*
    Close all file descriptors except standard input, standard output,
    standard error and write-only end of pipe  */

        nfds = getdtablesize ( );

        for ( fd = 3; fd < nfds; fd++ )
        {
            if ( fd != pfd [ 1 ] ) close ( fd );
        }

/*  Attach standard input to /dev/null to force non-interactive operation  */

        fd = open ( "/dev/null", 0 );

        if ( fd == -1 ) exit ( TRUE );

        close ( 0 );

        dup ( fd );

        close ( fd );

/*  Attach standard output to pipe  */

        close ( 1 );

        dup ( pfd [ 1 ] );

        close ( pfd [ 1 ] );

/*  Search for first non-blank character in command string  */

        for ( nb = 0; nb < string_length; nb++ )
        {
            if ( COMMAND [ nb ] != ' ' ) break;
        }

/*  Set up Unix command string as null-terminated string  */

        command = (char *) malloc ( string_length - nb + 1 );

        if ( command == (char *) NULL ) exit ( TRUE );

        for ( i = 0; nb < string_length; i++, nb++ )
        {
            command [ i ] = COMMAND [ nb ];
        }

        command [ i ] = '\0';

/*  Start up a new C shell to run the command  */

        execlp ( "/bin/csh", "csh", "-e", "-f", "-c", command, (char *) NULL );

/*  execlp() returns only on failure, so exit with non-zero status  */

        exit ( TRUE );
    }
}

/*  Copy contents of pipe to Semper's console output stream  */

int flush_pipe ( pipe, buffer, nbuf, error_flag )

int   pipe;
char *buffer;
int   *nbuf;
int  *error_flag;
{
    int32_t npipe;
    int  i, n;
    char data [ 1024 ];
    int  status = FALSE;

/*  Transfer contents of pipe to Semper's console output stream  */

    n = read ( pipe, data, 1024 );

    for ( i = 0; i < n; i++ )
    {
        if ( data [ i ] == '\n' )
        {
            if ( *nbuf == 0 )
            {
                buffer [ 0 ] = ' ';
                *nbuf = 1;
            }

            if ( ! *error_flag )
            {
                if ( semcon_F77 ( buffer, *nbuf ) == F_TRUE )
                {
                    *error_flag = TRUE;

                    status = TRUE;
                }
            }

            *nbuf = 0;
        }

        else
        {
            if ( *nbuf < RECLEN )
            {
                buffer [ *nbuf ] = data [ i ];

                *nbuf += 1;
            }
        }
    }

/*  Check for abandon request  */

    if ( ! *error_flag )
    {
        if ( sembrk_F77 ( ) == F_TRUE )
        {
            *error_flag = TRUE;

            status = TRUE;
        }
    }

    return ( status );

/*  Copyright (C) 1993-1994:  Synoptics Ltd,  All Rights Reserved  */
}

logicalfunction cunix ( COMMAND, string_length )
/*===============================================*/

char* COMMAND;
int32_t  string_length;
{
        return cunix_ ( COMMAND, string_length );
}
