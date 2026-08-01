#!/usr/bin/env python3
"""Double-fork daemonizer — properly detaches the serve.py from the controlling
shell so it survives the end of the bash tool call."""
import os
import sys
import subprocess
import time

def daemonize():
    # First fork
    pid = os.fork()
    if pid > 0:
        # Parent exits immediately
        sys.exit(0)

    # Decouple from parent environment
    os.setsid()
    os.umask(0)

    # Second fork
    pid = os.fork()
    if pid > 0:
        sys.exit(0)

    # Now we're a daemon — redirect std streams
    sys.stdout.flush()
    sys.stderr.flush()
    log = open('/tmp/server_daemon.log', 'ab', buffering=0)
    devnull = open('/dev/null', 'r')
    os.dup2(devnull.fileno(), sys.stdin.fileno())
    os.dup2(log.fileno(), sys.stdout.fileno())
    os.dup2(log.fileno(), sys.stderr.fileno())

    # exec the serve.py — auto-restart on crash
    while True:
        try:
            os.execvp('python3', ['python3', '-u', 'serve.py', '--port', '3000'])
        except Exception as e:
            log.write(('Restart failed: %s\n' % e).encode())
            time.sleep(2)

if __name__ == '__main__':
    daemonize()
