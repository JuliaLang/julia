// Source for libkilo used with kilo.jl

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

struct termios orig_termios;

enum editorKey {
	// Use large int value so that we don't conflict with chars
	ARROW_LEFT = 1000,
	ARROW_RIGHT,
	ARROW_UP,
	ARROW_DOWN,
	DEL_KEY,
	HOME_KEY,
	END_KEY,
	PAGE_UP,
	PAGE_DOWN
};

// Print an error and exit
void die(const char *s) {
	perror(s);
	exit(1);
}


// Disable raw mode for the terminal
void disableRawMode() {
	tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
}

// Enable raw mode for the terminal
void enableRawMode() {
	if (tcgetattr(STDIN_FILENO, &orig_termios) == -1) die("tcgetattr");
  	atexit(disableRawMode);

	struct termios raw = orig_termios;

	// FLAGS:
	// IXON:   disable ctrl-s (stop input from being sent to the term)
	//           and ctrl-q (resume input to the term)
	// ICRNL:  fix ctrl-m fron converting a \r (13) to a \n (10)
	raw.c_iflag &= ~(ICRNL | IXON | BRKINT | INPCK | ISTRIP);
	// ISIG:   disable ctrl-c (terminate) & ctrl-z (suspend)
	// IEXTEN: disable ctrl-v (input raw byte)
	raw.c_lflag &= ~(ECHO | ICANON | ISIG | IEXTEN);
	// OPOST: don't confert \n to \r\n in outout
	//        we must now enter the full \r\n if we want a newline
	raw.c_oflag &= ~(OPOST);
	// CS8: set the char size to 8 bits per byte
	raw.c_cflag |= (CS8);

	// Set timeout for read()
	//  return nothing if no input is supplied
	raw.c_cc[VMIN] = 0;
	raw.c_cc[VTIME] = 1;

  	if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr");
}


int editorReadKey() {
	int nread;

	char c;
	while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
		if (nread == -1 && errno != EAGAIN) die("read");
	}

	// Escape characters
	if (c == '\x1b') {
		char seq[3];

		if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
		if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';

		if (seq[0] == '[') {
			if (seq[1] >= '0' && seq[1] <= '9') {
				if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
				if (seq[2] == '~') {
					switch (seq[1]) {
						case '1': return HOME_KEY;
						case '4': return END_KEY;
						case '3': return DEL_KEY;
						case '5': return PAGE_UP;
						case '6': return PAGE_DOWN;
						case '7': return HOME_KEY;
						case '8': return END_KEY;
					}
				}
			} else {
				switch (seq[1]) {
					// Arrow keys
					case 'A': return ARROW_UP;
					case 'B': return ARROW_DOWN;
					case 'C': return ARROW_RIGHT;
					case 'D': return ARROW_LEFT;
					case 'H': return HOME_KEY;
					case 'F': return END_KEY;
				}
			}
		} else if (seq[0] == 'O') {
			switch (seq[1]) {
				case 'H': return HOME_KEY;
				case 'F': return END_KEY;
			}
		}

		return '\x1b';
	} else {
		return c;
	}
}
