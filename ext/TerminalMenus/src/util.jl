# Enum for escaped (multi-byte) keys such as the arrows or the home/end keys
@enum(Key,
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY,
    END_KEY,
    PAGE_UP,
    PAGE_DOWN)

# The user terminal
terminal = Base.Terminals.TTYTerminal(get(ENV, "TERM", @static is_windows() ? "" : "dumb"), STDIN, STDOUT, STDERR)

# Enable raw mode. Allows us to process keyboard inputs directly.
enableRawMode() = Base.Terminals.raw!(terminal, true)

# Disable raw mode. Give control back to Julia REPL if interactive session.
disableRawMode() = Base.Terminals.raw!(terminal, false)

# Reads a single byte from STDIN
readNextChar() = Char(read(STDIN,1)[1])

# Read the next key from STDIN. It is also able to read several bytes for
#   escaped keys such as the arrow keys, home/end keys, etc.
# Escaped keys are returned using the `Key` enum.
function readKey() ::UInt32
    c = readNextChar()

	# Escape characters
	if c == '\x1b'
        STDIN.buffer.size < 3 && return '\x1b'
        esc_a = readNextChar()
        esc_b = readNextChar()

		if esc_a == '['
			if esc_b >= '0' && esc_b <= '9'
				STDIN.buffer.size < 4 && return '\x1b'
                esc_c = readNextChar()

				if esc_c == '~'
					if esc_b == '1'
                        return HOME_KEY
					elseif esc_b == '4'
                        return END_KEY
					elseif esc_b == '3'
                        return DEL_KEY
					elseif esc_b == '5'
                        return PAGE_UP
					elseif esc_b == '6'
                        return PAGE_DOWN
					elseif esc_b == '7'
                        return HOME_KEY
					elseif esc_b == '8'
                        return END_KEY
                    else
                        return '\x1b'
                    end
                end

			else
				# Arrow keys
				if esc_b == 'A'
                    return ARROW_UP
				elseif esc_b == 'B'
                    return ARROW_DOWN
				elseif esc_b == 'C'
                    return ARROW_RIGHT
				elseif esc_b == 'D'
                    return ARROW_LEFT
				elseif esc_b == 'H'
                    return HOME_KEY
				elseif esc_b == 'F'
                    return END_KEY
                else
                    return '\x1b'
                end
			end
		elseif esc_a == 'O'
            if esc_a == 'H'
                return HOME_KEY
            elseif esc_a == 'F'
                return END_KEY
            end
		end

		return '\x1b'
    else
		return c;
	end
end
