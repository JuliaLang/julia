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

# Enable raw mode. Allows us to process keyboard inputs directly.
function enableRawMode(term)
    try
        REPL.Terminals.raw!(term, true)
        return true
    catch err
        @warn("TerminalMenus: Unable to enter raw mode: $err")
    end
    return false
end

# Disable raw mode. Give control back to Julia REPL if interactive session.
function disableRawMode(term)
    try
        REPL.Terminals.raw!(term, false)
        return true
    catch err
        @warn("TerminalMenus: Unable to disable raw mode: $err")
    end
    return false
end


# Reads a single byte from stdin
readNextChar(stream::IO=stdin) = Char(read(stream,1)[1])

# Read the next key from stdin. It is also able to read several bytes for
#   escaped keys such as the arrow keys, home/end keys, etc.
# Escaped keys are returned using the `Key` enum.
readKey(stream::IO=stdin) = UInt32(_readKey(stream))
function _readKey(stream::IO=stdin)
    c = readNextChar(stream)

	# Escape characters
	if c == '\x1b'
        stream.buffer.size < 2 && return '\x1b'
        esc_a = readNextChar(stream)

        if esc_a == 'v'  # M-v
            return PAGE_UP
        elseif esc_a == '<'  # M-<
            return HOME_KEY
        elseif esc_a == '>'  # M->
            return END_KEY
        end

        stream.buffer.size < 3 && return '\x1b'
        esc_b = readNextChar(stream)

		if esc_a == '[' || esc_a == 'O'
			if esc_b >= '0' && esc_b <= '9'
				stream.buffer.size < 4 && return '\x1b'
                esc_c = readNextChar(stream)

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
		elseif esc_a == 'H'
            return HOME_KEY
        elseif esc_a == 'F'
            return END_KEY
		end

		return '\x1b'

    elseif c == '\x16'  # C-v
        return PAGE_DOWN
    elseif c == '\x10'  # C-p
        return ARROW_UP
    elseif c == '\x0e'  # C-n
        return ARROW_DOWN
    else
		return c
	end
end
