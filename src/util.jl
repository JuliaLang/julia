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
        warn("TerminalMenus: Unable to enter raw mode: $err")
    end
    return false
end

# Disable raw mode. Give control back to Julia REPL if interactive session.
function disableRawMode(term)
    try
        REPL.Terminals.raw!(term, false)
        return true
    catch err
        warn("TerminalMenus: Unable to disable raw mode: $err")
    end
    return false
end

function trimWidth(str::String, term_width::Int, trim_right=true, pad::Int=2)
    max_str_len = term_width - pad - 5
    if length(str) <= max_str_len || length(str) < 6
        return str
    end
    if trim_right
        return string(str[1:max_str_len], "..")
    else
        return string("..", str[length(str) - max_str_len:end])
    end
end


# Reads a single byte from stdin
readNextChar(stream::IO=Base.stdin) = Char(read(stream,1)[1])

# Read the next key from stdin. It is also able to read several bytes for
#   escaped keys such as the arrow keys, home/end keys, etc.
# Escaped keys are returned using the `Key` enum.
function readKey(stream::IO=Base.stdin) ::UInt32
    c = readNextChar(stream)

	# Escape characters
	if c == '\x1b'
        stream.buffer.size < 2 && return '\x1b'
        esc_a = readNextChar(stream)

        if esc_a == 'v'  # M-v
            return UInt32(PAGE_UP)
        elseif esc_a == '<'  # M-<
            return UInt32(HOME_KEY)
        elseif esc_a == '>'  # M->
            return UInt32(END_KEY)
        end

        stream.buffer.size < 3 && return '\x1b'
        esc_b = readNextChar(stream)

		if esc_a == '[' || esc_a == 'O'
			if esc_b >= '0' && esc_b <= '9'
				stream.buffer.size < 4 && return '\x1b'
                esc_c = readNextChar(stream)

				if esc_c == '~'
					if esc_b == '1'
                        return UInt32(HOME_KEY)
					elseif esc_b == '4'
                        return UInt32(END_KEY)
					elseif esc_b == '3'
                        return UInt32(DEL_KEY)
					elseif esc_b == '5'
                        return UInt32(PAGE_UP)
					elseif esc_b == '6'
                        return UInt32(PAGE_DOWN)
					elseif esc_b == '7'
                        return UInt32(HOME_KEY)
					elseif esc_b == '8'
                        return UInt32(END_KEY)
                    else
                        return UInt32('\x1b')
                    end
                end

			else
				# Arrow keys
				if esc_b == 'A'
                    return UInt32(ARROW_UP)
				elseif esc_b == 'B'
                    return UInt32(ARROW_DOWN)
				elseif esc_b == 'C'
                    return UInt32(ARROW_RIGHT)
				elseif esc_b == 'D'
                    return UInt32(ARROW_LEFT)
				elseif esc_b == 'H'
                    return UInt32(HOME_KEY)
				elseif esc_b == 'F'
                    return UInt32(END_KEY)
                else
                    return UInt32('\x1b')
                end
			end
		elseif esc_a == 'H'
            return UInt32(HOME_KEY)
        elseif esc_a == 'F'
            return UInt32(END_KEY)
		end

        return UInt32('\x1b')

    elseif c == '\x16'  # C-v
        return UInt32(PAGE_DOWN)
    elseif c == '\x10'  # C-p
        return UInt32(ARROW_UP)
    elseif c == '\x0e'  # C-n
        return UInt32(ARROW_DOWN)
    else
        return UInt32(c)
	end
end
