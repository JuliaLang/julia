import Tokenize

function speed_test()
    tot_files = 0
    tot_tokens = 0
    tot_errors = 0
    dir = dirname(Base.find_source_file("base.jl"))
    for (root, dirs, files) in walkdir(dir)
        for file in files
            if endswith(file, ".jl")
                tot_files += 1
                file = joinpath(root, file)
                str = readstring(file)
                l = tokenize(str)
                while !Tokenize.Lexers.eof(l)
                    t = Tokenize.Lexers.next_token(l)
                    tot_tokens += 1
                    if t.kind == Tokens.ERROR
                        tot_errors += 1
                    end
                end
            end
        end
    end
    tot_files, tot_tokens, tot_errors
end

tot_files, tot_tokens, tot_errors = speed_test()
tot_time = @belapsed speed_test()
print("Lexed ", tot_files, " files in ", @sprintf("%3.4f", tot_time),
      " seconds with a total of ", tot_tokens, " tokens with ", tot_errors, " errors")
