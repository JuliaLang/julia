using Test
import TOML: tryparsefile

# These tests need to be updated if the error strings change

tmp = tempname()

@testset "error printing" begin

# Special printing for invalid bare key character
write(tmp, "fooα = 3")
p = tryparsefile(tmp)
err = sprint(Base.showerror, p)
@test contains(err, """
    $tmp:1:4 error: invalid bare key character: 'α'
      fooα = 3
         ^""")

# Error is at EOF
write(tmp, "foo = [1, 2,")
p = tryparsefile(tmp)
err = sprint(Base.showerror, p)
@test contains(err, """
    $tmp:1:12 error: unexpected end of file, expected a value
      foo = [1, 2,
                  ^""")
# A bit of unicode
write(tmp, "\"fαβ\" = [1.2, 1.2.3]")
p = tryparsefile(tmp)
err = sprint(Base.showerror, p)
@test contains(err, """
        $tmp:1:18 error: failed to parse value
          "fαβ" = [1.2, 1.2.3]
                           ^""")

end # testset
