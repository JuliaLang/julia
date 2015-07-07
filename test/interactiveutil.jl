# Not sure how to test edit without needing to mock the fs layer

# Test the clipboard: doesn't work cause on linux the clipboard
# requires and xserver display. Julia should probably default to reading and writing to a file
# if this is the case though.
#clipboard("Clipboard Test")
#@test clipboard() == "Clipboard Test"

# Not sure how detailed the versioninfo check needs to be
versioninfo(true)
versioninfo(false)

abstract Foo
@test length(methodswith(Expr)) > 0
@test length(methodswith(Expr, Base)) > 0
@test length(methodswith(Expr, Core)) == 0
@test length(methodswith(Foo, Base)) == 0

