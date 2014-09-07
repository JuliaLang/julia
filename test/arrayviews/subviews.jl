# testing subviews

using ArrayViews
using Base.Test

#### Test Subviews

## tools to facilitate array view testing
function _test_arrview(a, r, subs...)
    v = view(a, subs...)

    siz_r = size(r)
    siz_v = size(v)

    if siz_r != siz_v
        error("Incorrect size: get $(siz_v), but expect $(siz_r)")
    end

    for i = 1 : length(v)
        if v[i] != r[i]         
            println("v = ")
            println(v)
            println("r = ")
            println(r)
            error("Incorrect content.")
        end
    end
end

macro test_arrview(a_, subs...)
    esc(:(_test_arrview($a_, ($a_)[$(subs...)], $(subs...))))
end

#### test views from arrays

avparent = reshape(1.:1680., (8, 7, 6, 5))

# 1D
@test_arrview(avparent, 3)
@test_arrview(avparent, :)
@test_arrview(avparent, 1:12)
@test_arrview(avparent, 3:2:36)

# 2D
@test_arrview(avparent, 4, 2)
@test_arrview(avparent, 4, :)
@test_arrview(avparent, 4, 3:10)
@test_arrview(avparent, 4, 2:2:10)

@test_arrview(avparent, :, 2)
@test_arrview(avparent, :, :)
@test_arrview(avparent, :, 3:10)
@test_arrview(avparent, :, 2:2:10)

@test_arrview(avparent, 1:6, 2)
@test_arrview(avparent, 1:6, :)
@test_arrview(avparent, 1:6, 3:10)
@test_arrview(avparent, 1:6, 2:2:10)

@test_arrview(avparent, 1:2:8, 2)
@test_arrview(avparent, 1:2:8, :)
@test_arrview(avparent, 1:2:8, 3:10)
@test_arrview(avparent, 1:2:8, 2:2:10)

# 3D
@test_arrview(avparent, 4, 3, 2)
@test_arrview(avparent, 4, 3, :)
@test_arrview(avparent, 4, 3, 2:5)
@test_arrview(avparent, 4, 3, 1:2:6)

@test_arrview(avparent, 4, :, 2)
@test_arrview(avparent, 4, :, :)
@test_arrview(avparent, 4, :, 2:5)
@test_arrview(avparent, 4, :, 1:2:6)

@test_arrview(avparent, 4, 3:7, 2)
@test_arrview(avparent, 4, 3:7, :)
@test_arrview(avparent, 4, 3:7, 2:5)
@test_arrview(avparent, 4, 3:7, 1:2:6)

@test_arrview(avparent, 4, 1:2:5, 2)
@test_arrview(avparent, 4, 1:2:5, :)
@test_arrview(avparent, 4, 1:2:5, 2:5)
@test_arrview(avparent, 4, 1:2:5, 1:2:6)

@test_arrview(avparent, :, 3, 2)
@test_arrview(avparent, :, 3, :)
@test_arrview(avparent, :, 3, 2:5)
@test_arrview(avparent, :, 3, 1:2:6)

@test_arrview(avparent, :, :, 2)
@test_arrview(avparent, :, :, :)
@test_arrview(avparent, :, :, 2:5)
@test_arrview(avparent, :, :, 1:2:6)

@test_arrview(avparent, :, 3:7, 2)
@test_arrview(avparent, :, 3:7, :)
@test_arrview(avparent, :, 3:7, 2:5)
@test_arrview(avparent, :, 3:7, 1:2:6)

@test_arrview(avparent, :, 1:2:5, 2)
@test_arrview(avparent, :, 1:2:5, :)
@test_arrview(avparent, :, 1:2:5, 2:5)
@test_arrview(avparent, :, 1:2:5, 1:2:6)

@test_arrview(avparent, 2:7, 3, 2)
@test_arrview(avparent, 2:7, 3, :)
@test_arrview(avparent, 2:7, 3, 2:5)
@test_arrview(avparent, 2:7, 3, 1:2:6)

@test_arrview(avparent, 2:7, :, 2)
@test_arrview(avparent, 2:7, :, :)
@test_arrview(avparent, 2:7, :, 2:5)
@test_arrview(avparent, 2:7, :, 1:2:6)

@test_arrview(avparent, 2:7, 3:7, 2)
@test_arrview(avparent, 2:7, 3:7, :)
@test_arrview(avparent, 2:7, 3:7, 2:5)
@test_arrview(avparent, 2:7, 3:7, 1:2:6)

@test_arrview(avparent, 2:7, 1:2:5, 2)
@test_arrview(avparent, 2:7, 1:2:5, :)
@test_arrview(avparent, 2:7, 1:2:5, 2:5)
@test_arrview(avparent, 2:7, 1:2:5, 1:2:6)

@test_arrview(avparent, 1:2:7, 3, 2)
@test_arrview(avparent, 1:2:7, 3, :)
@test_arrview(avparent, 1:2:7, 3, 2:5)
@test_arrview(avparent, 1:2:7, 3, 1:2:6)

@test_arrview(avparent, 1:2:7, :, 2)
@test_arrview(avparent, 1:2:7, :, :)
@test_arrview(avparent, 1:2:7, :, 2:5)
@test_arrview(avparent, 1:2:7, :, 1:2:6)

@test_arrview(avparent, 1:2:7, 3:7, 2)
@test_arrview(avparent, 1:2:7, 3:7, :)
@test_arrview(avparent, 1:2:7, 3:7, 2:5)
@test_arrview(avparent, 1:2:7, 3:7, 1:2:6)

@test_arrview(avparent, 1:2:7, 1:2:5, 2)
@test_arrview(avparent, 1:2:7, 1:2:5, :)
@test_arrview(avparent, 1:2:7, 1:2:5, 2:5)
@test_arrview(avparent, 1:2:7, 1:2:5, 1:2:6)

# Some 4D Tests
@test_arrview(avparent, 4, :,     3, 4)
@test_arrview(avparent, 4, :,     :, 4)
@test_arrview(avparent, 4, :,   3:5, 4)
@test_arrview(avparent, 4, :, 1:2:5, 4)

@test_arrview(avparent, :, :,     3, 4)
@test_arrview(avparent, :, :,     :, 4)
@test_arrview(avparent, :, :,   3:5, 4)
@test_arrview(avparent, :, :, 1:2:5, 4)

@test_arrview(avparent, 2:7, :,     3, 4)
@test_arrview(avparent, 2:7, :,     :, 4)
@test_arrview(avparent, 2:7, :,   3:5, 4)
@test_arrview(avparent, 2:7, :, 1:2:5, 4)

@test_arrview(avparent, 1:2:7, :,     3, 4)
@test_arrview(avparent, 1:2:7, :,     :, 4)
@test_arrview(avparent, 1:2:7, :,   3:5, 4)
@test_arrview(avparent, 1:2:7, :, 1:2:5, 4)

@test_arrview(avparent, 4, :,     3, :)
@test_arrview(avparent, 4, :,     :, :)
@test_arrview(avparent, 4, :,   3:5, :)
@test_arrview(avparent, 4, :, 1:2:5, :)

@test_arrview(avparent, :, :,     3, :)
@test_arrview(avparent, :, :,     :, :)
@test_arrview(avparent, :, :,   3:5, :)
@test_arrview(avparent, :, :, 1:2:5, :)

@test_arrview(avparent, 2:7, :,     3, :)
@test_arrview(avparent, 2:7, :,     :, :)
@test_arrview(avparent, 2:7, :,   3:5, :)
@test_arrview(avparent, 2:7, :, 1:2:5, :)

@test_arrview(avparent, 1:2:7, :,     3, :)
@test_arrview(avparent, 1:2:7, :,     :, :)
@test_arrview(avparent, 1:2:7, :,   3:5, :)
@test_arrview(avparent, 1:2:7, :, 1:2:5, :)

@test_arrview(avparent, 4, :,     3, 2:5)
@test_arrview(avparent, 4, :,     :, 2:5)
@test_arrview(avparent, 4, :,   3:5, 2:5)
@test_arrview(avparent, 4, :, 1:2:5, 2:5)

@test_arrview(avparent, :, :,     3, 2:5)
@test_arrview(avparent, :, :,     :, 2:5)
@test_arrview(avparent, :, :,   3:5, 2:5)
@test_arrview(avparent, :, :, 1:2:5, 2:5)

@test_arrview(avparent, 2:7, :,     3, 2:5)
@test_arrview(avparent, 2:7, :,     :, 2:5)
@test_arrview(avparent, 2:7, :,   3:5, 2:5)
@test_arrview(avparent, 2:7, :, 1:2:5, 2:5)

@test_arrview(avparent, 1:2:7, :,     3, 2:5)
@test_arrview(avparent, 1:2:7, :,     :, 2:5)
@test_arrview(avparent, 1:2:7, :,   3:5, 2:5)
@test_arrview(avparent, 1:2:7, :, 1:2:5, 2:5)


#### Test Subviews of Views

function print_subscripts(subs1, subs2)
    println("Error happens on: ")

    subs1s = join([repr(i) for i in subs1], ", ")
    subs2s = join([repr(i) for i in subs2], ", ")

    println("  subs1 = [$subs1s]")
    println("  subs2 = [$subs2s]")
end

function test_arrview2(a, subs1, subs2)
    v = view(a, subs1...)
    v2 = view(v, subs2...)
    v2r = view(copy(v), subs2...)

    siz_v = size(v2)
    siz_r = size(v2r)

    if siz_v != siz_r
        print_subscripts(subs1, subs2)
        error("Incorrect size: get $(siz_v), but expect $(siz_r)")
    end

    for i = 1 : length(v2)
        if v2[i] != v2r[i]         
            print_subscripts(subs1, subs2)
            println("v = ")
            println(v2)
            println("r = ")
            println(v2r)
            error("Incorrect content.")
        end
    end
end

avparent = reshape(1:6912, (12, 12, 8, 6))

# 1D --> 1D
for sa in {(:), 1:36, 2:2:36}
    v1 = view(avparent, sa)
    for sb in {4, (:), 1:length(v1), 3:2:length(v1)}
        test_arrview2(avparent, (sa,), (sb,))
    end
end

# 2D --> 2D
for sa1 in {(:), 1:10, 2:2:12}, sa2 = {(:), 1:12, 2:2:16}
    v1 = view(avparent, sa1, sa2)
    for sb1 in {4, (:), 2:size(v1,1), 2:2:size(v1,1)}, 
        sb2 in {4, (:), 2:size(v1,2), 2:2:size(v1,2)}
        test_arrview2(avparent, (sa1, sa2), (sb1, sb2))
    end
end

# 3D --> 3D
for sa1 in {(:), 1:10, 2:2:12}, 
    sa2 in {(:), 1:10, 2:2:12}, 
    sa3 in {(:), 1:7, 2:2:8}
    v1 = view(avparent, sa1, sa2, sa3)
    (d1, d2, d3) = size(v1)
    for sb1 in {(:), 2:d1, 2:2:d1}, 
        sb2 in {(:), 2:d2, 2:2:d2}, 
        sb3 in {(:), 2:d3, 2:2:d3}
        test_arrview2(avparent, (sa1, sa2, sa3), (sb1, sb2, sb3))
    end
end

# Test Linear Algebra on views

avparent = rand(7, 8)
bvparent = rand(8, 5)

_ab = avparent * bvparent

@test _ab == view(avparent, :, :) * bvparent
@test _ab == avparent * view(bvparent, :, :)
@test _ab == view(avparent, :, :) * view(bvparent, :, :)

for j = 1:size(bvparent,2)
    @test_approx_eq _ab[:,j] view(avparent,:,:) * view(bvparent,:,j)
end

@test avparent[:, 2:2:7] * bvparent[1:3, 1:2:5] == view(avparent, :, 2:2:7) * view(bvparent, 1:3, 1:2:5)

