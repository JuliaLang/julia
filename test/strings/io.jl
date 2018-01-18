# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "join()" begin
    @test join([]) == ""
    @test join(["a"],"?") == "a"
    @test join("HELLO",'-') == "H-E-L-L-O"
    @test join(1:5, ", ", " and ") == "1, 2, 3, 4 and 5"
    @test join(["apples", "bananas", "pineapples"], ", ", " and ") == "apples, bananas and pineapples"
    @test_throws MethodError join(1, 2, 3, 4)
end

# issue #9178 `join` calls `done()` twice on the iterables
mutable struct i9178
    nnext::Int64
    ndone::Int64
end
Base.start(jt::i9178) = (jt.nnext=0 ; jt.ndone=0 ; 0)
Base.done(jt::i9178, n) = (jt.ndone += 1 ; n > 3)
Base.next(jt::i9178, n) = (jt.nnext += 1 ; ("$(jt.nnext),$(jt.ndone)", n+1))
@test join(i9178(0,0), ";") == "1,1;2,2;3,3;4,4"

# quotes + interpolation (issue #455)
@test "$("string")" == "string"
arr = ["a","b","c"]
@test "[$(join(arr, " - "))]" == "[a - b - c]"

# join with empty input
myio = IOBuffer()
join(myio, "", "", 1)
@test isempty(take!(myio))

@testset "#11659" begin
    # The indentation code was not correctly counting tab stops
    @test Base.indentation("      \t") == (8, true)
    @test Base.indentation("  \tfoob") == (8, false)
    @test Base.indentation(" \t \t")   == (16, true)

    @test Base.unindent("\tfoo",0) == "\tfoo"
    @test Base.unindent("\tfoo",4) == "    foo"
    @test Base.unindent("    \tfoo",4) == "    foo"
    @test Base.unindent("\t\n    \tfoo",4) == "    \n    foo"
    @test Base.unindent("\tfoo\tbar",4) == "    foo     bar"
    @test Base.unindent("\n\tfoo",4) == "\n    foo"
    @test Base.unindent("\n    \tfoo",4) == "\n    foo"
    @test Base.unindent("\n\t\n    \tfoo",4) == "\n    \n    foo"
    @test Base.unindent("\n\tfoo\tbar",4) == "\n    foo     bar"
end

# issue #22021, string realloc bug with join
s22021 = String["\"\"\"
     non_max_suppression(boxes, scores, max_output_size; iou_threshold=nothing)

Greedily selects a subset of bounding boxes in descending order of score,

pruning away boxes that have high intersection-over-union (IOU) overlap
with previously selected boxes.  Bounding boxes are supplied as
[y1, x1, y2, x2], where (y1, x1) and (y2, x2) are the coordinates of any
diagonal pair of box corners and the coordinates can be provided as normalized
(i.e., lying in the interval [0, 1]) or absolute.  Note that this algorithm
is agnostic to where the origin is in the coordinate system.  Note that this
algorithm is invariant to orthogonal transformations and translations
of the coordinate system; thus translating or reflections of the coordinate
system result in the same boxes being selected by the algorithm.

The output of this operation is a set of integers indexing into the input
collection of bounding boxes representing the selected boxes.  The bounding
box coordinates corresponding to the selected indices can then be obtained
using the `tf.gather operation`.  For example:

  selected_indices = tf.image.non_max_suppression(
      boxes, scores, max_output_size, iou_threshold)
  selected_boxes = tf.gather(boxes, selected_indices)
\"\"\"",
    "    tf.@op function non_max_suppression(v13566, v13567, v13568; name=nothing, iou_threshold=nothing) ",
    "            local desc ",
    "            tf.with_op_name((()->begin  ",
    "                        desc = tf.NodeDescription(\"NonMaxSuppression\") ",
    "                        begin  ",
    "                            begin  ",
    "                                v13566 = convert(TensorFlow.Tensor{Float32}, v13566) ",
    "                                begin  ",
    "                                end",
    "                            end",
    "                            begin  ",
    "                                v13567 = convert(TensorFlow.Tensor{Float32}, v13567) ",
    "                                begin  ",
    "                                end",
    "                            end",
    "                            begin  ",
    "                                v13568 = convert(TensorFlow.Tensor{Int32}, v13568) ",
    "                                begin  ",
    "                                end",
    "                            end",
    "                        end ",
    "                        begin  ",
    "                            begin  ",
    "                                tf.add_input(desc, v13566)",
    "                            end",
    "                            begin  ",
    "                                tf.add_input(desc, v13567)",
    "                            end",
    "                            begin  ",
    "                                tf.add_input(desc, v13568)",
    "                            end",
    "                        end ",
    "                        begin  ",
    "                            begin  ",
    "                                if iou_threshold !== nothing ",
    "                                    desc[\"iou_threshold\"] = Base.identity(iou_threshold)",
    "                                end",
    "                            end",
    "                        end",
    "                    end), name, \"NonMaxSuppression\") ",
    "            tf.Tensor(tf.Operation(desc))",
    "        end"]

for i = 1:10
    buf = IOBuffer()
    print(buf, join(s22021, "\n"))
    @test isvalid(String, take!(buf))
end
