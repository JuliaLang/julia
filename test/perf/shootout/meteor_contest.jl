# The Computer Language Benchmarks Game
# http://shootout.alioth.debian.org/
#
# Contributed by David Campbell
# based on Python version by Olof Kraigher

const width  =  5
const height = 10

# directions
const E  = 0
const NE = 1
const NW = 2
const W  = 3
const SW = 4
const SE = 5

const rotate = Dict(E => NE, NE => NW, NW => W, W => SW, SW => SE, SE => E)
const flip   = Dict(E => W, NE => NW, NW => NE, W => E, SW => SE, SE => SW)

const move = Dict(
    E  => (x, y) -> (x +   1,     y),
    W  => (x, y) -> (x -   1,     y),
    NE => (x, y) -> (x + y%2,     y - 1),
    NW => (x, y) -> (x + y%2 - 1, y - 1),
    SE => (x, y) -> (x + y%2,     y + 1),
    SW => (x, y) -> (x + y%2 - 1, y + 1)
)

const pieces = (
    (E,  E,  E,  SE),
    (SE, SW, W,  SW),
    (W,  W,  SW, SE),
    (E,  E,  SW, SE),
    (NW, W,  NW, SE, SW),
    (E,  E,  NE, W),
    (NW, NE, NE, W),
    (NE, SE, E,  NE),
    (SE, SE, E,  SE),
    (E,  NW, NW, NW)
)

const solutions = Any[]
const masks = zeros(UInt64, 10)
const masksAtCell = Array(Any, width*height, height)

valid(x, y) = (0 <= x < width) && (0 <= y < height)
legal(mask::UInt64, board::UInt64) = (mask & board) == 0
zerocount(mask::UInt64) = 50 - count_ones(mask)

function findFreeCell(board::UInt64)
    for y in 0:height-1
        for x in 0:width-1
            if board & (uint64(1) << (x + width*y)) == 0
                return x, y
            end
        end
    end
end

function floodFill(board::UInt64, fixme)
    x, y = fixme
    if !valid(x,y)
        return board
    end

    if board & (uint64(1) << (x + width*y)) != 0
        return board
    end

    board |= uint64(1) << (x + width*y)
    for f in values(move)
        board |= floodFill(board, f(x, y))
    end

    return board
end

function noIslands(mask::UInt64)
    zeroes_ = zerocount(mask)

    if zeroes_ < 5
        return false
    end

    while mask != 0x3FFFFFFFFFFFF
        mask = floodFill(mask, findFreeCell(mask))
        new_zeroes = zerocount(mask)

        if zeroes_ - new_zeroes < 5
            return false
        end

        zeroes_ = new_zeroes
    end

    return true
end

function getBitmask(x, y, piece)
    mask = (uint64(1) << (x + width*y))

    for cell_ in piece
        x, y = move[cell_](x,y)
        if valid(x, y)
            mask |= uint64(1) << (x + width*y)
        else
            return false, uint64(0)
        end
    end

    return true, uint64(mask)
end

function allBitmasks(piece, color)
    bitmasks = UInt64[]
    for orientations in 0:1
        for rotations in 0:(6 - 3*(color == 4))-1
            for y in 0:height-1
                for x in 0:width-1
                    isValid, mask = getBitmask(x, y, piece)
                    if isValid && noIslands(uint64(mask))
                        push!(bitmasks, mask)
                    end
                end
            end
            piece = [rotate[cell_] for cell_ in piece]
        end
        piece = [flip[cell_] for cell_ in piece]
    end
    return bitmasks
end

function generateBitmasks()
    for i = 1:length(masksAtCell)
        masksAtCell[i] = UInt64[]
    end

    color = 0
    for piece in pieces
        masks = allBitmasks(piece, color)
        sort!(masks)
        cellMask = uint64(1) << (width*height - 1)
        cellCounter = width*height - 1

        j = length(masks) - 1

        while j >= 0
            if (masks[j + 1] & cellMask) == cellMask
                push!(masksAtCell[cellCounter + 1, color + 1], masks[j + 1])
                j -= 1
            else
                cellMask >>= 1
                cellCounter -= 1
            end
        end
        color += 1
    end
end

function solveCell(cell_, board::UInt64, n)
    if length(solutions) >= n
        return
    end

    if board == 0x0003FFFFFFFFFFFF
        # Solved
        s = stringOfMasks(masks)
        push!(solutions, s)
        push!(solutions, reverse(s))
        return
    end

    if board & (uint64(1) << cell_) != 0
        # Cell full
        solveCell(cell_ - 1, uint64(board), n)
        return
    end

    if cell_ < 0
        # Out of board
        return
    end

    for color in 0:9
        if masks[color + 1] == 0
            for mask in masksAtCell[cell_ + 1, color + 1]
                if legal(mask, board)
                    masks[color + 1] = mask
                    solveCell(cell_ - 1, uint64(board | mask), n)
                    masks[color + 1] = 0
                end
            end
        end
    end
end

function solve(n)
    generateBitmasks()
    solveCell(width*height-1, uint64(0), n)
end

function stringOfMasks(masks)
    s = ""
    mask::UInt64 = 1
    for y in 0:height-1
        for x in 0:width-1
            for color in 0:9
                if (masks[color+1] & mask) != 0
                    s = string(s, color)
                    break
                elseif color == 9
                    s *= "."
                end
            end
            mask <<= 1
        end
    end
    return s
end

function printSolution(s)
    for y in 0:height-1
        if y%2 == 1
            print(" ")
        end
        for x in 0:width-1
            print("$(s[x + y*width + 1]) ")
        end
        println()
    end
end

function meteor_contest(n::Int=2098)
    empty!(solutions)
    fill!(masks, 0)
    solve(n)
#    println("$(length(solutions)) solutions found")
#    println()
#    printSolution(minimum(solutions))
#    println()
#    printSolution(maximum(solutions))
#    println()
end
