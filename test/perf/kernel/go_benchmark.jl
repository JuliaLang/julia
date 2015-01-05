# Benchmark implementing the board logic for the game of go and
# exercising it by playing random games. Derived from
# http://www.lysator.liu.se/~gunnar/gtp/brown-1.0.tar.gz
import Base.getindex

const EMPTY = 0
const WHITE = 1
const BLACK = 2

# Function to find the opposite color.
other_color(color::Int) = WHITE + BLACK - color

# Used in the final_status[] array.
const DEAD = 0
const ALIVE = 1
const SEKI = 2
const WHITE_TERRITORY = 3
const BLACK_TERRITORY = 4
const UNKNOWN = 5

type XorRand
  state::UInt32
end

function xor_srand(rand::XorRand, seed::UInt32)
  rand.state = seed
end

function xor_randn(rand::XorRand, n::UInt32)
  rand.state $= rand.state << 13
  rand.state $= rand.state >> 17
  rand.state $= rand.state << 5
  rand.state % n
end

xor_randn(rand::XorRand, n::Int) = convert(Int, xor_randn(rand, convert(UInt32, n)))

# Offsets for the four directly adjacent neighbors. Used for looping.
const deltai = (-1, 1, 0, 0)
const deltaj = (0, 0, -1, 1)
neighbor(i::Int, j::Int, k::Int) = (i + deltai[k], j + deltaj[k])

type Board
  size::Int
  komi::Float64

  # Board represented by a 1D array. The first board_size*board_size
  # elements are used. Vertices are indexed row by row, starting with 0
  # in the upper left corner.
  board::Matrix{Int}

  # Stones are linked together in a circular list for each string.
  next_stone::Matrix{Int}

  # Storage for final status computations.
  final_status::Matrix{Int}

  # Point which would be an illegal ko recapture.
  ko_i::Int
  ko_j::Int

  # xor-shift random number generator.
  rand::XorRand

  function Board(n::Int)
    init(new(), n, convert(UInt32, 2463534242))
  end
end

function init(board::Board, n::Int, seed::UInt32)
  board.size = n
  board.komi = 0.0
  board.board = zeros(Int, n, n)
  board.next_stone = zeros(Int, n, n)
  board.final_status = zeros(Int, n, n)
  board.ko_i = 0
  board.ko_j = 0
  board.rand = XorRand(seed)
  board
end

getindex(board::Board, pos::Int) = board.board[pos]
getindex(board::Board, i::Int, j::Int) = board.board[i, j]

# Functions to convert between 1D and 2D coordinates. The 2D coordinate
# (i, j) points to row i and column j, starting with (1,1) in the
# upper left corner.
POS(board::Board, i::Int, j::Int) = (j - 1) * board.size + i
IJ(board::Board, pos::Int) = (1 + mod((pos - 1), board.size), 1 + fld(pos - 1, board.size))

function set_komi(board::Board, komi::Float64)
  board.komi = komi
end

function set_random_seed(board::Board, seed::UInt32)
  xor_srand(board.rand, seed)
end

function clear(board::Board)
  board.board[:] = EMPTY
end

is_pass_move(i::Int, j::Int) = i == 0 && j == 0

function on_board(board::Board, i::Int, j::Int)
  i >= 1 && i <= board.size && j >= 1 && j <= board.size
end

function legal_move(board::Board, i::Int, j::Int, color::Int)
  other = other_color(color)

  # Pass is always legal.
  if is_pass_move(i, j)
    return true
  end

  # Already occupied.
  if board[i, j] != EMPTY
    return false
  end

  # Illegal ko recapture. It is not illegal to fill the ko so we must
  # check the color of at least one neighbor.
  if i == board.ko_i && j == board.ko_j && ((on_board(board, i - 1, j) && board[i - 1, j] == other) || (on_board(board, i + 1, j) && board[i + 1, j] == other))
    return false
  end

  true
end

# Does the string at (i, j) have any more liberty than the one at (libi, libj)?
function has_additional_liberty(board::Board, i::Int, j::Int, libi::Int, libj::Int)
  start = POS(board, i, j)
  pos = start
  while true
    (ai, aj) = IJ(board, pos)
    for k = 1:4
      (bi, bj) = neighbor(ai, aj, k)
      if on_board(board, bi, bj) && board[bi, bj] == EMPTY && (bi != libi || bj != libj)
        return true
      end
    end

    pos = board.next_stone[pos]
    if pos == start
      break
    end
  end

  false
end

# Does (ai, aj) provide a liberty for a stone at (i, j)?
function provides_liberty(board::Board, ai::Int, aj::Int, i::Int, j::Int, color::Int)
  # A vertex off the board does not provide a liberty.
  if !on_board(board, ai, aj)
    return false
  end

  # An empty vertex IS a liberty.
  if board[ai, aj] == EMPTY
    return true
  end

  # A friendly string provides a liberty to (i, j) if it currently
  # has more liberties than the one at (i, j).
  if board[ai, aj] == color
    return has_additional_liberty(board, ai, aj, i, j)
  end

  # An unfriendly string provides a liberty if and only if it is
  # captured, i.e. if it currently only has the liberty at (i, j).
  !has_additional_liberty(board, ai, aj, i, j)
end

# Is a move at ij suicide for color?
function suicide(board::Board, i::Int, j::Int, color::Int)
  for k = 1:4
    if provides_liberty(board, neighbor(i, j, k)..., i, j, color)
      return false
    end
  end
  true
end

# Remove a string from the board array. There is no need to modify
# the next_stone array since this only matters where there are
# stones present and the entire string is removed.
function remove_string(board::Board, i::Int, j::Int)
  start = POS(board, i, j)
  pos = start
  removed = 0
  while true
    board.board[pos] = EMPTY
    removed += 1
    pos = board.next_stone[pos]
    if pos == start
      break
    end
  end
  removed
end

# Do two vertices belong to the same string? It is required that both
# pos1 and pos2 point to vertices with stones.
function same_string(board::Board, pos1::Int, pos2::Int)
  pos = pos1
  while true
    if pos == pos2
      return true
    end
    pos = board.next_stone[pos]
    if pos == pos1
      break
    end
  end
  false
end

# Play at (i, j) for color. No legality check is done here. We need
# to properly update the board array, the next_stone array, and the
# ko point.
function play_move(board::Board, i::Int, j::Int, color::Int)
  pos = POS(board, i, j)
  captured_stones = 0

  # Reset the ko point.
  board.ko_i = 0
  board.ko_j = 0

  # Nothing more happens if the move was a pass.
  if is_pass_move(i, j)
    return
  end

  # If the move is a suicide we only need to remove the adjacent
  # friendly stones.
  if suicide(board, i, j, color)
    for k = 1:4
      (ai, aj) = neighbor(i, j, k)
      if on_board(board, ai, aj) && board[ai, aj] == color
        remove_string(board, ai, aj)
      end
    end
    return
  end

  # Not suicide. Remove captured opponent strings.
  for k = 1:4
    (ai, aj) = neighbor(i, j, k)
    if on_board(board, ai, aj) && board[ai, aj] == other_color(color) && !has_additional_liberty(board, ai, aj, i, j)
      captured_stones += remove_string(board, ai, aj)
    end
  end

  # Put down the new stone. Initially build a single stone string by
  # setting next_stone[pos] pointing to itself.
  board.board[pos] = color
  board.next_stone[pos] = pos

  # If we have friendly neighbor strings we need to link the strings
  # together.
  for k = 1:4
    (ai, aj) = neighbor(i, j, k)
    pos2 = POS(board, ai, aj)
    # Make sure that the stones are not already linked together. This
    # may happen if the same string neighbors the new stone in more
    # than one direction.
    if on_board(board, ai, aj) && board[pos2] == color && !same_string(board, pos, pos2)
      # The strings are linked together simply by swapping the the
      # next_stone pointers.
      (board.next_stone[pos], board.next_stone[pos2]) = (board.next_stone[pos2], board.next_stone[pos])
    end
  end

  # If we have captured exactly one stone and the new string is a
  # single stone it may have been a ko capture.
  if captured_stones == 1 && board.next_stone[pos] == pos
    # Check whether the new string has exactly one liberty. If so it
    # would be an illegal ko capture to play there immediately. We
    # know that there must be a liberty immediately adjacent to the
    # new stone since we captured one stone.
    for k = 1:4
      (ai, aj) = neighbor(i, j, k)
      if on_board(board, ai, aj) && board[ai, aj] == EMPTY
        if !has_additional_liberty(board, i, j, ai, aj)
          board.ko_i = ai
          board.ko_j = aj
        end
        break
      end
    end
  end
end

# Generate a move.
function generate_move(board::Board, color::Int)
  moves = zeros(Int, 2, board.size * board.size)
  num_moves = 0

  for ai = 1:board.size, aj = 1:board.size
    # Consider moving at (ai, aj) if it is legal and not suicide.
    if legal_move(board, ai, aj, color) && !suicide(board, ai, aj, color)
      # Further require the move not to be suicide for the opponent...
      if !suicide(board, ai, aj, other_color(color))
        num_moves += 1
        moves[:,num_moves] = [ai, aj]
      else
        # ...however, if the move captures at least one stone,
        # consider it anyway.
        for k = 1:4
          (bi, bj) = neighbor(ai, aj, k)
          if on_board(board, bi, bj) && board[bi, bj] == other_color(color)
            num_moves += 1
            moves[:,num_moves] = [ai, aj]
            break
          end
        end
      end
    end
  end

  # Choose one of the considered moves randomly with uniform
  # distribution. (Strictly speaking the moves with smaller 1D
  # coordinates tend to have a very slightly higher probability to be
  # chosen, but for all practical purposes we get a uniform
  # distribution.)
  if num_moves > 0
    move = moves[:,1 + xor_randn(board.rand, num_moves)]
    return (move[1], move[2])
  else
    # But pass if no move was considered.
    return (0, 0)
  end
end

# Set a final status value for an entire string.
function set_final_status_string(board::Board, pos::Int, status::Int)
  start = pos
  while true
    board.final_status[pos] = status
    pos = board.next_stone[pos]
    if pos == start
      break
    end
  end
end


# Compute final status. This function is only valid to call in a
# position where generate_move() would return pass for at least one
# color.
#
# Due to the nature of the move generation algorithm, the final
# status of stones can be determined by a very simple algorithm:
#
# 1. Stones with two or more liberties are alive with territory.
# 2. Stones in atari are dead.
#
# Moreover alive stones are unconditionally alive even if the
# opponent is allowed an arbitrary number of consecutive moves.
# Similarly dead stones cannot be brought alive even by an arbitrary
# number of consecutive moves.
#
# Seki is not an option. The move generation algorithm would never
# leave a seki on the board.
#
# Comment: This algorithm doesn't work properly if the game ends with
#          an unfilled ko. If three passes are required for game end,
#          that will not happen.
#
function compute_final_status(board::Board)
  board.final_status[:] = UNKNOWN

  for i = 1:board.size, j = 1:board.size
    if board[i, j] == EMPTY
      for k = 1:4
        (ai, aj) = neighbor(i, j, k)
        if !on_board(board, ai, aj)
          continue
        end
        # When the game is finished, we know for sure that (ai, aj)
        # contains a stone. The move generation algorithm would
        # never leave two adjacent empty vertices. Check the number
        # of liberties to decide its status, unless it's known
        # already.
        #
        # If we should be called in a non-final position, just make
        # sure we don't call set_final_status_string() on an empty
        # vertex.
        pos = POS(board, ai, aj)
        if board.final_status[ai, aj] == UNKNOWN
          if board[ai, aj] != EMPTY
            if has_additional_liberty(board, ai, aj, i, j)
              set_final_status_string(board, pos, ALIVE)
            else
              set_final_status_string(board, pos, DEAD)
            end
          end
        end
        # Set the final status of the pos vertex to either black
        # or white territory.
        if board.final_status[i, j] == UNKNOWN
          if (board.final_status[ai, aj] == ALIVE) $ (board[ai, aj] == WHITE)
            board.final_status[i, j] = BLACK_TERRITORY
          else
            board.final_status[i, j] = WHITE_TERRITORY
          end
        end
      end
    end
  end
end

get_final_status(board::Board, i::Int, j::Int) = board.final_status[i, j]

function compute_score(board::Board)
  score = board.komi
  compute_final_status(board)
  for i = 1:board.size, j = 1:board.size
    status = get_final_status(board, i, j)
    if status == BLACK_TERRITORY
      score -= 1.0
    elseif status == WHITE_TERRITORY
      score += 1.0
    elseif (status == ALIVE) $ (board[i, j] == WHITE)
      score -= 1.0
    else
      score += 1.0
    end
  end
  score
end

function benchmark(num_games_per_point::Int)
  random_seed = convert(UInt32, 1)
  board_size = 9
  komi = 0.5

  board = Board(board_size)
  set_komi(board, komi)
  set_random_seed(board, random_seed)

  for i = 1:board.size, j = 1:board.size
    white_wins = 0
    black_wins = 0
    for k = 1:num_games_per_point
      passes = 0
      num_moves = 1
      color = WHITE
      clear(board)
      play_move(board, i, j, BLACK)
      while passes < 3 && num_moves < 600
        (movei, movej) = generate_move(board, color)
        play_move(board, movei, movej, color)
        if is_pass_move(movei, movej)
          passes += 1
        else
          passes = 0
        end
        num_moves += 1
        color = other_color(color)
      end
      if passes == 3
        if compute_score(board) > 0
          white_wins += 1
        else
          black_wins += 1
        end
      end
    end
#    @printf("%d %d %f\n", i - 1, j - 1, black_wins / (black_wins + white_wins))
  end
end

function main(args)
  n = 10
  if length(args) > 0
    n = parseint(args[1])
  end
  @time benchmark(n)
end
