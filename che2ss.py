from typing import Generic, Iterable, Iterator, List, Optional, Tuple, Type, TypeVar
from enum import IntFlag
import typing
from re import compile
STARTING_BOARD_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
BB_LIGHT_SQUARES = 0x55aa_55aa_55aa_55aa
BB_DARK_SQUARES = 0xaa55_aa55_aa55_aa55
BB_EMPTY = 0
FEN_CASTLING_REGEX = compile(r"^(?:-|[KQABCDEFGH]{0,2}[kqabcdefgh]{0,2})\Z")
SQUARES=[A1, B1, C1, D1, E1, F1, G1, H1,A2, B2, C2, D2, E2, F2, G2, H2,A3, B3, C3, D3, E3, F3, G3, H3,A4, B4, C4, D4, E4, F4, G4, H4,A5, B5, C5, D5, E5, F5, G5, H5,A6, B6, C6, D6, E6, F6, G6, H6,A7, B7, C7, D7, E7, F7, G7, H7,A8, B8, C8, D8, E8, F8, G8, H8,]=range(64)
BB_SQUARES = [BB_A1, BB_B1, BB_C1, BB_D1, BB_E1, BB_F1, BB_G1, BB_H1,BB_A2, BB_B2, BB_C2, BB_D2, BB_E2, BB_F2, BB_G2, BB_H2,BB_A3, BB_B3, BB_C3, BB_D3, BB_E3, BB_F3, BB_G3, BB_H3,BB_A4, BB_B4, BB_C4, BB_D4, BB_E4, BB_F4, BB_G4, BB_H4,BB_A5, BB_B5, BB_C5, BB_D5, BB_E5, BB_F5, BB_G5, BB_H5,BB_A6, BB_B6, BB_C6, BB_D6, BB_E6, BB_F6, BB_G6, BB_H6,BB_A7, BB_B7, BB_C7, BB_D7, BB_E7, BB_F7, BB_G7, BB_H7,BB_A8, BB_B8, BB_C8, BB_D8, BB_E8, BB_F8, BB_G8, BB_H8] = [1 << sq for sq in SQUARES]
PIECE_SYMBOLS = [None, "p", "n", "b", "r", "q", "k"]
PIECE_NAMES = [None, "pawn", "knight", "bishop", "rook", "queen", "king"]
PIECE_TYPES = [PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING] = range(1,7)
def piece_symbol(piece_type: int, _PIECE_SYMBOLS: List[Optional[str]] = PIECE_SYMBOLS) -> str:
    return typing.cast(str, _PIECE_SYMBOLS[piece_type])
BB_RANKS = [BB_RANK_1,BB_RANK_2,BB_RANK_3,BB_RANK_4,BB_RANK_5,BB_RANK_6,BB_RANK_7,BB_RANK_8] = [0xff << (8 * i) for i in range(8)]
BB_FILES = [BB_FILE_A,BB_FILE_B,BB_FILE_C,BB_FILE_D,BB_FILE_E,BB_FILE_F,BB_FILE_G,BB_FILE_H] = [0x0101_0101_0101_0101 << i for i in range(8)]
FILE_NAMES = ["a", "b", "c", "d", "e", "f", "g", "h"]
RANK_NAMES = ["1", "2", "3", "4", "5", "6", "7", "8"]
SQUARE_NAMES = [f + r for r in RANK_NAMES for f in FILE_NAMES]
BB_BACKRANKS = BB_RANK_1 | BB_RANK_8
def square_mirror(square: int) -> int:
    return square ^ 0x38
BB_CORNERS = BB_A1 | BB_H1 | BB_A8 | BB_H8
BB_CENTER = BB_D4 | BB_E4 | BB_D5 | BB_E5
BaseBoardT = TypeVar("BaseBoardT", bound="BaseBoard")
STARTING_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
BoardT = TypeVar("BoardT", bound="Board")
BB_ALL = 0xffff_ffff_ffff_ffff
COLORS =[WHITE, BLACK] = [True, False]
SQUARES_180 = [square_mirror(sq) for sq in SQUARES]
def square_distance(a: int, b: int) -> int:
    return max(abs(square_file(a) - square_file(b)), abs(square_rank(a) - square_rank(b)))
def square_file(square: int) -> int:
    return square & 7
def square_rank(square: int) -> int:
    return square >> 3
def square_name(square: int) -> str:
    return SQUARE_NAMES[square]
def _edges(square: int) -> int:
    return (((BB_RANK_1 | BB_RANK_8) & ~BB_RANKS[square_rank(square)]) |
            ((BB_FILE_A | BB_FILE_H) & ~BB_FILES[square_file(square)]))
def _carry_rippler(mask: int) -> Iterator[int]:
    subset = BB_EMPTY
    while True:
        yield subset
        subset = (subset - mask) & mask
        if not subset:
            break
def _sliding_attacks(square: int, occupied: int, deltas: Iterable[int]) -> int:
    attacks = BB_EMPTY
    for delta in deltas:
        sq = square
        while True:
            sq += delta
            if not (0 <= sq < 64) or square_distance(sq, sq - delta) > 2:
                break
            attacks |= BB_SQUARES[sq]
            if occupied & BB_SQUARES[sq]:
                break
    return attacks
def _step_attacks(square: int, deltas: Iterable[int]) -> int:
    return _sliding_attacks(square, BB_ALL, deltas)
def _attack_table(deltas: List[int]) -> Tuple[List[int], List[typing.Dict[int, int]]]:
    mask_table = []
    attack_table = []
    for square in SQUARES:
        attacks = {}
        mask = _sliding_attacks(square, 0, deltas) & ~_edges(square)
        for subset in _carry_rippler(mask):
            attacks[subset] = _sliding_attacks(square, subset, deltas)
        attack_table.append(attacks)
        mask_table.append(mask)
    return mask_table, attack_table
def msb(bb: int) -> int:
    return bb.bit_length() - 1
BB_DIAG_MASKS, BB_DIAG_ATTACKS = _attack_table([-9, -7, 7, 9])
BB_FILE_MASKS, BB_FILE_ATTACKS = _attack_table([-8, 8])
BB_RANK_MASKS, BB_RANK_ATTACKS = _attack_table([-1, 1])
BB_KNIGHT_ATTACKS = [_step_attacks(sq, [17, 15, 10, 6, -17, -15, -10, -6]) for sq in SQUARES]
BB_KING_ATTACKS = [_step_attacks(sq, [9, 8, 7, 1, -9, -8, -7, -1]) for sq in SQUARES]
BB_PAWN_ATTACKS = [[_step_attacks(sq, deltas) for sq in SQUARES] for deltas in [[-7, -9], [7, 9]]]
def scan_reversed(bb: int, *, _BB_SQUARES: List[int] = BB_SQUARES) -> Iterator[int]:
    while bb:
        r = bb.bit_length() - 1
        yield r
        bb ^= _BB_SQUARES[r]
def _rays() -> Tuple[List[List[int]], List[List[int]]]:
    rays = []
    between = []
    for a, bb_a in enumerate(BB_SQUARES):
        rays_row = []
        between_row = []
        for b, bb_b in enumerate(BB_SQUARES):
            if BB_DIAG_ATTACKS[a][0] & bb_b:
                rays_row.append((BB_DIAG_ATTACKS[a][0] & BB_DIAG_ATTACKS[b][0]) | bb_a | bb_b)
                between_row.append(BB_DIAG_ATTACKS[a][BB_DIAG_MASKS[a] & bb_b] & BB_DIAG_ATTACKS[b][BB_DIAG_MASKS[b] & bb_a])
            elif BB_RANK_ATTACKS[a][0] & bb_b:
                rays_row.append(BB_RANK_ATTACKS[a][0] | bb_a)
                between_row.append(BB_RANK_ATTACKS[a][BB_RANK_MASKS[a] & bb_b] & BB_RANK_ATTACKS[b][BB_RANK_MASKS[b] & bb_a])
            elif BB_FILE_ATTACKS[a][0] & bb_b:
                rays_row.append(BB_FILE_ATTACKS[a][0] | bb_a)
                between_row.append(BB_FILE_ATTACKS[a][BB_FILE_MASKS[a] & bb_b] & BB_FILE_ATTACKS[b][BB_FILE_MASKS[b] & bb_a])
            else:
                rays_row.append(BB_EMPTY)
                between_row.append(BB_EMPTY)
        rays.append(rays_row)
        between.append(between_row)
    return rays, between
BB_RAYS, BB_BETWEEN = _rays()
def lsb(bb:int) -> int:
    return (bb & -bb).bit_length() - 1
class Status(IntFlag):
    VALID = 0
    NO_WHITE_KING = 1
    NO_BLACK_KING = 2
    TOO_MANY_KINGS = 4
    TOO_MANY_WHITE_PAWNS = 8
    TOO_MANY_BLACK_PAWNS = 16
    PAWNS_ON_BACKRANK = 32
    TOO_MANY_WHITE_PIECES = 64
    TOO_MANY_BLACK_PIECES = 128
    BAD_CASTLING_RIGHTS = 256
    INVALID_EP_SQUARE = 512
    OPPOSITE_CHECK = 1024
    EMPTY = 2048
    RACE_CHECK = 4096
    RACE_OVER = 8192
    RACE_MATERIAL = 16384
    TOO_MANY_CHECKERS = 32768
class Move:
    def __init__(self, from_square: int, to_square: int, promotion: Optional[int] = None, drop: Optional[int] = None) -> None:
        self.from_square = from_square
        self.to_square = to_square
        self.promotion = promotion
        self.drop = drop
    def uci(self) -> str:
        if self.drop:
            return piece_symbol(self.drop).upper() + "@" + SQUARE_NAMES[self.to_square]
        elif self.promotion:
            return SQUARE_NAMES[self.from_square] + SQUARE_NAMES[self.to_square] + piece_symbol(self.promotion)
        elif self:
            return SQUARE_NAMES[self.from_square] + SQUARE_NAMES[self.to_square]
        else:
            return "0000"
    def xboard(self) -> str:
        return self.uci() if self else "@@@@"
    def __bool__(self) -> bool:
        return bool(self.from_square or self.to_square or self.promotion or self.drop)
    def __eq__(self, other: object) -> bool:
        if isinstance(other, Move):
            return (self.from_square == other.from_square and self.to_square == other.to_square and self.promotion == other.promotion and self.drop == other.drop)
        else:
            return NotImplemented
    def __repr__(self) -> str:
        return f"Move.from_uci({self.uci()!r})"
    def __str__(self) -> str:
        return self.uci()
    def __hash__(self) -> int:
        return hash((self.to_square, self.from_square, self.promotion, self.drop))
    @classmethod
    def from_uci(cls, uci: str) -> "Move":
        if uci == "0000":
            return cls.null()
        elif len(uci) == 4 and "@" == uci[1]:
            drop = PIECE_SYMBOLS.index(uci[0].lower())
            square = SQUARE_NAMES.index(uci[2:])
            return cls(square, square, drop=drop)
        elif 4 <= len(uci) <= 5:
            from_square = SQUARE_NAMES.index(uci[0:2])
            to_square = SQUARE_NAMES.index(uci[2:4])
            promotion = PIECE_SYMBOLS.index(uci[4]) if len(uci) == 5 else None
            if from_square == to_square:
                raise ValueError
            return cls(from_square, to_square, promotion=promotion)
        else:
            raise ValueError
    @classmethod
    def null(cls) -> "Move":
        return cls(0, 0)
class Piece:	
	def __init__(self, piece_type: int, color: bool) -> None:
		self.piece_type = piece_type
		self.color = color
	def symbol(self) -> str:
		symbol = piece_symbol(self.piece_type)
		return symbol.upper() if self.color else symbol
	def __hash__(self) -> int:
		return hash(self.piece_type * (self.color + 1))
	def __repr__(self) -> str:
		return f"Piece.from_symbol({self.symbol()!r})"
	def __str__(self) -> str:
		return self.symbol()
	@classmethod
	def from_symbol(cls, symbol: str) -> "Piece":
		return cls(PIECE_SYMBOLS.index(symbol.lower()), symbol.isupper())
class _BoardState(Generic[BoardT]):
    def __init__(self, board: BoardT) -> None:
        self.pawns = board.pawns
        self.knights = board.knights
        self.bishops = board.bishops
        self.rooks = board.rooks
        self.queens = board.queens
        self.kings = board.kings
        self.occupied_w = board.occupied_co[WHITE]
        self.occupied_b = board.occupied_co[BLACK]
        self.occupied = board.occupied
        self.promoted = board.promoted
        self.turn = board.turn
        self.castling_rights = board.castling_rights
        self.ep_square = board.ep_square
        self.halfmove_clock = board.halfmove_clock
        self.fullmove_number = board.fullmove_number
    def restore(self, board: BoardT) -> None:
        board.pawns = self.pawns
        board.knights = self.knights
        board.bishops = self.bishops
        board.rooks = self.rooks
        board.queens = self.queens
        board.kings = self.kings
        board.occupied_co[WHITE] = self.occupied_w
        board.occupied_co[BLACK] = self.occupied_b
        board.occupied = self.occupied
        board.promoted = self.promoted
        board.turn = self.turn
        board.castling_rights = self.castling_rights
        board.ep_square = self.ep_square
        board.halfmove_clock = self.halfmove_clock
        board.fullmove_number = self.fullmove_number
class BaseBoard:
    def __init__(self, board_fen: Optional[str] = STARTING_BOARD_FEN) -> None:
        self.occupied_co = [BB_EMPTY, BB_EMPTY]
        if board_fen is None:
            self._clear_board()
        elif board_fen == STARTING_BOARD_FEN:
            self._reset_board()
        else:
            self._set_board_fen(board_fen)
    def _reset_board(self) -> None:
        self.pawns = BB_RANK_2 | BB_RANK_7
        self.knights = BB_B1 | BB_G1 | BB_B8 | BB_G8
        self.bishops = BB_C1 | BB_F1 | BB_C8 | BB_F8
        self.rooks = BB_CORNERS
        self.queens = BB_D1 | BB_D8
        self.kings = BB_E1 | BB_E8
        self.promoted = BB_EMPTY
        self.occupied_co[WHITE] = BB_RANK_1 | BB_RANK_2
        self.occupied_co[BLACK] = BB_RANK_7 | BB_RANK_8
        self.occupied = BB_RANK_1 | BB_RANK_2 | BB_RANK_7 | BB_RANK_8
    def board_fen(self, *, promoted: Optional[bool] = False) -> str:
        builder = []
        empty = 0
        for square in SQUARES_180:
            piece = self.piece_at(square)
            if not piece:
                empty += 1
            else:
                if empty:
                    builder.append(str(empty))
                    empty = 0
                builder.append(piece.symbol())
                if promoted and BB_SQUARES[square] & self.promoted:
                    builder.append("~")
            if BB_SQUARES[square] & BB_FILE_H:
                if empty:
                    builder.append(str(empty))
                    empty = 0
                if square != H1:
                    builder.append("/")
        return "".join(builder)
    def reset_board(self) -> None:
        self._reset_board()
    def _clear_board(self) -> None:
        self.pawns = BB_EMPTY
        self.knights = BB_EMPTY
        self.bishops = BB_EMPTY
        self.rooks = BB_EMPTY
        self.queens = BB_EMPTY
        self.kings = BB_EMPTY
        self.promoted = BB_EMPTY
        self.occupied_co[WHITE] = BB_EMPTY
        self.occupied_co[BLACK] = BB_EMPTY
        self.occupied = BB_EMPTY
    def clear_board(self) -> None:
        self._clear_board()
    def pieces_mask(self, piece_type: int, color: bool) -> int:
        if piece_type == PAWN:
            bb = self.pawns
        elif piece_type == KNIGHT:
            bb = self.knights
        elif piece_type == BISHOP:
            bb = self.bishops
        elif piece_type == ROOK:
            bb = self.rooks
        elif piece_type == QUEEN:
            bb = self.queens
        elif piece_type == KING:
            bb = self.kings
        return bb & self.occupied_co[color]
    def piece_type_at(self, square: int) -> Optional[int]:
        mask = BB_SQUARES[square]
        if not self.occupied & mask:
            return None
        elif self.pawns & mask:
            return PAWN
        elif self.knights & mask:
            return KNIGHT
        elif self.bishops & mask:
            return BISHOP
        elif self.rooks & mask:
            return ROOK
        elif self.queens & mask:
            return QUEEN
        else:
            return KING
    def color_at(self, square: int) -> Optional[bool]:
        mask = BB_SQUARES[square]
        if self.occupied_co[WHITE] & mask:
            return WHITE
        elif self.occupied_co[BLACK] & mask:
            return BLACK
        else:
            return None
    def king(self, color: bool) -> Optional[int]:
        king_mask = self.occupied_co[color] & self.kings & ~self.promoted
        return msb(king_mask) if king_mask else None
    def attacks_mask(self, square: int) -> int:
        bb_square = BB_SQUARES[square]
        if bb_square & self.pawns:
            color = bool(bb_square & self.occupied_co[WHITE])
            return BB_PAWN_ATTACKS[color][square]
        elif bb_square & self.knights:
            return BB_KNIGHT_ATTACKS[square]
        elif bb_square & self.kings:
            return BB_KING_ATTACKS[square]
        else:
            attacks = 0
            if bb_square & self.bishops or bb_square & self.queens:
                attacks = BB_DIAG_ATTACKS[square][BB_DIAG_MASKS[square] & self.occupied]
            if bb_square & self.rooks or bb_square & self.queens:
                attacks |= (BB_RANK_ATTACKS[square][BB_RANK_MASKS[square] & self.occupied] |
                            BB_FILE_ATTACKS[square][BB_FILE_MASKS[square] & self.occupied])
            return attacks
    def _attackers_mask(self, color: bool, square: int, occupied: int) -> int:
        rank_pieces = BB_RANK_MASKS[square] & occupied
        file_pieces = BB_FILE_MASKS[square] & occupied
        diag_pieces = BB_DIAG_MASKS[square] & occupied
        queens_and_rooks = self.queens | self.rooks
        queens_and_bishops = self.queens | self.bishops
        attackers = ((BB_KING_ATTACKS[square] & self.kings) |(BB_KNIGHT_ATTACKS[square] & self.knights) |(BB_RANK_ATTACKS[square][rank_pieces] & queens_and_rooks) |(BB_FILE_ATTACKS[square][file_pieces] & queens_and_rooks) |(BB_DIAG_ATTACKS[square][diag_pieces] & queens_and_bishops) |(BB_PAWN_ATTACKS[not color][square] & self.pawns))
        return attackers & self.occupied_co[color]
    def attackers_mask(self, color: bool, square: int) -> int:
        return self._attackers_mask(color, square, self.occupied)
    def is_attacked_by(self, color: bool, square: int) -> bool:
        return bool(self.attackers_mask(color, square))
    def pin_mask(self, color: bool, square: int) -> int:
        king = self.king(color)
        if king is None:
            return BB_ALL
        square_mask = BB_SQUARES[square]
        for attacks, sliders in [(BB_FILE_ATTACKS, self.rooks | self.queens),(BB_RANK_ATTACKS, self.rooks | self.queens),(BB_DIAG_ATTACKS, self.bishops | self.queens)]:
            rays = attacks[king][0]
            if rays & square_mask:
                snipers = rays & sliders & self.occupied_co[not color]
                for sniper in scan_reversed(snipers):
                    if BB_BETWEEN[sniper][king] & (self.occupied | square_mask) == square_mask:
                        return BB_RAYS[king][sniper]
                break
        return BB_ALL
    def is_pinned(self, color: bool, square: int) -> bool:
        return self.pin_mask(color, square) != BB_ALL
    def _set_board_fen(self, fen: str) -> None:
        fen = fen.strip()
        if " " in fen:
            raise ValueError
        rows = fen.split("/")
        if len(rows) != 8:
            raise ValueError
        for row in rows:
            field_sum = 0
            previous_was_digit = False
            previous_was_piece = False
            for c in row:
                if c in ["1", "2", "3", "4", "5", "6", "7", "8"]:
                    if previous_was_digit:
                        raise ValueError
                    field_sum += int(c)
                    previous_was_digit = True
                    previous_was_piece = False
                elif c == "~":
                    if not previous_was_piece:
                        raise ValueError
                    previous_was_digit = False
                    previous_was_piece = False
                elif c.lower() in PIECE_SYMBOLS:
                    field_sum += 1
                    previous_was_digit = False
                    previous_was_piece = True
                else:
                    raise ValueError
            if field_sum != 8:
                raise ValueError
        self._clear_board()
        square_index = 0
        for c in fen:
            if c in ["1", "2", "3", "4", "5", "6", "7", "8"]:
                square_index += int(c)
            elif c.lower() in PIECE_SYMBOLS:
                piece = Piece.from_symbol(c)
                self._set_piece_at(SQUARES_180[square_index], piece.piece_type, piece.color)
                square_index += 1
            elif c == "~":
                self.promoted |= BB_SQUARES[SQUARES_180[square_index - 1]]
    def _remove_piece_at(self, square: int) -> Optional[int]:
        piece_type = self.piece_type_at(square)
        mask = BB_SQUARES[square]
        if piece_type == PAWN:
            self.pawns ^= mask
        elif piece_type == KNIGHT:
            self.knights ^= mask
        elif piece_type == BISHOP:
            self.bishops ^= mask
        elif piece_type == ROOK:
            self.rooks ^= mask
        elif piece_type == QUEEN:
            self.queens ^= mask
        elif piece_type == KING:
            self.kings ^= mask
        else:
            return None
        self.occupied ^= mask
        self.occupied_co[WHITE] &= ~mask
        self.occupied_co[BLACK] &= ~mask
        self.promoted &= ~mask
        return piece_type
    def _set_piece_at(self, square: int, piece_type: int, color: bool, promoted: bool = False) -> None:
        self._remove_piece_at(square)
        mask = BB_SQUARES[square]
        if piece_type == PAWN:
            self.pawns |= mask
        elif piece_type == KNIGHT:
            self.knights |= mask
        elif piece_type == BISHOP:
            self.bishops |= mask
        elif piece_type == ROOK:
            self.rooks |= mask
        elif piece_type == QUEEN:
            self.queens |= mask
        elif piece_type == KING:
            self.kings |= mask
        else:
            return
        self.occupied ^= mask
        self.occupied_co[color] ^= mask
        if promoted:
            self.promoted ^= mask
    def set_board_fen(self, fen: str) -> None:
        self._set_board_fen(fen)
    def _set_chess960_pos(self, sharnagl: int) -> None:
        if not 0 <= sharnagl <= 959:
            raise ValueError
        n, bw = divmod(sharnagl, 4)
        n, bb = divmod(n, 4)
        n, q = divmod(n, 6)
        for n1 in range(0, 4):
            n2 = n + (3 - n1) * (4 - n1) // 2 - 5
            if n1 < n2 and 1 <= n2 <= 4:
                break
        bw_file = bw * 2 + 1
        bb_file = bb * 2
        self.bishops = (BB_FILES[bw_file] | BB_FILES[bb_file]) & BB_BACKRANKS
        q_file = q
        q_file += int(min(bw_file, bb_file) <= q_file)
        q_file += int(max(bw_file, bb_file) <= q_file)
        self.queens = BB_FILES[q_file] & BB_BACKRANKS
        used = [bw_file, bb_file, q_file]
        self.knights = BB_EMPTY
        for i in range(0, 8):
            if i not in used:
                if n1 == 0 or n2 == 0:
                    self.knights |= BB_FILES[i] & BB_BACKRANKS
                    used.append(i)
                n1 -= 1
                n2 -= 1
        for i in range(0, 8):
            if i not in used:
                self.rooks = BB_FILES[i] & BB_BACKRANKS
                used.append(i)
                break
        for i in range(1, 8):
            if i not in used:
                self.kings = BB_FILES[i] & BB_BACKRANKS
                used.append(i)
                break
        for i in range(2, 8):
            if i not in used:
                self.rooks |= BB_FILES[i] & BB_BACKRANKS
                break
        self.pawns = BB_RANK_2 | BB_RANK_7
        self.occupied_co[WHITE] = BB_RANK_1 | BB_RANK_2
        self.occupied_co[BLACK] = BB_RANK_7 | BB_RANK_8
        self.occupied = BB_RANK_1 | BB_RANK_2 | BB_RANK_7 | BB_RANK_8
        self.promoted = BB_EMPTY
    def set_chess960_pos(self, sharnagl: int) -> None:
        self._set_chess960_pos(sharnagl)
    def __repr__(self) -> str:
        return f"{type(self).__name__}({self.board_fen()!r})"
    def piece_at(self, square: int) -> Optional[Piece]:
        piece_type = self.piece_type_at(square)
        if piece_type:
            mask = BB_SQUARES[square]
            color = bool(self.occupied_co[WHITE] & mask)
            return Piece(piece_type, color)
        else:
            return None
    def __str__(self) -> str:
        builder = []
        for square in SQUARES_180:
            Piece = self.piece_at(square)
            if Piece:
                builder.append(Piece.symbol())
            else:
                builder.append(".")
            if BB_SQUARES[square] & BB_FILE_H:
                if square != H1:
                    builder.append("\n")
            else:
                builder.append(" ")
        return "".join(builder)
    def _repr_svg_(self) -> str:
        import chess.svg
        return chess.svg.board(board=self, size=400)
    def __eq__(self, board: object) -> bool:
        if isinstance(board, BaseBoard):
            return (self.occupied == board.occupied and self.occupied_co[WHITE] == board.occupied_co[WHITE] and self.pawns == board.pawns and self.knights == board.knights and self.bishops == board.bishops and self.rooks == board.rooks and self.queens == board.queens and self.kings == board.kings)
        else:
            return NotImplemented
    def apply_transform(self, f: typing.Callable[[int], int]) -> None:
        self.pawns = f(self.pawns)
        self.knights = f(self.knights)
        self.bishops = f(self.bishops)
        self.rooks = f(self.rooks)
        self.queens = f(self.queens)
        self.kings = f(self.kings)
        self.occupied_co[WHITE] = f(self.occupied_co[WHITE])
        self.occupied_co[BLACK] = f(self.occupied_co[BLACK])
        self.occupied = f(self.occupied)
        self.promoted = f(self.promoted)
    def transform(self: BaseBoardT, f: typing.Callable[[int], int]) -> BaseBoardT:
        board = self.copy()
        board.apply_transform(f)
        return board
    def copy(self: BaseBoardT) -> BaseBoardT:
        board = type(self)(None)
        board.pawns = self.pawns
        board.knights = self.knights
        board.bishops = self.bishops
        board.rooks = self.rooks
        board.queens = self.queens
        board.kings = self.kings
        board.occupied_co[WHITE] = self.occupied_co[WHITE]
        board.occupied_co[BLACK] = self.occupied_co[BLACK]
        board.occupied = self.occupied
        board.promoted = self.promoted
        return board
    def __copy__(self: BaseBoardT) -> BaseBoardT:
        return self.copy()
    def __deepcopy__(self: BaseBoardT, memo: typing.Dict[int, object]) -> BaseBoardT:
        board = self.copy()
        memo[id(self)] = board
        return board
    @classmethod
    def empty(cls: Type[BaseBoardT]) -> BaseBoardT:
        return cls(None)
    @classmethod
    def from_chess960_pos(cls: Type[BaseBoardT], sharnagl: int) -> BaseBoardT:
        board = cls.empty()
        board.set_chess960_pos(sharnagl)
        return board
class Board(BaseBoard):
    aliases = ["Standard", "Chess", "Classical", "Normal"]
    uci_variant: typing.ClassVar[Optional[str]] = "chess"
    xboard_variant: typing.ClassVar[Optional[str]] = "normal"
    starting_fen = STARTING_FEN
    tbw_suffix: typing.ClassVar[Optional[str]] = ".rtbw"
    tbz_suffix: typing.ClassVar[Optional[str]] = ".rtbz"
    tbw_magic: typing.ClassVar[Optional[bytes]] = b"\x71\xe8\x23\x5d"
    tbz_magic: typing.ClassVar[Optional[bytes]] = b"\xd7\x66\x0c\xa5"
    pawnless_tbw_suffix: typing.ClassVar[Optional[str]] = None
    pawnless_tbz_suffix: typing.ClassVar[Optional[str]] = None
    pawnless_tbw_magic: typing.ClassVar[Optional[bytes]] = None
    pawnless_tbz_magic: typing.ClassVar[Optional[bytes]] = None
    connected_kings = False
    one_king = True
    captures_compulsory = False
    def __init__(self: BoardT, fen: Optional[str] = STARTING_FEN, *, chess960: bool = False) -> None:
        BaseBoard.__init__(self, None)
        self.chess960 = chess960
        self.ep_square: Optional[int] = None
        self.move_stack: List[Move] = []
        self._stack: List[_BoardState[BoardT]] = []
        if fen == type(self).starting_fen:
            self.reset()
        else:
            self.set_fen(fen)
    def _set_castling_fen(self, castling_fen: str) -> None:
        if not castling_fen or castling_fen == "-":
            self.castling_rights = BB_EMPTY
            return
        if not FEN_CASTLING_REGEX.match(castling_fen):
            raise ValueError
        self.castling_rights = BB_EMPTY
        for flag in castling_fen:
            color = WHITE if flag.isupper() else BLACK
            flag = flag.lower()
            backrank = BB_RANK_1 if color == WHITE else BB_RANK_8
            rooks = self.occupied_co[color] & self.rooks & backrank
            king = self.king(color)
            if flag == "q":
                if king is not None and lsb(rooks) < king:
                    self.castling_rights |= rooks & -rooks
                else:
                    self.castling_rights |= BB_FILE_A & backrank
            elif flag == "k":
                rook = msb(rooks)
                if king is not None and king < rook:
                    self.castling_rights |= BB_SQUARES[rook]
                else:
                    self.castling_rights |= BB_FILE_H & backrank
            else:
                self.castling_rights |= BB_FILES[FILE_NAMES.index(flag)] & backrank
    def set_fen(self, fen: str) -> None:
        parts = fen.split()
        board_part = parts.pop(0)
        try:
            turn_part = parts.pop(0)
        except IndexError:
            turn = WHITE
        else:
            if turn_part == "w":
                turn = WHITE
            elif turn_part == "b":
                turn = BLACK
            else:
                raise ValueError
        try:
            castling_part = parts.pop(0)
        except IndexError:
            castling_part = "-"
        try:
            ep_part = parts.pop(0)
        except IndexError:
            ep_square = None
        else:
            ep_square = None if ep_part == "-" else SQUARE_NAMES.index(ep_part)
        try:
            halfmove_part = parts.pop(0)
        except IndexError:
            halfmove_clock = 0
        else:
            halfmove_clock = int(halfmove_part)
            if halfmove_clock < 0:
                raise ValueError
        try:
            fullmove_part = parts.pop(0)
        except IndexError:
            fullmove_number = 1
        else:
            fullmove_number = int(fullmove_part)
            if fullmove_number < 0:
                raise ValueError
            fullmove_number = max(fullmove_number, 1)
        if parts:
            raise ValueError
        self._set_board_fen(board_part)
        self.turn = turn
        self._set_castling_fen(castling_part)
        self.ep_square = ep_square
        self.halfmove_clock = halfmove_clock
        self.fullmove_number = fullmove_number
    def _ep_skewered(self, king: int, capturer: int) -> bool:
        assert self.ep_square is not None
        last_double = self.ep_square + (-8 if self.turn == WHITE else 8)
        occupancy = (self.occupied & ~BB_SQUARES[last_double] & ~BB_SQUARES[capturer] | BB_SQUARES[self.ep_square])
        horizontal_attackers = self.occupied_co[not self.turn] & (self.rooks | self.queens)
        if BB_RANK_ATTACKS[king][BB_RANK_MASKS[king] & occupancy] & horizontal_attackers:
            return True
        diagonal_attackers = self.occupied_co[not self.turn] & (self.bishops | self.queens)
        if BB_DIAG_ATTACKS[king][BB_DIAG_MASKS[king] & occupancy] & diagonal_attackers:
            return True
        return False
    @property
    def pseudo_legal_moves(self) -> "PseudoLegalMoveGenerator":
        return PseudoLegalMoveGenerator(self)
    @property
    def legal_moves(self) -> "LegalMoveGenerator":
        return LegalMoveGenerator(self)
    def reset(self) -> None:
        self.turn = WHITE
        self.castling_rights = BB_CORNERS
        self.ep_square = None
        self.halfmove_clock = 0
        self.fullmove_number = 1
        self.reset_board()
    def _from_chess960(self, chess960: bool, from_square: int, to_square: int, promotion: Optional[int] = None, drop: Optional[int] = None) -> Move:
        if not chess960 and promotion is None and drop is None:
            if from_square == E1 and self.kings & BB_E1:
                if to_square == H1:
                    return Move(E1, G1)
                elif to_square == A1:
                    return Move(E1, C1)
            elif from_square == E8 and self.kings & BB_E8:
                if to_square == H8:
                    return Move(E8, G8)
                elif to_square == A8:
                    return Move(E8, C8)
        return Move(from_square, to_square, promotion, drop)
    def _to_chess960(self, move: Move) -> Move:
        if move.from_square == E1 and self.kings & BB_E1:
            if move.to_square == G1 and not self.rooks & BB_G1:
                return Move(E1, H1)
            elif move.to_square == C1 and not self.rooks & BB_C1:
                return Move(E1, A1)
        elif move.from_square == E8 and self.kings & BB_E8:
            if move.to_square == G8 and not self.rooks & BB_G8:
                return Move(E8, H8)
            elif move.to_square == C8 and not self.rooks & BB_C8:
                return Move(E8, A8)
        return move
    def piece_at(self, square: int) -> Optional[Piece]:
        piece_type = self.piece_type_at(square)
        if piece_type:
            mask = BB_SQUARES[square]
            color = bool(self.occupied_co[WHITE] & mask)
            return Piece(piece_type, color)
        else:
            return None
    def _attacked_for_king(self, path: int, occupied: int) -> bool:
        return any(self._attackers_mask(not self.turn, sq, occupied) for sq in scan_reversed(path))
    def is_check(self) -> bool:
        return bool(self.checkers_mask())
    def _slider_blockers(self, king: int) -> int:
        rooks_and_queens = self.rooks | self.queens
        bishops_and_queens = self.bishops | self.queens
        snipers = ((BB_RANK_ATTACKS[king][0] & rooks_and_queens) |(BB_FILE_ATTACKS[king][0] & rooks_and_queens) |(BB_DIAG_ATTACKS[king][0] & bishops_and_queens))
        blockers = 0
        for sniper in scan_reversed(snipers & self.occupied_co[not self.turn]):
            b = BB_BETWEEN[king][sniper] & self.occupied
            if b and BB_SQUARES[msb(b)] == b:
                blockers |= b
        return blockers & self.occupied_co[self.turn]
    def _generate_evasions(self, king: int, checkers: int, from_mask: int = BB_ALL, to_mask: int = BB_ALL) -> Iterator[Move]:
        sliders = checkers & (self.bishops | self.rooks | self.queens)
        attacked = 0
        for checker in scan_reversed(sliders):
            attacked |= BB_RAYS[king][checker] & ~BB_SQUARES[checker]
        if BB_SQUARES[king] & from_mask:
            for to_square in scan_reversed(BB_KING_ATTACKS[king] & ~self.occupied_co[self.turn] & ~attacked & to_mask):
                yield Move(king, to_square)
        checker = msb(checkers)
        if BB_SQUARES[checker] == checkers:
            target = BB_BETWEEN[king][checker] | checkers
            yield from self.generate_pseudo_legal_moves(~self.kings & from_mask, target & to_mask)
            if self.ep_square and not BB_SQUARES[self.ep_square] & target:
                last_double = self.ep_square + (-8 if self.turn == WHITE else 8)
                if last_double == checker:
                    yield from self.generate_pseudo_legal_ep(from_mask, to_mask)
    def generate_pseudo_legal_ep(self, from_mask: int = BB_ALL, to_mask: int = BB_ALL) -> Iterator[Move]:
        if not self.ep_square or not BB_SQUARES[self.ep_square] & to_mask:
            return
        if BB_SQUARES[self.ep_square] & self.occupied:
            return
        capturers = (self.pawns & self.occupied_co[self.turn] & from_mask & BB_PAWN_ATTACKS[not self.turn][self.ep_square] & BB_RANKS[4 if self.turn else 3])
        for capturer in scan_reversed(capturers):
            yield Move(capturer, self.ep_square)
    def generate_legal_moves(self, from_mask: int = BB_ALL, to_mask: int = BB_ALL) -> Iterator[Move]:
        if self.is_variant_end():
            return
        king_mask = self.kings & self.occupied_co[self.turn]
        if king_mask:
            king = msb(king_mask)
            blockers = self._slider_blockers(king)
            checkers = self.attackers_mask(not self.turn, king)
            if checkers:
                for move in self._generate_evasions(king, checkers, from_mask, to_mask):
                    if self._is_safe(king, blockers, move):
                        yield move
            else:
                for move in self.generate_pseudo_legal_moves(from_mask, to_mask):
                    if self._is_safe(king, blockers, move):
                        yield move
        else:
            yield from self.generate_pseudo_legal_moves(from_mask, to_mask)
    def generate_pseudo_legal_moves(self, from_mask: int = BB_ALL, to_mask: int = BB_ALL) -> Iterator[Move]:
        our_pieces = self.occupied_co[self.turn]
        non_pawns = our_pieces & ~self.pawns & from_mask
        for from_square in scan_reversed(non_pawns):
            moves = self.attacks_mask(from_square) & ~our_pieces & to_mask
            for to_square in scan_reversed(moves):
                yield Move(from_square, to_square)
        if from_mask & self.kings:
            yield from self.generate_castling_moves(from_mask, to_mask)
        pawns = self.pawns & self.occupied_co[self.turn] & from_mask
        if not pawns:
            return
        capturers = pawns
        for from_square in scan_reversed(capturers):
            targets = (BB_PAWN_ATTACKS[self.turn][from_square] & self.occupied_co[not self.turn] & to_mask)
            for to_square in scan_reversed(targets):
                if square_rank(to_square) in [0, 7]:
                    yield Move(from_square, to_square, QUEEN)
                    yield Move(from_square, to_square, ROOK)
                    yield Move(from_square, to_square, BISHOP)
                    yield Move(from_square, to_square, KNIGHT)
                else:
                    yield Move(from_square, to_square)
        if self.turn == WHITE:
            single_moves = pawns << 8 & ~self.occupied
            double_moves = single_moves << 8 & ~self.occupied & (BB_RANK_3 | BB_RANK_4)
        else:
            single_moves = pawns >> 8 & ~self.occupied
            double_moves = single_moves >> 8 & ~self.occupied & (BB_RANK_6 | BB_RANK_5)
        single_moves &= to_mask
        double_moves &= to_mask
        for to_square in scan_reversed(single_moves):
            from_square = to_square + (8 if self.turn == BLACK else -8)
            if square_rank(to_square) in [0, 7]:
                yield Move(from_square, to_square, QUEEN)
                yield Move(from_square, to_square, ROOK)
                yield Move(from_square, to_square, BISHOP)
                yield Move(from_square, to_square, KNIGHT)
            else:
                yield Move(from_square, to_square)
        for to_square in scan_reversed(double_moves):
            from_square = to_square + (16 if self.turn == BLACK else -16)
            yield Move(from_square, to_square)
        if self.ep_square:
            yield from self.generate_pseudo_legal_ep(from_mask, to_mask)
    def _is_safe(self, king: int, blockers: int, move: Move) -> bool:
        if move.from_square == king:
            if self.is_castling(move):
                return True
            else:
                return not self.is_attacked_by(not self.turn, move.to_square)
        elif self.is_en_passant(move):
            return bool(self.pin_mask(self.turn, move.from_square) & BB_SQUARES[move.to_square] and
                        not self._ep_skewered(king, move.from_square))
        else:
            return bool(not blockers & BB_SQUARES[move.from_square] or
                        BB_RAYS[move.from_square][move.to_square] & BB_SQUARES[king])
    def is_en_passant(self, move: Move) -> bool:
        return (self.ep_square == move.to_square and
                bool(self.pawns & BB_SQUARES[move.from_square]) and
                abs(move.to_square - move.from_square) in [7, 9] and
                not self.occupied & BB_SQUARES[move.to_square])
    def result(self, *, claim_draw: bool = False) -> str:
        if self.is_variant_loss():
            return "white wins" if self.turn == WHITE else "black wins"
        elif self.is_variant_win():
            return "black wins" if self.turn == WHITE else "white wins"
        elif self.is_variant_draw():
            return "no one wins"
        if self.is_checkmate():
            return "white wins" if self.turn == WHITE else "black wins"
        if self.is_insufficient_material():
            return "no one wins"
        if not any(self.generate_legal_moves()):
            return "no one wins"
        return '*'
    def generate_castling_moves(self, from_mask: int = BB_ALL, to_mask: int = BB_ALL) -> Iterator[Move]:
        if self.is_variant_end():
            return
        backrank = BB_RANK_1 if self.turn == WHITE else BB_RANK_8
        king = self.occupied_co[self.turn] & self.kings & ~self.promoted & backrank & from_mask
        king = king & -king
        if not king:
            return
        bb_c = BB_FILE_C & backrank
        bb_d = BB_FILE_D & backrank
        bb_f = BB_FILE_F & backrank
        bb_g = BB_FILE_G & backrank
        for candidate in scan_reversed(self.clean_castling_rights() & backrank & to_mask):
            rook = BB_SQUARES[candidate]
            a_side = rook < king
            king_to = bb_c if a_side else bb_g
            rook_to = bb_d if a_side else bb_f
            king_path = BB_BETWEEN[msb(king)][msb(king_to)]
            rook_path = BB_BETWEEN[candidate][msb(rook_to)]
            if not ((self.occupied ^ king ^ rook) & (king_path | rook_path | king_to | rook_to) or
                    self._attacked_for_king(king_path | king, self.occupied ^ king) or
                    self._attacked_for_king(king_to, self.occupied ^ king ^ rook ^ rook_to)):
                yield self._from_chess960(self.chess960, msb(king), candidate)
    def is_variant_end(self) -> bool:
        return False
    def is_variant_loss(self) -> bool:
        return False
    def is_variant_win(self) -> bool:
        return False
    def is_variant_draw(self) -> bool:
        return False
    def is_insufficient_material(self) -> bool:
        return all(self.has_insufficient_material(color) for color in COLORS)
    def has_insufficient_material(self, color: bool) -> bool:
        if self.occupied_co[color] & (self.pawns | self.rooks | self.queens):
            return False
        if self.occupied_co[color] & self.bishops:
            same_color = (not self.bishops & BB_DARK_SQUARES) or (not self.bishops & BB_LIGHT_SQUARES)
            return same_color and not (self.occupied_co[not color] & ~self.kings & ~self.rooks & ~self.queens)
        return True
    def is_checkmate(self) -> bool:
        if not self.is_check():
            return False
        return not any(self.generate_legal_moves())
    def is_castling(self, move: Move) -> bool:
        if self.kings & BB_SQUARES[move.from_square]:
            diff = square_file(move.from_square) - square_file(move.to_square)
            return abs(diff) > 1 or bool(self.rooks & self.occupied_co[self.turn] & BB_SQUARES[move.to_square])
        return False
    def _push_capture(self, move: Move, capture_square: int, piece_type: int, was_promoted: bool) -> None: return not any(self.generate_legal_moves())
    def push(self: BoardT, move: Move) -> None:
        move = self._to_chess960(move)
        self.move_stack.append(self._from_chess960(self.chess960, move.from_square, move.to_square, move.promotion, move.drop))
        self._stack.append(self._board_state())
        ep_square = self.ep_square
        self.ep_square = None
        self.halfmove_clock += 1
        if self.turn == BLACK:
            self.fullmove_number += 1
        if not move:
            self.turn = not self.turn
            return
        if move.drop:
            self._set_piece_at(move.to_square, move.drop, self.turn)
            self.turn = not self.turn
            return
        if self.is_zeroing(move):
            self.halfmove_clock = 0
        from_bb = BB_SQUARES[move.from_square]
        to_bb = BB_SQUARES[move.to_square]
        promoted = bool(self.promoted & from_bb)
        piece_type = self._remove_piece_at(move.from_square)
        assert piece_type is not None,''
        capture_square = move.to_square
        captured_piece_type = self.piece_type_at(capture_square)
        self.castling_rights = self.clean_castling_rights() & ~to_bb & ~from_bb
        if piece_type == KING and not promoted:
            if self.turn == WHITE:
                self.castling_rights &= ~BB_RANK_1
            else:
                self.castling_rights &= ~BB_RANK_8
        elif captured_piece_type == KING and not self.promoted & to_bb:
            if self.turn == WHITE and square_rank(move.to_square) == 7:
                self.castling_rights &= ~BB_RANK_8
            elif self.turn == BLACK and square_rank(move.to_square) == 0:
                self.castling_rights &= ~BB_RANK_1
        if piece_type == PAWN:
            diff = move.to_square - move.from_square
            if diff == 16 and square_rank(move.from_square) == 1:
                self.ep_square = move.from_square + 8
            elif diff == -16 and square_rank(move.from_square) == 6:
                self.ep_square = move.from_square - 8
            elif move.to_square == ep_square and abs(diff) in [7, 9] and not captured_piece_type:
                down = -8 if self.turn == WHITE else 8
                capture_square = ep_square + down
                captured_piece_type = self._remove_piece_at(capture_square)
        if move.promotion:
            promoted = True
            piece_type = move.promotion
        castling = piece_type == KING and self.occupied_co[self.turn] & to_bb
        if castling:
            a_side = square_file(move.to_square) < square_file(move.from_square)
            self._remove_piece_at(move.from_square)
            self._remove_piece_at(move.to_square)
            if a_side:
                self._set_piece_at(C1 if self.turn == WHITE else C8, KING, self.turn)
                self._set_piece_at(D1 if self.turn == WHITE else D8, ROOK, self.turn)
            else:
                self._set_piece_at(G1 if self.turn == WHITE else G8, KING, self.turn)
                self._set_piece_at(F1 if self.turn == WHITE else F8, ROOK, self.turn)
        if not castling:
            was_promoted = bool(self.promoted & to_bb)
            self._set_piece_at(move.to_square, piece_type, self.turn, promoted)
            if captured_piece_type:
                self._push_capture(move, capture_square, captured_piece_type, was_promoted)
        self.turn = not self.turn
    def pop(self: BoardT) -> Move:
        move = self.move_stack.pop()
        self._stack.pop().restore(self)
        return move
    def peek(self) -> Move:
        return self.move_stack[-1]
    def _board_state(self: BoardT) -> _BoardState[BoardT]:
        return _BoardState(self)
    def is_zeroing(self, move: Move) -> bool:
        touched = BB_SQUARES[move.from_square] ^ BB_SQUARES[move.to_square]
        return bool(touched & self.pawns or touched & self.occupied_co[not self.turn] or move.drop == PAWN)
    def clean_castling_rights(self) -> int:
        if self._stack:
            return self.castling_rights
        castling = self.castling_rights & self.rooks
        white_castling = castling & BB_RANK_1 & self.occupied_co[WHITE]
        black_castling = castling & BB_RANK_8 & self.occupied_co[BLACK]
        if not self.chess960:
            white_castling &= (BB_A1 | BB_H1)
            black_castling &= (BB_A8 | BB_H8)
            if not self.occupied_co[WHITE] & self.kings & ~self.promoted & BB_E1:
                white_castling = 0
            if not self.occupied_co[BLACK] & self.kings & ~self.promoted & BB_E8:
                black_castling = 0
            return white_castling | black_castling
        else:
            white_king_mask = self.occupied_co[WHITE] & self.kings & BB_RANK_1 & ~self.promoted
            black_king_mask = self.occupied_co[BLACK] & self.kings & BB_RANK_8 & ~self.promoted
            if not white_king_mask:
                white_castling = 0
            if not black_king_mask:
                black_castling = 0
            white_a_side = white_castling & -white_castling
            white_h_side = BB_SQUARES[msb(white_castling)] if white_castling else 0
            if white_a_side and msb(white_a_side) > msb(white_king_mask):
                white_a_side = 0
            if white_h_side and msb(white_h_side) < msb(white_king_mask):
                white_h_side = 0
            black_a_side = (black_castling & -black_castling)
            black_h_side = BB_SQUARES[msb(black_castling)] if black_castling else BB_EMPTY
            if black_a_side and msb(black_a_side) > msb(black_king_mask):
                black_a_side = 0
            if black_h_side and msb(black_h_side) < msb(black_king_mask):
                black_h_side = 0
            return black_a_side | black_h_side | white_a_side | white_h_side
    def checkers_mask(self) -> int:
        king = self.king(self.turn)
        return BB_EMPTY if king is None else self.attackers_mask(not self.turn, king)
    def uci(self, move: Move, *, chess960: Optional[bool] = None) -> str:
        if chess960 is None:
            chess960 = self.chess960
        move = self._to_chess960(move)
        move = self._from_chess960(chess960, move.from_square, move.to_square, move.promotion, move.drop)
        return move.uci()
class PseudoLegalMoveGenerator:
    def __init__(self, board: Board) -> None:
        self.board = board
    def __bool__(self) -> bool:
        return any(self.board.generate_pseudo_legal_moves())
    def count(self) -> int:
        return len(list(self))
    def __iter__(self) -> Iterator[Move]:
        return self.board.generate_pseudo_legal_moves()
    def __contains__(self, move: Move) -> bool:
        return self.board.is_pseudo_legal(move)
    def __repr__(self) -> str:
        builder = []
        for move in self:
            if self.board.is_legal(move):
                builder.append(self.board.san(move))
            else:
                builder.append(self.board.uci(move))
        sans = ", ".join(builder)
        return f"<PseudoLegalMoveGenerator at {id(self):#x} ({sans})>"
class LegalMoveGenerator:
    def __init__(self, board: Board) -> None:
        self.board = board
    def __bool__(self) -> bool:
        return any(self.board.generate_legal_moves())
    def count(self) -> int:
        return len(list(self))
    def __iter__(self) -> Iterator[Move]:
        return self.board.generate_legal_moves()
    def __contains__(self, move: Move) -> bool:
        return self.board.is_legal(move)
    def __repr__(self) -> str:
        sans = ", ".join(self.board.san(move) for move in self)
        return f"<LegalMoveGenerator at {id(self):#x} ({sans})>"
from tkinter import Tk,Button,Text,INSERT
from math import floor
from threading import Thread
from contextlib import redirect_stdout
from io import StringIO
ch={'P':0,'N':48,'B':32,'R':32,'Q':65,'K':0}
uni={'P':'♟','N':'♞','B':'♝','R':'♜','Q':'♛','K':'♚','p':'♙','n':'♘','b':'♗','r':'♖','q':'♕','k':'♔'}
dic={'P':850,'N':3300,'B':4000,'R':5000,'Q':9500,'K':0}
di={'P':(0,0,0,0,0,0,0,0,-50,-50,25,50,50,-50,-250,-250,-40,-40,20,40,40,-40,-200,-200,-30,-30,15,30,30,-30,-150,-150,-20,-20,10,20,20,-20,-100,-100,-10,-10,5,10,10,-10,-50,-50,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),'N':(-50,-50,-50,-50,-50,-50,-50,-50,-50,0,0,0,0,0,0,-50,-50,0,75,75,75,75,50,-50,-50,0,75,200,200,150,100,-50,-50,0,75,200,200,100,50,-50,-50,0,75,75,75,75,50,-50,-50,0,0,0,0,0,0,-50,-50,-50,-50,-50,-50,-50,-50,-50),'B':(-50,-50,-50,-50,-50,-50,-50,-50,-50,0,0,0,0,0,0,-50,-50,0,75,75,75,75,0,-50,-50,0,75,150,150,75,0,-50,-50,0,75,150,150,75,0,-50,-50,0,75,75,75,75,0,-50,-50,0,0,0,0,0,0,-50,-50,-50,-50,-50,-50,-50,-50,-50),'R':(0,40,80,100,100,100,75,40,0,40,80,100,100,100,75,40,0,40,80,100,100,100,75,40,0,40,80,100,100,100,75,40,0,40,80,100,100,100,75,40,0,40,80,100,100,100,75,40,0,40,80,100,100,100,75,40,0,40,80,100,100,100,75,40),'Q':(0,0,0,0,0,0,0,0,0,60,60,60,60,60,60,0,0,60,120,120,120,120,60,0,0,60,120,180,180,120,60,0,0,60,120,180,180,120,60,0,0,60,120,120,120,120,60,0,0,60,60,60,60,60,60,0,0,0,0,0,0,0,0,0),'K':(-100,-75,-125,-175,-175,-125,-75,-100,-75,-50,-100,-150,-150,-100,-50,-75,-50,-25,-75,-125,-125,-75,-25,-50,-25,0,-50,-100,-100,-50,0,-25,0,25,-25,-75,-75,-25,25,0,25,50,0,-50,-50,0,50,25,75,100,50,0,0,50,100,75,100,125,75,25,25,75,125,100),'k':(-180,-120,-90,-60,-60,-90,-120,-180,-120,-60,-30,0,0,-30,-60,-120,-90,-30,0,30,30,0,-30,-90,-60,0,30,60,60,30,0,-60,-60,0,30,60,60,30,0,-60,-90,-30,0,30,30,0,-30,-90,-120,-60,-30,0,0,-30,-60,-120,-180,-120,-90,-60,-60,-90,-120,-180)}
r=0
n=0
board=Board()
tot=StringIO()
root=Tk()
root.title('smart chess')
def alt(x)->str:
	alv=str(x).replace(' ','').replace('\n','').replace('.',' ')
	for i in uni:
		alv=alv.replace(i,uni[i])
	return alv
def g(i:int)->None:
	with redirect_stdout(tot):
		print("abcdefgh"[i-8*floor(i/8)]+str(8-floor(i/8)))
def f(x:str)->None:
	root=Tk()
	root.title('smart chess')
	for i in range(64):
		Button(root,text=x[i],width=4,command=lambda i=i:g(i),font=("David",24)).grid(row=floor(i/8),column=i-8*floor(i/8))
	Button(root,text='send',command=lambda:exit(),width=4,font=('David',24)).grid(row=9,column=4)
	root.mainloop()
t=Thread(target=f,args=[alt(board)])
t.setDaemon(True)
t.start()
board.push(Move.from_uci('0000'))
def el(z:list,bi:list)->None:
	for j in list(board.legal_moves):
		board.push(j)
		q=board.is_checkmate()*(-1000000)
		bb=str(board)
		bbb=[sum([((2*(x==x.capitalize())-1)*dic[x.capitalize()]+di[x.capitalize()][int(31.5*((2*(x==x.capitalize())-1)+1)-(2*(x==x.capitalize())-1)*i)])*(bb.replace(' ','').replace('\n','')[i]==x) for i in range(64)]) for x in ['P','p','R','r','N','n','B','b','Q','q','K','k']]
		z+=[len(list(board.legal_moves))*5+sum(bbb)+q+r+check+(n==1)*100+(sum(bi)==0)*1500+('R' in bb[8:16])*1000]
		board.pop()
def er(x,logic=1)->None:#if endgame di['K']=di['k']
	global n
	global check
	c=str(x)
	for i in list(x.legal_moves):
		x.push(i)
		z=[10**6]*x.is_checkmate()
		n+=(c.replace(' ','').replace('\n','')['abcdefgh'.index(str(x.peek())[0])+(8-int(str(x.peek())[1]))*8]=='N')+(n==1)/2
		ind=0
		r=0
		for j in alt(x):
			if j=='♖' and '♙' not in [alt(x)[i] for i in [ind%8+8*i for i in range(8)]]:r+=375 if '♟' not in [alt(x)[i] for i in [ind%8+8*i for i in range(8)]] else 125
			ind+=1
		check=x.is_check()*sum([(str(c).replace(' ','').replace('\n','')['abcdefgh'.index(str(x.peek())[0])+(8-int(str(x.peek())[1]))*8]==i)*ch[i] for i in 'PNBRQK'])
		bi=[str(x).replace(' ','').replace('\n','')['abcdefgh'.index(str(x.uci(j))[0])+(8-int(str(x.uci(j))[1]))*8]=='B' for j in list(x.pseudo_legal_moves)]
		el(z,bi)
		x.pop()
		yield min(z)
while board.result()[0]=='*':
	while True:
		while t.is_alive():
			none=None
		F=tot.getvalue().split('\n')[-3]+tot.getvalue().split('\n')[-2]
		tot=StringIO()
		if F+'q' in [str(i) for i in list(board.legal_moves)]:
			F+='0'
		if F in [str(i) for i in list(board.legal_moves)]:
			board.push(Move.from_uci(F))
			break
		else:
			t=Thread(target=f,args=[alt(board)])
			t.setDaemon(True)
			t.start()
	if board.result()[0]=='*':
		moves=list(er(board))
		board.push(list(board.legal_moves)[moves.index(max(moves))])
		t=Thread(target=f,args=[alt(board)])
		t.setDaemon(True)
		t.start()
root=Tk()
root.title('Chess')
te=Text(root)
te.insert(INSERT,board.result())
te.pack()
root.mainloop()
