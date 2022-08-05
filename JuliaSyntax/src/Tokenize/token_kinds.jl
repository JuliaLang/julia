@enum(Kind::UInt16,
    NONE,      # Placeholder; never emitted by lexer
    ENDMARKER, # EOF
    COMMENT, # aadsdsa, #= fdsf #=
    WHITESPACE, # '\n   \t'
    IDENTIFIER, # foo, Σxx
    AT_SIGN, # @
    COMMA, #,
    SEMICOLON, # ;

    begin_errors,
        EOF_MULTICOMMENT,
        EOF_CHAR,
        INVALID_NUMERIC_CONSTANT,
        INVALID_OPERATOR,
        INVALID_INTERPOLATION_TERMINATOR,
        ERROR,
    end_errors,

    begin_keywords,
        KEYWORD, # general
        BAREMODULE,
        BEGIN,
        BREAK,
        CATCH,
        CONST,
        CONTINUE,
        DO,
        ELSE,
        ELSEIF,
        END,
        EXPORT,
        FINALLY,
        FOR,
        FUNCTION,
        GLOBAL,
        IF,
        IMPORT,
        LET,
        LOCAL,
        MACRO,
        MODULE,
        QUOTE,
        RETURN,
        STRUCT,
        TRY,
        USING,
        WHILE,
        begin_contextual_keywords,
            ABSTRACT,
            AS,
            DOC,
            MUTABLE,
            OUTER,
            PRIMITIVE,
            TYPE,
            VAR,
        end_contextual_keywords,
    end_keywords,

    begin_cstparser,
        INVISIBLE_BRACKETS,
        NOTHING,  # A literal `nothing`
        WS,
        SEMICOLON_WS,
        NEWLINE_WS,
        EMPTY_WS,
    end_cstparser,

    begin_literal,
        LITERAL, # general
        INTEGER, # 4
        BIN_INT, # 0b1
        HEX_INT, # 0x0
        OCT_INT, # 0o0
        FLOAT, # 3.5, 3.7e+3
        STRING, # "foo" (without the " delimiters)
        CHAR, # 'a'
        CMD, # `cmd ...` (without delimiters)
        TRUE, FALSE,
    end_literal,

    begin_delimiters,
        LSQUARE, # [
        RSQUARE, # [
        LBRACE, # {
        RBRACE, # }
        LPAREN, # (
        RPAREN,  # )
        DQUOTE,  # " (double quote)
        TRIPLE_DQUOTE, # """
        BACKTICK,  # `
        TRIPLE_BACKTICK, # ```
    end_delimiters,

    begin_ops,
        OP, # general
        DDDOT, # ...

        # Level 1
        begin_assignments,
            EQ, # =
            PLUS_EQ, # +=
            MINUS_EQ, # -=
            STAR_EQ, # *=
            FWD_SLASH_EQ, # /=
            FWDFWD_SLASH_EQ, # //=
            OR_EQ, # |=
            CIRCUMFLEX_EQ, # ^=
            DIVISION_EQ, # ÷=
            REM_EQ, # %=
            LBITSHIFT_EQ, # <<=
            RBITSHIFT_EQ, # >>=
            UNSIGNED_BITSHIFT_EQ, # >>>=
            BACKSLASH_EQ, # \=
            AND_EQ, # &=
            COLON_EQ, # :=
            APPROX, # ~
            EX_OR_EQ, # $=
            XOR_EQ, # ⊻=
        end_assignments,

        begin_pairarrow,
            PAIR_ARROW, # =>
        end_pairarrow,

        # Level 2
        begin_conditional,
            CONDITIONAL, # ?
        end_conditional,

        # Level 3
        begin_arrow,
            RIGHT_ARROW, # -->
            LEFT_ARROW, # <--
            DOUBLE_ARROW, # <-->
            LEFTWARDS_ARROW, # ←
            RIGHTWARDS_ARROW, # →
            LEFT_RIGHT_ARROW, # ↔
            LEFTWARDS_ARROW_WITH_STROKE, # ↚
            RIGHTWARDS_ARROW_WITH_STROKE, # ↛
            LEFTWARDS_TWO_HEADED_ARROW,# ↞
            RIGHTWARDS_TWO_HEADED_ARROW, # ↠
            LEFTWARDS_ARROW_WITH_TAIL, # ↢
            RIGHTWARDS_ARROW_WITH_TAIL, # ↣
            LEFTWARDS_ARROW_FROM_BAR,# ↤
            RIGHTWARDS_ARROW_FROM_BAR, # ↦
            LEFT_RIGHT_ARROW_WITH_STROKE, # ↮
            LEFT_RIGHT_DOUBLE_ARROW_WITH_STROKE, # ⇎
            LEFTWARDS_DOUBLE_ARROW_WITH_STROKE, # ⇍
            RIGHTWARDS_DOUBLE_ARROW_WITH_STROKE, # ⇏
            LEFTWARDS_DOUBLE_ARROW, # ⇐
            RIGHTWARDS_DOUBLE_ARROW, # ⇒
            LEFT_RIGHT_DOUBLE_ARROW, # ⇔
            RIGHT_ARROW_WITH_SMALL_CIRCLE, # ⇴
            THREE_RIGHTWARDS_ARROWS, # ⇶
            LEFTWARDS_ARROW_WITH_VERTICAL_STROKE, # ⇷
            RIGHTWARDS_ARROW_WITH_VERTICAL_STROKE, # ⇸
            LEFT_RIGHT_ARROW_WITH_VERTICAL_STROKE, # ⇹
            LEFTWARDS_ARROW_WITH_DOUBLE_VERTICAL_STROKE, # ⇺
            RIGHTWARDS_ARROW_WITH_DOUBLE_VERTICAL_STROKE, # ⇻
            LEFT_RIGHT_ARROW_WITH_DOUBLE_VERTICAL_STROKE, # ⇼
            LEFTWARDS_OPEN_HEADED_ARROW, # ⇽
            RIGHTWARDS_OPEN_HEADED_ARROW, # ⇾
            LEFT_RIGHT_OPEN_HEADED_ARROW, # ⇿
            LONG_LEFTWARDS_ARROW, # ⟵
            LONG_RIGHTWARDS_ARROW, # ⟶
            LONG_LEFT_RIGHT_ARROW, # ⟷
            LONG_RIGHTWARDS_DOUBLE_ARROW, # ⟹
            LONG_LEFT_RIGHT_DOUBLE_ARROW, # ⟺
            LONG_LEFTWARDS_ARROW_FROM_BAR, # ⟻
            LONG_RIGHTWARDS_ARROW_FROM_BAR, # ⟼
            LONG_LEFTWARDS_DOUBLE_ARROW_FROM_BAR, # ⟽
            LONG_RIGHTWARDS_DOUBLE_ARROW_FROM_BAR, # ⟾
            LONG_RIGHTWARDS_SQUIGGLE_ARROW, # ⟿
            RIGHTWARDS_TWO_HEADED_ARROW_WITH_VERTICAL_STROKE, # ⤀
            RIGHTWARDS_TWO_HEADED_ARROW_WITH_DOUBLE_VERTICAL_STROKE, # ⤁
            LEFTWARDS_DOUBLE_ARROW_WITH_VERTICAL_STROKE, # ⤂
            RIGHTWARDS_DOUBLE_ARROW_WITH_VERTICAL_STROKE, # ⤃
            LEFT_RIGHT_DOUBLE_ARROW_WITH_VERTICAL_STROKE, # ⤄
            RIGHTWARDS_TWO_HEADED_ARROW_FROM_BAR, # ⤅
            LEFTWARDS_DOUBLE_ARROW_FROM_BAR, # ⤆
            RIGHTWARDS_DOUBLE_ARROW_FROM_BAR, # ⤇
            LEFTWARDS_DOUBLE_DASH_ARROW, # ⤌
            RIGHTWARDS_DOUBLE_DASH_ARROW, # ⤍
            LEFTWARDS_TRIPLE_DASH_ARROW, # ⤎
            RIGHTWARDS_TRIPLE_DASH_ARROW, # ⤏
            RIGHTWARDS_TWO_HEADED_TRIPLE_DASH_ARROW, # ⤐
            RIGHTWARDS_ARROW_WITH_DOTTED_STEM, # ⤑
            RIGHTWARDS_ARROW_WITH_TAIL_WITH_VERTICAL_STROKE, # ⤔
            RIGHTWARDS_ARROW_WITH_TAIL_WITH_DOUBLE_VERTICAL_STROKE, # ⤕
            RIGHTWARDS_TWO_HEADED_ARROW_WITH_TAIL, # ⤖
            RIGHTWARDS_TWO_HEADED_ARROW_WITH_TAIL_WITH_VERTICAL_STROKE, # ⤗
            RIGHTWARDS_TWO_HEADED_ARROW_WITH_TAIL_WITH_DOUBLE_VERTICAL_STROKE, # ⤘
            LEFTWARDS_ARROW_TO_BLACK_DIAMOND, # ⤝
            RIGHTWARDS_ARROW_TO_BLACK_DIAMOND, # ⤞
            LEFTWARDS_ARROW_FROM_BAR_TO_BLACK_DIAMOND, # ⤟
            RIGHTWARDS_ARROW_FROM_BAR_TO_BLACK_DIAMOND, # ⤠
            SHORT_RIGHTWARDS_ARROW_ABOVE_LEFTWARDS_ARROW, # ⥄
            RIGHTWARDS_ARROW_WITH_PLUS_BELOW, # ⥅
            LEFTWARDS_ARROW_WITH_PLUS_BELOW, # ⥆
            RIGHTWARDS_ARROW_THROUGH_X, # ⥇
            LEFT_RIGHT_ARROW_THROUGH_SMALL_CIRCLE, # ⥈
            LEFT_BARB_UP_RIGHT_BARB_DOWN_HARPOON, # ⥊
            LEFT_BARB_DOWN_RIGHT_BARB_UP_HARPOON, # ⥋
            LEFT_BARB_UP_RIGHT_BARB_UP_HARPOON, # ⥎
            LEFT_BARB_DOWN_RIGHT_BARB_DOWN_HARPOON, # ⥐
            LEFTWARDS_HARPOON_WITH_BARB_UP_TO_BAR, # ⥒
            RIGHTWARDS_HARPOON_WITH_BARB_UP_TO_BAR, # ⥓
            LEFTWARDS_HARPOON_WITH_BARB_DOWN_TO_BAR, # ⥖
            RIGHTWARDS_HARPOON_WITH_BARB_DOWN_TO_BAR, # ⥗
            LEFTWARDS_HARPOON_WITH_BARB_UP_FROM_BAR, # ⥚
            RIGHTWARDS_HARPOON_WITH_BARB_UP_FROM_BAR, # ⥛
            LEFTWARDS_HARPOON_WITH_BARB_DOWN_FROM_BAR, # ⥞
            RIGHTWARDS_HARPOON_WITH_BARB_DOWN_FROM_BAR, # ⥟
            LEFTWARDS_HARPOON_WITH_BARB_UP_ABOVE_LEFTWARDS_HARPOON_WITH_BARB_DOWN, # ⥢
            RIGHTWARDS_HARPOON_WITH_BARB_UP_ABOVE_RIGHTWARDS_HARPOON_WITH_BARB_DOWN, # ⥤
            LEFTWARDS_HARPOON_WITH_BARB_UP_ABOVE_RIGHTWARDS_HARPOON_WITH_BARB_UP, # ⥦
            LEFTWARDS_HARPOON_WITH_BARB_DOWN_ABOVE_RIGHTWARDS_HARPOON_WITH_BARB_DOWN, # ⥧
            RIGHTWARDS_HARPOON_WITH_BARB_UP_ABOVE_LEFTWARDS_HARPOON_WITH_BARB_UP, # ⥨
            RIGHTWARDS_HARPOON_WITH_BARB_DOWN_ABOVE_LEFTWARDS_HARPOON_WITH_BARB_DOWN, # ⥩
            LEFTWARDS_HARPOON_WITH_BARB_UP_ABOVE_LONG_DASH, # ⥪
            LEFTWARDS_HARPOON_WITH_BARB_DOWN_BELOW_LONG_DASH, # ⥫
            RIGHTWARDS_HARPOON_WITH_BARB_UP_ABOVE_LONG_DASH, # ⥬
            RIGHTWARDS_HARPOON_WITH_BARB_DOWN_BELOW_LONG_DASH, # ⥭
            RIGHT_DOUBLE_ARROW_WITH_ROUNDED_HEAD, # ⥰
            RULE_DELAYED, # ⧴
            THREE_LEFTWARDS_ARROWS, # ⬱
            LEFT_ARROW_WITH_SMALL_CIRCLE, # ⬰
            LEFT_ARROW_WITH_CIRCLED_PLUS, # ⬲
            LONG_LEFTWARDS_SQUIGGLE_ARROW, # ⬳
            LEFTWARDS_TWO_HEADED_ARROW_WITH_VERTICAL_STROKE, # ⬴
            LEFTWARDS_TWO_HEADED_ARROW_WITH_DOUBLE_VERTICAL_STROKE, # ⬵
            LEFTWARDS_TWO_HEADED_ARROW_FROM_BAR, # ⬶
            LEFTWARDS_TWO_HEADED_TRIPLE_DASH_ARROW, # ⬷
            LEFTWARDS_ARROW_WITH_DOTTED_STEM, # ⬸
            LEFTWARDS_ARROW_WITH_TAIL_WITH_VERTICAL_STROKE, # ⬹
            LEFTWARDS_ARROW_WITH_TAIL_WITH_DOUBLE_VERTICAL_STROKE, # ⬺
            LEFTWARDS_TWO_HEADED_ARROW_WITH_TAIL, # ⬻
            LEFTWARDS_TWO_HEADED_ARROW_WITH_TAIL_WITH_VERTICAL_STROKE, # ⬼
            LEFTWARDS_TWO_HEADED_ARROW_WITH_TAIL_WITH_DOUBLE_VERTICAL_STROKE, # ⬽
            LEFTWARDS_ARROW_THROUGH_X, # ⬾
            WAVE_ARROW_POINTING_DIRECTLY_LEFT, # ⬿
            EQUALS_SIGN_ABOVE_LEFTWARDS_ARROW, # ⭀
            REVERSE_TILDE_OPERATOR_ABOVE_LEFTWARDS_ARROW, # ⭁
            LEFTWARDS_ARROW_ABOVE_REVERSE_ALMOST_EQUAL_TO, # ⭂
            RIGHTWARDS_ARROW_THROUGH_GREATER_THAN, # ⭃
            RIGHTWARDS_ARROW_THROUGH_SUPERSET, # ⭄
            REVERSE_TILDE_OPERATOR_ABOVE_RIGHTWARDS_ARROW, # ⭇
            RIGHTWARDS_ARROW_ABOVE_REVERSE_ALMOST_EQUAL_TO, # ⭈
            TILDE_OPERATOR_ABOVE_LEFTWARDS_ARROW, # ⭉
            LEFTWARDS_ARROW_ABOVE_ALMOST_EQUAL_TO, # ⭊
            LEFTWARDS_ARROW_ABOVE_REVERSE_TILDE_OPERATOR, # ⭋
            RIGHTWARDS_ARROW_ABOVE_REVERSE_TILDE_OPERATOR, # ⭌
            HALFWIDTH_LEFTWARDS_ARROW, # ￩
            HALFWIDTH_RIGHTWARDS_ARROW, # ￫
            CIRCLE_ARROW_RIGHT,
            LEFT_SQUIGGLE_ARROW, # ⇜
            RIGHT_SQUIGGLE_ARROW, # ⇝
            LEFT_WAVE_ARROW, # ↜
            RIGHT_WAVE_ARROW, # ↝
            LEFTWARDS_ARROW_WITH_HOOK, # ↩
            RIGHTWARDS_ARROW_WITH_HOOK, # ↪
            LOOP_ARROW_LEFT, # ↫
            LOOP_ARROW_RIGHT, # ↬
            LEFT_HARPOON_UP, # ↼
            LEFT_HARPOON_DOWN, # ↽
            RIGHT_HARPOON_UP, # ⇀
            RIGHT_HARPOON_DOWN, # ⇁
            RIGHT_LEFT_ARROWS, # ⇄
            LEFT_RIGHT_ARROWS, # ⇆
            LEFT_LEFT_ARROWS, # ⇇
            RIGHT_RIGHT_ARROWS, # ⇉
            LEFT_RIGHT_HARPOONS, # ⇋
            RIGHT_LEFT_HARPOONS, # ⇌
            L_LEFT_ARROW, # ⇚
            R_RIGHT_ARROW, # ⇛
            LEFT_DASH_ARROW, # ⇠
            RIGHT_DASH_ARROW, # ⇢
            CURVE_ARROW_RIGHT, # ↷
            CURVE_ARROW_LEFT,# ↶
            CIRCLE_ARROW_LEFT,# ↺
        end_arrow,

        # Level 4
        begin_lazyor,
            LAZY_OR, # ||
        end_lazyor,

        # Level 5
        begin_lazyand,
            LAZY_AND, # &&
        end_lazyand,

        # Level 6
        begin_comparison,
            ISSUBTYPE, # <:
            ISSUPERTYPE, # >:
            GREATER, # >
            LESS, # <
            GREATER_EQ, # >=
            GREATER_THAN_OR_EQUAL_TO, # ≥
            LESS_EQ, # <=
            LESS_THAN_OR_EQUAL_TO, # ≤
            EQEQ, # ==
            EQEQEQ, # ===
            IDENTICAL_TO, # ≡
            NOT_EQ, # !=
            NOT_EQUAL_TO, # ≠
            NOT_IS, # !==
            NOT_IDENTICAL_TO, # ≢
            ELEMENT_OF, # ∈
            IN, # in
            ISA, # isa
            NOT_AN_ELEMENT_OF, # ∉
            CONTAINS_AS_MEMBER, # ∋
            DOES_NOT_CONTAIN_AS_MEMBER, # ∌
            SUBSET_OF_OR_EQUAL_TO, # ⊆
            NEITHER_A_SUBSET_OF_NOR_EQUAL_TO, # ⊈
            SUBSET_OF, # ⊂
            NOT_A_SUBSET_OF, # ⊄
            SUBSET_OF_WITH_NOT_EQUAL_TO, # ⊊
            PROPORTIONAL_TO, # ∝
            SMALL_ELEMENT_OF, # ∊
            SMALL_CONTAINS_AS_MEMBER, # ∍
            PARALLEL_TO, # ∥
            NOT_PARALLEL_TO, # ∦
            PROPORTION, # ∷
            GEOMETRIC_PROPORTION, # ∺
            HOMOTHETIC, # ∻
            REVERSED_TILDE, # ∽
            INVERTED_LAZY_S, # ∾
            NOT_TILDE, # ≁
            ASYMPTOTICALLY_EQUAL_TO, # ≃
            NOT_ASYMPTOTICALLY_EQUAL_TO, # ≄
            APPROXIMATELY_EQUAL_TO, # ≅
            APPROXIMATELY_BUT_NOT_ACTUALLY_EQUAL_TO, # ≆
            NEITHER_APPROXIMATELY_NOR_ACTUALLY_EQUAL_TO, # ≇
            ALMOST_EQUAL_TO, # ≈
            NOT_ALMOST_EQUAL_TO, # ≉
            ALMOST_EQUAL_OR_EQUAL_TO, # ≊
            TRIPLE_TILDE, # ≋
            ALL_EQUAL_TO, # ≌
            EQUIVALENT_TO, # ≍
            GEOMETRICALLY_EQUIVALENT_TO, # ≎
            APPROACHES_THE_LIMIT, # ≐
            GEOMETRICALLY_EQUAL_TO, # ≑
            APPROXIMATELY_EQUAL_TO_OR_THE_IMAGE_OF, # ≒
            IMAGE_OF_OR_APPROXIMATELY_EQUAL_TO, # ≓
            COLON_EQUALS, # ≔
            EQUALS_COLON, # ≕
            RING_IN_EQUAL_TO, # ≖
            RING_EQUAL_TO, # ≗
            CORRESPONDS_TO, # ≘
            ESTIMATES, # ≙
            EQUIANGULAR_TO, # ≚
            STAR_EQUALS, # ≛
            DELTA_EQUAL_TO, # ≜
            EQUAL_TO_BY_DEFINITION, # ≝
            MEASURED_BY, # ≞
            QUESTIONED_EQUAL_TO, # ≟
            STRICTLY_EQUIVALENT_TO, # ≣
            LESS_THAN_OVER_EQUAL_TO, # ≦
            GREATER_THAN_OVER_EQUAL_TO, # ≧
            LESS_THAN_BUT_NOT_EQUAL_TO, # ≨
            GREATER_THAN_BUT_NOT_EQUAL_TO, # ≩
            MUCH_LESS_THAN, # ≪
            MUCH_GREATER_THAN, # ≫
            BETWEEN, # ≬
            NOT_EQUIVALENT_TO, # ≭
            NOT_LESS_THAN, # ≮
            NOT_GREATER_THAN, # ≯
            NEITHER_LESS_THAN_NOR_EQUAL_TO, # ≰
            NEITHER_GREATER_THAN_NOR_EQUAL_TO, # ≱
            LESS_THAN_OR_EQUIVALENT_TO, # ≲
            GREATER_THAN_OR_EQUIVALENT_TO, # ≳
            NEITHER_LESS_THAN_NOR_EQUIVALENT_TO, # ≴
            NEITHER_GREATER_THAN_NOR_EQUIVALENT_TO, # ≵
            LESS_THAN_OR_GREATER_THAN, # ≶
            GREATER_THAN_OR_LESS_THAN, # ≷
            NEITHER_LESS_THAN_NOR_GREATER_THAN, # ≸
            NEITHER_GREATER_THAN_NOR_LESS_THAN, # ≹
            PRECEDES, # ≺
            SUCCEEDS, # ≻
            PRECEDES_OR_EQUAL_TO, # ≼
            SUCCEEDS_OR_EQUAL_TO, # ≽
            PRECEDES_OR_EQUIVALENT_TO, # ≾
            SUCCEEDS_OR_EQUIVALENT_TO, # ≿
            DOES_NOT_PRECEDE, # ⊀
            DOES_NOT_SUCCEED, # ⊁
            SUPERSET_OF, # ⊃
            NOT_A_SUPERSET_OF, # ⊅
            SUPERSET_OF_OR_EQUAL_TO, # ⊇
            NEITHER_A_SUPERSET_OF_NOR_EQUAL_TO, # ⊉
            SUPERSET_OF_WITH_NOT_EQUAL_TO, # ⊋
            SQUARE_IMAGE_OF, # ⊏
            SQUARE_ORIGINAL_OF, # ⊐
            SQUARE_IMAGE_OF_OR_EQUAL_TO, # ⊑
            SQUARE_ORIGINAL_OF_OR_EQUAL_TO, # ⊒
            CIRCLED_EQUALS, # ⊜
            FORCES, # ⊩
            DOES_NOT_PROVE, # ⊬
            DOES_NOT_FORCE, # ⊮
            PRECEDES_UNDER_RELATION, # ⊰
            SUCCEEDS_UNDER_RELATION, # ⊱
            NORMAL_SUBGROUP_OF, # ⊲
            CONTAINS_AS_NORMAL_SUBGROUP, # ⊳
            NORMAL_SUBGROUP_OF_OR_EQUAL_TO, # ⊴
            CONTAINS_AS_NORMAL_SUBGROUP_OR_EQUAL_TO, # ⊵
            ORIGINAL_OF, # ⊶
            IMAGE_OF, # ⊷
            REVERSED_TILDE_EQUALS, # ⋍
            DOUBLE_SUBSET, # ⋐
            DOUBLE_SUPERSET, # ⋑
            EQUAL_AND_PARALLEL_TO, # ⋕
            LESS_THAN_WITH_DOT, # ⋖
            GREATER_THAN_WITH_DOT, # ⋗
            VERY_MUCH_LESS_THAN, # ⋘
            VERY_MUCH_GREATER_THAN, # ⋙
            LESS_THAN_EQUAL_TO_OR_GREATER_THAN, # ⋚
            GREATER_THAN_EQUAL_TO_OR_LESS_THAN, # ⋛
            EQUAL_TO_OR_LESS_THAN, # ⋜
            EQUAL_TO_OR_GREATER_THAN, # ⋝
            EQUAL_TO_OR_PRECEDES, # ⋞
            EQUAL_TO_OR_SUCCEEDS, # ⋟
            DOES_NOT_PRECEDE_OR_EQUAL, # ⋠
            DOES_NOT_SUCCEED_OR_EQUAL, # ⋡
            NOT_SQUARE_IMAGE_OF_OR_EQUAL_TO, # ⋢
            NOT_SQUARE_ORIGINAL_OF_OR_EQUAL_TO, # ⋣
            SQUARE_IMAGE_OF_OR_NOT_EQUAL_TO, # ⋤
            SQUARE_ORIGINAL_OF_OR_NOT_EQUAL_TO, # ⋥
            LESS_THAN_BUT_NOT_EQUIVALENT_TO, # ⋦
            GREATER_THAN_BUT_NOT_EQUIVALENT_TO, # ⋧
            PRECEDES_BUT_NOT_EQUIVALENT_TO, # ⋨
            SUCCEEDS_BUT_NOT_EQUIVALENT_TO, # ⋩
            NOT_NORMAL_SUBGROUP_OF, # ⋪
            DOES_NOT_CONTAIN_AS_NORMAL_SUBGROUP, # ⋫
            NOT_NORMAL_SUBGROUP_OF_OR_EQUAL_TO, # ⋬
            DOES_NOT_CONTAIN_AS_NORMAL_SUBGROUP_OR_EQUAL, # ⋭
            ELEMENT_OF_WITH_LONG_HORIZONTAL_STROKE, # ⋲
            ELEMENT_OF_WITH_VERTICAL_BAR_AT_END_OF_HORIZONTAL_STROKE, # ⋳
            SMALL_ELEMENT_OF_WITH_VERTICAL_BAR_AT_END_OF_HORIZONTAL_STROKE, # ⋴
            ELEMENT_OF_WITH_DOT_ABOVE, # ⋵
            ELEMENT_OF_WITH_OVERBAR, # ⋶
            SMALL_ELEMENT_OF_WITH_OVERBAR, # ⋷
            ELEMENT_OF_WITH_UNDERBAR, # ⋸
            ELEMENT_OF_WITH_TWO_HORIZONTAL_STROKES, # ⋹
            CONTAINS_WITH_LONG_HORIZONTAL_STROKE, # ⋺
            CONTAINS_WITH_VERTICAL_BAR_AT_END_OF_HORIZONTAL_STROKE, # ⋻
            SMALL_CONTAINS_WITH_VERTICAL_BAR_AT_END_OF_HORIZONTAL_STROKE, # ⋼
            CONTAINS_WITH_OVERBAR, # ⋽
            SMALL_CONTAINS_WITH_OVERBAR, # ⋾
            Z_NOTATION_BAG_MEMBERSHIP, # ⋿
            REVERSE_SOLIDUS_PRECEDING_SUBSET, # ⟈
            SUPERSET_PRECEDING_SOLIDUS, # ⟉
            ELEMENT_OF_OPENING_UPWARDS, # ⟒
            CIRCLED_PARALLEL, # ⦷
            CIRCLED_LESS_THAN, # ⧀
            CIRCLED_GREATER_THAN, # ⧁
            INCREASES_AS, # ⧡
            EQUALS_SIGN_AND_SLANTED_PARALLEL, # ⧣
            EQUALS_SIGN_AND_SLANTED_PARALLEL_WITH_TILDE_ABOVE, # ⧤
            IDENTICAL_TO_AND_SLANTED_PARALLEL, # ⧥
            EQUALS_SIGN_WITH_DOT_BELOW, # ⩦
            IDENTICAL_WITH_DOT_ABOVE, # ⩧
            TILDE_OPERATOR_WITH_DOT_ABOVE, # ⩪
            TILDE_OPERATOR_WITH_RISING_DOTS, # ⩫
            SIMILAR_MINUS_SIMILAR, # ⩬
            CONGRUENT_WITH_DOT_ABOVE, # ⩭
            EQUALS_WITH_ASTERISK, # ⩮
            ALMOST_EQUAL_TO_WITH_CIRCUMFLEX_ACCENT, # ⩯
            APPROXIMATELY_EQUAL_OR_EQUAL_TO, # ⩰
            EQUALS_SIGN_ABOVE_PLUS_SIGN, # ⩱
            PLUS_SIGN_ABOVE_EQUALS_SIGN, # ⩲
            EQUALS_SIGN_ABOVE_TILDE_OPERATOR, # ⩳
            DOUBLE_COLON_EQUAL, # ⩴
            TWO_CONSECUTIVE_EQUALS_SIGNS, # ⩵
            THREE_CONSECUTIVE_EQUALS_SIGNS, # ⩶
            EQUALS_SIGN_WITH_TWO_DOTS_ABOVE_AND_TWO_DOTS_BELOW, # ⩷
            EQUIVALENT_WITH_FOUR_DOTS_ABOVE, # ⩸
            LESS_THAN_WITH_CIRCLE_INSIDE, # ⩹
            GREATER_THAN_WITH_CIRCLE_INSIDE, # ⩺
            LESS_THAN_WITH_QUESTION_MARK_ABOVE, # ⩻
            GREATER_THAN_WITH_QUESTION_MARK_ABOVE, # ⩼
            LESS_THAN_OR_SLANTED_EQUAL_TO, # ⩽
            GREATER_THAN_OR_SLANTED_EQUAL_TO, # ⩾
            LESS_THAN_OR_SLANTED_EQUAL_TO_WITH_DOT_INSIDE, # ⩿
            GREATER_THAN_OR_SLANTED_EQUAL_TO_WITH_DOT_INSIDE, # ⪀
            LESS_THAN_OR_SLANTED_EQUAL_TO_WITH_DOT_ABOVE, # ⪁
            GREATER_THAN_OR_SLANTED_EQUAL_TO_WITH_DOT_ABOVE, # ⪂
            LESS_THAN_OR_SLANTED_EQUAL_TO_WITH_DOT_ABOVE_RIGHT, # ⪃
            GREATER_THAN_OR_SLANTED_EQUAL_TO_WITH_DOT_ABOVE_LEFT, # ⪄
            LESS_THAN_OR_APPROXIMATE, # ⪅
            GREATER_THAN_OR_APPROXIMATE, # ⪆
            LESS_THAN_AND_SINGLE_LINE_NOT_EQUAL_TO, # ⪇
            GREATER_THAN_AND_SINGLE_LINE_NOT_EQUAL_TO, # ⪈
            LESS_THAN_AND_NOT_APPROXIMATE, # ⪉
            GREATER_THAN_AND_NOT_APPROXIMATE, # ⪊
            LESS_THAN_ABOVE_DOUBLE_LINE_EQUAL_ABOVE_GREATER_THAN, # ⪋
            GREATER_THAN_ABOVE_DOUBLE_LINE_EQUAL_ABOVE_LESS_THAN, # ⪌
            LESS_THAN_ABOVE_SIMILAR_OR_EQUAL, # ⪍
            GREATER_THAN_ABOVE_SIMILAR_OR_EQUAL, # ⪎
            LESS_THAN_ABOVE_SIMILAR_ABOVE_GREATER_THAN, # ⪏
            GREATER_THAN_ABOVE_SIMILAR_ABOVE_LESS_THAN, # ⪐
            LESS_THAN_ABOVE_GREATER_THAN_ABOVE_DOUBLE_LINE_EQUAL, # ⪑
            GREATER_THAN_ABOVE_LESS_THAN_ABOVE_DOUBLE_LINE_EQUAL, # ⪒
            LESS_THAN_ABOVE_SLANTED_EQUAL_ABOVE_GREATER_THAN_ABOVE_SLANTED_EQUAL, # ⪓
            GREATER_THAN_ABOVE_SLANTED_EQUAL_ABOVE_LESS_THAN_ABOVE_SLANTED_EQUAL, # ⪔
            SLANTED_EQUAL_TO_OR_LESS_THAN, # ⪕
            SLANTED_EQUAL_TO_OR_GREATER_THAN, # ⪖
            SLANTED_EQUAL_TO_OR_LESS_THAN_WITH_DOT_INSIDE, # ⪗
            SLANTED_EQUAL_TO_OR_GREATER_THAN_WITH_DOT_INSIDE, # ⪘
            DOUBLE_LINE_EQUAL_TO_OR_LESS_THAN, # ⪙
            DOUBLE_LINE_EQUAL_TO_OR_GREATER_THAN, # ⪚
            DOUBLE_LINE_SLANTED_EQUAL_TO_OR_LESS_THAN, # ⪛
            DOUBLE_LINE_SLANTED_EQUAL_TO_OR_GREATER_THAN, # ⪜
            SIMILAR_OR_LESS_THAN, # ⪝
            SIMILAR_OR_GREATER_THAN, # ⪞
            SIMILAR_ABOVE_LESS_THAN_ABOVE_EQUALS_SIGN, # ⪟
            SIMILAR_ABOVE_GREATER_THAN_ABOVE_EQUALS_SIGN, # ⪠
            DOUBLE_NESTED_LESS_THAN, # ⪡
            DOUBLE_NESTED_GREATER_THAN, # ⪢
            DOUBLE_NESTED_LESS_THAN_WITH_UNDERBAR, # ⪣
            GREATER_THAN_OVERLAPPING_LESS_THAN, # ⪤
            GREATER_THAN_BESIDE_LESS_THAN, # ⪥
            LESS_THAN_CLOSED_BY_CURVE, # ⪦
            GREATER_THAN_CLOSED_BY_CURVE, # ⪧
            LESS_THAN_CLOSED_BY_CURVE_ABOVE_SLANTED_EQUAL, # ⪨
            GREATER_THAN_CLOSED_BY_CURVE_ABOVE_SLANTED_EQUAL, # ⪩
            SMALLER_THAN, # ⪪
            LARGER_THAN, # ⪫
            SMALLER_THAN_OR_EQUAL_TO, # ⪬
            LARGER_THAN_OR_EQUAL_TO, # ⪭
            EQUALS_SIGN_WITH_BUMPY_ABOVE, # ⪮
            PRECEDES_ABOVE_SINGLE_LINE_EQUALS_SIGN, # ⪯
            SUCCEEDS_ABOVE_SINGLE_LINE_EQUALS_SIGN, # ⪰
            PRECEDES_ABOVE_SINGLE_LINE_NOT_EQUAL_TO, # ⪱
            SUCCEEDS_ABOVE_SINGLE_LINE_NOT_EQUAL_TO, # ⪲
            PRECEDES_ABOVE_EQUALS_SIGN, # ⪳
            SUCCEEDS_ABOVE_EQUALS_SIGN, # ⪴
            PRECEDES_ABOVE_NOT_EQUAL_TO, # ⪵
            SUCCEEDS_ABOVE_NOT_EQUAL_TO, # ⪶
            PRECEDES_ABOVE_ALMOST_EQUAL_TO, # ⪷
            SUCCEEDS_ABOVE_ALMOST_EQUAL_TO, # ⪸
            PRECEDES_ABOVE_NOT_ALMOST_EQUAL_TO, # ⪹
            SUCCEEDS_ABOVE_NOT_ALMOST_EQUAL_TO, # ⪺
            DOUBLE_PRECEDES, # ⪻
            DOUBLE_SUCCEEDS, # ⪼
            SUBSET_WITH_DOT, # ⪽
            SUPERSET_WITH_DOT, # ⪾
            SUBSET_WITH_PLUS_SIGN_BELOW, # ⪿
            SUPERSET_WITH_PLUS_SIGN_BELOW, # ⫀
            SUBSET_WITH_MULTIPLICATION_SIGN_BELOW, # ⫁
            SUPERSET_WITH_MULTIPLICATION_SIGN_BELOW, # ⫂
            SUBSET_OF_OR_EQUAL_TO_WITH_DOT_ABOVE, # ⫃
            SUPERSET_OF_OR_EQUAL_TO_WITH_DOT_ABOVE, # ⫄
            SUBSET_OF_ABOVE_EQUALS_SIGN, # ⫅
            SUPERSET_OF_ABOVE_EQUALS_SIGN, # ⫆
            SUBSET_OF_ABOVE_TILDE_OPERATOR, # ⫇
            SUPERSET_OF_ABOVE_TILDE_OPERATOR, # ⫈
            SUBSET_OF_ABOVE_ALMOST_EQUAL_TO, # ⫉
            SUPERSET_OF_ABOVE_ALMOST_EQUAL_TO, # ⫊
            SUBSET_OF_ABOVE_NOT_EQUAL_TO, # ⫋
            SUPERSET_OF_ABOVE_NOT_EQUAL_TO, # ⫌
            SQUARE_LEFT_OPEN_BOX_OPERATOR, # ⫍
            SQUARE_RIGHT_OPEN_BOX_OPERATOR, # ⫎
            CLOSED_SUBSET, # ⫏
            CLOSED_SUPERSET, # ⫐
            CLOSED_SUBSET_OR_EQUAL_TO, # ⫑
            CLOSED_SUPERSET_OR_EQUAL_TO, # ⫒
            SUBSET_ABOVE_SUPERSET, # ⫓
            SUPERSET_ABOVE_SUBSET, # ⫔
            SUBSET_ABOVE_SUBSET, # ⫕
            SUPERSET_ABOVE_SUPERSET, # ⫖
            SUPERSET_BESIDE_SUBSET, # ⫗
            SUPERSET_BESIDE_AND_JOINED_BY_DASH_WITH_SUBSET, # ⫘
            ELEMENT_OF_OPENING_DOWNWARDS, # ⫙
            TRIPLE_NESTED_LESS_THAN, # ⫷
            TRIPLE_NESTED_GREATER_THAN, # ⫸
            DOUBLE_LINE_SLANTED_LESS_THAN_OR_EQUAL_TO, # ⫹
            DOUBLE_LINE_SLANTED_GREATER_THAN_OR_EQUAL_TO, # ⫺
            RIGHT_TACK, # ⊢
            LEFT_TACK, # ⊣
            DOUBLE_DOWN_TACK, # ⫪
            DOUBLE_UP_TACK, # ⫫
            PERP, # ⟂
        end_comparison,

        # Level 7
        begin_pipe,
            LPIPE, # <|
            RPIPE, # |>
        end_pipe,

        # Level 8
        begin_colon,
            COLON, # :
            DDOT, # ..
            LDOTS, # …
            TRICOLON, # ⁝
            VDOTS, # ⋮
            DDOTS, # ⋱
            ADOTS, # ⋰
            CDOTS, # ⋯
        end_colon,

        # Level 9
        begin_plus,
            EX_OR, # $
            PLUS, # +
            MINUS, # -
            PLUSPLUS, # ++
            CIRCLED_PLUS, # ⊕
            CIRCLED_MINUS, # ⊖
            SQUARED_PLUS, # ⊞
            SQUARED_MINUS, # ⊟
            OR, # |
            UNION, # ∪
            LOGICAL_OR, # ∨
            SQUARE_CUP, # ⊔
            PLUS_MINUS_SIGN, # ±
            MINUS_OR_PLUS_SIGN, # ∓
            DOT_PLUS, # ∔
            DOT_MINUS, # ∸
            MINUS_TILDE, # ≂
            DIFFERENCE_BETWEEN, # ≏
            MULTISET_UNION, # ⊎
            XOR, # ⊻
            NOR, # ⊽
            CURLY_LOGICAL_OR, # ⋎
            DOUBLE_UNION, # ⋓
            DOUBLE_PLUS, # ⧺
            TRIPLE_PLUS, # ⧻
            TWO_LOGICAL_OR_OPERATOR, # ⨈
            PLUS_SIGN_WITH_SMALL_CIRCLE_ABOVE, # ⨢
            PLUS_SIGN_WITH_CIRCUMFLEX_ACCENT_ABOVE, # ⨣
            PLUS_SIGN_WITH_TILDE_ABOVE, # ⨤
            PLUS_SIGN_WITH_DOT_BELOW, # ⨥
            PLUS_SIGN_WITH_TILDE_BELOW, # ⨦
            PLUS_SIGN_WITH_SUBSCRIPT_TWO, # ⨧
            PLUS_SIGN_WITH_BLACK_TRIANGLE, # ⨨
            MINUS_SIGN_WITH_COMMA_ABOVE, # ⨩
            MINUS_SIGN_WITH_DOT_BELOW, # ⨪
            MINUS_SIGN_WITH_FALLING_DOTS, # ⨫
            MINUS_SIGN_WITH_RISING_DOTS, # ⨬
            PLUS_SIGN_IN_LEFT_HALF_CIRCLE, # ⨭
            PLUS_SIGN_IN_RIGHT_HALF_CIRCLE, # ⨮
            PLUS_SIGN_IN_TRIANGLE, # ⨹
            MINUS_SIGN_IN_TRIANGLE, # ⨺
            UNION_WITH_MINUS_SIGN, # ⩁
            UNION_WITH_OVERBAR, # ⩂
            UNION_WITH_LOGICAL_OR, # ⩅
            UNION_BESIDE_AND_JOINED_WITH_UNION, # ⩊
            CLOSED_UNION_WITH_SERIFS, # ⩌
            DOUBLE_SQUARE_UNION, # ⩏
            CLOSED_UNION_WITH_SERIFS_AND_SMASH_PRODUCT, # ⩐
            LOGICAL_OR_WITH_DOT_ABOVE, # ⩒
            DOUBLE_LOGICAL_OR, # ⩔
            TWO_INTERSECTING_LOGICAL_OR, # ⩖
            SLOPING_LARGE_OR, # ⩗
            LOGICAL_OR_WITH_MIDDLE_STEM, # ⩛
            LOGICAL_OR_WITH_HORIZONTAL_DASH, # ⩝
            SMALL_VEE_WITH_UNDERBAR, # ⩡
            LOGICAL_OR_WITH_DOUBLE_OVERBAR, # ⩢
            LOGICAL_OR_WITH_DOUBLE_UNDERBAR, # ⩣
            BROKEN_BAR, # ¦
        end_plus,

        # Level 10
        begin_bitshifts,
          LBITSHIFT, # <<
          RBITSHIFT, # >>
          UNSIGNED_BITSHIFT, # >>>
        end_bitshifts,

        # Level 11
        begin_times,
            STAR,  # *
            FWD_SLASH, # /
            DIVISION_SIGN, # ÷
            REM, # %
            UNICODE_DOT, # ⋅
            RING_OPERATOR, # ∘
            MULTIPLICATION_SIGN, # ×
            BACKSLASH, # \
            AND, # &
            INTERSECTION, # ∩
            LOGICAL_AND, # ∧
            CIRCLED_TIMES, # ⊗
            CIRCLED_DIVISION_SLASH, # ⊘
            CIRCLED_DOT_OPERATOR, # ⊙
            CIRCLED_RING_OPERATOR, # ⊚
            CIRCLED_ASTERISK_OPERATOR, # ⊛
            SQUARED_TIMES, # ⊠
            SQUARED_DOT_OPERATOR, # ⊡
            SQUARE_CAP, # ⊓
            ASTERISK_OPERATOR, # ∗
            BULLET_OPERATOR, # ∙
            DOES_NOT_DIVIDE, # ∤
            TURNED_AMPERSAND, # ⅋
            WREATH_PRODUCT, # ≀
            NAND, # ⊼
            DIAMOND_OPERATOR, # ⋄
            STAR_OPERATOR, # ⋆
            DIVISION_TIMES, # ⋇
            LEFT_NORMAL_FACTOR_SEMIDIRECT_PRODUCT, # ⋉
            RIGHT_NORMAL_FACTOR_SEMIDIRECT_PRODUCT, # ⋊
            LEFT_SEMIDIRECT_PRODUCT, # ⋋
            RIGHT_SEMIDIRECT_PRODUCT, # ⋌
            CURLY_LOGICAL_AND, # ⋏
            DOUBLE_INTERSECTION, # ⋒
            AND_WITH_DOT, # ⟑
            CIRCLED_REVERSE_SOLIDUS, # ⦸
            CIRCLED_ANTICLOCKWISE_ROTATED_DIVISION_SIGN, # ⦼
            CIRCLED_WHITE_BULLET, # ⦾
            CIRCLED_BULLET, # ⦿
            SOLIDUS_WITH_OVERBAR, # ⧶
            REVERSE_SOLIDUS_WITH_HORIZONTAL_STROKE, # ⧷
            TWO_LOGICAL_AND_OPERATOR, # ⨇
            MULTIPLICATION_SIGN_WITH_DOT_ABOVE, # ⨰
            MULTIPLICATION_SIGN_WITH_UNDERBAR, # ⨱
            SEMIDIRECT_PRODUCT_WITH_BOTTOM_CLOSED, # ⨲
            SMASH_PRODUCT, # ⨳
            MULTIPLICATION_SIGN_IN_LEFT_HALF_CIRCLE, # ⨴
            MULTIPLICATION_SIGN_IN_RIGHT_HALF_CIRCLE, # ⨵
            CIRCLED_MULTIPLICATION_SIGN_WITH_CIRCUMFLEX_ACCENT, # ⨶
            MULTIPLICATION_SIGN_IN_DOUBLE_CIRCLE, # ⨷
            CIRCLED_DIVISION_SIGN, # ⨸
            MULTIPLICATION_SIGN_IN_TRIANGLE, # ⨻
            INTERIOR_PRODUCT, # ⨼
            RIGHTHAND_INTERIOR_PRODUCT, # ⨽
            INTERSECTION_WITH_DOT, # ⩀
            INTERSECTION_WITH_OVERBAR, # ⩃
            INTERSECTION_WITH_LOGICAL_AND, # ⩄
            INTERSECTION_BESIDE_AND_JOINED_WITH_INTERSECTION, # ⩋
            CLOSED_INTERSECTION_WITH_SERIFS, # ⩍
            DOUBLE_SQUARE_INTERSECTION, # ⩎
            LOGICAL_AND_WITH_DOT_ABOVE, # ⩑
            DOUBLE_LOGICAL_AND, # ⩓
            TWO_INTERSECTING_LOGICAL_AND, # ⩕
            SLOPING_LARGE_AND, # ⩘
            LOGICAL_AND_WITH_MIDDLE_STEM, # ⩚
            LOGICAL_AND_WITH_HORIZONTAL_DASH, # ⩜
            LOGICAL_AND_WITH_DOUBLE_OVERBAR, # ⩞
            LOGICAL_AND_WITH_UNDERBAR, # ⩟
            LOGICAL_AND_WITH_DOUBLE_UNDERBAR, # ⩠
            TRANSVERSAL_INTERSECTION, # ⫛
            MULTISET_MULTIPLICATION, # ⊍
            WHITE_RIGHT_POINTING_TRIANGLE, # ▷
            JOIN, # ⨝
            LEFT_OUTER_JOIN, # ⟕
            RIGHT_OUTER_JOIN, # ⟖
            FULL_OUTER_JOIN, # ⟗
            NOT_SLASH, # ⌿
            BB_SEMI, # ⨟
        end_times,

        # Level 12
        begin_rational,
            FWDFWD_SLASH, # //
        end_rational,

        # Level 13
        begin_power,
            CIRCUMFLEX_ACCENT, # ^
            UPWARDS_ARROW, # ↑
            DOWNWARDS_ARROW, # ↓
            DOWNWARDS_ARROW_LEFTWARDS_OF_UPWARDS_ARROW, # ⇵
            UPWARDS_QUADRUPLE_ARROW, # ⟰
            DOWNWARDS_QUADRUPLE_ARROW, # ⟱
            DOWNWARDS_ARROW_WITH_HORIZONTAL_STROKE, # ⤈
            UPWARDS_ARROW_WITH_HORIZONTAL_STROKE, # ⤉
            UPWARDS_TRIPLE_ARROW, # ⤊
            DOWNWARDS_TRIPLE_ARROW, # ⤋
            UPWARDS_ARROW_TO_BAR, # ⤒
            DOWNWARDS_ARROW_TO_BAR, # ⤓
            UPWARDS_TWO_HEADED_ARROW_FROM_SMALL_CIRCLE, # ⥉
            UP_BARB_RIGHT_DOWN_BARB_LEFT_HARPOON, # ⥌
            UP_BARB_LEFT_DOWN_BARB_RIGHT_HARPOON, # ⥍
            UP_BARB_RIGHT_DOWN_BARB_RIGHT_HARPOON, # ⥏
            UP_BARB_LEFT_DOWN_BARB_LEFT_HARPOON, # ⥑
            UPWARDS_HARPOON_WITH_BARB_RIGHT_TO_BAR, # ⥔
            DOWNWARDS_HARPOON_WITH_BARB_RIGHT_TO_BAR, # ⥕
            UPWARDS_HARPOON_WITH_BARB_LEFT_TO_BAR, # ⥘
            DOWNWARDS_HARPOON_WITH_BARB_LEFT_TO_BAR, # ⥙
            UPWARDS_HARPOON_WITH_BARB_RIGHT_FROM_BAR, # ⥜
            DOWNWARDS_HARPOON_WITH_BARB_RIGHT_FROM_BAR, # ⥝
            UPWARDS_HARPOON_WITH_BARB_LEFT_FROM_BAR, # ⥠
            DOWNWARDS_HARPOON_WITH_BARB_LEFT_FROM_BAR, # ⥡
            UPWARDS_HARPOON_WITH_BARB_LEFT_BESIDE_UPWARDS_HARPOON_WITH_BARB_RIGHT, # ⥣
            DOWNWARDS_HARPOON_WITH_BARB_LEFT_BESIDE_DOWNWARDS_HARPOON_WITH_BARB_RIGHT, # ⥥
            UPWARDS_HARPOON_WITH_BARB_LEFT_BESIDE_DOWNWARDS_HARPOON_WITH_BARB_RIGHT, # ⥮
            DOWNWARDS_HARPOON_WITH_BARB_LEFT_BESIDE_UPWARDS_HARPOON_WITH_BARB_RIGHT, # ⥯
            HALFWIDTH_UPWARDS_ARROW, # ￪
            HALFWIDTH_DOWNWARDS_ARROW, # ￬
        end_power,

        # Level 14
        begin_decl,
            DECLARATION, # ::
        end_decl,

        # Level 15
        begin_where,
            WHERE,
        end_where,

        # Level 16
        begin_dot,
            DOT,# .
        end_dot,

        NOT, # !
        PRIME, # '
        TRANSPOSE, # .'
        ANON_FUNC, # ->

        begin_unicode_ops,
            NOT_SIGN, # ¬
            SQUARE_ROOT, # √
            CUBE_ROOT, # ∛
            QUAD_ROOT, # ∜
        end_unicode_ops,
    end_ops,

    # Kinds emitted by the parser. There's two types of these:
    # 1. Implied tokens which have a position but might have zero width in the
    #    source text.
    #
    # In some cases we want to generate parse tree nodes in a standard form,
    # but some of the leaf tokens are implied rather than existing in the
    # source text, or the lexed tokens need to be re-kinded to represent
    # special forms which only the parser can infer. These are "parser tokens".
    #
    # Some examples:
    #
    # Docstrings - the macro name is invisible
    #   "doc" foo() = 1   ==>  (macrocall (core @doc) . (= (call foo) 1))
    #
    # String macros - the macro name does not appear in the source text, so we
    # need a special kind of token to imply it.
    #
    # In these cases, we use some special kinds which can be emitted as zero
    # width tokens to keep the parse tree more uniform.
    begin_parser_tokens,
        TOMBSTONE,           # Empty placeholder for kind to be filled later

        # Macro names are modelled as a special kind of identifier because the
        # @ may not be attached to the macro name in the source (or may not be
        # associated with a token at all in the case of implied macro calls
        # like CORE_DOC_MACRO_NAME)
        begin_macro_names,
            MACRO_NAME,                  # A macro name identifier
            STRING_MACRO_NAME,           # macname"some_str"
            CMD_MACRO_NAME,              # macname`some_str`
            DOT_MACRO_NAME,              # The macro name of @.
            CORE_DOC_MACRO_NAME,         # Core.@doc
            CORE_CMD_MACRO_NAME,         # Core.@cmd
            CORE_INT128_STR_MACRO_NAME,  # Core.@int128_str
            CORE_UINT128_STR_MACRO_NAME, # Core.@uint128_str
            CORE_BIG_STR_MACRO_NAME,     # Core.@big_str
        end_macro_names,
    end_parser_tokens,

    # 2. Nonterminals which are exposed in the AST, but where the surface
    #    syntax doesn't have a token corresponding to the node type.
    begin_syntax_kinds,
        BLOCK,
        CALL,
        COMPARISON,
        CURLY,
        INERT,           # QuoteNode; not quasiquote
        STRING_INTERP,   # "a $x"
        TOPLEVEL,
        TUPLE,
        REF,
        VECT,
        MACROCALL,
        KW,             # the = in f(a=1)
        PARAMETERS,     # the list after ; in f(; a=1)
        # Concatenation syntax
        BRACES,
        BRACESCAT,
        HCAT,
        VCAT,
        NCAT,
        TYPED_HCAT,
        TYPED_VCAT,
        TYPED_NCAT,
        ROW,
        NROW,
        # Comprehensions
        GENERATOR,
        FILTER,
        FLATTEN,
        COMPREHENSION,
        TYPED_COMPREHENSION,
    end_syntax_kinds,
)


const UNICODE_OPS = Dict{Char, Kind}(
'−' => MINUS,
'÷' => DIVISION_SIGN,
'¬' => NOT_SIGN,
'√' => SQUARE_ROOT,
'∛' => CUBE_ROOT,
'∜' => QUAD_ROOT,
'←' => LEFTWARDS_ARROW,
'→' => RIGHTWARDS_ARROW,
'↔' => LEFT_RIGHT_ARROW,
'↚' => LEFTWARDS_ARROW_WITH_STROKE,
'↛' => RIGHTWARDS_ARROW_WITH_STROKE,
'↞' => LEFTWARDS_TWO_HEADED_ARROW,
'↠' => RIGHTWARDS_TWO_HEADED_ARROW,
'↢' => LEFTWARDS_ARROW_WITH_TAIL,
'↣' => RIGHTWARDS_ARROW_WITH_TAIL,
'↤' => LEFTWARDS_ARROW_FROM_BAR,
'↦' => RIGHTWARDS_ARROW_FROM_BAR,
'↮' => LEFT_RIGHT_ARROW_WITH_STROKE,
'⇎' => LEFT_RIGHT_DOUBLE_ARROW_WITH_STROKE,
'⇍' => LEFTWARDS_DOUBLE_ARROW_WITH_STROKE,
'⇏' => RIGHTWARDS_DOUBLE_ARROW_WITH_STROKE,
'⇐' => LEFTWARDS_DOUBLE_ARROW,
'⇒' => RIGHTWARDS_DOUBLE_ARROW,
'⇔' => LEFT_RIGHT_DOUBLE_ARROW,
'⇴' => RIGHT_ARROW_WITH_SMALL_CIRCLE,
'⇶' => THREE_RIGHTWARDS_ARROWS,
'⇷' => LEFTWARDS_ARROW_WITH_VERTICAL_STROKE,
'⇸' => RIGHTWARDS_ARROW_WITH_VERTICAL_STROKE,
'⇹' => LEFT_RIGHT_ARROW_WITH_VERTICAL_STROKE,
'⇺' => LEFTWARDS_ARROW_WITH_DOUBLE_VERTICAL_STROKE,
'⇻' => RIGHTWARDS_ARROW_WITH_DOUBLE_VERTICAL_STROKE,
'⇼' => LEFT_RIGHT_ARROW_WITH_DOUBLE_VERTICAL_STROKE,
'⇽' => LEFTWARDS_OPEN_HEADED_ARROW,
'⇾' => RIGHTWARDS_OPEN_HEADED_ARROW,
'⇿' => LEFT_RIGHT_OPEN_HEADED_ARROW,
'⟵' => LONG_LEFTWARDS_ARROW,
'⟶' => LONG_RIGHTWARDS_ARROW,
'⟷' => LONG_LEFT_RIGHT_ARROW,
'⟹' => LONG_RIGHTWARDS_DOUBLE_ARROW,
'⟺' => LONG_LEFT_RIGHT_DOUBLE_ARROW,
'⟻' => LONG_LEFTWARDS_ARROW_FROM_BAR,
'⟼' => LONG_RIGHTWARDS_ARROW_FROM_BAR,
'⟽' => LONG_LEFTWARDS_DOUBLE_ARROW_FROM_BAR,
'⟾' => LONG_RIGHTWARDS_DOUBLE_ARROW_FROM_BAR,
'⟿' => LONG_RIGHTWARDS_SQUIGGLE_ARROW,
'⤀' => RIGHTWARDS_TWO_HEADED_ARROW_WITH_VERTICAL_STROKE,
'⤁' => RIGHTWARDS_TWO_HEADED_ARROW_WITH_DOUBLE_VERTICAL_STROKE,
'⤂' => LEFTWARDS_DOUBLE_ARROW_WITH_VERTICAL_STROKE,
'⤃' => RIGHTWARDS_DOUBLE_ARROW_WITH_VERTICAL_STROKE,
'⤄' => LEFT_RIGHT_DOUBLE_ARROW_WITH_VERTICAL_STROKE,
'⤅' => RIGHTWARDS_TWO_HEADED_ARROW_FROM_BAR,
'⤆' => LEFTWARDS_DOUBLE_ARROW_FROM_BAR,
'⤇' => RIGHTWARDS_DOUBLE_ARROW_FROM_BAR,
'⤌' => LEFTWARDS_DOUBLE_DASH_ARROW,
'⤍' => RIGHTWARDS_DOUBLE_DASH_ARROW,
'⤎' => LEFTWARDS_TRIPLE_DASH_ARROW,
'⤏' => RIGHTWARDS_TRIPLE_DASH_ARROW,
'⤐' => RIGHTWARDS_TWO_HEADED_TRIPLE_DASH_ARROW,
'⤑' => RIGHTWARDS_ARROW_WITH_DOTTED_STEM,
'⤔' => RIGHTWARDS_ARROW_WITH_TAIL_WITH_VERTICAL_STROKE,
'⤕' => RIGHTWARDS_ARROW_WITH_TAIL_WITH_DOUBLE_VERTICAL_STROKE,
'⤖' => RIGHTWARDS_TWO_HEADED_ARROW_WITH_TAIL,
'⤗' => RIGHTWARDS_TWO_HEADED_ARROW_WITH_TAIL_WITH_VERTICAL_STROKE,
'⤘' => RIGHTWARDS_TWO_HEADED_ARROW_WITH_TAIL_WITH_DOUBLE_VERTICAL_STROKE,
'⤝' => LEFTWARDS_ARROW_TO_BLACK_DIAMOND,
'⤞' => RIGHTWARDS_ARROW_TO_BLACK_DIAMOND,
'⤟' => LEFTWARDS_ARROW_FROM_BAR_TO_BLACK_DIAMOND,
'⤠' => RIGHTWARDS_ARROW_FROM_BAR_TO_BLACK_DIAMOND,
'⥄' => SHORT_RIGHTWARDS_ARROW_ABOVE_LEFTWARDS_ARROW,
'⥅' => RIGHTWARDS_ARROW_WITH_PLUS_BELOW,
'⥆' => LEFTWARDS_ARROW_WITH_PLUS_BELOW,
'⥇' => RIGHTWARDS_ARROW_THROUGH_X,
'⥈' => LEFT_RIGHT_ARROW_THROUGH_SMALL_CIRCLE,
'⥊' => LEFT_BARB_UP_RIGHT_BARB_DOWN_HARPOON,
'⥋' => LEFT_BARB_DOWN_RIGHT_BARB_UP_HARPOON,
'⥎' => LEFT_BARB_UP_RIGHT_BARB_UP_HARPOON,
'⥐' => LEFT_BARB_DOWN_RIGHT_BARB_DOWN_HARPOON,
'⥒' => LEFTWARDS_HARPOON_WITH_BARB_UP_TO_BAR,
'⥓' => RIGHTWARDS_HARPOON_WITH_BARB_UP_TO_BAR,
'⥖' => LEFTWARDS_HARPOON_WITH_BARB_DOWN_TO_BAR,
'⥗' => RIGHTWARDS_HARPOON_WITH_BARB_DOWN_TO_BAR,
'⥚' => LEFTWARDS_HARPOON_WITH_BARB_UP_FROM_BAR,
'⥛' => RIGHTWARDS_HARPOON_WITH_BARB_UP_FROM_BAR,
'⥞' => LEFTWARDS_HARPOON_WITH_BARB_DOWN_FROM_BAR,
'⥟' => RIGHTWARDS_HARPOON_WITH_BARB_DOWN_FROM_BAR,
'⥢' => LEFTWARDS_HARPOON_WITH_BARB_UP_ABOVE_LEFTWARDS_HARPOON_WITH_BARB_DOWN,
'⥤' => RIGHTWARDS_HARPOON_WITH_BARB_UP_ABOVE_RIGHTWARDS_HARPOON_WITH_BARB_DOWN,
'⥦' => LEFTWARDS_HARPOON_WITH_BARB_UP_ABOVE_RIGHTWARDS_HARPOON_WITH_BARB_UP,
'⥧' => LEFTWARDS_HARPOON_WITH_BARB_DOWN_ABOVE_RIGHTWARDS_HARPOON_WITH_BARB_DOWN,
'⥨' => RIGHTWARDS_HARPOON_WITH_BARB_UP_ABOVE_LEFTWARDS_HARPOON_WITH_BARB_UP,
'⥩' => RIGHTWARDS_HARPOON_WITH_BARB_DOWN_ABOVE_LEFTWARDS_HARPOON_WITH_BARB_DOWN,
'⥪' => LEFTWARDS_HARPOON_WITH_BARB_UP_ABOVE_LONG_DASH,
'⥫' => LEFTWARDS_HARPOON_WITH_BARB_DOWN_BELOW_LONG_DASH,
'⥬' => RIGHTWARDS_HARPOON_WITH_BARB_UP_ABOVE_LONG_DASH,
'⥭' => RIGHTWARDS_HARPOON_WITH_BARB_DOWN_BELOW_LONG_DASH,
'⥰' => RIGHT_DOUBLE_ARROW_WITH_ROUNDED_HEAD,
'⧴' => RULE_DELAYED,
'⬱' => THREE_LEFTWARDS_ARROWS,
'⬰' => LEFT_ARROW_WITH_SMALL_CIRCLE,
'⬲' => LEFT_ARROW_WITH_CIRCLED_PLUS,
'⬳' => LONG_LEFTWARDS_SQUIGGLE_ARROW,
'⬴' => LEFTWARDS_TWO_HEADED_ARROW_WITH_VERTICAL_STROKE,
'⬵' => LEFTWARDS_TWO_HEADED_ARROW_WITH_DOUBLE_VERTICAL_STROKE,
'⬶' => LEFTWARDS_TWO_HEADED_ARROW_FROM_BAR,
'⬷' => LEFTWARDS_TWO_HEADED_TRIPLE_DASH_ARROW,
'⬸' => LEFTWARDS_ARROW_WITH_DOTTED_STEM,
'⬹' => LEFTWARDS_ARROW_WITH_TAIL_WITH_VERTICAL_STROKE,
'⬺' => LEFTWARDS_ARROW_WITH_TAIL_WITH_DOUBLE_VERTICAL_STROKE,
'⬻' => LEFTWARDS_TWO_HEADED_ARROW_WITH_TAIL,
'⬼' => LEFTWARDS_TWO_HEADED_ARROW_WITH_TAIL_WITH_VERTICAL_STROKE,
'⬽' => LEFTWARDS_TWO_HEADED_ARROW_WITH_TAIL_WITH_DOUBLE_VERTICAL_STROKE,
'⬾' => LEFTWARDS_ARROW_THROUGH_X,
'⬿' => WAVE_ARROW_POINTING_DIRECTLY_LEFT,
'⭀' => EQUALS_SIGN_ABOVE_LEFTWARDS_ARROW,
'⭁' => REVERSE_TILDE_OPERATOR_ABOVE_LEFTWARDS_ARROW,
'⭂' => LEFTWARDS_ARROW_ABOVE_REVERSE_ALMOST_EQUAL_TO,
'⭃' => RIGHTWARDS_ARROW_THROUGH_GREATER_THAN,
'⭄' => RIGHTWARDS_ARROW_THROUGH_SUPERSET,
'⭇' => REVERSE_TILDE_OPERATOR_ABOVE_RIGHTWARDS_ARROW,
'⭈' => RIGHTWARDS_ARROW_ABOVE_REVERSE_ALMOST_EQUAL_TO,
'⭉' => TILDE_OPERATOR_ABOVE_LEFTWARDS_ARROW,
'⭊' => LEFTWARDS_ARROW_ABOVE_ALMOST_EQUAL_TO,
'⭋' => LEFTWARDS_ARROW_ABOVE_REVERSE_TILDE_OPERATOR,
'⭌' => RIGHTWARDS_ARROW_ABOVE_REVERSE_TILDE_OPERATOR,
'￩' => HALFWIDTH_LEFTWARDS_ARROW,
'￫' => HALFWIDTH_RIGHTWARDS_ARROW,
'≥' => GREATER_THAN_OR_EQUAL_TO,
'≤' => LESS_THAN_OR_EQUAL_TO,
'≡' => IDENTICAL_TO,
'≠' => NOT_EQUAL_TO,
'≢' => NOT_IDENTICAL_TO,
'∈' => ELEMENT_OF,
'∉' => NOT_AN_ELEMENT_OF,
'∋' => CONTAINS_AS_MEMBER,
'∌' => DOES_NOT_CONTAIN_AS_MEMBER,
'⊆' => SUBSET_OF_OR_EQUAL_TO,
'⊈' => NEITHER_A_SUBSET_OF_NOR_EQUAL_TO,
'⊂' => SUBSET_OF,
'⊄' => NOT_A_SUBSET_OF,
'⊊' => SUBSET_OF_WITH_NOT_EQUAL_TO,
'∝' => PROPORTIONAL_TO,
'∊' => SMALL_ELEMENT_OF,
'∍' => SMALL_CONTAINS_AS_MEMBER,
'∥' => PARALLEL_TO,
'∦' => NOT_PARALLEL_TO,
'∷' => PROPORTION,
'∺' => GEOMETRIC_PROPORTION,
'∻' => HOMOTHETIC,
'∽' => REVERSED_TILDE,
'∾' => INVERTED_LAZY_S,
'≁' => NOT_TILDE,
'≃' => ASYMPTOTICALLY_EQUAL_TO,
'≄' => NOT_ASYMPTOTICALLY_EQUAL_TO,
'≅' => APPROXIMATELY_EQUAL_TO,
'≆' => APPROXIMATELY_BUT_NOT_ACTUALLY_EQUAL_TO,
'≇' => NEITHER_APPROXIMATELY_NOR_ACTUALLY_EQUAL_TO,
'≈' => ALMOST_EQUAL_TO,
'≉' => NOT_ALMOST_EQUAL_TO,
'≊' => ALMOST_EQUAL_OR_EQUAL_TO,
'≋' => TRIPLE_TILDE,
'≌' => ALL_EQUAL_TO,
'≍' => EQUIVALENT_TO,
'≎' => GEOMETRICALLY_EQUIVALENT_TO,
'≐' => APPROACHES_THE_LIMIT,
'≑' => GEOMETRICALLY_EQUAL_TO,
'≒' => APPROXIMATELY_EQUAL_TO_OR_THE_IMAGE_OF,
'≓' => IMAGE_OF_OR_APPROXIMATELY_EQUAL_TO,
'≔' => COLON_EQUALS,
'≕' => EQUALS_COLON,
'≖' => RING_IN_EQUAL_TO,
'≗' => RING_EQUAL_TO,
'≘' => CORRESPONDS_TO,
'≙' => ESTIMATES,
'≚' => EQUIANGULAR_TO,
'≛' => STAR_EQUALS,
'≜' => DELTA_EQUAL_TO,
'≝' => EQUAL_TO_BY_DEFINITION,
'≞' => MEASURED_BY,
'≟' => QUESTIONED_EQUAL_TO,
'≣' => STRICTLY_EQUIVALENT_TO,
'≦' => LESS_THAN_OVER_EQUAL_TO,
'≧' => GREATER_THAN_OVER_EQUAL_TO,
'≨' => LESS_THAN_BUT_NOT_EQUAL_TO,
'≩' => GREATER_THAN_BUT_NOT_EQUAL_TO,
'≪' => MUCH_LESS_THAN,
'≫' => MUCH_GREATER_THAN,
'≬' => BETWEEN,
'≭' => NOT_EQUIVALENT_TO,
'≮' => NOT_LESS_THAN,
'≯' => NOT_GREATER_THAN,
'≰' => NEITHER_LESS_THAN_NOR_EQUAL_TO,
'≱' => NEITHER_GREATER_THAN_NOR_EQUAL_TO,
'≲' => LESS_THAN_OR_EQUIVALENT_TO,
'≳' => GREATER_THAN_OR_EQUIVALENT_TO,
'≴' => NEITHER_LESS_THAN_NOR_EQUIVALENT_TO,
'≵' => NEITHER_GREATER_THAN_NOR_EQUIVALENT_TO,
'≶' => LESS_THAN_OR_GREATER_THAN,
'≷' => GREATER_THAN_OR_LESS_THAN,
'≸' => NEITHER_LESS_THAN_NOR_GREATER_THAN,
'≹' => NEITHER_GREATER_THAN_NOR_LESS_THAN,
'≺' => PRECEDES,
'≻' => SUCCEEDS,
'≼' => PRECEDES_OR_EQUAL_TO,
'≽' => SUCCEEDS_OR_EQUAL_TO,
'≾' => PRECEDES_OR_EQUIVALENT_TO,
'≿' => SUCCEEDS_OR_EQUIVALENT_TO,
'⊀' => DOES_NOT_PRECEDE,
'⊁' => DOES_NOT_SUCCEED,
'⊃' => SUPERSET_OF,
'⊅' => NOT_A_SUPERSET_OF,
'⊇' => SUPERSET_OF_OR_EQUAL_TO,
'⊉' => NEITHER_A_SUPERSET_OF_NOR_EQUAL_TO,
'⊋' => SUPERSET_OF_WITH_NOT_EQUAL_TO,
'⊏' => SQUARE_IMAGE_OF,
'⊐' => SQUARE_ORIGINAL_OF,
'⊑' => SQUARE_IMAGE_OF_OR_EQUAL_TO,
'⊒' => SQUARE_ORIGINAL_OF_OR_EQUAL_TO,
'⊜' => CIRCLED_EQUALS,
'⊩' => FORCES,
'⊬' => DOES_NOT_PROVE,
'⊮' => DOES_NOT_FORCE,
'⊰' => PRECEDES_UNDER_RELATION,
'⊱' => SUCCEEDS_UNDER_RELATION,
'⊲' => NORMAL_SUBGROUP_OF,
'⊳' => CONTAINS_AS_NORMAL_SUBGROUP,
'⊴' => NORMAL_SUBGROUP_OF_OR_EQUAL_TO,
'⊵' => CONTAINS_AS_NORMAL_SUBGROUP_OR_EQUAL_TO,
'⊶' => ORIGINAL_OF,
'⊷' => IMAGE_OF,
'⋍' => REVERSED_TILDE_EQUALS,
'⋐' => DOUBLE_SUBSET,
'⋑' => DOUBLE_SUPERSET,
'⋕' => EQUAL_AND_PARALLEL_TO,
'⋖' => LESS_THAN_WITH_DOT,
'⋗' => GREATER_THAN_WITH_DOT,
'⋘' => VERY_MUCH_LESS_THAN,
'⋙' => VERY_MUCH_GREATER_THAN,
'⋚' => LESS_THAN_EQUAL_TO_OR_GREATER_THAN,
'⋛' => GREATER_THAN_EQUAL_TO_OR_LESS_THAN,
'⋜' => EQUAL_TO_OR_LESS_THAN,
'⋝' => EQUAL_TO_OR_GREATER_THAN,
'⋞' => EQUAL_TO_OR_PRECEDES,
'⋟' => EQUAL_TO_OR_SUCCEEDS,
'⋠' => DOES_NOT_PRECEDE_OR_EQUAL,
'⋡' => DOES_NOT_SUCCEED_OR_EQUAL,
'⋢' => NOT_SQUARE_IMAGE_OF_OR_EQUAL_TO,
'⋣' => NOT_SQUARE_ORIGINAL_OF_OR_EQUAL_TO,
'⋤' => SQUARE_IMAGE_OF_OR_NOT_EQUAL_TO,
'⋥' => SQUARE_ORIGINAL_OF_OR_NOT_EQUAL_TO,
'⋦' => LESS_THAN_BUT_NOT_EQUIVALENT_TO,
'⋧' => GREATER_THAN_BUT_NOT_EQUIVALENT_TO,
'⋨' => PRECEDES_BUT_NOT_EQUIVALENT_TO,
'⋩' => SUCCEEDS_BUT_NOT_EQUIVALENT_TO,
'⋪' => NOT_NORMAL_SUBGROUP_OF,
'⋫' => DOES_NOT_CONTAIN_AS_NORMAL_SUBGROUP,
'⋬' => NOT_NORMAL_SUBGROUP_OF_OR_EQUAL_TO,
'⋭' => DOES_NOT_CONTAIN_AS_NORMAL_SUBGROUP_OR_EQUAL,
'⋲' => ELEMENT_OF_WITH_LONG_HORIZONTAL_STROKE,
'⋳' => ELEMENT_OF_WITH_VERTICAL_BAR_AT_END_OF_HORIZONTAL_STROKE,
'⋴' => SMALL_ELEMENT_OF_WITH_VERTICAL_BAR_AT_END_OF_HORIZONTAL_STROKE,
'⋵' => ELEMENT_OF_WITH_DOT_ABOVE,
'⋶' => ELEMENT_OF_WITH_OVERBAR,
'⋷' => SMALL_ELEMENT_OF_WITH_OVERBAR,
'⋸' => ELEMENT_OF_WITH_UNDERBAR,
'⋹' => ELEMENT_OF_WITH_TWO_HORIZONTAL_STROKES,
'⋺' => CONTAINS_WITH_LONG_HORIZONTAL_STROKE,
'⋻' => CONTAINS_WITH_VERTICAL_BAR_AT_END_OF_HORIZONTAL_STROKE,
'⋼' => SMALL_CONTAINS_WITH_VERTICAL_BAR_AT_END_OF_HORIZONTAL_STROKE,
'⋽' => CONTAINS_WITH_OVERBAR,
'⋾' => SMALL_CONTAINS_WITH_OVERBAR,
'⋿' => Z_NOTATION_BAG_MEMBERSHIP,
'⟈' => REVERSE_SOLIDUS_PRECEDING_SUBSET,
'⟉' => SUPERSET_PRECEDING_SOLIDUS,
'⟒' => ELEMENT_OF_OPENING_UPWARDS,
'⦷' => CIRCLED_PARALLEL,
'⧀' => CIRCLED_LESS_THAN,
'⧁' => CIRCLED_GREATER_THAN,
'⧡' => INCREASES_AS,
'⧣' => EQUALS_SIGN_AND_SLANTED_PARALLEL,
'⧤' => EQUALS_SIGN_AND_SLANTED_PARALLEL_WITH_TILDE_ABOVE,
'⧥' => IDENTICAL_TO_AND_SLANTED_PARALLEL,
'⩦' => EQUALS_SIGN_WITH_DOT_BELOW,
'⩧' => IDENTICAL_WITH_DOT_ABOVE,
'⩪' => TILDE_OPERATOR_WITH_DOT_ABOVE,
'⩫' => TILDE_OPERATOR_WITH_RISING_DOTS,
'⩬' => SIMILAR_MINUS_SIMILAR,
'⩭' => CONGRUENT_WITH_DOT_ABOVE,
'⩮' => EQUALS_WITH_ASTERISK,
'⩯' => ALMOST_EQUAL_TO_WITH_CIRCUMFLEX_ACCENT,
'⩰' => APPROXIMATELY_EQUAL_OR_EQUAL_TO,
'⩱' => EQUALS_SIGN_ABOVE_PLUS_SIGN,
'⩲' => PLUS_SIGN_ABOVE_EQUALS_SIGN,
'⩳' => EQUALS_SIGN_ABOVE_TILDE_OPERATOR,
'⩴' => DOUBLE_COLON_EQUAL,
'⩵' => TWO_CONSECUTIVE_EQUALS_SIGNS,
'⩶' => THREE_CONSECUTIVE_EQUALS_SIGNS,
'⩷' => EQUALS_SIGN_WITH_TWO_DOTS_ABOVE_AND_TWO_DOTS_BELOW,
'⩸' => EQUIVALENT_WITH_FOUR_DOTS_ABOVE,
'⩹' => LESS_THAN_WITH_CIRCLE_INSIDE,
'⩺' => GREATER_THAN_WITH_CIRCLE_INSIDE,
'⩻' => LESS_THAN_WITH_QUESTION_MARK_ABOVE,
'⩼' => GREATER_THAN_WITH_QUESTION_MARK_ABOVE,
'⩽' => LESS_THAN_OR_SLANTED_EQUAL_TO,
'⩾' => GREATER_THAN_OR_SLANTED_EQUAL_TO,
'⩿' => LESS_THAN_OR_SLANTED_EQUAL_TO_WITH_DOT_INSIDE,
'⪀' => GREATER_THAN_OR_SLANTED_EQUAL_TO_WITH_DOT_INSIDE,
'⪁' => LESS_THAN_OR_SLANTED_EQUAL_TO_WITH_DOT_ABOVE,
'⪂' => GREATER_THAN_OR_SLANTED_EQUAL_TO_WITH_DOT_ABOVE,
'⪃' => LESS_THAN_OR_SLANTED_EQUAL_TO_WITH_DOT_ABOVE_RIGHT,
'⪄' => GREATER_THAN_OR_SLANTED_EQUAL_TO_WITH_DOT_ABOVE_LEFT,
'⪅' => LESS_THAN_OR_APPROXIMATE,
'⪆' => GREATER_THAN_OR_APPROXIMATE,
'⪇' => LESS_THAN_AND_SINGLE_LINE_NOT_EQUAL_TO,
'⪈' => GREATER_THAN_AND_SINGLE_LINE_NOT_EQUAL_TO,
'⪉' => LESS_THAN_AND_NOT_APPROXIMATE,
'⪊' => GREATER_THAN_AND_NOT_APPROXIMATE,
'⪋' => LESS_THAN_ABOVE_DOUBLE_LINE_EQUAL_ABOVE_GREATER_THAN,
'⪌' => GREATER_THAN_ABOVE_DOUBLE_LINE_EQUAL_ABOVE_LESS_THAN,
'⪍' => LESS_THAN_ABOVE_SIMILAR_OR_EQUAL,
'⪎' => GREATER_THAN_ABOVE_SIMILAR_OR_EQUAL,
'⪏' => LESS_THAN_ABOVE_SIMILAR_ABOVE_GREATER_THAN,
'⪐' => GREATER_THAN_ABOVE_SIMILAR_ABOVE_LESS_THAN,
'⪑' => LESS_THAN_ABOVE_GREATER_THAN_ABOVE_DOUBLE_LINE_EQUAL,
'⪒' => GREATER_THAN_ABOVE_LESS_THAN_ABOVE_DOUBLE_LINE_EQUAL,
'⪓' => LESS_THAN_ABOVE_SLANTED_EQUAL_ABOVE_GREATER_THAN_ABOVE_SLANTED_EQUAL,
'⪔' => GREATER_THAN_ABOVE_SLANTED_EQUAL_ABOVE_LESS_THAN_ABOVE_SLANTED_EQUAL,
'⪕' => SLANTED_EQUAL_TO_OR_LESS_THAN,
'⪖' => SLANTED_EQUAL_TO_OR_GREATER_THAN,
'⪗' => SLANTED_EQUAL_TO_OR_LESS_THAN_WITH_DOT_INSIDE,
'⪘' => SLANTED_EQUAL_TO_OR_GREATER_THAN_WITH_DOT_INSIDE,
'⪙' => DOUBLE_LINE_EQUAL_TO_OR_LESS_THAN,
'⪚' => DOUBLE_LINE_EQUAL_TO_OR_GREATER_THAN,
'⪛' => DOUBLE_LINE_SLANTED_EQUAL_TO_OR_LESS_THAN,
'⪜' => DOUBLE_LINE_SLANTED_EQUAL_TO_OR_GREATER_THAN,
'⪝' => SIMILAR_OR_LESS_THAN,
'⪞' => SIMILAR_OR_GREATER_THAN,
'⪟' => SIMILAR_ABOVE_LESS_THAN_ABOVE_EQUALS_SIGN,
'⪠' => SIMILAR_ABOVE_GREATER_THAN_ABOVE_EQUALS_SIGN,
'⪡' => DOUBLE_NESTED_LESS_THAN,
'⪢' => DOUBLE_NESTED_GREATER_THAN,
'⪣' => DOUBLE_NESTED_LESS_THAN_WITH_UNDERBAR,
'⪤' => GREATER_THAN_OVERLAPPING_LESS_THAN,
'⪥' => GREATER_THAN_BESIDE_LESS_THAN,
'⪦' => LESS_THAN_CLOSED_BY_CURVE,
'⪧' => GREATER_THAN_CLOSED_BY_CURVE,
'⪨' => LESS_THAN_CLOSED_BY_CURVE_ABOVE_SLANTED_EQUAL,
'⪩' => GREATER_THAN_CLOSED_BY_CURVE_ABOVE_SLANTED_EQUAL,
'⪪' => SMALLER_THAN,
'⪫' => LARGER_THAN,
'⪬' => SMALLER_THAN_OR_EQUAL_TO,
'⪭' => LARGER_THAN_OR_EQUAL_TO,
'⪮' => EQUALS_SIGN_WITH_BUMPY_ABOVE,
'⪯' => PRECEDES_ABOVE_SINGLE_LINE_EQUALS_SIGN,
'⪰' => SUCCEEDS_ABOVE_SINGLE_LINE_EQUALS_SIGN,
'⪱' => PRECEDES_ABOVE_SINGLE_LINE_NOT_EQUAL_TO,
'⪲' => SUCCEEDS_ABOVE_SINGLE_LINE_NOT_EQUAL_TO,
'⪳' => PRECEDES_ABOVE_EQUALS_SIGN,
'⪴' => SUCCEEDS_ABOVE_EQUALS_SIGN,
'⪵' => PRECEDES_ABOVE_NOT_EQUAL_TO,
'⪶' => SUCCEEDS_ABOVE_NOT_EQUAL_TO,
'⪷' => PRECEDES_ABOVE_ALMOST_EQUAL_TO,
'⪸' => SUCCEEDS_ABOVE_ALMOST_EQUAL_TO,
'⪹' => PRECEDES_ABOVE_NOT_ALMOST_EQUAL_TO,
'⪺' => SUCCEEDS_ABOVE_NOT_ALMOST_EQUAL_TO,
'⪻' => DOUBLE_PRECEDES,
'⪼' => DOUBLE_SUCCEEDS,
'⪽' => SUBSET_WITH_DOT,
'⪾' => SUPERSET_WITH_DOT,
'⪿' => SUBSET_WITH_PLUS_SIGN_BELOW,
'⫀' => SUPERSET_WITH_PLUS_SIGN_BELOW,
'⫁' => SUBSET_WITH_MULTIPLICATION_SIGN_BELOW,
'⫂' => SUPERSET_WITH_MULTIPLICATION_SIGN_BELOW,
'⫃' => SUBSET_OF_OR_EQUAL_TO_WITH_DOT_ABOVE,
'⫄' => SUPERSET_OF_OR_EQUAL_TO_WITH_DOT_ABOVE,
'⫅' => SUBSET_OF_ABOVE_EQUALS_SIGN,
'⫆' => SUPERSET_OF_ABOVE_EQUALS_SIGN,
'⫇' => SUBSET_OF_ABOVE_TILDE_OPERATOR,
'⫈' => SUPERSET_OF_ABOVE_TILDE_OPERATOR,
'⫉' => SUBSET_OF_ABOVE_ALMOST_EQUAL_TO,
'⫊' => SUPERSET_OF_ABOVE_ALMOST_EQUAL_TO,
'⫋' => SUBSET_OF_ABOVE_NOT_EQUAL_TO,
'⫌' => SUPERSET_OF_ABOVE_NOT_EQUAL_TO,
'⫍' => SQUARE_LEFT_OPEN_BOX_OPERATOR,
'⫎' => SQUARE_RIGHT_OPEN_BOX_OPERATOR,
'⫏' => CLOSED_SUBSET,
'⫐' => CLOSED_SUPERSET,
'⫑' => CLOSED_SUBSET_OR_EQUAL_TO,
'⫒' => CLOSED_SUPERSET_OR_EQUAL_TO,
'⫓' => SUBSET_ABOVE_SUPERSET,
'⫔' => SUPERSET_ABOVE_SUBSET,
'⫕' => SUBSET_ABOVE_SUBSET,
'⫖' => SUPERSET_ABOVE_SUPERSET,
'⫗' => SUPERSET_BESIDE_SUBSET,
'⫘' => SUPERSET_BESIDE_AND_JOINED_BY_DASH_WITH_SUBSET,
'⫙' => ELEMENT_OF_OPENING_DOWNWARDS,
'⫷' => TRIPLE_NESTED_LESS_THAN,
'⫸' => TRIPLE_NESTED_GREATER_THAN,
'⫹' => DOUBLE_LINE_SLANTED_LESS_THAN_OR_EQUAL_TO,
'⫺' => DOUBLE_LINE_SLANTED_GREATER_THAN_OR_EQUAL_TO,
'⊢' => RIGHT_TACK,
'⊣' => LEFT_TACK,
'⫪' => DOUBLE_DOWN_TACK,
'⫫' => DOUBLE_UP_TACK,
'⟂' => PERP,
'⊕' => CIRCLED_PLUS,
'⊖' => CIRCLED_MINUS,
'⊞' => SQUARED_PLUS,
'⊟' => SQUARED_MINUS,
'|' => OR,
'∪' => UNION,
'∨' => LOGICAL_OR,
'⊔' => SQUARE_CUP,
'±' => PLUS_MINUS_SIGN,
'∓' => MINUS_OR_PLUS_SIGN,
'∔' => DOT_PLUS,
'∸' => DOT_MINUS,
'≂' => MINUS_TILDE,
'≏' => DIFFERENCE_BETWEEN,
'⊎' => MULTISET_UNION,
'⊻' => XOR,
'⊽' => NOR,
'⋎' => CURLY_LOGICAL_OR,
'⋓' => DOUBLE_UNION,
'⧺' => DOUBLE_PLUS,
'⧻' => TRIPLE_PLUS,
'⨈' => TWO_LOGICAL_OR_OPERATOR,
'⨢' => PLUS_SIGN_WITH_SMALL_CIRCLE_ABOVE,
'⨣' => PLUS_SIGN_WITH_CIRCUMFLEX_ACCENT_ABOVE,
'⨤' => PLUS_SIGN_WITH_TILDE_ABOVE,
'⨥' => PLUS_SIGN_WITH_DOT_BELOW,
'⨦' => PLUS_SIGN_WITH_TILDE_BELOW,
'⨧' => PLUS_SIGN_WITH_SUBSCRIPT_TWO,
'⨨' => PLUS_SIGN_WITH_BLACK_TRIANGLE,
'⨩' => MINUS_SIGN_WITH_COMMA_ABOVE,
'⨪' => MINUS_SIGN_WITH_DOT_BELOW,
'⨫' => MINUS_SIGN_WITH_FALLING_DOTS,
'⨬' => MINUS_SIGN_WITH_RISING_DOTS,
'⨭' => PLUS_SIGN_IN_LEFT_HALF_CIRCLE,
'⨮' => PLUS_SIGN_IN_RIGHT_HALF_CIRCLE,
'⨹' => PLUS_SIGN_IN_TRIANGLE,
'⨺' => MINUS_SIGN_IN_TRIANGLE,
'⩁' => UNION_WITH_MINUS_SIGN,
'⩂' => UNION_WITH_OVERBAR,
'⩅' => UNION_WITH_LOGICAL_OR,
'⩊' => UNION_BESIDE_AND_JOINED_WITH_UNION,
'⩌' => CLOSED_UNION_WITH_SERIFS,
'⩏' => DOUBLE_SQUARE_UNION,
'⩐' => CLOSED_UNION_WITH_SERIFS_AND_SMASH_PRODUCT,
'⩒' => LOGICAL_OR_WITH_DOT_ABOVE,
'⩔' => DOUBLE_LOGICAL_OR,
'⩖' => TWO_INTERSECTING_LOGICAL_OR,
'⩗' => SLOPING_LARGE_OR,
'⩛' => LOGICAL_OR_WITH_MIDDLE_STEM,
'⩝' => LOGICAL_OR_WITH_HORIZONTAL_DASH,
'⩡' => SMALL_VEE_WITH_UNDERBAR,
'⩢' => LOGICAL_OR_WITH_DOUBLE_OVERBAR,
'⩣' => LOGICAL_OR_WITH_DOUBLE_UNDERBAR,
'∘' => RING_OPERATOR,
'×' => MULTIPLICATION_SIGN,
'∩' => INTERSECTION,
'∧' => LOGICAL_AND,
'⊗' => CIRCLED_TIMES,
'⊘' => CIRCLED_DIVISION_SLASH,
'⊙' => CIRCLED_DOT_OPERATOR,
'⊚' => CIRCLED_RING_OPERATOR,
'⊛' => CIRCLED_ASTERISK_OPERATOR,
'⊠' => SQUARED_TIMES,
'⊡' => SQUARED_DOT_OPERATOR,
'⊓' => SQUARE_CAP,
'∗' => ASTERISK_OPERATOR,
'∙' => BULLET_OPERATOR,
'∤' => DOES_NOT_DIVIDE,
'⅋' => TURNED_AMPERSAND,
'≀' => WREATH_PRODUCT,
'⊼' => NAND,
'⋄' => DIAMOND_OPERATOR,
'⋆' => STAR_OPERATOR,
'⋇' => DIVISION_TIMES,
'⋉' => LEFT_NORMAL_FACTOR_SEMIDIRECT_PRODUCT,
'⋊' => RIGHT_NORMAL_FACTOR_SEMIDIRECT_PRODUCT,
'⋋' => LEFT_SEMIDIRECT_PRODUCT,
'⋌' => RIGHT_SEMIDIRECT_PRODUCT,
'⋏' => CURLY_LOGICAL_AND,
'⋒' => DOUBLE_INTERSECTION,
'⟑' => AND_WITH_DOT,
'⦸' => CIRCLED_REVERSE_SOLIDUS,
'⦼' => CIRCLED_ANTICLOCKWISE_ROTATED_DIVISION_SIGN,
'⦾' => CIRCLED_WHITE_BULLET,
'⦿' => CIRCLED_BULLET,
'⧶' => SOLIDUS_WITH_OVERBAR,
'⧷' => REVERSE_SOLIDUS_WITH_HORIZONTAL_STROKE,
'⨇' => TWO_LOGICAL_AND_OPERATOR,
'⨰' => MULTIPLICATION_SIGN_WITH_DOT_ABOVE,
'⨱' => MULTIPLICATION_SIGN_WITH_UNDERBAR,
'⨲' => SEMIDIRECT_PRODUCT_WITH_BOTTOM_CLOSED,
'⨳' => SMASH_PRODUCT,
'⨴' => MULTIPLICATION_SIGN_IN_LEFT_HALF_CIRCLE,
'⨵' => MULTIPLICATION_SIGN_IN_RIGHT_HALF_CIRCLE,
'⨶' => CIRCLED_MULTIPLICATION_SIGN_WITH_CIRCUMFLEX_ACCENT,
'⨷' => MULTIPLICATION_SIGN_IN_DOUBLE_CIRCLE,
'⨸' => CIRCLED_DIVISION_SIGN,
'⨻' => MULTIPLICATION_SIGN_IN_TRIANGLE,
'⨼' => INTERIOR_PRODUCT,
'⨽' => RIGHTHAND_INTERIOR_PRODUCT,
'⩀' => INTERSECTION_WITH_DOT,
'⩃' => INTERSECTION_WITH_OVERBAR,
'⩄' => INTERSECTION_WITH_LOGICAL_AND,
'⩋' => INTERSECTION_BESIDE_AND_JOINED_WITH_INTERSECTION,
'⩍' => CLOSED_INTERSECTION_WITH_SERIFS,
'⩎' => DOUBLE_SQUARE_INTERSECTION,
'⩑' => LOGICAL_AND_WITH_DOT_ABOVE,
'⩓' => DOUBLE_LOGICAL_AND,
'⩕' => TWO_INTERSECTING_LOGICAL_AND,
'⩘' => SLOPING_LARGE_AND,
'⩚' => LOGICAL_AND_WITH_MIDDLE_STEM,
'⩜' => LOGICAL_AND_WITH_HORIZONTAL_DASH,
'⩞' => LOGICAL_AND_WITH_DOUBLE_OVERBAR,
'⩟' => LOGICAL_AND_WITH_UNDERBAR,
'⩠' => LOGICAL_AND_WITH_DOUBLE_UNDERBAR,
'⫛' => TRANSVERSAL_INTERSECTION,
'⊍' => MULTISET_MULTIPLICATION,
'▷' => WHITE_RIGHT_POINTING_TRIANGLE,
'⨝' => JOIN,
'⟕' => LEFT_OUTER_JOIN,
'⟖' => RIGHT_OUTER_JOIN,
'⟗' => FULL_OUTER_JOIN,
'^' => CIRCUMFLEX_ACCENT,
'↑' => UPWARDS_ARROW,
'↓' => DOWNWARDS_ARROW,
'⇵' => DOWNWARDS_ARROW_LEFTWARDS_OF_UPWARDS_ARROW,
'⟰' => UPWARDS_QUADRUPLE_ARROW,
'⟱' => DOWNWARDS_QUADRUPLE_ARROW,
'⤈' => DOWNWARDS_ARROW_WITH_HORIZONTAL_STROKE,
'⤉' => UPWARDS_ARROW_WITH_HORIZONTAL_STROKE,
'⤊' => UPWARDS_TRIPLE_ARROW,
'⤋' => DOWNWARDS_TRIPLE_ARROW,
'⤒' => UPWARDS_ARROW_TO_BAR,
'⤓' => DOWNWARDS_ARROW_TO_BAR,
'⥉' => UPWARDS_TWO_HEADED_ARROW_FROM_SMALL_CIRCLE,
'⥌' => UP_BARB_RIGHT_DOWN_BARB_LEFT_HARPOON,
'⥍' => UP_BARB_LEFT_DOWN_BARB_RIGHT_HARPOON,
'⥏' => UP_BARB_RIGHT_DOWN_BARB_RIGHT_HARPOON,
'⥑' => UP_BARB_LEFT_DOWN_BARB_LEFT_HARPOON,
'⥔' => UPWARDS_HARPOON_WITH_BARB_RIGHT_TO_BAR,
'⥕' => DOWNWARDS_HARPOON_WITH_BARB_RIGHT_TO_BAR,
'⥘' => UPWARDS_HARPOON_WITH_BARB_LEFT_TO_BAR,
'⥙' => DOWNWARDS_HARPOON_WITH_BARB_LEFT_TO_BAR,
'⥜' => UPWARDS_HARPOON_WITH_BARB_RIGHT_FROM_BAR,
'⥝' => DOWNWARDS_HARPOON_WITH_BARB_RIGHT_FROM_BAR,
'⥠' => UPWARDS_HARPOON_WITH_BARB_LEFT_FROM_BAR,
'⥡' => DOWNWARDS_HARPOON_WITH_BARB_LEFT_FROM_BAR,
'⥣' => UPWARDS_HARPOON_WITH_BARB_LEFT_BESIDE_UPWARDS_HARPOON_WITH_BARB_RIGHT,
'⥥' => DOWNWARDS_HARPOON_WITH_BARB_LEFT_BESIDE_DOWNWARDS_HARPOON_WITH_BARB_RIGHT,
'⥮' => UPWARDS_HARPOON_WITH_BARB_LEFT_BESIDE_DOWNWARDS_HARPOON_WITH_BARB_RIGHT,
'⥯' => DOWNWARDS_HARPOON_WITH_BARB_LEFT_BESIDE_UPWARDS_HARPOON_WITH_BARB_RIGHT,
'￪' => HALFWIDTH_UPWARDS_ARROW,
'￬' => HALFWIDTH_DOWNWARDS_ARROW,
# Lookalikes which are normalized into UNICODE_DOT
# https://github.com/JuliaLang/julia/pull/25157
'\u00b7' => UNICODE_DOT, # '·' Middle Dot,
'\u0387' => UNICODE_DOT, # '·' Greek Ano Teleia,
'⋅' => UNICODE_DOT,
'…' => LDOTS,
'⁝' => TRICOLON,
'⋮' => VDOTS,
'⋱' => DDOTS,
'⋰' => ADOTS,
'⋯' => CDOTS,
'↻' => CIRCLE_ARROW_RIGHT,
'⇜' => LEFT_SQUIGGLE_ARROW,
'⇝' => RIGHT_SQUIGGLE_ARROW,
'↜' => LEFT_WAVE_ARROW,
'↝' => RIGHT_WAVE_ARROW,
'↩' => LEFTWARDS_ARROW_WITH_HOOK,
'↪' => RIGHTWARDS_ARROW_WITH_HOOK,
'↫' => LOOP_ARROW_LEFT,
'↬' => LOOP_ARROW_RIGHT,
'↼' => LEFT_HARPOON_UP,
'↽' => LEFT_HARPOON_DOWN,
'⇀' => RIGHT_HARPOON_UP,
'⇁' => RIGHT_HARPOON_DOWN,
'⇄' => RIGHT_LEFT_ARROWS,
'⇆' => LEFT_RIGHT_ARROWS,
'⇇' => LEFT_LEFT_ARROWS,
'⇉' => RIGHT_RIGHT_ARROWS,
'⇋' => LEFT_RIGHT_HARPOONS,
'⇌' => RIGHT_LEFT_HARPOONS,
'⇚' => L_LEFT_ARROW,
'⇛' => R_RIGHT_ARROW,
'⇠' => LEFT_DASH_ARROW,
'⇢' => RIGHT_DASH_ARROW,
'↷' => CURVE_ARROW_RIGHT,
'↶' => CURVE_ARROW_LEFT,
'↺' => CIRCLE_ARROW_LEFT,
'¦' => BROKEN_BAR,
'⌿' => NOT_SLASH,
'⨟' => BB_SEMI)


const UNICODE_OPS_REVERSE = Dict{Kind,Symbol}()
for (k, v) in UNICODE_OPS
    k in ('\u00b7', '\u0387') && continue
    UNICODE_OPS_REVERSE[v] = Symbol(k)
end

UNICODE_OPS_REVERSE[EQ] = :(=)
UNICODE_OPS_REVERSE[PLUS_EQ] = :(+=)
UNICODE_OPS_REVERSE[MINUS_EQ] = :(-=)
UNICODE_OPS_REVERSE[STAR_EQ] = :(*=)
UNICODE_OPS_REVERSE[FWD_SLASH_EQ] = :(/=)
UNICODE_OPS_REVERSE[FWDFWD_SLASH_EQ] = :(//=)
UNICODE_OPS_REVERSE[OR_EQ] = :(|=)
UNICODE_OPS_REVERSE[CIRCUMFLEX_EQ] = :(^=)
UNICODE_OPS_REVERSE[DIVISION_EQ] = :(÷=)
UNICODE_OPS_REVERSE[REM_EQ] = :(%=)
UNICODE_OPS_REVERSE[LBITSHIFT_EQ] = :(<<=)
UNICODE_OPS_REVERSE[RBITSHIFT_EQ] = :(>>=)
UNICODE_OPS_REVERSE[LBITSHIFT] = :(<<)
UNICODE_OPS_REVERSE[RBITSHIFT] = :(>>)
UNICODE_OPS_REVERSE[UNSIGNED_BITSHIFT] = :(>>>)
UNICODE_OPS_REVERSE[UNSIGNED_BITSHIFT_EQ] = :(>>>=)
UNICODE_OPS_REVERSE[BACKSLASH_EQ] = :(\=)
UNICODE_OPS_REVERSE[AND_EQ] = :(&=)
UNICODE_OPS_REVERSE[COLON_EQ] = :(:=)
UNICODE_OPS_REVERSE[PAIR_ARROW] = :(=>)
UNICODE_OPS_REVERSE[APPROX] = :(~)
UNICODE_OPS_REVERSE[EX_OR_EQ] = :($=)
UNICODE_OPS_REVERSE[XOR_EQ] = :(⊻=)
UNICODE_OPS_REVERSE[RIGHT_ARROW] = :(-->)
UNICODE_OPS_REVERSE[LAZY_OR] = :(||)
UNICODE_OPS_REVERSE[LAZY_AND] = :(&&)
UNICODE_OPS_REVERSE[ISSUBTYPE] = :(<:)
UNICODE_OPS_REVERSE[ISSUPERTYPE] = :(>:)
UNICODE_OPS_REVERSE[GREATER] = :(>)
UNICODE_OPS_REVERSE[LESS] = :(<)
UNICODE_OPS_REVERSE[GREATER_EQ] = :(>=)
UNICODE_OPS_REVERSE[GREATER_THAN_OR_EQUAL_TO] = :(≥)
UNICODE_OPS_REVERSE[LESS_EQ] = :(<=)
UNICODE_OPS_REVERSE[LESS_THAN_OR_EQUAL_TO] = :(≤)
UNICODE_OPS_REVERSE[EQEQ] = :(==)
UNICODE_OPS_REVERSE[EQEQEQ] = :(===)
UNICODE_OPS_REVERSE[IDENTICAL_TO] = :(≡)
UNICODE_OPS_REVERSE[NOT_EQ] = :(!=)
UNICODE_OPS_REVERSE[NOT_EQUAL_TO] = :(≠)
UNICODE_OPS_REVERSE[NOT_IS] = :(!==)
UNICODE_OPS_REVERSE[NOT_IDENTICAL_TO] = :(≢)
UNICODE_OPS_REVERSE[IN] = :(in)
UNICODE_OPS_REVERSE[ISA] = :(isa)
UNICODE_OPS_REVERSE[LPIPE] = :(<|)
UNICODE_OPS_REVERSE[RPIPE] = :(|>)
UNICODE_OPS_REVERSE[COLON] = :(:)
UNICODE_OPS_REVERSE[DDOT] = :(..)
UNICODE_OPS_REVERSE[EX_OR] = :($)
UNICODE_OPS_REVERSE[PLUS] = :(+)
UNICODE_OPS_REVERSE[MINUS] = :(-)
UNICODE_OPS_REVERSE[PLUSPLUS] = :(++)
UNICODE_OPS_REVERSE[OR] = :(|)
UNICODE_OPS_REVERSE[STAR] = :(*)
UNICODE_OPS_REVERSE[FWD_SLASH] = :(/)
UNICODE_OPS_REVERSE[REM] = :(%)
UNICODE_OPS_REVERSE[BACKSLASH] = :(\)
UNICODE_OPS_REVERSE[AND] = :(&)
UNICODE_OPS_REVERSE[FWDFWD_SLASH] = :(//)
UNICODE_OPS_REVERSE[CIRCUMFLEX_ACCENT] = :(^)
UNICODE_OPS_REVERSE[DECLARATION] = :(::)
UNICODE_OPS_REVERSE[CONDITIONAL] = :?
UNICODE_OPS_REVERSE[DOT] = :(.)
UNICODE_OPS_REVERSE[NOT] = :(!)
UNICODE_OPS_REVERSE[PRIME] = Symbol(''')
UNICODE_OPS_REVERSE[DDDOT] = :(...)
UNICODE_OPS_REVERSE[TRANSPOSE] = Symbol(".'")
UNICODE_OPS_REVERSE[ANON_FUNC] = :(->)
UNICODE_OPS_REVERSE[WHERE] = :where
