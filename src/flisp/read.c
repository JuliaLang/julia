enum {
    TOK_NONE, TOK_OPEN, TOK_CLOSE, TOK_DOT, TOK_QUOTE, TOK_SYM, TOK_NUM,
    TOK_BQ, TOK_COMMA, TOK_COMMAAT, TOK_COMMADOT,
    TOK_SHARPDOT, TOK_LABEL, TOK_BACKREF, TOK_SHARPQUOTE, TOK_SHARPOPEN,
    TOK_OPENB, TOK_CLOSEB, TOK_SHARPSYM, TOK_GENSYM, TOK_DOUBLEQUOTE
};

#define F value2c(ios_t*,readstate->source)

// defines which characters are ordinary symbol characters.
// exceptions are '.', which is an ordinary symbol character
// unless it's the only character in the symbol, and '#', which is
// an ordinary symbol character unless it's the first character.
static inline int symchar(char c)
{
    static char *special = "()[]'\";`,\\| \f\n\r\t\v";
    return !strchr(special, c);
}

// like strtoull, but accepts "0b" prefix for base 2 and "0o" prefix for base 8
static unsigned long long strtoull_0b0o(const char *nptr, char **endptr, int base)
{
    if (*nptr == '0') {
        if ((base == 2 && nptr[1] == 'b' && nptr[2] >= '0' && nptr[2] <= '1') ||
            (base == 8 && nptr[1] == 'o' && nptr[2] >= '0' && nptr[2] <= '7')) {
            nptr += 2;
        }
    }
    return strtoull(nptr, endptr, base);
}

int isnumtok_base(char *tok, value_t *pval, int base)
{
    char *end;
    int64_t i64;
    uint64_t ui64;
    double d;
    if (*tok == '\0')
        return 0;
    if (!((tok[0]=='0' && tok[1]=='x') || (base >= 15)) &&
        strpbrk(tok, ".eEpP")) {
        d = strtod_c(tok, &end);
        if (*end == '\0') {
            if (pval) *pval = mk_double(d);
            return 1;
        }
        // floats can end in f or f0
        if (end > tok && end[0] == 'f' &&
            (end[1] == '\0' ||
             (end[1] == '0' && end[2] == '\0'))) {
            if (pval) *pval = mk_float((float)d);
            return 1;
        }
    }
    // hexadecimal float literals
    else if (((tok[0]=='0' && tok[1]=='x') || (base == 16)) &&
        strpbrk(tok, "pP")) {
        d = strtod_c(tok, &end);
        if (*end == '\0') {
            if (pval) *pval = mk_double(d);
            return 1;
        }
        // floats can end in f or f0
        if (end > tok && end[0] == 'f' &&
            (end[1] == '\0' ||
             (end[1] == '0' && end[2] == '\0'))) {
            if (pval) *pval = mk_float((float)d);
            return 1;
        }
    }

    if (tok[0] == '+') {
        if (!strcmp(tok,"+NaN") || !strcasecmp(tok,"+nan.0")) {
            if (pval) *pval = mk_double(D_PNAN);
            return 1;
        }
        if (!strcmp(tok,"+Inf") || !strcasecmp(tok,"+inf.0")) {
            if (pval) *pval = mk_double(D_PINF);
            return 1;
        }
    }
    else if (tok[0] == '-') {
        if (!strcmp(tok,"-NaN") || !strcasecmp(tok,"-nan.0")) {
            if (pval) *pval = mk_double(D_NNAN);
            return 1;
        }
        if (!strcmp(tok,"-Inf") || !strcasecmp(tok,"-inf.0")) {
            if (pval) *pval = mk_double(D_NINF);
            return 1;
        }
        errno = 0;
        i64 = strtoll(tok, &end, base);
        if (errno)
            return 0;
        int done = (*end == '\0');  // must access *end before alloc
        if (pval) *pval = return_from_int64(i64);
        return done;
    }
    errno = 0;
    ui64 = strtoull_0b0o(tok, &end, base);
    if (errno)
        return 0;
    int done = (*end == '\0');  // must access *end before alloc
    if (pval) *pval = return_from_uint64(ui64);
    return done;
}

static int isnumtok(char *tok, value_t *pval)
{
    return isnumtok_base(tok, pval, 0);
}

static int read_numtok(char *tok, value_t *pval, int base)
{
    int result;
    errno = 0;
    result = isnumtok_base(tok, pval, base);
    if (errno == ERANGE)
        lerrorf(ParseError, "read: overflow in numeric constant %s", tok);
    return result;
}

static u_int32_t toktype = TOK_NONE;
static value_t tokval;
static char buf[256];

static char nextchar(void)
{
    int ch;
    char c;
    ios_t *f = F;

    do {
        if (f->bpos < f->size) {
            ch = f->buf[f->bpos++];
        }
        else {
            ch = ios_getc(f);
            if (ch == IOS_EOF)
                return 0;
        }
        c = (char)ch;
        if (c == ';') {
            // single-line comment
            do {
                ch = ios_getc(f);
                if (ch == IOS_EOF)
                    return 0;
            } while ((char)ch != '\n');
            c = (char)ch;
        }
    } while (c==' ' || isspace((unsigned char)c));
    return c;
}

static void take(void)
{
    toktype = TOK_NONE;
}

static void accumchar(char c, int *pi)
{
    buf[(*pi)++] = c;
    if (*pi >= (int)(sizeof(buf)-1))
        lerror(ParseError, "read: token too long");
}

// return: 1 if escaped (forced to be symbol)
static int read_token(char c, int digits)
{
    int i=0, ch, escaped=0, issym=0, first=1;

    while (1) {
        if (!first) {
            ch = ios_getc(F);
            if (ch == IOS_EOF)
                goto terminate;
            c = (char)ch;
        }
        first = 0;
        if (c == '|') {
            issym = 1;
            escaped = !escaped;
        }
        else if (c == '\\') {
            issym = 1;
            ch = ios_getc(F);
            if (ch == IOS_EOF)
                goto terminate;
            accumchar((char)ch, &i);
        }
        else if (!escaped && !(symchar(c) && (!digits || isdigit(c)))) {
            break;
        }
        else {
            accumchar(c, &i);
        }
    }
    ios_ungetc(c, F);
 terminate:
    buf[i++] = '\0';
    return issym;
}

static value_t do_read_sexpr(value_t label);

static u_int32_t peek(void)
{
    char c, *end;
    fixnum_t x;
    int ch, base;

    if (toktype != TOK_NONE)
        return toktype;
    c = nextchar();
    if (ios_eof(F)) return TOK_NONE;
    if (c == '(') {
        toktype = TOK_OPEN;
    }
    else if (c == ')') {
        toktype = TOK_CLOSE;
    }
    else if (c == '[') {
        toktype = TOK_OPENB;
    }
    else if (c == ']') {
        toktype = TOK_CLOSEB;
    }
    else if (c == '\'') {
        toktype = TOK_QUOTE;
    }
    else if (c == '`') {
        toktype = TOK_BQ;
    }
    else if (c == '"') {
        toktype = TOK_DOUBLEQUOTE;
    }
    else if (c == '#') {
        ch = ios_getc(F); c = (char)ch;
        if (ch == IOS_EOF)
            lerror(ParseError, "read: invalid read macro");
        if (c == '.') {
            toktype = TOK_SHARPDOT;
        }
        else if (c == '\'') {
            toktype = TOK_SHARPQUOTE;
        }
        else if (c == '\\') {
            uint32_t cval;
            if (ios_getutf8(F, &cval) == IOS_EOF)
                lerror(ParseError, "read: end of input in character constant");
            if (cval == (uint32_t)'u' || cval == (uint32_t)'U' ||
                cval == (uint32_t)'x') {
                read_token('u', 0);
                if (buf[1] != '\0') {  // not a solitary 'u','U','x'
                    if (!read_numtok(&buf[1], &tokval, 16))
                        lerror(ParseError,
                               "read: invalid hex character constant");
                    cval = numval(tokval);
                }
            }
            else if (cval >= 'a' && cval <= 'z') {
                read_token((char)cval, 0);
                tokval = symbol(buf);
                if (buf[1] == '\0')       /* one character */;
                else if (tokval == nulsym)        cval = 0x00;
                else if (tokval == alarmsym)      cval = 0x07;
                else if (tokval == backspacesym)  cval = 0x08;
                else if (tokval == tabsym)        cval = 0x09;
                else if (tokval == linefeedsym)   cval = 0x0A;
                else if (tokval == newlinesym)    cval = 0x0A;
                else if (tokval == vtabsym)       cval = 0x0B;
                else if (tokval == pagesym)       cval = 0x0C;
                else if (tokval == returnsym)     cval = 0x0D;
                else if (tokval == escsym)        cval = 0x1B;
                else if (tokval == spacesym)      cval = 0x20;
                else if (tokval == deletesym)     cval = 0x7F;
                else
                    lerrorf(ParseError, "read: unknown character #\\%s", buf);
            }
            toktype = TOK_NUM;
            tokval = mk_wchar(cval);
        }
        else if (c == '(') {
            toktype = TOK_SHARPOPEN;
        }
        else if (c == '<') {
            lerror(ParseError, "read: unreadable object");
        }
        else if (isdigit(c)) {
            read_token(c, 1);
            c = (char)ios_getc(F);
            if (c == '#')
                toktype = TOK_BACKREF;
            else if (c == '=')
                toktype = TOK_LABEL;
            else
                lerror(ParseError, "read: invalid label");
            errno = 0;
            x = strtol(buf, &end, 10);
            if (*end != '\0' || errno)
                lerror(ParseError, "read: invalid label");
            tokval = fixnum(x);
        }
        else if (c == '!') {
            // #! single line comment for shbang script support
            do {
                ch = ios_getc(F);
            } while (ch != IOS_EOF && (char)ch != '\n');
            return peek();
        }
        else if (c == '|') {
            // multiline comment
            int commentlevel=1;
            while (1) {
                ch = ios_getc(F);
            hashpipe_gotc:
                if (ch == IOS_EOF)
                    lerror(ParseError, "read: eof within comment");
                if ((char)ch == '|') {
                    ch = ios_getc(F);
                    if ((char)ch == '#') {
                        commentlevel--;
                        if (commentlevel == 0)
                            break;
                        else
                            continue;
                    }
                    goto hashpipe_gotc;
                }
                else if ((char)ch == '#') {
                    ch = ios_getc(F);
                    if ((char)ch == '|')
                        commentlevel++;
                    else
                        goto hashpipe_gotc;
                }
            }
            // this was whitespace, so keep peeking
            return peek();
        }
        else if (c == ';') {
            // datum comment
            (void)do_read_sexpr(UNBOUND); // skip
            return peek();
        }
        else if (c == ':') {
            // gensym
            ch = ios_getc(F);
            if ((char)ch == 'g')
                ch = ios_getc(F);
            read_token((char)ch, 0);
            errno = 0;
            x = strtol(buf, &end, 10);
            if (*end != '\0' || buf[0] == '\0' || errno)
                lerror(ParseError, "read: invalid gensym label");
            toktype = TOK_GENSYM;
            tokval = fixnum(x);
        }
        else if (symchar(c)) {
            read_token(ch, 0);

            if (((c == 'b' && (base= 2)) ||
                 (c == 'o' && (base= 8)) ||
                 (c == 'd' && (base=10)) ||
                 (c == 'x' && (base=16))) &&
                (isdigit_base(buf[1],base) ||
                 buf[1]=='-')) {
                if (!read_numtok(&buf[1], &tokval, base))
                    lerrorf(ParseError, "read: invalid base %d constant", base);
                return (toktype=TOK_NUM);
            }

            toktype = TOK_SHARPSYM;
            tokval = symbol(buf);
        }
        else {
            lerror(ParseError, "read: unknown read macro");
        }
    }
    else if (c == ',') {
        toktype = TOK_COMMA;
        ch = ios_getc(F);
        if (ch == IOS_EOF)
            return toktype;
        if ((char)ch == '@')
            toktype = TOK_COMMAAT;
        else if ((char)ch == '.')
            toktype = TOK_COMMADOT;
        else
            ios_ungetc((char)ch, F);
    }
    else {
        if (!read_token(c, 0)) {
            if (buf[0]=='.' && buf[1]=='\0') {
                return (toktype=TOK_DOT);
            }
            else {
                if (read_numtok(buf, &tokval, 0))
                    return (toktype=TOK_NUM);
            }
        }
        toktype = TOK_SYM;
        tokval = symbol(buf);
    }
    return toktype;
}

// NOTE: this is NOT an efficient operation. it is only used by the
// reader, and requires at least 1 and up to 3 garbage collections!
static value_t vector_grow(value_t v, int rewrite_refs)
{
    size_t i, s = vector_size(v);
    size_t d = vector_grow_amt(s);
    PUSH(v);
    value_t newv = alloc_vector(s+d, 1);
    v = Stack[SP-1];
    for(i=0; i < s; i++)
        vector_elt(newv, i) = vector_elt(v, i);
    // use gc to rewrite references from the old vector to the new
    Stack[SP-1] = newv;
    if (s > 0 && rewrite_refs) {
        ((size_t*)ptr(v))[0] |= 0x1;
        vector_elt(v, 0) = newv;
        gc(0);
    }
    return POP();
}

static value_t read_vector(value_t label, u_int32_t closer)
{
    value_t v=the_empty_vector, elt;
    u_int32_t i=0;
    PUSH(v);
    if (label != UNBOUND)
        ptrhash_put(&readstate->backrefs, (void*)label, (void*)v);
    while (peek() != closer) {
        if (ios_eof(F))
            lerror(ParseError, "read: unexpected end of input");
        v = Stack[SP-1]; // reload after possible alloc in peek()
        if (i >= vector_size(v)) {
            v = Stack[SP-1] = vector_grow(v, label != UNBOUND);
            if (label != UNBOUND)
                ptrhash_put(&readstate->backrefs, (void*)label, (void*)v);
        }
        elt = do_read_sexpr(UNBOUND);
        v = Stack[SP-1];
        vector_elt(v,i) = elt;
        i++;
    }
    take();
    if (i > 0)
        vector_setsize(v, i);
    return POP();
}

static value_t read_string(void)
{
    char *buf, *temp;
    char eseq[10];
    size_t i=0, j, sz = 64, ndig;
    int c;
    value_t s;
    u_int32_t wc=0;

    buf = (char*)malloc(sz);
    while (1) {
        if (i >= sz-4) {  // -4: leaves room for longest utf8 sequence
            sz *= 2;
            temp = (char*)realloc(buf, sz);
            if (temp == NULL) {
                free(buf);
                lerror(ParseError, "read: out of memory reading string");
            }
            buf = temp;
        }
        c = ios_getc(F);
        if (c == IOS_EOF) {
            free(buf);
            lerror(ParseError, "read: unexpected end of input in string");
        }
        if (c == '"')
            break;
        else if (c == '\\') {
            c = ios_getc(F);
            if (c == IOS_EOF) {
                free(buf);
                lerror(ParseError, "read: end of input in escape sequence");
            }
            j=0;
            if (octal_digit(c)) {
                do {
                    eseq[j++] = c;
                    c = ios_getc(F);
                } while (octal_digit(c) && j<3 && (c!=IOS_EOF));
                if (c!=IOS_EOF) ios_ungetc(c, F);
                eseq[j] = '\0';
                wc = strtol(eseq, NULL, 8);
                // \DDD and \xXX read bytes, not characters
                buf[i++] = ((char)wc);
            }
            else if ((c=='x' && (ndig=2)) ||
                     (c=='u' && (ndig=4)) ||
                     (c=='U' && (ndig=8))) {
                c = ios_getc(F);
                while (hex_digit(c) && j<ndig && (c!=IOS_EOF)) {
                    eseq[j++] = c;
                    c = ios_getc(F);
                }
                if (c!=IOS_EOF) ios_ungetc(c, F);
                eseq[j] = '\0';
                if (j) wc = strtol(eseq, NULL, 16);
                if (!j || wc > 0x10ffff) {
                    free(buf);
                    lerror(ParseError, "read: invalid escape sequence");
                }
                if (ndig == 2)
                    buf[i++] = ((char)wc);
                else
                    i += u8_wc_toutf8(&buf[i], wc);
            }
            else {
                buf[i++] = read_escape_control_char((char)c);
            }
        }
        else {
            buf[i++] = c;
        }
    }
    s = cvalue_string(i);
    memcpy(cvalue_data(s), buf, i);
    free(buf);
    return s;
}

// build a list of conses. this is complicated by the fact that all conses
// can move whenever a new cons is allocated. we have to refer to every cons
// through a handle to a relocatable pointer (i.e. a pointer on the stack).
static void read_list(value_t *pval, value_t label)
{
    value_t c, *pc;
    u_int32_t t;

    PUSH(NIL);
    pc = &Stack[SP-1];  // to keep track of current cons cell
    t = peek();
    while (t != TOK_CLOSE) {
        if (ios_eof(F))
            lerror(ParseError, "read: unexpected end of input");
        c = mk_cons(); car_(c) = cdr_(c) = NIL;
        if (iscons(*pc)) {
            cdr_(*pc) = c;
        }
        else {
            *pval = c;
            if (label != UNBOUND)
                ptrhash_put(&readstate->backrefs, (void*)label, (void*)c);
        }
        *pc = c;
        c = do_read_sexpr(UNBOUND); // must be on separate lines due to
        car_(*pc) = c;              // undefined evaluation order

        t = peek();
        if (t == TOK_DOT) {
            take();
            c = do_read_sexpr(UNBOUND);
            cdr_(*pc) = c;
            t = peek();
            if (ios_eof(F))
                lerror(ParseError, "read: unexpected end of input");
            if (t != TOK_CLOSE)
                lerror(ParseError, "read: expected ')'");
        }
    }
    take();
    (void)POP();
}

// label is the backreference we'd like to fix up with this read
static value_t do_read_sexpr(value_t label)
{
    value_t v, sym, oldtokval, *head;
    value_t *pv;
    u_int32_t t;
    char c;

    t = peek();
    take();
    switch (t) {
    case TOK_CLOSE:
        lerror(ParseError, "read: unexpected ')'");
    case TOK_CLOSEB:
        lerror(ParseError, "read: unexpected ']'");
    case TOK_DOT:
        lerror(ParseError, "read: unexpected '.'");
    case TOK_SYM:
    case TOK_NUM:
        return tokval;
    case TOK_COMMA:
        head = &COMMA; goto listwith;
    case TOK_COMMAAT:
        head = &COMMAAT; goto listwith;
    case TOK_COMMADOT:
        head = &COMMADOT; goto listwith;
    case TOK_BQ:
        head = &BACKQUOTE; goto listwith;
    case TOK_QUOTE:
        head = &QUOTE;
    listwith:
#ifdef MEMDEBUG2
        v = fl_list2(*head, NIL);
#else
        v = cons_reserve(2);
        car_(v) = *head;
        cdr_(v) = tagptr(((cons_t*)ptr(v))+1, TAG_CONS);
        car_(cdr_(v)) = cdr_(cdr_(v)) = NIL;
#endif
        PUSH(v);
        if (label != UNBOUND)
            ptrhash_put(&readstate->backrefs, (void*)label, (void*)v);
        v = do_read_sexpr(UNBOUND);
        car_(cdr_(Stack[SP-1])) = v;
        return POP();
    case TOK_SHARPQUOTE:
        // femtoLisp doesn't need symbol-function, so #' does nothing
        return do_read_sexpr(label);
    case TOK_OPEN:
        PUSH(NIL);
        read_list(&Stack[SP-1], label);
        return POP();
    case TOK_SHARPSYM:
        sym = tokval;
        if (sym == tsym || sym == Tsym)
            return FL_T;
        else if (sym == fsym || sym == Fsym)
            return FL_F;
        // constructor notation
        c = nextchar();
        if (c != '(') {
            take();
            lerrorf(ParseError, "read: expected argument list for %s",
                    symbol_name(tokval));
        }
        PUSH(NIL);
        read_list(&Stack[SP-1], UNBOUND);
        if (sym == vu8sym) {
            sym = arraysym;
            Stack[SP-1] = fl_cons(uint8sym, Stack[SP-1]);
        }
        else if (sym == fnsym) {
            sym = FUNCTION;
        }
        v = symbol_value(sym);
        if (v == UNBOUND)
            fl_raise(fl_list2(UnboundError, sym));
        return fl_apply(v, POP());
    case TOK_OPENB:
        return read_vector(label, TOK_CLOSEB);
    case TOK_SHARPOPEN:
        return read_vector(label, TOK_CLOSE);
    case TOK_SHARPDOT:
        // eval-when-read
        // evaluated expressions can refer to existing backreferences, but they
        // cannot see pending labels. in other words:
        // (... #2=#.#0# ... )    OK
        // (... #2=#.(#2#) ... )  DO NOT WANT
        sym = do_read_sexpr(UNBOUND);
        if (issymbol(sym)) {
            v = symbol_value(sym);
            if (v == UNBOUND)
                fl_raise(fl_list2(UnboundError, sym));
            return v;
        }
        return fl_toplevel_eval(sym);
    case TOK_LABEL:
        // create backreference label
        if (ptrhash_has(&readstate->backrefs, (void*)tokval))
            lerrorf(ParseError, "read: label %ld redefined", numval(tokval));
        oldtokval = tokval;
        v = do_read_sexpr(tokval);
        ptrhash_put(&readstate->backrefs, (void*)oldtokval, (void*)v);
        return v;
    case TOK_BACKREF:
        // look up backreference
        v = (value_t)ptrhash_get(&readstate->backrefs, (void*)tokval);
        if (v == (value_t)HT_NOTFOUND)
            lerrorf(ParseError, "read: undefined label %ld", numval(tokval));
        return v;
    case TOK_GENSYM:
        pv = (value_t*)ptrhash_bp(&readstate->gensyms, (void*)tokval);
        if (*pv == (value_t)HT_NOTFOUND)
            *pv = fl_gensym(NULL, 0);
        return *pv;
    case TOK_DOUBLEQUOTE:
        return read_string();
    }
    return FL_UNSPECIFIED;
}

value_t fl_read_sexpr(value_t f)
{
    value_t v;
    fl_readstate_t state;
    state.prev = readstate;
    htable_new(&state.backrefs, 8);
    htable_new(&state.gensyms, 8);
    state.source = f;
    readstate = &state;
    assert(toktype == TOK_NONE);
    fl_gc_handle(&tokval);

    v = do_read_sexpr(UNBOUND);

    fl_free_gc_handles(1);
    readstate = state.prev;
    free_readstate(&state);
    return v;
}
