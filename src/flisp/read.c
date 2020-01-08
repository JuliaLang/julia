enum {
    TOK_NONE, TOK_OPEN, TOK_CLOSE, TOK_DOT, TOK_QUOTE, TOK_SYM, TOK_NUM,
    TOK_BQ, TOK_COMMA, TOK_COMMAAT, TOK_COMMADOT,
    TOK_SHARPDOT, TOK_LABEL, TOK_BACKREF, TOK_SHARPQUOTE, TOK_SHARPOPEN,
    TOK_OPENB, TOK_CLOSEB, TOK_SHARPSYM, TOK_GENSYM, TOK_DOUBLEQUOTE
};

#define readF(fl_ctx) value2c(ios_t*,fl_ctx->readstate->source)

// defines which characters are ordinary symbol characters.
// exceptions are '.', which is an ordinary symbol character
// unless it's the only character in the symbol, and '#', which is
// an ordinary symbol character unless it's the first character.
static inline int symchar(char c)
{
    static const char *special = "()[]'\";`,\\| \f\n\r\t\v";
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

int isnumtok_base(fl_context_t *fl_ctx, char *tok, value_t *pval, int base)
{
    char *end;
    int64_t i64;
    uint64_t ui64;
    double d;
    if (*tok == '\0')
        return 0;
    if (!((tok[0]=='0' && tok[1]=='x') || (base >= 15)) &&
        strpbrk(tok, ".eEpP")) {
        d = jl_strtod_c(tok, &end);
        if (*end == '\0') {
            if (pval) *pval = mk_double(fl_ctx, d);
            return 1;
        }
        // floats can end in f or f0
        if (end > tok && end[0] == 'f' &&
            (end[1] == '\0' ||
             (end[1] == '0' && end[2] == '\0'))) {
            if (pval) *pval = mk_float(fl_ctx, (float)d);
            return 1;
        }
    }
    // hexadecimal float literals
    else if (((tok[0]=='0' && tok[1]=='x') || (base == 16)) &&
        strpbrk(tok, "pP")) {
        d = jl_strtod_c(tok, &end);
        if (*end == '\0') {
            if (pval) *pval = mk_double(fl_ctx, d);
            return 1;
        }
        // floats can end in f or f0
        if (end > tok && end[0] == 'f' &&
            (end[1] == '\0' ||
             (end[1] == '0' && end[2] == '\0'))) {
            if (pval) *pval = mk_float(fl_ctx, (float)d);
            return 1;
        }
    }

    if (tok[0] == '+') {
        if (!strcmp(tok,"+NaN") || !strcasecmp(tok,"+nan.0")) {
            if (pval) *pval = mk_double(fl_ctx, D_PNAN);
            return 1;
        }
        if (!strcmp(tok,"+Inf") || !strcasecmp(tok,"+inf.0")) {
            if (pval) *pval = mk_double(fl_ctx, D_PINF);
            return 1;
        }
    }
    else if (tok[0] == '-') {
        if (!strcmp(tok,"-NaN") || !strcasecmp(tok,"-nan.0")) {
            if (pval) *pval = mk_double(fl_ctx, D_NNAN);
            return 1;
        }
        if (!strcmp(tok,"-Inf") || !strcasecmp(tok,"-inf.0")) {
            if (pval) *pval = mk_double(fl_ctx, D_NINF);
            return 1;
        }
        errno = 0;
        i64 = strtoll(tok, &end, base);
        if (errno)
            return 0;
        int done = (*end == '\0');  // must access *end before alloc
        if (pval) *pval = return_from_int64(fl_ctx, i64);
        return done;
    }
    errno = 0;
    ui64 = strtoull_0b0o(tok, &end, base);
    if (errno)
        return 0;
    int done = (*end == '\0');  // must access *end before alloc
    if (pval) *pval = return_from_uint64(fl_ctx, ui64);
    return done;
}

static int isnumtok(fl_context_t *fl_ctx, char *tok, value_t *pval)
{
    return isnumtok_base(fl_ctx, tok, pval, 0);
}

static int read_numtok(fl_context_t *fl_ctx, char *tok, value_t *pval, int base)
{
    int result;
    errno = 0;
    result = isnumtok_base(fl_ctx, tok, pval, base);
    if (errno == ERANGE)
        lerrorf(fl_ctx, fl_ctx->ParseError, "read: overflow in numeric constant %s", tok);
    return result;
}

static char nextchar(fl_context_t *fl_ctx)
{
    int ch;
    char c;
    ios_t *f = readF(fl_ctx);

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

static void take(fl_context_t *fl_ctx)
{
    fl_ctx->readtoktype = TOK_NONE;
}

static void accumchar(fl_context_t *fl_ctx, char c, int *pi)
{
    fl_ctx->readbuf[(*pi)++] = c;
    if (*pi >= (int)(sizeof(fl_ctx->readbuf)-1))
        lerror(fl_ctx, fl_ctx->ParseError, "read: token too long");
}

// return: 1 if escaped (forced to be symbol)
static int read_token(fl_context_t *fl_ctx, char c, int digits)
{
    int i=0, ch, escaped=0, issym=0, first=1;

    while (1) {
        if (!first) {
            ch = ios_getc(readF(fl_ctx));
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
            ch = ios_getc(readF(fl_ctx));
            if (ch == IOS_EOF)
                goto terminate;
            accumchar(fl_ctx, (char)ch, &i);
        }
        else if (!escaped && !(symchar(c) && (!digits || isdigit(c)))) {
            break;
        }
        else {
            accumchar(fl_ctx, c, &i);
        }
    }
    ios_ungetc(c, readF(fl_ctx));
 terminate:
    fl_ctx->readbuf[i++] = '\0';
    return issym;
}

static value_t do_read_sexpr(fl_context_t *fl_ctx, value_t label);

static uint32_t peek(fl_context_t *fl_ctx)
{
    char c, *end;
    fixnum_t x;
    int ch, base;

    if (fl_ctx->readtoktype != TOK_NONE)
        return fl_ctx->readtoktype;
    c = nextchar(fl_ctx);
    if (ios_eof(readF(fl_ctx))) return TOK_NONE;
    if (c == '(') {
        fl_ctx->readtoktype = TOK_OPEN;
    }
    else if (c == ')') {
        fl_ctx->readtoktype = TOK_CLOSE;
    }
    else if (c == '[') {
        fl_ctx->readtoktype = TOK_OPENB;
    }
    else if (c == ']') {
        fl_ctx->readtoktype = TOK_CLOSEB;
    }
    else if (c == '\'') {
        fl_ctx->readtoktype = TOK_QUOTE;
    }
    else if (c == '`') {
        fl_ctx->readtoktype = TOK_BQ;
    }
    else if (c == '"') {
        fl_ctx->readtoktype = TOK_DOUBLEQUOTE;
    }
    else if (c == '#') {
        ch = ios_getc(readF(fl_ctx)); c = (char)ch;
        if (ch == IOS_EOF)
            lerror(fl_ctx, fl_ctx->ParseError, "read: invalid read macro");
        if (c == '.') {
            fl_ctx->readtoktype = TOK_SHARPDOT;
        }
        else if (c == '\'') {
            fl_ctx->readtoktype = TOK_SHARPQUOTE;
        }
        else if (c == '\\') {
            uint32_t cval;
            if (ios_getutf8(readF(fl_ctx), &cval) == IOS_EOF)
                lerror(fl_ctx, fl_ctx->ParseError, "read: end of input in character constant");
            if (cval == (uint32_t)'u' || cval == (uint32_t)'U' ||
                cval == (uint32_t)'x') {
                read_token(fl_ctx, 'u', 0);
                if (fl_ctx->readbuf[1] != '\0') {  // not a solitary 'u','U','x'
                    if (!read_numtok(fl_ctx, &fl_ctx->readbuf[1], &fl_ctx->readtokval, 16))
                        lerror(fl_ctx, fl_ctx->ParseError,
                               "read: invalid hex character constant");
                    cval = numval(fl_ctx->readtokval);
                }
            }
            else if (cval >= 'a' && cval <= 'z') {
                read_token(fl_ctx, (char)cval, 0);
                fl_ctx->readtokval = symbol(fl_ctx, fl_ctx->readbuf);
                if (fl_ctx->readbuf[1] == '\0')       /* one character */;
                else if (fl_ctx->readtokval == fl_ctx->nulsym)        cval = 0x00;
                else if (fl_ctx->readtokval == fl_ctx->alarmsym)      cval = 0x07;
                else if (fl_ctx->readtokval == fl_ctx->backspacesym)  cval = 0x08;
                else if (fl_ctx->readtokval == fl_ctx->tabsym)        cval = 0x09;
                else if (fl_ctx->readtokval == fl_ctx->linefeedsym)   cval = 0x0A;
                else if (fl_ctx->readtokval == fl_ctx->newlinesym)    cval = 0x0A;
                else if (fl_ctx->readtokval == fl_ctx->vtabsym)       cval = 0x0B;
                else if (fl_ctx->readtokval == fl_ctx->pagesym)       cval = 0x0C;
                else if (fl_ctx->readtokval == fl_ctx->returnsym)     cval = 0x0D;
                else if (fl_ctx->readtokval == fl_ctx->escsym)        cval = 0x1B;
                else if (fl_ctx->readtokval == fl_ctx->spacesym)      cval = 0x20;
                else if (fl_ctx->readtokval == fl_ctx->deletesym)     cval = 0x7F;
                else
                    lerrorf(fl_ctx, fl_ctx->ParseError, "read: unknown character #\\%s", fl_ctx->readbuf);
            }
            fl_ctx->readtoktype = TOK_NUM;
            fl_ctx->readtokval = mk_wchar(fl_ctx, cval);
        }
        else if (c == '(') {
            fl_ctx->readtoktype = TOK_SHARPOPEN;
        }
        else if (c == '<') {
            lerror(fl_ctx, fl_ctx->ParseError, "read: unreadable object");
        }
        else if (isdigit(c)) {
            read_token(fl_ctx, c, 1);
            c = (char)ios_getc(readF(fl_ctx));
            if (c == '#')
                fl_ctx->readtoktype = TOK_BACKREF;
            else if (c == '=')
                fl_ctx->readtoktype = TOK_LABEL;
            else
                lerror(fl_ctx, fl_ctx->ParseError, "read: invalid label");
            errno = 0;
            x = strtol(fl_ctx->readbuf, &end, 10);
            if (*end != '\0' || errno)
                lerror(fl_ctx, fl_ctx->ParseError, "read: invalid label");
            fl_ctx->readtokval = fixnum(x);
        }
        else if (c == '!') {
            // #! single line comment for shbang script support
            do {
                ch = ios_getc(readF(fl_ctx));
            } while (ch != IOS_EOF && (char)ch != '\n');
            return peek(fl_ctx);
        }
        else if (c == '|') {
            // multiline comment
            int commentlevel=1;
            while (1) {
                ch = ios_getc(readF(fl_ctx));
            hashpipe_gotc:
                if (ch == IOS_EOF)
                    lerror(fl_ctx, fl_ctx->ParseError, "read: eof within comment");
                if ((char)ch == '|') {
                    ch = ios_getc(readF(fl_ctx));
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
                    ch = ios_getc(readF(fl_ctx));
                    if ((char)ch == '|')
                        commentlevel++;
                    else
                        goto hashpipe_gotc;
                }
            }
            // this was whitespace, so keep peeking
            return peek(fl_ctx);
        }
        else if (c == ';') {
            // datum comment
            (void)do_read_sexpr(fl_ctx, UNBOUND); // skip
            return peek(fl_ctx);
        }
        else if (c == ':') {
            // gensym
            ch = ios_getc(readF(fl_ctx));
            if ((char)ch == 'g')
                ch = ios_getc(readF(fl_ctx));
            read_token(fl_ctx, (char)ch, 0);
            errno = 0;
            x = strtol(fl_ctx->readbuf, &end, 10);
            if (*end != '\0' || fl_ctx->readbuf[0] == '\0' || errno)
                lerror(fl_ctx, fl_ctx->ParseError, "read: invalid gensym label");
            fl_ctx->readtoktype = TOK_GENSYM;
            fl_ctx->readtokval = fixnum(x);
        }
        else if (symchar(c)) {
            read_token(fl_ctx, ch, 0);

            if (((c == 'b' && (base= 2)) ||
                 (c == 'o' && (base= 8)) ||
                 (c == 'd' && (base=10)) ||
                 (c == 'x' && (base=16))) &&
                (isdigit_base(fl_ctx->readbuf[1],base) ||
                 fl_ctx->readbuf[1]=='-')) {
                if (!read_numtok(fl_ctx, &fl_ctx->readbuf[1], &fl_ctx->readtokval, base))
                    lerrorf(fl_ctx, fl_ctx->ParseError, "read: invalid base %d constant", base);
                return (fl_ctx->readtoktype=TOK_NUM);
            }

            fl_ctx->readtoktype = TOK_SHARPSYM;
            fl_ctx->readtokval = symbol(fl_ctx, fl_ctx->readbuf);
        }
        else {
            lerror(fl_ctx, fl_ctx->ParseError, "read: unknown read macro");
        }
    }
    else if (c == ',') {
        fl_ctx->readtoktype = TOK_COMMA;
        ch = ios_getc(readF(fl_ctx));
        if (ch == IOS_EOF)
            return fl_ctx->readtoktype;
        if ((char)ch == '@')
            fl_ctx->readtoktype = TOK_COMMAAT;
        else if ((char)ch == '.')
            fl_ctx->readtoktype = TOK_COMMADOT;
        else
            ios_ungetc((char)ch, readF(fl_ctx));
    }
    else {
        if (!read_token(fl_ctx, c, 0)) {
            if (fl_ctx->readbuf[0]=='.' && fl_ctx->readbuf[1]=='\0') {
                return (fl_ctx->readtoktype=TOK_DOT);
            }
            else {
                if (read_numtok(fl_ctx, fl_ctx->readbuf, &fl_ctx->readtokval, 0))
                    return (fl_ctx->readtoktype=TOK_NUM);
            }
        }
        fl_ctx->readtoktype = TOK_SYM;
        fl_ctx->readtokval = symbol(fl_ctx, fl_ctx->readbuf);
    }
    return fl_ctx->readtoktype;
}

// NOTE: this is NOT an efficient operation. it is only used by the
// reader, and requires at least 1 and up to 3 garbage collections!
static value_t vector_grow(fl_context_t *fl_ctx, value_t v, int rewrite_refs)
{
    size_t i, s = vector_size(v);
    size_t d = vector_grow_amt(s);
    PUSH(fl_ctx, v);
    value_t newv = alloc_vector(fl_ctx, s+d, 1);
    v = fl_ctx->Stack[fl_ctx->SP-1];
    for(i=0; i < s; i++)
        vector_elt(newv, i) = vector_elt(v, i);
    // use gc to rewrite references from the old vector to the new
    fl_ctx->Stack[fl_ctx->SP-1] = newv;
    if (s > 0 && rewrite_refs) {
        ((size_t*)ptr(v))[0] |= 0x1;
        vector_elt(v, 0) = newv;
        gc(fl_ctx, 0);
    }
    return POP(fl_ctx);
}

static value_t read_vector(fl_context_t *fl_ctx, value_t label, uint32_t closer)
{
    value_t v=fl_ctx->the_empty_vector, elt;
    uint32_t i=0;
    PUSH(fl_ctx, v);
    if (label != UNBOUND)
        ptrhash_put(&fl_ctx->readstate->backrefs, (void*)label, (void*)v);
    while (peek(fl_ctx) != closer) {
        if (ios_eof(readF(fl_ctx)))
            lerror(fl_ctx, fl_ctx->ParseError, "read: unexpected end of input");
        v = fl_ctx->Stack[fl_ctx->SP-1]; // reload after possible alloc in peek()
        if (i >= vector_size(v)) {
            v = fl_ctx->Stack[fl_ctx->SP-1] = vector_grow(fl_ctx, v, label != UNBOUND);
            if (label != UNBOUND)
                ptrhash_put(&fl_ctx->readstate->backrefs, (void*)label, (void*)v);
        }
        elt = do_read_sexpr(fl_ctx, UNBOUND);
        v = fl_ctx->Stack[fl_ctx->SP-1];
        vector_elt(v,i) = elt;
        i++;
    }
    take(fl_ctx);
    if (i > 0)
        vector_setsize(v, i);
    return POP(fl_ctx);
}

static value_t read_string(fl_context_t *fl_ctx)
{
    char *buf, *temp;
    char eseq[10];
    size_t i=0, j, sz = 64, ndig;
    int c;
    value_t s;
    uint32_t wc=0;

    buf = (char*)malloc(sz);
    if (buf == NULL) {
        lerror(fl_ctx, fl_ctx->ParseError, "read: out of memory reading string");
    }
    while (1) {
        if (i >= sz-4) {  // -4: leaves room for longest utf8 sequence
            sz *= 2;
            temp = (char*)realloc(buf, sz);
            if (temp == NULL) {
                free(buf);
                lerror(fl_ctx, fl_ctx->ParseError, "read: out of memory reading string");
            }
            buf = temp;
        }
        c = ios_getc(readF(fl_ctx));
        if (c == IOS_EOF) {
            free(buf);
            lerror(fl_ctx, fl_ctx->ParseError, "read: unexpected end of input in string");
        }
        if (c == '"')
            break;
        else if (c == '\\') {
            c = ios_getc(readF(fl_ctx));
            if (c == IOS_EOF) {
                free(buf);
                lerror(fl_ctx, fl_ctx->ParseError, "read: end of input in escape sequence");
            }
            j=0;
            if (octal_digit(c)) {
                do {
                    eseq[j++] = c;
                    c = ios_getc(readF(fl_ctx));
                } while (octal_digit(c) && j<3 && (c!=IOS_EOF));
                if (c!=IOS_EOF) ios_ungetc(c, readF(fl_ctx));
                eseq[j] = '\0';
                wc = strtol(eseq, NULL, 8);
                // \DDD and \xXX read bytes, not characters
                buf[i++] = ((char)wc);
            }
            else if ((c=='x' && (ndig=2)) ||
                     (c=='u' && (ndig=4)) ||
                     (c=='U' && (ndig=8))) {
                c = ios_getc(readF(fl_ctx));
                while (hex_digit(c) && j<ndig && (c!=IOS_EOF)) {
                    eseq[j++] = c;
                    c = ios_getc(readF(fl_ctx));
                }
                if (c!=IOS_EOF) ios_ungetc(c, readF(fl_ctx));
                eseq[j] = '\0';
                if (j) wc = strtol(eseq, NULL, 16);
                if (!j || wc > 0x10ffff) {
                    free(buf);
                    lerror(fl_ctx, fl_ctx->ParseError, "read: invalid escape sequence");
                }
                if (ndig == 2)
                    buf[i++] = ((char)wc);
                else
                    i += u8_wc_toutf8(&buf[i], wc);
            }
            else {
                char esc = read_escape_control_char((char)c);
                if (esc == (char)c && !strchr("\\'\"$`", esc)) {
                    free(buf);
                    lerror(fl_ctx, fl_ctx->ParseError, "read: invalid escape sequence");
                }
                buf[i++] = esc;
            }
        }
        else {
            buf[i++] = c;
        }
    }
    s = cvalue_string(fl_ctx, i);
    memcpy(cvalue_data(s), buf, i);
    free(buf);
    return s;
}

// build a list of conses. this is complicated by the fact that all conses
// can move whenever a new cons is allocated. we have to refer to every cons
// through a handle to a relocatable pointer (i.e. a pointer on the stack).
static void read_list(fl_context_t *fl_ctx, value_t *pval, value_t label)
{
    value_t c, *pc;
    uint32_t t;

    PUSH(fl_ctx, fl_ctx->NIL);
    pc = &fl_ctx->Stack[fl_ctx->SP-1];  // to keep track of current cons cell
    t = peek(fl_ctx);
    while (t != TOK_CLOSE) {
        if (ios_eof(readF(fl_ctx)))
            lerror(fl_ctx, fl_ctx->ParseError, "read: unexpected end of input");
        c = mk_cons(fl_ctx); car_(c) = cdr_(c) = fl_ctx->NIL;
        if (iscons(*pc)) {
            cdr_(*pc) = c;
        }
        else {
            *pval = c;
            if (label != UNBOUND)
                ptrhash_put(&fl_ctx->readstate->backrefs, (void*)label, (void*)c);
        }
        *pc = c;
        c = do_read_sexpr(fl_ctx, UNBOUND); // must be on separate lines due to
        car_(*pc) = c;              // undefined evaluation order

        t = peek(fl_ctx);
        if (t == TOK_DOT) {
            take(fl_ctx);
            c = do_read_sexpr(fl_ctx, UNBOUND);
            cdr_(*pc) = c;
            t = peek(fl_ctx);
            if (ios_eof(readF(fl_ctx)))
                lerror(fl_ctx, fl_ctx->ParseError, "read: unexpected end of input");
            if (t != TOK_CLOSE)
                lerror(fl_ctx, fl_ctx->ParseError, "read: expected ')'");
        }
    }
    take(fl_ctx);
    (void)POP(fl_ctx);
}

// label is the backreference we'd like to fix up with this read
static value_t do_read_sexpr(fl_context_t *fl_ctx, value_t label)
{
    value_t v, sym, oldtokval, *head;
    value_t *pv;
    uint32_t t;
    char c;

    t = peek(fl_ctx);
    take(fl_ctx);
    switch (t) {
    case TOK_CLOSE:
        lerror(fl_ctx, fl_ctx->ParseError, "read: unexpected ')'");
    case TOK_CLOSEB:
        lerror(fl_ctx, fl_ctx->ParseError, "read: unexpected ']'");
    case TOK_DOT:
        lerror(fl_ctx, fl_ctx->ParseError, "read: unexpected '.'");
    case TOK_SYM:
    case TOK_NUM:
        return fl_ctx->readtokval;
    case TOK_COMMA:
        head = &fl_ctx->COMMA; goto listwith;
    case TOK_COMMAAT:
        head = &fl_ctx->COMMAAT; goto listwith;
    case TOK_COMMADOT:
        head = &fl_ctx->COMMADOT; goto listwith;
    case TOK_BQ:
        head = &fl_ctx->BACKQUOTE; goto listwith;
    case TOK_QUOTE:
        head = &fl_ctx->QUOTE;
    listwith:
#ifdef MEMDEBUG2
        v = fl_list2(fl_ctx, *head, fl_ctx->NIL);
#else
        v = cons_reserve(fl_ctx, 2);
        car_(v) = *head;
        cdr_(v) = tagptr(((cons_t*)ptr(v))+1, TAG_CONS);
        car_(cdr_(v)) = cdr_(cdr_(v)) = fl_ctx->NIL;
#endif
        PUSH(fl_ctx, v);
        if (label != UNBOUND)
            ptrhash_put(&fl_ctx->readstate->backrefs, (void*)label, (void*)v);
        v = do_read_sexpr(fl_ctx, UNBOUND);
        car_(cdr_(fl_ctx->Stack[fl_ctx->SP-1])) = v;
        return POP(fl_ctx);
    case TOK_SHARPQUOTE:
        // femtoLisp doesn't need symbol-function, so #' does nothing
        return do_read_sexpr(fl_ctx, label);
    case TOK_OPEN:
        PUSH(fl_ctx, fl_ctx->NIL);
        read_list(fl_ctx, &fl_ctx->Stack[fl_ctx->SP-1], label);
        return POP(fl_ctx);
    case TOK_SHARPSYM:
        sym = fl_ctx->readtokval;
        if (sym == fl_ctx->tsym || sym == fl_ctx->Tsym)
            return fl_ctx->T;
        else if (sym == fl_ctx->fsym || sym == fl_ctx->Fsym)
            return fl_ctx->F;
        // constructor notation
        c = nextchar(fl_ctx);
        if (c != '(') {
            take(fl_ctx);
            lerrorf(fl_ctx, fl_ctx->ParseError, "read: expected argument list for %s",
                    symbol_name(fl_ctx, fl_ctx->readtokval));
        }
        PUSH(fl_ctx, fl_ctx->NIL);
        read_list(fl_ctx, &fl_ctx->Stack[fl_ctx->SP-1], UNBOUND);
        if (sym == fl_ctx->vu8sym) {
            sym = fl_ctx->arraysym;
            fl_ctx->Stack[fl_ctx->SP-1] = fl_cons(fl_ctx, fl_ctx->uint8sym, fl_ctx->Stack[fl_ctx->SP-1]);
        }
        else if (sym == fl_ctx->fnsym) {
            sym = fl_ctx->FUNCTION;
        }
        v = symbol_value(sym);
        if (v == UNBOUND)
            fl_raise(fl_ctx, fl_list2(fl_ctx, fl_ctx->UnboundError, sym));
        return fl_apply(fl_ctx, v, POP(fl_ctx));
    case TOK_OPENB:
        return read_vector(fl_ctx, label, TOK_CLOSEB);
    case TOK_SHARPOPEN:
        return read_vector(fl_ctx, label, TOK_CLOSE);
    case TOK_SHARPDOT:
        // eval-when-read
        // evaluated expressions can refer to existing backreferences, but they
        // cannot see pending labels. in other words:
        // (... #2=#.#0# ... )    OK
        // (... #2=#.(#2#) ... )  DO NOT WANT
        sym = do_read_sexpr(fl_ctx, UNBOUND);
        if (issymbol(sym)) {
            v = symbol_value(sym);
            if (v == UNBOUND)
                fl_raise(fl_ctx, fl_list2(fl_ctx, fl_ctx->UnboundError, sym));
            return v;
        }
        return fl_toplevel_eval(fl_ctx, sym);
    case TOK_LABEL:
        // create backreference label
        if (ptrhash_has(&fl_ctx->readstate->backrefs, (void*)fl_ctx->readtokval))
            lerrorf(fl_ctx, fl_ctx->ParseError, "read: label %ld redefined", numval(fl_ctx->readtokval));
        oldtokval = fl_ctx->readtokval;
        v = do_read_sexpr(fl_ctx, fl_ctx->readtokval);
        ptrhash_put(&fl_ctx->readstate->backrefs, (void*)oldtokval, (void*)v);
        return v;
    case TOK_BACKREF:
        // look up backreference
        v = (value_t)ptrhash_get(&fl_ctx->readstate->backrefs, (void*)fl_ctx->readtokval);
        if (v == (value_t)HT_NOTFOUND)
            lerrorf(fl_ctx, fl_ctx->ParseError, "read: undefined label %ld", numval(fl_ctx->readtokval));
        return v;
    case TOK_GENSYM:
        pv = (value_t*)ptrhash_bp(&fl_ctx->readstate->gensyms, (void*)fl_ctx->readtokval);
        if (*pv == (value_t)HT_NOTFOUND)
            *pv = fl_gensym(fl_ctx, NULL, 0);
        return *pv;
    case TOK_DOUBLEQUOTE:
        return read_string(fl_ctx);
    }
    return FL_UNSPECIFIED(fl_ctx);
}

value_t fl_read_sexpr(fl_context_t *fl_ctx, value_t f)
{
    value_t v;
    fl_readstate_t state;
    state.prev = fl_ctx->readstate;
    htable_new(&state.backrefs, 8);
    htable_new(&state.gensyms, 8);
    state.source = f;
    fl_ctx->readstate = &state;
    assert(fl_ctx->readtoktype == TOK_NONE);
    fl_gc_handle(fl_ctx, &fl_ctx->readtokval);

    v = do_read_sexpr(fl_ctx, UNBOUND);

    fl_free_gc_handles(fl_ctx, 1);
    fl_ctx->readstate = state.prev;
    free_readstate(&state);
    return v;
}

static void fl_read_init(fl_context_t *fl_ctx)
{
    fl_ctx->readtoktype = TOK_NONE;
    fl_ctx->readtokval = 0;
    memset(fl_ctx->readbuf, 0, sizeof(fl_ctx->readbuf));
}
