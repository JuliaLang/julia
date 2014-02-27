/*
    repl-readline.c: Julia REPL with readline support.
    Copyright (C) 2009-2011, Jeff Bezanson, Stefan Karpinski, Viral B. Shah.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*/

#include "repl.h"
#ifdef __WIN32__
# define WIN32_LEAN_AND_MEAN
# include <windows.h>
# include <shlobj.h>
#endif

extern int asprintf(char **strp, const char *fmt, ...);

#define USE_READLINE_STATIC
#include <readline/readline.h>
#include <readline/history.h>

int prompt_length;
int disable_history;
static char *history_file = NULL;
static jl_value_t *rl_ast = NULL;
static char *strtok_saveptr;

// yes, readline uses inconsistent indexing internally.
#define history_rem(n) remove_history(n-history_base)

static void init_history(void)
{
    using_history();
    if (disable_history) return;
    struct stat stat_info;
    if (!stat(".julia_history", &stat_info)) {
        // history file in current dir
        history_file = ".julia_history";
    }
    else {
        char *histenv = getenv("JULIA_HISTORY");
        if (histenv) {
            history_file = histenv;
        }
        else {
#ifndef __WIN32__
            char *home = getenv("HOME");
            if (!home) return;
            asprintf(&history_file, "%s/.julia_history", home);
#else
            char *homedrive = getenv("HOMEDRIVE");
            char *homepath = getenv("HOMEPATH");
            if (!homedrive || !homepath) return;
            asprintf(&history_file, "%s/%s/.julia_history", homedrive, homepath);
#endif
        }
    }
    if (!stat(history_file, &stat_info)) {
        read_history(history_file);
        for (;;) {
            HIST_ENTRY *entry = history_get(history_base);
            if (entry && isspace(entry->line[0]))
                free_history_entry(history_rem(history_base));
            else break;
        }
        int i, j, k;
        for (i=1 ;; i++) {
            HIST_ENTRY *first = history_get(i);
            if (!first) break;
            int length = strlen(first->line)+1;
            for (j = i+1 ;; j++) {
                HIST_ENTRY *child = history_get(j);
                if (!child || !isspace(child->line[0])) break;
                length += strlen(child->line)+1;
            }
            if (j == i+1) continue;
            first->line = (char*)realloc(first->line, length);
            char *p = strchr(first->line, '\0');
            for (k = i+1; k < j; k++) {
                *p = '\n';
                #ifndef __WIN32__
                p = stpcpy(p+1, history_get(i+1)->line);
                #else
                p = strcpy(p+1, history_get(i+1)->line);
                #endif
                free_history_entry(history_rem(i+1));
            }
        }
    }
    else if (errno == ENOENT) {
        write_history(history_file);
    }
    else {
        jl_printf(jl_uv_stderr, "history file error: %s\n", strerror(errno));
        exit(1);
    }
}

static int last_hist_is_temp = 0;
static int last_hist_offset = -1;

static void add_history_temporary(char *input)
{
    if (!input || !*input) return;
    if (last_hist_is_temp) {
        history_rem(history_length);
        last_hist_is_temp = 0;
    }
    last_hist_offset = -1;
    add_history(input);
    last_hist_is_temp = 1;
}

static void add_history_permanent(char *input)
{
    if (!input || !*input) return;
    if (last_hist_is_temp) {
        history_rem(history_length);
        last_hist_is_temp = 0;
    }
    last_hist_offset = -1;
    HIST_ENTRY *entry = history_get(history_length);
    if (entry && !strcmp(input, entry->line)) return;
    last_hist_offset = where_history();
    add_history(input);
    if (history_file)
        append_history(1, history_file);
}

static int line_start(int point)
{
    if (!point) return 0;
    int i = point-1;
    for (; i; i--) if (rl_line_buffer[i] == '\n') return i+1;
    return rl_line_buffer[i] == '\n' ? 1 : 0;
}

static int line_end(int point)
{
    char *nl = strchr(rl_line_buffer + point, '\n');
    if (!nl) return rl_end;
    return nl - rl_line_buffer;
}

static int strip_initial_spaces = 0;
static int spaces_suppressed = 0;

static void reset_indent(void)
{
    strip_initial_spaces = 0;
    spaces_suppressed = 0;
}

// TODO: is it appropriate to call this on the int values readline uses?
static int jl_word_char(uint32_t wc)
{
    return strchr(rl_completer_word_break_characters, wc) == NULL;
}

static int newline_callback(int count, int key)
{
    if (!rl_point) return 0;
    spaces_suppressed = 0;
    rl_insert_text("\n");
    int i;
    for (i = 0; i < prompt_length; i++)
        rl_insert_text(" ");
    return 0;
}

static jl_value_t* call_jl_function_with_string(const char *fname, const char *arg, size_t arglen)
{
    jl_value_t *f = jl_get_global(jl_base_module,jl_symbol(fname));
    assert(f);
    jl_value_t **fargs;
    JL_GC_PUSHARGS(fargs, 1);
    fargs[0] = jl_pchar_to_string((char*)arg, arglen);
    jl_value_t *result = jl_apply((jl_function_t*)f, fargs, 1);
    JL_GC_POP();
    return result;
}

static jl_value_t* repl_parse_input_line(char *buf)
{
    if (buf[0] == ';') {
        buf++;
        return call_jl_function_with_string("repl_hook", buf, strlen(buf));
    }
    else if (buf[0] == '?') {
        char *tmpbuf;
        asprintf(&tmpbuf, "@Base.help %s", buf+1);
        jl_value_t *v = jl_parse_input_line(tmpbuf);
        free(tmpbuf);
        return v;
    }
    return jl_parse_input_line(buf);
}

static int return_callback(int count, int key)
{
    static int consecutive_returns = 0;
    if (rl_point > prompt_length && rl_point == rl_end &&
        rl_line_buffer[rl_point-prompt_length-1] == '\n')
        consecutive_returns++;
    else
        consecutive_returns = 0;
    add_history_temporary(rl_line_buffer);
    rl_ast = repl_parse_input_line(rl_line_buffer);
    rl_done = !rl_ast || !jl_is_expr(rl_ast) ||
        (((jl_expr_t*)rl_ast)->head != jl_incomplete_sym) ||
        consecutive_returns > 1;
    if (!rl_done) {
        newline_callback(count, key);
    }
    else {
        reset_indent();
        rl_point = rl_end;
        rl_redisplay();
    }
    return 0;
}

static int suppress_space(void)
{
    int i;
    for (i = line_start(rl_point); i < rl_point; i++)
        if (rl_line_buffer[i] != ' ') return 0;
    if (spaces_suppressed < strip_initial_spaces) return 1;
    return 0;
}

static int space_callback(int count, int key)
{
    if (!rl_point) strip_initial_spaces++;
    else if (suppress_space()) spaces_suppressed++;
    else rl_insert_text(" ");
    return 0;
}

#if defined(_WIN32)
char *strtok_r(char *str, const char *delim, char **save)
{
    char *res, *last;

    if (!save)
        return strtok(str, delim);
    if (!str && !(str = *save))
        return NULL;
    last = str + strlen(str);
    if ((*save = res = strtok(str, delim))) {
        *save += strlen(res);
        if (*save < last)
            (*save)++;
        else
            *save = NULL;
    }
    return res;
}
#endif

int complete_method_table()
{
    if (rl_point < 2 || rl_line_buffer[rl_point-1] != '(') return 0;

    // extract the token preceding the (
    int tokenstart = rl_point-2;

    // check for special operators
    if (strchr("\\><=|&+-*/%^~", rl_line_buffer[tokenstart])){
        while (tokenstart>0 && strchr("<>=!", rl_line_buffer[tokenstart-1])){
            tokenstart--;
        }
    }
    else{
        // check for functions that might contain ! but not start with !
        while (tokenstart>=0 && (jl_word_char(rl_line_buffer[tokenstart]) ||
              rl_line_buffer[tokenstart] == '!')) {
            tokenstart--;
        }
        tokenstart++;
        // ! can't be the first character of a function, unless it's the only character
        if (tokenstart != rl_point-2 && rl_line_buffer[tokenstart] == '!'){
            tokenstart ++;
        }
    }

    jl_value_t* result = call_jl_function_with_string("repl_methods",
                                                      &rl_line_buffer[tokenstart],
                                                      rl_point-tokenstart-1);

    if (!jl_is_byte_string(result)) return 0;
    char *completions = jl_string_data(result);

    int nallocmethods = 0;
    size_t nchars = strlen(completions);
    for (size_t i=0; i<nchars; i++) nallocmethods += completions[i] == '\n';

    char **methods = malloc(sizeof(char *)*(nallocmethods+1));
    methods[0] = NULL;
    methods[1] = strtok_r(completions, "\n", &strtok_saveptr);
    if (methods[1] == NULL) {
       free(methods);
       return 0;
    }
    int maxlen = strlen(methods[1]);
    int nmethods = 1;

    char *method;
    while (nmethods < nallocmethods &&
           (method = strtok_r(NULL, "\n", &strtok_saveptr))) {
        int len = strlen(method);
        if (len > maxlen) maxlen = len;
        methods[nmethods+1] = method;
        nmethods++;
    }

    rl_display_match_list(methods, nmethods, maxlen);
    free(methods);
    rl_forced_update_display();
    return 1;
}

int complete_filenames_query()
{
    if (rl_line_buffer[0] == ';')
        return 1; //rl_filename_completion_function(ch, c);

    int instring = 0;
    int incmd = 0;
    int escaped = 0;
    int nearquote = 0;
    int i;
    for (i = 0; i < rl_point; ++i) {
        switch (rl_line_buffer[i]) {
            case '\\':
                if (instring)
                    escaped ^= 1;
                nearquote = 0;
                break;
            case '\'':
                if (!instring)
                    nearquote = 1;
                break;
            case '"':
                if (!escaped && !nearquote && !incmd)
                    instring ^= 1;
                escaped = nearquote = 0;
                break;
            case '`':
                if (!escaped && !nearquote && !instring)
                    incmd ^= 1;
                escaped = nearquote = 0;
                break;
            default:
                escaped = nearquote = 0;
        }
    }
    return (instring || incmd);
}

int complete_filenames()
{
    if (!complete_filenames_query())
        return 0;

    char * bk_completer_word_break_characters = rl_completer_word_break_characters;
    char * (*bk_completion_entry_function)(const char*, int) = rl_completion_entry_function;

    rl_completer_word_break_characters = " \t\n\"\\'`@$><=;|&{(";
    rl_completion_entry_function = rl_filename_completion_function;

    rl_complete_internal('!');

    rl_completer_word_break_characters = bk_completer_word_break_characters;
    rl_completion_entry_function = bk_completion_entry_function;

    return 1;
}

static int tab_callback(int count, int key)
{
    if (!rl_point) {
        strip_initial_spaces += tab_width;
        return 0;
    }
    int i;
    for (i = line_start(rl_point); i < rl_point; i++) {
        if (rl_line_buffer[i] != ' ') {
            // do tab completion
            i = rl_point;
            if (!complete_method_table()) {
                if (!complete_filenames())
                    rl_complete_internal('!');
                if (i < rl_point && rl_line_buffer[rl_point-1] == ' ') {
                    rl_delete_text(rl_point-1, rl_point);
                    rl_point = rl_point-1;
                }
            }
            return 0;
        }
    }
    // indent to next tab stop
    if (suppress_space()) {
        spaces_suppressed += tab_width;
    }
    else {
        i = line_start(rl_point) + prompt_length;
        do { rl_insert_text(" "); } while ((rl_point - i) % tab_width);
    }
    return 0;
}

static int line_start_callback(int count, int key)
{
    reset_indent();
    int start = line_start(rl_point);
    int flush_left = rl_point == 0 || rl_point == start + prompt_length;
    rl_point = flush_left ? 0 : (!start ? start : start + prompt_length);
    return 0;
}

static int line_end_callback(int count, int key)
{
    reset_indent();
    int end = line_end(rl_point);
    int flush_right = rl_point == end;
    rl_point = flush_right ? rl_end : end;
    return 0;
}

static int line_kill_callback(int count, int key)
{
    reset_indent();
    int end = line_end(rl_point);
    int flush_right = rl_point == end;
    int kill = flush_right ? end + prompt_length + 1 : end;
    if (kill > rl_end) kill = rl_end;
    rl_kill_text(rl_point, kill);
    return 0;
}

static int backspace_callback(int count, int key)
{
    reset_indent();
    if (!rl_point) return 0;

    int i = line_start(rl_point), j = rl_point, k;
    if (!i || rl_point <= i + prompt_length) goto backspace;
    for (k = i; k < rl_point; k++)
        if (rl_line_buffer[k] != ' ') goto backspace;

//unindent:
    k = i + prompt_length;
    do { rl_point--; } while ((rl_point - k) % tab_width);
    goto finish;

backspace:
    do {
        rl_point = (i == 0 || rl_point-i > prompt_length) ? rl_point-1 : i-1;
    } while (locale_is_utf8 && !isutf(rl_line_buffer[rl_point]) && rl_point > i-1);

finish:
    rl_delete_text(rl_point, j);
    return 0;
}

static int delete_callback(int count, int key)
{
    reset_indent();
    int j = rl_point;
    do {
        j += (rl_line_buffer[j] == '\n') ? prompt_length+1 : 1;
    } while (locale_is_utf8 && !isutf(rl_line_buffer[j]));
    if (rl_end < j) j = rl_end;
    rl_delete_text(rl_point, j);
    return 0;
}

static int left_callback(int count, int key)
{
    reset_indent();
    if (rl_point > 0) {
        int i = line_start(rl_point);
        do {
            rl_point = (i == 0 || rl_point-i > prompt_length) ? rl_point-1 : i-1;
        } while (locale_is_utf8 && !isutf(rl_line_buffer[rl_point]) && rl_point > i-1);
    }
    return 0;
}

static int right_callback(int count, int key)
{
    reset_indent();
    do {
        rl_point += (rl_line_buffer[rl_point] == '\n') ? prompt_length+1 : 1;
    } while (locale_is_utf8 && !isutf(rl_line_buffer[rl_point]));
    if (rl_end < rl_point) rl_point = rl_end;
    return 0;
}

static int up_callback(int count, int key)
{
    reset_indent();
    int i = line_start(rl_point);
    if (i > 0) {
        int j = line_start(i-1);
        if (j == 0) rl_point -= prompt_length;
        rl_point += j - i;
        if (rl_point >= i) rl_point = i - 1;
    }
    else {
        last_hist_offset = -1;
        rl_get_previous_history(count, key);
        rl_point = line_end(0);
    }
    return 0;
}

static int down_callback(int count, int key)
{
    reset_indent();
    int j = line_end(rl_point);
    if (j < rl_end) {
        int i = line_start(rl_point);
        if (i == 0) rl_point += prompt_length;
        rl_point += j - i + 1;
        int k = line_end(j+1);
        if (rl_point > k) rl_point = k;
        return 0;
    }
    else {
        if (last_hist_offset >= 0) {
            history_set_pos(last_hist_offset);
            last_hist_offset = -1;
        }
        return rl_get_next_history(count, key);
    }
}

static int callback_en=0;

void jl_input_line_callback(char *input)
{
    int end=0, doprint=1;
    if (!input) {
        end = 1;
        rl_ast = NULL;
    }
    else if (!rl_ast) {
        // In vi mode, it's possible for this function to be called w/o a
        // previous call to return_callback.
        rl_ast = repl_parse_input_line(rl_line_buffer);
    }

    if (rl_ast != NULL) {
        doprint = !ends_with_semicolon(input);
        add_history_permanent(input);
        jl_putc('\n', jl_uv_stdout);
        free(input);
    }

    callback_en = 0;
    rl_callback_handler_remove();
    handle_input(rl_ast, end, doprint);
    rl_ast = NULL;
}

static int common_prefix(const char *s1, const char *s2)
{
    int i = 0;
    while (s1[i] && s2[i] && s1[i] == s2[i])
        i++;
    return i;
}

static char *lang_keywords[] =
    { "if", "else", "elseif", "while", "for", "begin", "end", "quote",
      "try", "catch", "return", "local", "abstract", "function", "macro", "ccall",
      "finally", "typealias", "break", "continue", "type", "global",
      "module", "using", "import", "export", "const", "let", "bitstype", "do",
      "baremodule", "importall", "immutable", NULL };

static int is_keyword(char *s)
{
    for(size_t i=0; lang_keywords[i]; i++) {
        if (!strcmp(lang_keywords[i],s))
            return 1;
    }
    return 0;
}

static int name_visible(char *name, const char *prefix)
{
    return !strchr(name,'#');
}

static void symtab_search(jl_sym_t *tree, int *pcount, ios_t *result,
                          jl_module_t *module, const char *str,
                          const char *prefix, int plen)
{
    do {
        if (common_prefix(prefix, tree->name) == plen &&
            name_visible(tree->name, prefix) &&
            (module ? jl_defines_or_exports_p(module, tree) : (jl_boundp(jl_current_module, tree) ||
                                                               is_keyword(tree->name)))) {
            ios_puts(str, result);
            ios_puts(tree->name + plen, result);
            ios_putc('\n', result);
            (*pcount)++;
        }
        if (tree->left)
            symtab_search(tree->left, pcount, result, module, str, prefix, plen);
        tree = tree->right;
    } while (tree != NULL);
}

static jl_module_t *find_submodule_named(jl_module_t *module, const char *name)
{
    jl_sym_t *s = jl_symbol_lookup(name);
    if (!s) return NULL;

    jl_binding_t *b = jl_get_binding(module, s);
    if (!b) return NULL;

    return (jl_is_module(b->value)) ? (jl_module_t *)b->value : NULL;
}

static int symtab_get_matches(jl_sym_t *tree, const char *str, char **answer)
{
    int x, plen, count=0;
    ios_t ans;

    // given str "X.Y.a", set module := X.Y and name := "a"
    jl_module_t *module = NULL;
    char *name = NULL, *strcopy = strdup(str);
    for (char *s=strcopy, *r=NULL;; s=NULL) {
        char *t = strtok_r(s, ".", &r);
        if (!t) {
            if (str[strlen(str)-1] == '.') {
                // this case is "Module."
                if (name) {
                    if (!module) module = jl_current_module;
                    module = find_submodule_named(module, name);
                    if (!module) goto symtab_get_matches_exit;
                }
                name = "";
            }
            break;
        }
        if (name) {
            if (!module) module = jl_current_module;
            module = find_submodule_named(module, name);
            if (!module) goto symtab_get_matches_exit;
        }
        name = t;
    }

    if (!name) goto symtab_get_matches_exit;
    plen = strlen(name);

    while (tree != NULL) {
        x = common_prefix(name, tree->name);
        if (x == plen) {
            ios_mem(&ans, 0);
            symtab_search(tree, &count, &ans, module, str, name, plen);
            size_t nb;
            *answer = ios_takebuf(&ans, &nb);
            break;
        }
        else {
            x = strcmp(name, tree->name);
            if (x < 0)
                tree = tree->left;
            else
                tree = tree->right;
        }
    }

symtab_get_matches_exit:
    free(strcopy);
    return count;
}

int tab_complete(const char *line, char **answer, int *plen)
{
    int len = *plen;

    while (len>=0) {
        if (!jl_word_char(line[len])) {
            break;
        }
        len--;
    }
    len++;
    *plen = len;

    return symtab_get_matches(jl_get_root_symbol(), &line[len], answer);
}

static char *do_completions(const char *ch, int c)
{
    static char *completions = NULL;
    char *ptr;
    int len, cnt;

    if (c == 0) {
        // first time
        if (completions)
            free(completions);
        completions = NULL;
        len = strlen(ch);
        if (len > 0)
            len--;
        cnt = tab_complete(ch, &completions, &len);
        if (cnt == 0)
            return NULL;
        ptr = strtok_r(completions, "\n", &strtok_saveptr);
    }
    else {
        ptr = strtok_r(NULL, "\n", &strtok_saveptr);
    }

    return ptr ? strdup(ptr) : NULL;
}

#ifndef __WIN32__
void sigtstp_handler(int arg)
{
    rl_cleanup_after_signal();

    sigset_t mask;
    sigemptyset(&mask);
    sigaddset(&mask, SIGTSTP);
    sigprocmask(SIG_UNBLOCK, &mask, NULL);
    signal(SIGTSTP, SIG_DFL);
    raise(SIGTSTP);

    signal(SIGTSTP, sigtstp_handler);
}

void sigcont_handler(int arg)
{
    rl_reset_after_signal();
    if (callback_en)
        rl_forced_update_display();
}
#endif

static void init_rl(void)
{
    rl_readline_name = "julia";
    rl_completion_entry_function = do_completions;
#if !defined(_WIN32)
    rl_sort_completion_matches = 0;
#endif
    for(size_t i=0; lang_keywords[i]; i++) {
        // make sure keywords are in symbol table
        (void)jl_symbol(lang_keywords[i]);
    }
    rl_completer_word_break_characters = " \t\n\"\\'`$><=;|&{}()[],+-*/?%^~!:";
    Keymap keymaps[] = {emacs_standard_keymap, vi_insertion_keymap};
    int i;
    for (i = 0; i < sizeof(keymaps)/sizeof(keymaps[0]); i++) {
        rl_bind_key_in_map(' ',        space_callback,      keymaps[i]);
        rl_bind_key_in_map('\t',       tab_callback,        keymaps[i]);
        rl_bind_key_in_map('\r',       return_callback,     keymaps[i]);
        rl_bind_key_in_map('\n',       newline_callback,    keymaps[i]);
        rl_bind_key_in_map('\v',       line_kill_callback,  keymaps[i]);
        rl_bind_key_in_map('\b',       backspace_callback,  keymaps[i]);
        rl_bind_key_in_map('\001',     line_start_callback, keymaps[i]);
        rl_bind_key_in_map('\005',     line_end_callback,   keymaps[i]);
        rl_bind_key_in_map('\002',     left_callback,       keymaps[i]);
        rl_bind_key_in_map('\006',     right_callback,      keymaps[i]);
        rl_bind_keyseq_in_map("\e[1~", line_start_callback, keymaps[i]);
        rl_bind_keyseq_in_map("\e[4~", line_end_callback,   keymaps[i]);
        rl_bind_keyseq_in_map("\e[3~", delete_callback,     keymaps[i]);
        rl_bind_keyseq_in_map("\e[5~", rl_named_function("beginning-of-history"), keymaps[i]);
        rl_bind_keyseq_in_map("\e[6~", rl_named_function("end-of-history"), keymaps[i]);
        rl_bind_keyseq_in_map("\e[A",  up_callback,         keymaps[i]);
        rl_bind_keyseq_in_map("\e[B",  down_callback,       keymaps[i]);
        rl_bind_keyseq_in_map("\e[D",  left_callback,       keymaps[i]);
        rl_bind_keyseq_in_map("\e[C",  right_callback,      keymaps[i]);
        rl_bind_keyseq_in_map("\\C-d", delete_callback,     keymaps[i]);
        rl_bind_keyseq_in_map("\e\r",  newline_callback,    keymaps[i]);
    }
#ifndef __WIN32__
    signal(SIGTSTP, sigtstp_handler);
    signal(SIGCONT, sigcont_handler);
#endif
}

void jl_prep_terminal(int meta_flag)
{
    FILE *rl_in = rl_instream;
    rl_instream = stdin;
    rl_prep_terminal(1);
    rl_instream = rl_in;
#ifdef __WIN32__
    if (jl_uv_stdin->type == UV_TTY) uv_tty_set_mode((uv_tty_t*)jl_uv_stdin,1); //raw (and libuv-processed)
#endif
}
/* Restore the terminal's normal settings and modes. */
void jl_deprep_terminal ()
{
    FILE *rl_in = rl_instream;
    rl_instream = stdin;
    rl_deprep_terminal();
    rl_instream = rl_in;
#ifdef __WIN32__
    if (jl_uv_stdin->type == UV_TTY) uv_tty_set_mode((uv_tty_t*)jl_uv_stdin,0); // cooked
#endif
}

void jl_init_repl(int history)
{
    disable_history = !history;

#ifdef __WIN32__
    rl_outstream=(void*)jl_uv_stdout;
#endif
    rl_catch_signals = 0;
    rl_prep_term_function = &jl_prep_terminal;
    rl_deprep_term_function = &jl_deprep_terminal;
    rl_instream = fopen("/dev/null","r");
    prompt_length = 7;  // == strlen("julia> ")
    init_history();
    rl_startup_hook = (Function*)init_rl;
}

static char *prompt_string=NULL;

void repl_callback_enable(char *prompt)
{
    callback_en = 1;
    if (prompt_string == NULL || strcmp(prompt, prompt_string)) {
        if (prompt_string) free(prompt_string);
        prompt_string = strdup(prompt);
    }
    rl_callback_handler_install(prompt_string, jl_input_line_callback);
}

#include "uv.h"

void jl_read_buffer(unsigned char *base, ssize_t nread)
{
    unsigned char *start = base;
    while(*start != 0 && nread > 0) {
        rl_stuff_char(*start);
        start++;
        nread--;
    }
    rl_callback_read_char();
}

DLLEXPORT void jl_reset_input(void)
{
    //move the cursor to a clean line:
    char *p = rl_line_buffer;
    int i;
    for (i = 0; *p != '\0'; p++, i++) {
        if (i >= rl_point && *p == '\n') {
            jl_putc('\n', jl_uv_stdout);
        }
    }
    rl_ast = NULL;
    //reset state:
    rl_reset_line_state();
    reset_indent();
    callback_en = 0;
    rl_callback_handler_remove();
}
