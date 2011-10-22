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

#include <readline/readline.h>
#include <readline/history.h>

char jl_prompt_color[] = "\001\033[1m\033[32m\002julia> \001\033[0m\033[1m\002";
static char *history_file = NULL;

// yes, readline uses inconsistent indexing internally.
#define history_rem(n) remove_history(n-history_base)

static void init_history(void) {
    using_history();
    char *home = getenv("HOME");
    if (!home) return;
    asprintf(&history_file, "%s/.julia_history", home);
    struct stat stat_info;
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
                p = stpcpy(p+1, history_get(i+1)->line);
                free_history_entry(history_rem(i+1));
            }
        }
    } else if (errno == ENOENT) {
        write_history(history_file);
    } else {
        ios_printf(ios_stderr, "history file error: %s\n", strerror(errno));
        exit(1);
    }
}

static int last_hist_is_temp = 0;
static int last_hist_offset = -1;

static void add_history_temporary(char *input) {
    if (!input || !*input) return;
    if (last_hist_is_temp) {
        history_rem(history_length);
        last_hist_is_temp = 0;
    }
    last_hist_offset = -1;
    add_history(input);
    last_hist_is_temp = 1;
}

static void add_history_permanent(char *input) {
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

static int line_start(int point) {
    if (!point) return 0;
    int i = point-1;
    for (; i && rl_line_buffer[i] != '\n'; i--) ;
    return i ? i+1 : 0;
}

static int line_end(int point) {
    char *nl = strchr(rl_line_buffer + point, '\n');
    if (!nl) return rl_end;
    return nl - rl_line_buffer;
}

static int strip_initial_spaces = 0;
static int spaces_suppressed = 0;

static void reset_indent(void) {
    strip_initial_spaces = 0;
    spaces_suppressed = 0;
}

static int newline_callback(int count, int key) {
    spaces_suppressed = 0;
    rl_insert_text("\n");
    int i;
    for (i = 0; i < prompt_length; i++)
        rl_insert_text(" ");
    return 0;
}

static int return_callback(int count, int key) {
    add_history_temporary(rl_line_buffer);
    rl_ast = jl_parse_input_line(rl_line_buffer);
    rl_done = !rl_ast || !jl_is_expr(rl_ast) ||
        (((jl_expr_t*)rl_ast)->head != jl_continue_sym);
    if (!rl_done) {
        newline_callback(count, key);
    } else {
        reset_indent();
        rl_point = rl_end;
        rl_redisplay();
    }
    return 0;
}

static int suppress_space(void) {
    int i;
    for (i = line_start(rl_point); i < rl_point; i++)
        if (rl_line_buffer[i] != ' ') return 0;
    if (spaces_suppressed < strip_initial_spaces) return 1;
    return 0;
}

static int space_callback(int count, int key) {
    if (!rl_point) strip_initial_spaces++;
    else if (suppress_space()) spaces_suppressed++;
    else rl_insert_text(" ");
    return 0;
}

static int tab_callback(int count, int key) {
    if (!rl_point) strip_initial_spaces += tab_width;
    else if (suppress_space()) spaces_suppressed += tab_width;
    else {
        int i;
        for (i=0; i < tab_width; i++) rl_insert_text(" ");
    }
    return 0;
}

static int line_start_callback(int count, int key) {
    int start = line_start(rl_point);
    int flush_left = rl_point == 0 || rl_point == start + prompt_length;
    rl_point = flush_left ? 0 : (!start ? start : start + prompt_length);
    reset_indent();
    return 0;
}

static int line_end_callback(int count, int key) {
    int end = line_end(rl_point);
    int flush_right = rl_point == end;
    rl_point = flush_right ? rl_end : end;
    reset_indent();
    return 0;
}

static int line_kill_callback(int count, int key) {
    int end = line_end(rl_point);
    int flush_right = rl_point == end;
    int kill = flush_right ? end + prompt_length + 1 : end;
    if (kill > rl_end) kill = rl_end;
    rl_kill_text(rl_point, kill);
    reset_indent();
    return 0;
}

static int backspace_callback(int count, int key) {
    if (rl_point > 0) {
        int j = rl_point;
        int i = line_start(rl_point);
        rl_point = (i == 0 || rl_point-i > prompt_length) ?
            rl_point-1 : i-1;
        rl_delete_text(rl_point, j);
    }
    reset_indent();
    return 0;
}

static int delete_callback(int count, int key) {
    int j = rl_point;
    j += (rl_line_buffer[j] == '\n') ? prompt_length+1 : 1;
    if (rl_end < j) j = rl_end;
    rl_delete_text(rl_point, j);
    reset_indent();
    return 0;
}

static int left_callback(int count, int key) {
    if (rl_point > 0) {
        int i = line_start(rl_point);
        rl_point = (i == 0 || rl_point-i > prompt_length) ?
            rl_point-1 : i-1;
    }
    reset_indent();
    return 0;
}

static int right_callback(int count, int key) {
    rl_point += (rl_line_buffer[rl_point] == '\n') ? prompt_length+1 : 1;
    if (rl_end < rl_point) rl_point = rl_end;
    reset_indent();
    return 0;
}

static int up_callback(int count, int key) {
    int i = line_start(rl_point);
    if (i > 0) {
        int j = line_start(i-1);
        if (j == 0) rl_point -= prompt_length;
        rl_point += j - i;
        if (rl_point >= i) rl_point = i - 1;
    } else {
        last_hist_offset = -1;
        rl_get_previous_history(count, key);
        rl_point = line_end(0);
        return 0;
    }
    reset_indent();
    return 0;
}

static int down_callback(int count, int key) {
    int j = line_end(rl_point);
    if (j < rl_end) {
        int i = line_start(rl_point);
        if (i == 0) rl_point += prompt_length;
        rl_point += j - i + 1;
        int k = line_end(j+1);
        if (rl_point > k) rl_point = k;
    } else {
        if (last_hist_offset >= 0) {
            history_set_pos(last_hist_offset);
            last_hist_offset = -1;
        }
        return rl_get_next_history(count, key);
    }
    reset_indent();
    return 0;
}

DLLEXPORT void jl_input_line_callback(char *input)
{
    int end=0, doprint=1;
    if (!input || ios_eof(ios_stdin)) {
        end = 1;
        rl_ast = NULL;
    }

    if (rl_ast != NULL) {
        doprint = !ends_with_semicolon(input);
        add_history_permanent(input);
        ios_printf(ios_stdout, "\n");
        free(input);
    }

    handle_input(rl_ast, end, doprint);
}

void read_expr(char *prompt)
{
    rl_ast = NULL;
    char *input = readline(prompt);
    jl_input_line_callback(input);
}

void init_repl_environment(void)
{
    init_history();
    Keymap keymaps[] = {emacs_standard_keymap, vi_insertion_keymap};
    int i;
    for (i = 0; i < sizeof(keymaps)/sizeof(keymaps[0]); i++) {
        rl_bind_key_in_map(' ',        space_callback,      keymaps[i]);
        rl_bind_key_in_map('\t',       tab_callback,        keymaps[i]);
        rl_bind_key_in_map('\r',       return_callback,     keymaps[i]);
        rl_bind_key_in_map('\n',       return_callback,     keymaps[i]);
        rl_bind_key_in_map('\v',       line_kill_callback,  keymaps[i]);
        rl_bind_key_in_map('\b',       backspace_callback,  keymaps[i]);
        rl_bind_key_in_map('\001',     line_start_callback, keymaps[i]);
        rl_bind_key_in_map('\005',     line_end_callback,   keymaps[i]);
        rl_bind_key_in_map('\002',     left_callback,       keymaps[i]);
        rl_bind_key_in_map('\006',     right_callback,      keymaps[i]);
        rl_bind_keyseq_in_map("\e[A",  up_callback,         keymaps[i]);
        rl_bind_keyseq_in_map("\e[B",  down_callback,       keymaps[i]);
        rl_bind_keyseq_in_map("\e[D",  left_callback,       keymaps[i]);
        rl_bind_keyseq_in_map("\e[C",  right_callback,      keymaps[i]);
        rl_bind_keyseq_in_map("\\C-d", delete_callback,     keymaps[i]);
    };
}

void exit_repl_environment(void)
{
    rl_callback_handler_remove();
}

DLLEXPORT void repl_callback_enable(void)
{
    if (jl_have_event_loop)
        rl_callback_handler_install(prompt_string, jl_input_line_callback);
}

void repl_callback_disable(void)
{
    rl_callback_handler_remove();
}

void repl_stdin_callback(void)
{
    rl_callback_read_char();
}

void repl_print_prompt(void)
{
    // handled by readline
}
