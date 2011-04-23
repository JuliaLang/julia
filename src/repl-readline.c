#include "repl.h"

char jl_prompt_color[] = "\001\033[1m\033[32m\002julia> \001\033[37m\002";
static char jl_input_color[]  = "\033[1m\033[37m";

static char *history_file = NULL;

#if defined(USE_EDITLINE)
static int rl_done;
#endif

// yes, readline uses inconsistent indexing internally.
#define history_rem(n) remove_history(n-history_base)

static void init_history() {
    using_history();
    char *home = getenv("HOME");
    if (!home) return;
    asprintf(&history_file, "%s/.julia_history", home);
    struct stat stat_info;
    if (!stat(history_file, &stat_info)) {
        read_history(history_file);
#if defined(USE_READLINE)
        for (;;) {
            HIST_ENTRY *entry = history_get(history_base);
            if (entry && isspace(entry->line[0]))
                free_history_entry(history_rem(history_base));
            else break;
        }
#endif
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
#if defined(USE_READLINE)
                free_history_entry(history_rem(i+1));
#endif
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
#if defined(USE_READLINE)
    if (history_file)
        append_history(1, history_file);
#endif
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

static int newline_callback(int count, int key) {
    rl_insert_text("\n");
    int i;
    for (i = 0; i < prompt_length; i++)
        rl_insert_text(" ");
    return 0;
}

static int return_callback(int count, int key) {
    if (have_color) {
        ios_printf(ios_stdout, jl_answer_color);
        ios_flush(ios_stdout);
    }
    add_history_temporary(rl_line_buffer);
    rl_ast = jl_parse_input_line(rl_line_buffer);
    rl_done = !rl_ast || !jl_is_expr(rl_ast) ||
        (((jl_expr_t*)rl_ast)->head != continue_sym);
    if (!rl_done && have_color) {
        ios_printf(ios_stdout, jl_input_color);
        ios_flush(ios_stdout);
    }
    if (!rl_done) {
        newline_callback(count, key);
    } else {
        rl_point = rl_end;
        rl_redisplay();
    }
    return 0;
}

static int space_callback(int count, int key) {
    if (rl_point > 0)
        rl_insert_text(" ");
    return 0;
}

static int tab_callback(int count, int key) {
    if (rl_point > 0) {
        int i;
        for (i=0; i < tab_width; i++)
            rl_insert_text(" ");
    }
    return 0;
}

static int line_start_callback(int count, int key) {
    int start = line_start(rl_point);
    int flush_left = rl_point == 0 || rl_point == start + prompt_length;
    rl_point = flush_left ? 0 : (!start ? start : start + prompt_length);
    return 0;
}

static int line_end_callback(int count, int key) {
    int end = line_end(rl_point);
    int flush_right = rl_point == end;
    rl_point = flush_right ? rl_end : end;
    return 0;
}

static int line_kill_callback(int count, int key) {
    int end = line_end(rl_point);
    int flush_right = rl_point == end;
    int kill = flush_right ? end + prompt_length + 1 : end;
    if (kill > rl_end) kill = rl_end;
    rl_kill_text(rl_point, kill);
    return 0;
}

static int backspace_callback(int count, int key) {
    if (rl_point > 0) {
        int j = rl_point;
        int i = line_start(rl_point);
        rl_point = (i == 0 || rl_point-i > prompt_length) ?
            rl_point-1 : i-1;
#if defined(USE_READLINE)
        rl_delete_text(rl_point, j);
#endif
    }
    return 0;
}

static int delete_callback(int count, int key) {
    int j = rl_point;
    j += (rl_line_buffer[j] == '\n') ? prompt_length+1 : 1;
    if (rl_end < j) j = rl_end;
    rl_delete_text(rl_point, j);
    return 0;
}

static int left_callback(int count, int key) {
    if (rl_point > 0) {
        int i = line_start(rl_point);
        rl_point = (i == 0 || rl_point-i > prompt_length) ?
            rl_point-1 : i-1;
    }
    return 0;
}

static int right_callback(int count, int key) {
    rl_point += (rl_line_buffer[rl_point] == '\n') ? prompt_length+1 : 1;
    if (rl_end < rl_point) rl_point = rl_end;
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

void init_repl_environment()
{
  no_readline = 0;

  if (!no_readline) {
    init_history();
    rl_bind_key(' ', space_callback);
    rl_bind_key('\t', tab_callback);
    rl_bind_key('\r', return_callback);
    rl_bind_key('\n', return_callback);
    rl_bind_key('\v', line_kill_callback);
    rl_bind_key('\b', backspace_callback);
    rl_bind_key('\001', line_start_callback);
    rl_bind_key('\005', line_end_callback);
    rl_bind_key('\002', left_callback);
    rl_bind_key('\006', right_callback);
#if defined(USE_READLINE)
    rl_bind_keyseq("\e[A", up_callback);
    rl_bind_keyseq("\e[B", down_callback);
    rl_bind_keyseq("\e[D", left_callback);
    rl_bind_keyseq("\e[C", right_callback);
    rl_bind_keyseq("\\C-d", delete_callback);
#endif
    }
}

void exit_repl_environment()
{
    if (!no_readline) {
        rl_callback_handler_remove();
    }
}
