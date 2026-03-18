/*
 * lean_signal.c — Signal handler for graceful shutdown
 *
 * Installs SIGINT/SIGTERM handlers that set a global flag.
 * Polled from Lean via lean_signal_is_set.
 */

#include <signal.h>
#include <lean/lean.h>

static volatile sig_atomic_t shutdown_flag = 0;

static void lean_signal_handler(int sig) {
    (void)sig;
    shutdown_flag = 1;
}

LEAN_EXPORT lean_obj_res lean_signal_install(lean_obj_arg w) {
    signal(SIGINT, lean_signal_handler);
    signal(SIGTERM, lean_signal_handler);
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res lean_signal_is_set(lean_obj_arg w) {
    return lean_io_result_mk_ok(lean_box(shutdown_flag ? 1 : 0));
}
