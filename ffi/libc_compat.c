/*
 * libc_compat.c — Compatibility stubs for glibc 2.42+
 *
 * glibc 2.42 removed __libc_csu_init and __libc_csu_fini, but
 * Lean4's bundled CRT files (from an older glibc) still reference them.
 * Providing empty stubs resolves the link error.
 */

void __libc_csu_init(void) {}
void __libc_csu_fini(void) {}
