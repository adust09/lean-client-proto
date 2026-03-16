/*
 * lean_sig.c — leanSig Lean4 FFI wrapper (Phase 2 stub)
 *
 * This file will contain the Lean4 FFI wrappers that convert
 * between lean_object* and the raw C types expected by lean_sig.h.
 *
 * Pattern: each @[extern "lean_sig_xxx_wrapper"] in Lean4 maps to
 * a function here that:
 *   1. Extracts raw data from lean_object* arguments
 *   2. Calls the underlying lean_sig_xxx C function
 *   3. Wraps the result back into lean_object* (Except/IO)
 */

#include <lean/lean.h>
#include "lean_sig.h"

/* Phase 2: implement wrappers here */
