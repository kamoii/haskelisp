#include <HsFFI.h>
#include <string.h>
#include <emacs-module.h>
/* extern void __stginit_Lib(void); */

/* Declare mandatory GPL symbol.  */
/* Shoudn't declare here. Library user should declare. */
/* int plugin_is_GPL_compatible; */

void __attribute__ ((constructor)) my_load(void);
void __attribute__ ((destructor)) my_unload(void);

// https://www.vex.net/~trebla/haskell/so.xhtml 参照
void my_load(void) {
  static int argc = 1;
  static char *argv[] = { "haskelisp", 0}, **argv_ = argv;
  hs_init(&argc, &argv_);
}
void my_unload(void) {
  hs_exit();
}

/* wrappers */

emacs_env *_get_emacs_env_from_rt(struct emacs_runtime *ert) {
  return ert->get_environment(ert);
}

/* Function registration.  */

emacs_value _make_function(emacs_env *env,
                           ptrdiff_t min_arity,
                           ptrdiff_t max_arity,
                           emacs_value (*function) (emacs_env *env,
                                                    ptrdiff_t nargs,
                                                    emacs_value args[],
                                                    void *),
                           const char *documentation,
                           void *data) {
  return env->make_function(env,
                            min_arity,
                            max_arity,
                            function,
                            documentation,
                            data);
}

emacs_value _funcall(emacs_env *env,
                     emacs_value function,
                     ptrdiff_t nargs,
                     emacs_value args[]) {
  return env->funcall(env,
                      function,
                      nargs,
                      args);
}

emacs_value _intern(emacs_env *env, const char *symbol_name) {
  return env->intern(env, symbol_name);
}

/* Type conversion.  */

emacs_value _type_of(emacs_env *env, emacs_value value) {
  return env->type_of(env,value);
}

int _is_not_nil(emacs_env *env, emacs_value value) {
  return env->is_not_nil(env,value) == true ? 1 : 0;
}

int _eq(emacs_env *env, emacs_value a, emacs_value b) {
  return env->eq(env, a, b) == true ? 1 : 0;
}

intmax_t _extract_integer(emacs_env *env, emacs_value value) {
  return env->extract_integer(env, value);
}

emacs_value _make_integer(emacs_env *env, intmax_t value) {
  return env->make_integer(env, value);
}

/* double (*extract_float) (emacs_env *env, emacs_value value); */

/* emacs_value (*make_float) (emacs_env *env, double value); */

int _copy_string_contents(emacs_env *env,
                          emacs_value value,
                          char *buffer,
                          ptrdiff_t *size_inout) {
  return env->copy_string_contents(env,value,buffer,size_inout) == true ? 1 : 0;
}


emacs_value _make_string(emacs_env *env,
                         const char *contents,
                         ptrdiff_t length) {
  return env->make_string(env,contents,length);
}

/* Memory management.  */

emacs_value _make_global_ref(emacs_env *env,
                             emacs_value any_reference) {
  return env->make_global_ref(env, any_reference);
}

/* Non-local exit handling.  */

enum emacs_funcall_exit _non_local_exit_check(emacs_env *env) {
  return env->non_local_exit_check(env);
}

void _non_local_exit_clear(emacs_env *env) {
  env->non_local_exit_clear(env);
}

enum emacs_funcall_exit _non_local_exit_get(emacs_env *env,
                                            emacs_value *non_local_exit_symbol_out,
                                            emacs_value *non_local_exit_data_out) {
  return env->non_local_exit_get(env,
                                 non_local_exit_symbol_out,
                                 non_local_exit_data_out);
}

void _non_local_exit_signal(emacs_env *env,
                            emacs_value non_local_exit_symbol,
                            emacs_value non_local_exit_data) {
  env->non_local_exit_signal(env, non_local_exit_symbol, non_local_exit_data);
}

void _non_local_exit_throw(emacs_env *env,
                           emacs_value tag,
                           emacs_value value) {
  env->non_local_exit_throw(env,tag,value);
}
