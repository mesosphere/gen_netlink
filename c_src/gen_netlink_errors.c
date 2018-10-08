#include <erl_nif.h>
#include <erl_driver.h>

static ERL_NIF_TERM erl_error_code(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 1) {
        return enif_make_badarg(env);
    }

    int err;
    if ( !enif_get_int(env, argv[0], &err) ) {
        return enif_make_badarg(env);
    }
    return enif_make_atom(env, erl_errno_id(err));
}

static ErlNifFunc nif_funcs[] =
{
    {"erl_error_code", 1, erl_error_code},
};
ERL_NIF_INIT(gen_netlink_errors, nif_funcs, NULL, NULL, NULL, NULL)
