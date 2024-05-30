mod infra;

// Your tests go here!
success_tests! {
    {
        name: fact,
        file: "fact.snek",
        input: "10",
        expected: "(vec 3628800 3628800)",
    },
    {
        name: lam0,
        file: "lam0.snek",
        input: "0",
        expected: "6",
    },
    {
        name: lam1,
        file: "lam1.snek",
        input: "0",
        expected: "50",
    },
    {
        name: lam2,
        file: "lam2.snek",
        input: "0",
        expected: "(vec 6 50)",
    },
    {
        name: lam_fac,
        file: "lam-fac.snek",
        input: "5",
        expected: "120",
    },
    {
        name: list_map,
        file: "list-map.snek",
        input: "100",
        expected: "(vec 110 (vec 120 (vec 130 false)))",
    },
    {
        name: ty0,
        file: "ty0.snek",
        input: "100",
        expected: "101",
    },
    {
        name: ty1,
        file: "ty1.snek",
        input: "100",
        expected: "11",
    },
    {
        name: ty2,
        file: "ty2.snek",
        input: "100",
        expected: "true",
    },
    {
        name: ty3,
        file: "ty3.snek",
        input: "100",
        expected: "12",
    },
    {
        name: list0,
        file: "list0.snek",
        input: "100",
        expected: "30",
    },
    {
        name: list1,
        file: "list1.snek",
        input: "100",
        expected: "60",
    },
    {
        name: list_fold,
        file: "list-fold.snek",
        input: "100",
        expected: "60",
    },
    {
        name: check0,
        file: "check0.snek",
        input: "100",
        expected: "false",
    },
    {
        name: check1,
        file: "check1.snek",
        input: "100",
        expected: "(vec (vec true false) (vec 1 false))",
    },
}

static_error_tests! {
    {
        name: lam_not_fun,
        file: "lam_not_fun.snek",
        expected: "Type Error",
    },
    {
        name: lam_arity,
        file: "lam_arity.snek",
        expected: "Type Error",
    },
    {
        name: ty0_err,
        file: "ty0-err.snek",
        expected: "Type Error",
    },
    {
        name: ty1_err,
        file: "ty1-err.snek",
        expected: "Type Error",
    },
    {
        name: ty2_err,
        file: "ty2-err.snek",
        expected: "Type Error",
    },
    {
        name: ty3_err,
        file: "ty3-err.snek",
        expected: "Type Error",
    },
    {
        name: list0_err,
        file: "list0-err.snek",
        expected: "Type Error",
    },
    {
        name: list1_err,
        file: "list1-err.snek",
        expected: "Type Error",
    },
    {
        name: list2_err,
        file: "list2-err.snek",
        expected: "Type Error",
    },
    {
        name: list_map_err,
        file: "list-map-err.snek",
        expected: "Type Error",
    },
    {
        name: list_fold_err,
        file: "list-fold-err.snek",
        expected: "Type Error",
    },
    {
        name: check0_err,
        file: "check0-err.snek",
        expected: "Type Error",
    },
    {
        name: check1_err,
        file: "check1-err.snek",
        expected: "Type Error",
    },
}
