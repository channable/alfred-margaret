# NOTE: You can build a standalone jar with `bazel build :acbench_deploy.jar`.

java_binary(
    name = "acbench",
    srcs = ["main.java"],
    deps = ["@hankcs_aho_corasick//jar"],
    main_class = "AcBench",
)
