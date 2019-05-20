# Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
# SPDX-License-Identifier: Apache-2.0

def _dar_to_scala_impl(ctx):
    codegen_out_dir = ctx.outputs.codegen_out
    srcjar_out_file = ctx.outputs.srcjar_out

    # Call Scala codegen
    gen_args = ctx.actions.args()
    gen_args.add("--output-directory=%s" % codegen_out_dir.path)
    gen_args.add("--verbosity=%s" % ctx.attr.verbosity)
    for dar in ctx.files.srcs:
        gen_args.add("./%s=%s" % (dar.path, ctx.attr.package_prefix))
    ctx.actions.run(
        mnemonic = "ScalaCodegen",
        inputs = ctx.files.srcs,
        outputs = [codegen_out_dir],
        arguments = [gen_args],
        progress_message = "scala codegen files: %s" % ctx.attr.name,
        executable = ctx.executable._codegen,
        use_default_shell_env = True,
    )

    # Create zipper_args file
    zipper_args_file = ctx.actions.declare_file(ctx.label.name + ".zipper_args")
    ctx.actions.run_shell(
        mnemonic = "CreateZipperArgsFile",
        outputs = [zipper_args_file],
        inputs = [codegen_out_dir],
        command = "find -L {src_path} -type f | sed -E 's#^{src_path}/(.*)$#\\1={src_path}/\\1#' | sort > {args_file}".format(
            src_path = codegen_out_dir.path,
            args_file = zipper_args_file.path,
        ),
        progress_message = "zipper_args_file: %s" % zipper_args_file.path,
        use_default_shell_env = True,
    )

    # Call zipper to create srcjar
    zipper_args = ctx.actions.args()
    zipper_args.add("c")
    zipper_args.add(srcjar_out_file.path)
    zipper_args.add("@%s" % zipper_args_file.path)
    ctx.actions.run(
        mnemonic = "CreateSrcJar",
        executable = ctx.executable._zipper,
        inputs = [codegen_out_dir, zipper_args_file],
        outputs = [srcjar_out_file],
        arguments = [zipper_args],
        progress_message = "srcjar: %s" % srcjar_out_file.path,
    )

dar_to_scala = rule(
    implementation = _dar_to_scala_impl,
    attrs = {
        "srcs": attr.label_list(
            mandatory = True,
            allow_files = True,
            doc = "DAR files.",
        ),
        "srcjar_out": attr.string(
            mandatory = True,
            doc = "The name of the output srcjar",
        ),
        "package_prefix": attr.string(
            mandatory = True,
            doc = "Package name e.g. 'com.digitalasset.mypackage'.",
        ),
        "verbosity": attr.int(
            default = 2,
        ),
        "_codegen": attr.label(
            default = Label("//language-support/scala/codegen:codegen-main"),
            cfg = "host",
            executable = True,
            allow_files = True,
        ),
        "_zipper": attr.label(
            default = Label("@bazel_tools//tools/zip:zipper"),
            cfg = "host",
            executable = True,
            allow_files = True,
        ),
    },
    outputs = {
        "codegen_out": "%{name}-src",
        "srcjar_out": "%{srcjar_out}",  # I want it to be explicit, other rules will depend on it
    },
    output_to_genfiles = True,
)