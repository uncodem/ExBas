const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize
    });

    const exe = b.addExecutable(.{
        .name = "vals",
        .root_module = exe_mod
    });

    // exe.linkLibC();
    b.installArtifact(exe);

    const run_exe = b.addRunArtifact(exe);
    const run_step = b.step("run", "Run the application");
    run_step.dependOn(&run_exe.step);

    const exe_tests = b.addTest(.{
        .root_source_file = b.path("src/tests.zig"),
        .optimize = optimize,
        .test_runner = .{ .path = b.path("test_runner.zig"), .mode = .simple },
        .target = target,
    });

    const run_tests = b.addRunArtifact(exe_tests);

    const test_step = b.step("test", "Run the tests");
    test_step.dependOn(&run_tests.step);
}

