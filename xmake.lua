
---@diagnostic disable

task("rtl", function()
  set_menu {
    usage = "xmake rtl [options]",
    description = "Generate RTL",
    options = {
      {'b', "build-dir", "kv", "build", "build directory"},
      {'M', "main-function", "kv", "PriorityEncoderHigh", "main class"},
      {'t', "target", "kv", "systemverilog", "output targer format"},
    }
  }
  
  on_run(function()
    import("core.base.option")
    local main = option.get("main-function")
    local build_dir = option.get("build-dir")
    local rtl_dir = path.join(build_dir, "rtl")
    local chisel_opts = {"-i", "test.runMain", "xs.utils.test." .. main .. "Top"}
    table.join2(chisel_opts, {"--throw-on-first-error", "--target", option.get("target"), "--split-verilog", "--full-stacktrace", "-td", rtl_dir})

    if os.exists(rtl_dir) then os.rmdir(rtl_dir) end

    if os.host() == "windows" then
      os.execv(os.shell(), table.join({"mill"}, chisel_opts))
    else
      os.execv("mill", chisel_opts)
    end

    os.rm(path.join(rtl_dir, "firrtl_black_box_resource_files.f"))
    os.rm(path.join(rtl_dir, "filelist.f"))
    os.rm(path.join(rtl_dir, "extern_modules.sv"))
  end)
end)

task("init", function()
  on_run(function()
    os.cd(os.scriptdir())
    os.exec("git submodule update --init")
  end)
  set_menu {}
end)

task("idea", function()
  on_run(function()
    if os.host() == "windows" then
      os.execv(os.shell(), { "mill", "-i", "mill.idea.GenIdea/idea" })
    else
      os.execv("mill", { "-i", "mill.idea.GenIdea/idea" })
    end
  end)
  set_menu {
    options = {}
  }
end)

task("comp", function()
  on_run(function()
    if os.host() == "windows" then
      os.execv(os.shell(), {"mill", "compile"})
      os.execv(os.shell(), {"mill", "test.compile"})
    else
      os.execv("mill", {"compile"})
      os.execv("mill", {"test.compile"})
    end
  end)
  set_menu {}
end)

task("clean", function()
  on_run(function()
    os.rmdir(path.join("build", "*"))
  end)
  set_menu {}
end)