
---@diagnostic disable

task("rtl", function()
  set_menu {
    usage = "xmake rtl [options]",
    description = "Generate Chisel RTL from a test top",
    options = {
      {'b', "build-dir", "kv", "build", "build directory"},
      {'M', "main-function", "kv", nil, "main class (required; use svmodule for SV templates)"},
      {'t', "target", "kv", "systemverilog", "output targer format"},
    }
  }
  
  on_run(function()
    import("core.base.option")
    local main = option.get("main-function")
    assert(main, "rtl requires -M <name>; use xmake svmodule to export SV templates")
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

task("svmodule", function()
  set_menu {
    usage = "xmake svmodule [options]",
    description = "Export all registered SV templates with default parameters",
    options = {
      {'l', "lint", "k", nil, "Lint every exported module as a top module"},
      {'L', "linter", "kv", "verilator", "SV linter (verilator)"}
    }
  }
  on_run(function()
    import("core.base.option")
    os.cd(os.scriptdir())
    local linter = option.get("linter")
    assert(linter == "verilator", "--linter must be verilator")
    local output = path.join("build", "svmodule")
    local args = {"--no-server", "runMain", "xs.utils.verilog.SvModuleMain", output}
    if os.host() == "windows" then
      local quoted = {}
      for _, arg in ipairs(args) do table.insert(quoted, "'" .. arg:gsub("'", "''") .. "'") end
      os.execv("powershell", {"-NoProfile", "-Command", "$ErrorActionPreference = 'Stop'; & mill " .. table.concat(quoted, " ") .. "; exit $LASTEXITCODE"})
    else
      os.execv("mill", args)
    end
    if option.get("lint") then
      -- Each module must be checked as a top with its own defaults, even when
      -- it is also instantiated with different parameters by another module.
      os.cd(output)
      local count = 0
      for file in io.lines("filelist.f") do
        assert(file:match("^[%w_$]+%.sv$"), "invalid generated filename: " .. file)
        local top = file:sub(1, -4)
        print("Linting " .. top)
        os.execv(linter, {"--lint-only", "--top-module", top, "-f", "filelist.f"})
        count = count + 1
      end
      assert(count > 0, "no generated SV modules to lint")
      print(string.format("Lint passed for %d SV modules", count))
    end
  end)
end)

task("test", function()
  set_menu {
    usage = "xmake test [options] [-- scalatest-args ...]",
    description = "Discover and run ScalaTest suites (default backend: Verilator)",
    options = {
      {'l', "list", "k", nil, "List discovered test suites without simulation"},
      {'s', "suite", "kv", nil, "Suite name or glob; comma-separated (e.g. FastQueueSpec)"},
      {'c', "case", "kv", nil, "Run test names containing this text (ScalaTest -z)"},
      {nil, "exact", "kv", nil, "Run an exact full test name (ScalaTest -t)"},
      {nil, "seed", "kv", nil, "Random seed: signed decimal or 0x hexadecimal"},
      {nil, "cycles", "kv", nil, "Random cycles per configuration (positive integer)"},
      {nil, "waves", "k", nil, "Generate VCD waveforms"},
      {nil, "simulator", "kv", "verilator", "Simulation backend: verilator or vcs"},
      {nil, "chisel-opts", "kv", nil, "Additional Chisel elaboration options"},
      {nil, "firtool-opts", "kv", nil, "Additional firtool options"},
      {nil, "test-args", "vs", nil, "Extra ScalaTest arguments after -- (e.g. -Dcustom=value)"}
    }
  }
  on_run(function()
    import("core.base.option")
    os.cd(os.scriptdir())
    assert(not (option.get("case") and option.get("exact")), "--case and --exact are mutually exclusive")
    local simulator = option.get("simulator")
    assert(simulator == "verilator" or simulator == "vcs", "--simulator must be verilator or vcs")
    local cycles = option.get("cycles")
    if cycles then
      assert(cycles:match("^%d+$") and tonumber(cycles) > 0 and tonumber(cycles) <= 2147483647,
        "--cycles must be an integer in [1, 2147483647]")
    end
    local seed = option.get("seed")
    if seed then
      assert(seed:match("^%-?%d+$") or seed:match("^0[xX]%x+$"), "--seed must be signed decimal or 0x hexadecimal")
    end
    local args = {"--no-server"}
    if option.get("list") then
      table.join2(args, {"show", "test.discoveredTestClasses"})
    else
      local suite = option.get("suite")
      if suite then
        table.insert(args, "test.testOnly")
        local count = 0
        for name in suite:gmatch("[^,]+") do
          name = name:match("^%s*(.-)%s*$")
          assert(#name > 0, "--suite must contain a suite name or glob")
          if not name:find(".", 1, true) then name = "xs.utils.test.chiselsim." .. name end
          table.insert(args, name)
          count = count + 1
        end
        assert(count > 0, "--suite must contain a suite name or glob")
        table.insert(args, "--") -- testOnly separates suite selectors from ScalaTest options
      else
        table.insert(args, "test")
      end
      table.insert(args, "-Dsimulator=" .. simulator)
      if option.get("case") then table.join2(args, {"-z", option.get("case")}) end
      if option.get("exact") then table.join2(args, {"-t", option.get("exact")}) end
      if seed then table.insert(args, "-Dseed=" .. seed) end
      if cycles then table.insert(args, "-Dcycles=" .. cycles) end
      if option.get("waves") then table.insert(args, "-DemitVcd=1") end
      if option.get("chisel-opts") then table.insert(args, "-DchiselOpts=" .. option.get("chisel-opts")) end
      if option.get("firtool-opts") then table.insert(args, "-DfirtoolOpts=" .. option.get("firtool-opts")) end
      table.join2(args, option.get("test-args") or {})
    end
    if os.host() == "windows" then
      -- Quote each PowerShell argument independently, including spaces/apostrophes.
      local quoted = {}
      for _, arg in ipairs(args) do table.insert(quoted, "'" .. arg:gsub("'", "''") .. "'") end
      os.execv("powershell", {"-NoProfile", "-Command", "$ErrorActionPreference = 'Stop'; & mill " .. table.concat(quoted, " ") .. "; exit $LASTEXITCODE"})
    else
      os.execv("mill", args)
    end
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
      os.execv("powershell", { "mill", "-i", "mill.idea.GenIdea/idea" })
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
    os.cd(os.scriptdir())
    if os.host() == "windows" then
      os.execv("powershell", {"-NoProfile", "-Command", "mill dist.jars; exit $LASTEXITCODE"})
    else
      os.execv("mill", {"dist.jars"})
    end
  end)
  set_menu {
    usage = "xmake comp",
    description = "Package chisel.jar and chisel.pom into build"
  }
end)

task("clean", function()
  on_run(function()
    os.rmdir(path.join("build", "*"))
  end)
  set_menu {}
end)
