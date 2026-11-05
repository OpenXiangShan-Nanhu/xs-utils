# 测试入口与编写规范

使用 ScalaTest + ChiselSim，默认仿真后端为 Verilator。测试由 Mill 自动发现，
**新增测试不需要修改 `xmake.lua` 或维护手工注册表**。
需要 PATH 中可用的 Mill、JDK、Verilator、make、C++ 编译器及项目已有的 firtool 环境。

## 运行测试

```sh
xmake test                                      # 所有测试，使用各用例默认种子/周期数
xmake test --list                               # 编译并列出所有可发现的测试套件，不仿真
xmake test -s FastQueueSpec                      # 简写，自动补 xs.utils.test.chiselsim. 前缀
xmake test -s 'xs.utils.test.chiselsim.*Spec'              # 完整类名或 glob；通配符请加引号
xmake test -s FastQueueSpec -c 'depth 3 '         # 测试全名包含指定文本；尾部空格避免匹配 depth 33
xmake test -s FastQueueSpec --seed=42 --cycles=200
xmake test -s FastQueueSpec -c 'depth 3 ' --seed=0x1234 --cycles=100 --waves
xmake test -s FastQueueSpec -- -Dcycles=20       # ScalaTest 参数透传（参见下表）
xmake test -s FastQueueThroughputSpec --cycles=1000 # 低占用稳态吞吐及 FIFO 顺序
xmake test -s SvMux1HSpec,SvPopcountSpec          # SV 组合逻辑及 inline wrapper
xmake test -s SvOHToUIntSpec --simulator=verilator # OHToUInt 兼容 API、边界及随机测试
```

| 参数 | 含义 |
| --- | --- |
| `-l, --list` | 列出 Mill 发现的所有套件；不执行测试，其他运行筛选不生效 |
| `-s, --suite` | 套件名或 glob；多个选择用逗号分隔，例如 `FooSpec,BarSpec` |
| `-c, --case` | ScalaTest `-z`：完整测试名的子串匹配，不是正则表达式 |
| `--exact` | ScalaTest `-t`：完整测试名精确匹配，包含 `describe` 前缀；与 `--case` 互斥 |
| `--seed` | 随机种子：有符号 64 位十进制，或 `0x` 非负十六进制（不超过 Long.MaxValue） |
| `--cycles` | 每条配置的随机周期数，范围 1～2147483647；不缩短定向测试 |
| `--waves` | 启用 VCD 波形，默认关闭 |
| `--simulator` | `verilator`（默认）或 `vcs`；VCS 需要自行安装并配置许可证 |
| `--chisel-opts` | 附加 Chisel 参数字符串，传给 ChiselSim 的 `chiselOpts` |
| `--firtool-opts` | 附加 firtool 参数字符串，传给 ChiselSim 的 `firtoolOpts` |
| `-- ...` | 原样透传 ScalaTest 参数，例如 `-- -DqueueMode=stress`；不要重复指定同一参数 |

长参数使用 `--key=value`，短参数可使用 `-s Value`；包含空格或通配符的值需要引号。

无套件筛选时调用 `mill --no-server test`；指定套件时调用 `test.testOnly`。
筛选、波形及其他参数转换为独立 ScalaTest 参数；Windows 分支逐项转义 PowerShell 参数。
任一测试失败时 `xmake test` 返回非零退出码。注意检查报告中的测试数量，筛选文本不能匹配时可能运行零个测试。
波形、生成 RTL 和仿真日志位于 `build/chiselsim/<SuiteName>/`，不提交生成文件。
VCS 入口沿用 ChiselSim 支持，本项目当前回归验证使用 Verilator。

## 批量导出 SV 模块与 lint

新增 SV 模块、注册依赖及 Chisel API 的完整示例见 [Verilog 接入指南](../main/scala/xs/utils/verilog/README.md)。

```sh
xmake svmodule                              # 仅生成 build/svmodule/*.sv
xmake svmodule -l                           # 生成后逐模块运行 Verilator lint
xmake svmodule --lint --linter=verilator     # 长参数形式
xmake svmodule -l -L verilator               # 短参数形式
```

入口 `xs.utils.verilog.SvModuleMain` 自动发现本库编译目录或 JAR 中
`xs.utils.verilog` 包下的 `VlogModule` 单例，复用 `VlogRegistry` 收集依赖并去重。
不实例化 Chisel 模块，不经过 `ChiselStage`、FIRRTL 或 firtool，也不需要测试 Top。
生成文件保留 SV 参数声明及其默认值；`filelist.f` 按依赖顺序记录相对文件名。
重复生成会覆盖对应文件，并仅清理上次 `filelist.f` 中已不再导出的文件。

时钟 ExtModule 的 SV 也统一放在伴生 `VlogModule` 模板中；
Chisel 实例注册与批量导出使用相同源码。
新增模块只需在此包下提供 `object ... extends VlogModule` 模板，
由 ExtModule 通过 `VlogRegistry.register` 注册，无需维护额外导出列表。

`-L` 仅选择 linter，`-l` 才开启 lint，目前支持 `verilator`。
每个导出模块都单独使用 `--top-module` 和默认参数执行 `--lint-only`，
确保被其他模块引用的辅助模块也按自身默认参数检查。
生成或 lint 失败时命令返回非零退出码；仅生成时不需要安装 Verilator。

`xmake test -s VlogRegistrySpec,SvDefaultsSpec` 检查依赖去重、前缀及默认参数的一致性。

各 SV 模块旧的独立生成 Top 已移除，统一使用 `xmake svmodule`。
其他 Chisel / 形式验证 Top 仍可通过 `xmake rtl -M <名称>` 生成 RTL。

## 测试用例编写规范

### 1. 自动发现约定

- 文件放在 `src/test/scala/xs/utils/test/chiselsim/`，包名使用 `xs.utils.test.chiselsim`。
- 每个功能一个公开、无构造参数的具体 `*Spec` 类，仿真测试继承 `SimTestSpec`。
- `SimTestSpec` 是抽象基类，不作为可运行套件注册；辅助 DUT/Bundle 使用独立名字。
- 非仿真的纯 Scala 测试可直接继承 ScalaTest 的测试类，同样由 Mill 发现。
- 不用 `object ... extends App` / `runMain` 编写回归用例，它们属于生成入口，不会自动注册为测试。
- 新增后先执行 `xmake test --list` 确认出现，再执行 `xmake test -s YourSpec`。

### 2. 用例组织与参数

- 使用 `describe("模块名")` 和 `it("稳定、明确的行为描述")`；名称应包含关键配置，方便 `--case` 筛选。
- 用例注册数量/名称保持稳定，不根据随机种子或运行时参数动态注册 `it`。
- 在 `it` 内或其调用的辅助函数中读取 `testSeed(default)`、`testCycles(default)`；禁止在构造器字段中读取参数。
- `testSeed()` 默认 `0x5eed`，`testCycles()` 默认 1000；允许各用例提供确定性的默认值，命令行覆盖默认值。
- 随机刺激使用 `new scala.util.Random(seed)`，禁止用当前时间或全局随机数作为隐式种子。
- 用 `info` 输出实际种子、周期数和关键配置；失败信息带上配置、周期和期望值，以便复现。
- 仿真波形、后端和编译参数由基类统一管理，不在测试里硬编码路径或调用外部仿真命令。

### 3. 断言与边界覆盖

- 通过 `simulate(new DUT)` 和 `poke` / `expect` / `clock.step` 驱动真实 DUT。
- 用独立软件参考模型检查结果，不能用 DUT 的输出反推参考模型的期望值。
- 类型化 SV wrapper 的测试使用 `Bundle` 或 `Vec[Bundle]` 数据，包含不同位宽的 `UInt` / `SInt` / `Bool` 字段；顶层端口保持聚合类型，逐字段 `poke` / `expect`，不要额外用 `asUInt` / `asTypeOf` 包住整个测试边界。
- 使用 `WrapperTestPayload` 和 `SimTestSpec.pokePayload` / `expectPayload` 共享软件字段布局，覆盖负数、字段/Vec 顺序及跨 64 位边界；仅原始 ExtModule 接口和明确要求 UInt 的工厂重载保留 packed 数据路径。
- 覆盖复位、边界、背压、并发操作和复位后复用；随机测试不能替代定向边界测试。
- 握手接口在停顿时保持生产者 valid/data；所有等待循环须有合理上限，避免错误导致无限等待。
- 不捕获并吞掉断言/仿真异常，不用打印“失败”代替测试失败。
- Verilator 二态仿真不验证 X 传播，涉及 X 的语义必须明确标注覆盖边界。

### 4. 最小仿真用例模板

```scala
package xs.utils.test.chiselsim

import chisel3._
import scala.util.Random

class ExampleRegister extends Module {
  val in = IO(Input(UInt(8.W)))
  val out = IO(Output(UInt(8.W)))
  out := RegNext(in, 0.U)
}

class ExampleRegisterSpec extends SimTestSpec {
  describe("ExampleRegister") {
    it("captures input after one cycle") {
      val seed = testSeed()
      val cycles = testCycles()
      val random = new Random(seed)
      info(s"seed=$seed cycles=$cycles")
      simulate(new ExampleRegister) { dut =>
        dut.out.expect(0.U)
        for (cycle <- 0 until cycles) {
          val value = random.nextInt(256)
          dut.in.poke(value.U)
          dut.clock.step()
          dut.out.expect(value.U, s"seed=$seed cycle=$cycle")
        }
      }
    }
  }
}
```

### 5. 新增测试专用参数

基类已注册 `seed`、`cycles`，并通过 ChiselSim 注册 simulator / emitVcd / chiselOpts / firtoolOpts。
新增专用参数应在套件中用 `CliOption.simple` 注册并验证，在用例执行期间读取：

```scala
import chisel3.simulator.scalatest.HasCliOptions.CliOption

// 位于具体 Spec 类体中：注册参数，而不是在这里读取运行时参数。
addOption(CliOption.simple[Int]("queueBurst", "Positive burst length", value => {
  val n = value.toInt
  require(n > 0, "queueBurst must be positive")
  n
}))
// 位于 it(...) 内：
val burst = getOption[Int]("queueBurst").getOrElse(8)
```

运行：`xmake test -s YourSpec -- -DqueueBurst=16`。
专用参数必须带套件筛选；不接受该参数的其他 ChiselSim 套件会拒绝未知选项。
若参数适用于所有仿真测试，再提升到 `SimTestSpec`；无需为每个专用参数修改 xmake task。
