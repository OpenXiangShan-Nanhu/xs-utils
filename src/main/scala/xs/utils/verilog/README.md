# Verilog 模块接入指南

本目录将可复用的 SystemVerilog 源码、Chisel 外部模块接口和用户 API 放在一起。
新增模块通常使用一个 `SvFoo.scala` 文件，包含以下四部分：

| 定义 | 职责 |
| --- | --- |
| `object SvFooImpl extends VlogModule` | 保存参数化 SV 模板，声明模块名和依赖 |
| `class SvFooImpl extends VlogExtModule` | 声明扁平 Chisel 端口、传递 SV 参数、注册源码 |
| `class SvFoo extends RawModule / Module` | 提供类型化接口、数据打包和时钟复位连接 |
| `object SvFoo` 的 `apply` | 推导类型与尺寸、检查输入、实例化 wrapper 并返回结果或接口 |

批量导出从模板生成 SV；Chisel 实例化通过 ExtModule 注册同一份模板。
两条路径共用 [VlogRegistry](VlogModule.scala)，无需为新增模块维护独立的 RTL 生成 Top。

## 1. 最小完整示例

下面的示例实现一个按 `enable` 屏蔽数据的 `SvMask`。它是接入模板，尚未作为实际模块加入库。
新建 `src/main/scala/xs/utils/verilog/SvMask.scala`，放入以下代码即可提供 SV 和 Chisel API：

```scala
package xs.utils.verilog

import chisel3._
import chisel3.experimental.{SourceInfo, inlineInstance, requireIsHardware}

class SvMaskImpl(dataWidth: Int = 32)
    extends VlogExtModule(Map("DATA_WIDTH" -> dataWidth)) {
  require(dataWidth > 0, "SvMask DATA_WIDTH must be positive")

  val io = IO(new Bundle {
    val enable = Input(Bool())
    val in = Input(UInt(dataWidth.W))
    val out = Output(UInt(dataWidth.W))
  })

  private val rtl = new VlogRegistry
  override val desiredName: String = rtl.nameOf(SvMaskImpl)
  rtl.register(this, SvMaskImpl)
}

object SvMaskImpl extends VlogModule {
  override val moduleName = "SvMask"

  override def render(ctx: VlogContext): String =
    s"""// VCS coverage exclude_file
       |module ${ctx.selfName} #(
       |  parameter integer DATA_WIDTH = 32
       |) (
       |  input  wire io_enable,
       |  input  wire [DATA_WIDTH - 1:0] io_in,
       |  output logic [DATA_WIDTH - 1:0] io_out
       |);
       |  always_comb begin
       |    io_out = io_enable ? io_in : '0;
       |  end
       |endmodule
       |""".stripMargin
}

class SvMask[T <: Data](gen: T = UInt(32.W)) extends RawModule {
  require(gen.isWidthKnown && gen.getWidth > 0,
    "SvMask requires a known, positive data width")

  val io = IO(new Bundle {
    val enable = Input(Bool())
    val in = Input(gen.cloneType)
    val out = Output(gen.cloneType)
  })

  private val core = Module(new SvMaskImpl(gen.getWidth))
  core.io.enable := io.enable
  core.io.in := io.in.asUInt
  io.out := core.io.out.asTypeOf(gen)
  inlineInstance(this)
}

object SvMask {
  def apply[T <: Data](in: T, enable: Bool)(implicit sourceInfo: SourceInfo): T = {
    requireIsHardware(in, "SvMask input")
    requireIsHardware(enable, "SvMask enable")
    require(in.isWidthKnown && in.getWidth > 0,
      "SvMask requires a known, positive input width")

    val mask = Module(new SvMask(chiselTypeOf(in)))
    mask.io.in := in
    mask.io.enable := enable
    mask.io.out
  }
}
```

在已有 Chisel `Module` 或 `RawModule` 中，可通过 `SvMask(data, enable)` 获得与 `data`
类型一致的结果；也可使用 `Module(new SvMask())` 创建默认 32 位接口，或用
`Module(new SvMask(new MyBundle))` 创建聚合类型接口。
硬件信号作为工厂输入显式传入，类型描述符 `gen` 则可使用默认值。

## 2. 模板、命名和注册

SV 模板必须是本库 `xs.utils.verilog` 包或其子包中的顶层 `object ... extends VlogModule`。
[SvModuleMain](SvModuleMain.scala) 从本库的编译目录或 JAR 自动发现这些单例，
不会扫描测试产物或其他依赖 JAR，也不需要在 xmake 中添加模块清单。

`moduleName` 是稳定的 SV 基础名，例如 `SvMask`。声明模块时使用 `${ctx.selfName}`；
ExtModule 的 `desiredName` 使用同一个 registry 的 `nameOf`。
registry 会捕获创建时的 `GlobalData.prefix`，因此声明、实例类型和文件名使用一致的前缀。
不要在这些位置手工拼接前缀或把具体参数值写入模块名。

Impl 的 `io_` 端口统一声明在 `val io = IO(new Bundle { ... })` 中；
单 bit valid/ready 的握手组使用 `Decoupled(payload)`，仅带单 bit valid 的组使用 `Valid(payload)`。
多路 packed valid/ready 掩码保留为 `UInt` 字段，避免改变 SV 端口布局。
时钟和复位仍独立声明。Bundle 展平后的端口名须与 SV 模板一致。

ExtModule 中的注册入口是：

```scala
private val rtl = new VlogRegistry
override val desiredName: String = rtl.nameOf(SvMaskImpl)
rtl.register(this, SvMaskImpl)
```

`register` 收集模板及其递归依赖，交给 `VlogExtModule.setInline`。
同一次 Chisel elaboration 中相同文件只注册一次；同名不同内容、非法模块名和依赖环会报错。
不同参数的实例应共用同一个 SV 模板，通过 ExtModule 的参数映射传递实例参数。

模板初始化和 `render` 应能在没有 Chisel elaboration 上下文时执行。
将 `IO`、`Wire`、`Module` 等硬件构建放在类或工厂方法中，SV 文本生成只依赖模板参数声明和 `ctx`。
新增模块的源码放在 `VlogModule.render` 中，供导出与 ExtModule 注册共同使用。

## 3. 引用其他 SV 模块

在 `render` 的插值字符串中，通过 `ctx.ref` 引用依赖，例如队列的水位编码：

```scala
s"""// 其余端口和逻辑略；io_waterline 为 DEPTH + 1 位 one-hot 水位。
   |${ctx.ref(SvOHToUIntImpl)} #(.INPUT_SIZE(DEPTH + 1)) u_count (
   |  .io_in(io_waterline),
   |  .io_out(io_count)
   |);
   |""".stripMargin
```

`ctx.ref(SvOHToUIntImpl)` 同时返回带前缀的模块名并记录依赖。
因此调用方只需注册自身模板，无需额外实例化 Chisel 子模块来携带依赖源码。
同一个依赖也可以被其他 ExtModule 直接实例化，源码仍会去重。
只写字面量 `SvOHToUInt` 不会建立依赖关系，也不会应用前缀。

可参考 [SvFastQueue.scala](SvFastQueue.scala) 中的编码器、mux 依赖，
以及 [SvMimoQueue.scala](SvMimoQueue.scala) 中的 mux、popcount 依赖。

## 4. 参数和 SV 写法

SV 参数声明、ExtModule 构造参数、wrapper 和工厂配置应使用一致的默认值。
目前常用默认值是数据宽度 `32`、输入路数 `2`、`gen = UInt(32.W)`、低位优先 `true`。
队列按结构选择深度：`SvFastQueue` 默认为 2，通用 `SvFastQueueDNImpl` 和 `SvMimoQueue` 默认为 4。
默认参数组合必须合法，并能直接通过 `xmake svmodule -l` 检查。

实现时遵循以下约定：

- ExtModule 的端口名、方向、位宽与 SV 声明逐项一致。数据通道可以使用扁平 `UInt`，
  聚合类型的打包与还原由 wrapper 完成。
- Boolean 配置映射为 SV 的 `0` / `1`；整型条件写成 `if (LOW_FIRST != 0)`，明确表达布尔含义。
- Scala 的 `s"""..."""` 内，SV 系统函数的 `$` 写成 `$$`，如 `$$clog2(INPUT_SIZE)`。
- 批量组合赋值用 `always_comb` 和过程式 `for`；时序逻辑用 `always_ff`。
  参数选择用 `generate if` 分开不同实现；`generate for` 用于重复模块实例。
- 中间数据在过程块外声明，二维点阵优先使用 packed array，例如
  `logic [INDEX_WIDTH - 1:0][INPUT_SIZE - 1:0] selected;`。
  循环中直接访问 `selected[b][i]`，避免在循环体里声明临时数据变量。
- 归约数组按依赖顺序赋值，遍历方向由数据依赖决定；避免自累加临时变量形成不清楚的依赖。
  组合输出和中间项在所有分支上完整赋值。
- 明确单元素、空输入、未命中、多命中和非法状态的语义。
  显式 SV 模块应避免零宽端口；允许空输入的 Chisel 工厂可在边界处返回规定的常量。

组合 wrapper 通常继承 `RawModule`；需要隐式时钟/复位的 wrapper 继承 `Module`，
并显式连接核心的时钟、复位。复位类型、同步/异步方式必须与 SV 实现和模块契约一致。
`inlineInstance(this)` 用于内联 wrapper，最终保留可复用的 SV 核心边界。

## 5. Chisel 工厂 API 与兼容入口

工厂负责从实际输入推导尺寸和类型，调用方通常无需重复指定宽度。
在 SV 边界检查正尺寸、已知位宽、输入数量和硬件绑定；检查应覆盖单元素等旁路分支。
提供泛型返回值时保留 `T`，不要把 `Bundle`、`Vec` 或 `SInt` 的公共接口退化为 `UInt`。

与 `chisel3.util` 对齐的 API，应先查阅本仓库 `dep/chisel` 的实现，核对：

- 参数类型与重载：`Bits`、`UInt`、`Seq[Bool]`、`Vec[Bool]` 等不能随意互换。
- 返回类型：例如 one-hot priority encoder 的序列输入返回序列，packed 输入返回 `UInt`。
- 空输入、单元素、显式位宽、signed 扩展和不同输入宽度的行为。
- 用户可观察的默认值及单元素旁路语义。

Scala 同名重载不应同时定义多套默认参数；可保留无配置参数重载，转调带完整参数的方法。
参考 [SvPriorityMux.scala](SvPriorityMux.scala)、[SvPriorityEncoder1H.scala](SvPriorityEncoder1H.scala)
和 [SvOHToUInt.scala](SvOHToUInt.scala)。

队列实现统一使用 `Sv` 前缀，兼容入口通过 `xs.utils.queue` 包别名保留：

| 兼容入口 | 本目录实现 |
| --- | --- |
| `queue.FastQueue` 类型及工厂 | `verilog.SvFastQueue` |
| `queue.FastQueueRaw` | `verilog.SvFastQueueRaw` |
| `queue.FastQueueNoX` | `verilog.SvFastQueueNoX` |
| `queue.FastQueueFactory` | `verilog.SvFastQueueFactory` |
| `queue.MimoQueue` 类型 | `verilog.SvMimoQueue` |
| `arb.VipArbiter` 类型及工厂 | `verilog.SvVipArbiter` |
| `arb.RRArbiter` 类型及工厂 | `verilog.SvRRArbiter` |

这些别名不会额外生成模块层级。SV 核心也使用新名称，如 `SvFastQueueD1`、
`SvFastQueueDN`、`SvMimoQueue`；既有 `queue.FastQueue` / `queue.MimoQueue` 调用无需改名。

## 6. 导出、lint 和测试

在仓库根目录运行：

```sh
xmake svmodule                              # 导出全部模板
xmake svmodule -l                           # 导出并逐模块 lint
xmake svmodule --lint --linter=verilator     # 完整选项写法
```

输出位于 `build/svmodule/`，每个模块一个 `.sv` 文件，`filelist.f` 按依赖顺序列出文件名。
导出保留 SV 参数声明和默认值，不需要 ChiselStage、firtool 或每模块一个生成 Top。
重复导出会清理旧 manifest 中已消失的文件，因此模块改名后旧 `.sv` 也会被移除。

`-L/--linter` 选择工具，`-l/--lint` 开启检查；当前支持 Verilator。
每个模块都会单独作为顶层，以自身默认参数运行 `--lint-only`。
仅导出不需要 Verilator，生成或 lint 失败会使命令返回非零退出码。

回归用例放在 `src/test/scala/xs/utils/test/chiselsim/`，继承 `SimTestSpec`，
由 Mill 自动发现。以示例模块为例，增加 `SvMaskSpec` 后运行：

```sh
xmake test --list
xmake test -s SvMaskSpec --simulator=verilator
xmake test -s VlogRegistrySpec,SvDefaultsSpec
```

测试应覆盖默认参数、非默认尺寸、输入边界、数据顺序、接口合法性和共享模板去重。
类型化 API 使用 `Bundle` / `Vec[Bundle]` 检查打包行为；时序模块再覆盖复位、背压和连续传输。
工厂 API 有原生 Chisel 对应实现时，同时对照原生实现和独立软件模型。
lint 检查默认配置的 RTL，功能仿真负责验证实际行为及其他参数组合。

更多规范见 [测试说明](../../../../../test/README.md)。
