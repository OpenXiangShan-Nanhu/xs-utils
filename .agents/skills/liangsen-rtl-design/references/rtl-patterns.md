# 通用 RTL 写法与审查反例

这些例子用于说明结构，不规定具体协议、数据宽度、器件或微架构。实例接线示例中的子模块需使用工程已有实现。

## 等效注释的约定

在批量或复杂计算块前写一至三行 Chisel 表达式，强调“整块做什么”，不复述循环展开。以下示例将 SV unpacked 数组对应到 Chisel Vec、packed 掩码对应到 UInt；参数在 Chisel 中对应 Scala 编译期值，REVERSE 对应 Boolean。

- 批量变换用 `VecInit/map`，前缀用 `scanLeft`，归约用 `reduce` 或与真实结构一致的 `reduceTree`；明确 seed 和有效位宽。
- Chisel 注释是语义摘要，不是改用单变量 SV 累加器的许可；显式数组阶段仍保留。
- 对子模块只能在核实契约后用 `Mux1H/PriorityMux` 等概括选择行为。仅例化结构已知时标明结构示意，不能臆测 helper 的功能。
- 对寄存更新写清周期、使能与复位，不能把组合映射注释直接套到时序块。不能精确覆盖时说明范围或拆成具名步骤，而非写无约束的“equivalent”。

## 单次赋值的边界

```systemverilog
// 合规：互斥分支，每条执行路径只写一次。
always_comb begin
  if (select_a) result = a;
  else          result = b;
end

// 不合规：select_a 为真时，同一组 bit 被写两次。
always_comb begin
  result = b;
  if (select_a) result = a;
end
```

- 独立 `if (a) out = ...; if (b) out = ...;` 不是天然互斥。
- `out = '0; out[idx] = value;` 也会重复写 bit，不能用“默认值”解释为合规。
- 按目标位循环并用完整 if/else 可以合规；对未知索引、越界值仍须保持预期语义，不能仅以二态测试推断等价。
- 函数中 `sum = a + b; if (wrap) sum = sum - limit;` 同样违规；改用独立的 `raw_sum` 和单次产生的结果。
- 时序 reset 分支和 else 更新分支互斥，允许给同一寄存器赋值；没有使能时保持不写，无需额外自赋值。

## 参数选择与过程式循环

以下为完整示例，仅演示配置位于外层、信号不在循环内声明：

```systemverilog
module lane_order #(
  parameter integer N = 4,
  parameter integer WIDTH = 8,
  parameter bit REVERSE = 0
) (
  input  wire  [WIDTH - 1:0] data_in [N],
  output logic [WIDTH - 1:0] data_out [N]
);
  // N >= 1, WIDTH >= 1.
  // Chisel equivalent: data_out := VecInit(if (REVERSE) data_in.reverse else data_in)
  generate
    if (REVERSE) begin : reverse_order
      always_comb begin
        for (integer i = 0; i < N; i++) begin
          data_out[i] = data_in[N - 1 - i];
        end
      end
    end else begin : original_order
      always_comb begin
        for (integer i = 0; i < N; i++) begin
          data_out[i] = data_in[i];
        end
      end
    end
  endgenerate
endmodule
```

## 数组式求和 / 前缀

```systemverilog
module array_sum #(
  parameter integer N = 4,
  parameter integer WIDTH = 8,
  parameter integer SUM_WIDTH = WIDTH + $clog2(N)
) (
  input  wire [WIDTH - 1:0]     data [N],
  output wire [SUM_WIDTH - 1:0] total
);
  // N >= 1, WIDTH >= 1; SUM_WIDTH must hold the full unsigned sum.
  logic [SUM_WIDTH - 1:0] sum_stage [N + 1];

  // Chisel equivalent: sum_stage := VecInit(data.scanLeft(0.U(SUM_WIDTH.W))
  //   ((s, x) => s + x.pad(SUM_WIDTH))); total := sum_stage.last
  always_comb begin
    sum_stage[0] = '0;
    for (integer i = 0; i < N; i++) begin
      sum_stage[i + 1] = sum_stage[i] + SUM_WIDTH'(data[i]);
    end
  end
  assign total = sum_stage[N];
endmodule
```

禁止替换成 `total = '0; for (...) total += data[i];`；禁止 seed 整个数组后又覆盖阶段。其他操作保留同样的阶段结构，并选择正确的初值、宽度与符号：

- OR/XOR 的中性值为全零；AND 为全一。
- min/max 可把首输入放入 stage[0]，从第二输入开始逐阶段选择；要求 N >= 1，并明确有符号比较方式。
- 优先级选择使用独立 `found_stage` 和 `value_stage` 数组，各阶段用互斥选择产生，不能靠后写覆盖先写来构造优先级。
- 树形实现将输入叶子和内部节点的索引域分开，按依赖顺序赋值一次；非二次幂时只对缺失叶子补中性元素。
- 空输入是接口契约问题：若允许 N=0，要单独用 generate if 定义结果与合法声明；不能依赖负范围或越界访问的工具行为。

## 普通矩阵计算与批量模块例化默认分离

```systemverilog
logic [INPUTS - 1:0] match [OUTPUTS];
wire [WIDTH - 1:0] selected [OUTPUTS];

// Chisel equivalent (each output o):
//   match(o) := VecInit((0 until INPUTS).map(i => valid(i) && (tag(i) === target(o)))).asUInt
always_comb begin
  for (integer o = 0; o < OUTPUTS; o++) begin
    for (integer i = 0; i < INPUTS; i++) begin
      match[o][i] = valid[i] && (tag[i] == target[o]);
    end
  end
end

// Chisel-style structure: Seq.fill(OUTPUTS)(Module(new DataSelector(WIDTH, INPUTS)))
// DataSelector denotes the project-equivalent helper, not a built-in selection policy.
generate
  for (genvar o = 0; o < OUTPUTS; o++) begin : gen_selector
    data_selector #(.WIDTH(WIDTH), .INPUTS(INPUTS)) u_select (
      .select(match[o]), .data(data), .result(selected[o])
    );
  end
endgenerate
```

`data_selector` 代表既有辅助模块；是否一热、优先级或多命中合并由接口规定，不能在重构时自行改变。注释中的 `DataSelector` 仅代表工程对应的 Chisel helper，此处只示意批量例化结构，不声明选择算法等价。普通矩阵映射默认在过程式循环计算，矩阵和结果向量在外部声明；这不是禁止例化循环包含相关的常量切片或补零赋值。


## genvar 相关切片与局部连线

当每个 lane 的切片边界、宽度或参数依赖循环索引时，`generate for` 可让常量表达式和相关连线保持在一起；简单独立的逐 lane 赋值若这样更清楚，也允许使用。不要为了避免 generate 内赋值，把自然的切片改成逐 bit 过程式循环。

```systemverilog
// ENQ_NUM >= 1, DEPTH >= ENQ_NUM; io_enq_valid has ENQ_NUM bits.
localparam integer ADDR_WIDTH = (DEPTH == 1) ? 1 : $clog2(DEPTH);
wire [ADDR_WIDTH:0] enq_rank [ENQ_NUM];

// Chisel equivalent: enq_rank := VecInit((0 until ENQ_NUM).map(e =>
//   PopCount(io_enq_valid.asBools.take(e)).pad(ADDR_WIDTH + 1)))
assign enq_rank[0] = '0;
generate
  for (genvar e = 1; e < ENQ_NUM; e++) begin : gen_rank_count
    localparam integer PREFIX_WIDTH = $clog2(e + 1);
    assign enq_rank[e][ADDR_WIDTH:PREFIX_WIDTH] = '0;
    SvPopcount #(.INPUT_SIZE(e)) u_rank_count (
      .io_in(io_enq_valid[e - 1:0]),
      .io_out(enq_rank[e][PREFIX_WIDTH - 1:0])
    );
  end
endgenerate
```

- `SvPopcount` 的输出宽度为 `$clog2(INPUT_SIZE + 1)`；`e` 与 `PREFIX_WIDTH` 在展开时均为常量，补零和子模块输出驱动不重叠的 bit。
- 第 0 路单独置零，避免零宽实例；给定参数约束保证上方切片合法。其他场景若可能出现空切片，用逐 lane 的 generate if 处理，不依赖非法或反向范围。
- 这类 generate 用法不豁免单驱动、每 bit 单次赋值、组合赋值完整性或数组式归约规则，也不保证自动形成平衡树。
