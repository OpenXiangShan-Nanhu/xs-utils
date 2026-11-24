# 通用 RTL 写法与审查反例

这些例子用于说明结构，不规定具体协议、数据宽度、器件或微架构。实例接线示例中的子模块需使用工程已有实现。

## 单次赋值的边界

```systemverilog
// 合规：case 的互斥分支完整赋值，每条执行路径只写一次。
logic [WIDTH - 1:0] result;

always_comb begin
  case (select)
    2'b00: begin
      result = a;
    end
    2'b01: begin
      result = b;
    end
    default: begin
      result = c;
    end
  endcase
end

// 不合规：select_a 为真时，同一组 bit 被写两次。
always_comb begin
  result = b;
  if (select_a) begin
    result = a;
  end
end
```

- 两个分别以 `a`、`b` 为条件并写入 `out` 的独立 `if` 不是天然互斥。
- `out = '0; out[idx] = value;` 也会重复写 bit，不能用“默认值”解释为合规。
- 按目标位循环并用完整 if/else 可以合规；对未知索引、越界值仍须保持预期语义，不能仅以二态测试推断等价。
- 函数中先赋值 `sum = a + b;`，再在 `wrap` 为真时改写 `sum`，同样违规；改用独立的 `raw_sum` 和单次产生的结果。
- 时序 reset 分支和 else 更新分支互斥，允许给同一寄存器赋值；没有使能时保持不写，无需额外自赋值。

## 表达式拆分

每个 SV 表达式优先一行，最多两行。把可独立检查的条件命名，最终表达式只组合这些条件。例如，将读写分支和公共条件分开，便于从覆盖率报告定位是哪一类请求或资格条件未命中。

```systemverilog
wire header_ok = param_ok && !corrupt;
wire read_ok   = is_read && full_beat && exact_mask;
wire write_ok  = is_write && mask_within_lanes;
wire legal     = header_ok && aligned && (read_ok || write_ok);
```

位宽相关的中间结果用显式类型或位宽；拆嵌套选择时保留分支优先级和 `X` 合并行为。短且语义直接的握手、归约或二选一保持原状，不为行数引入无意义的转接信号。

## 参数选择与 generate 连续赋值

以下完整示例将配置选择放在外层，直接使用 generate if 和 for，省略 `generate/endgenerate`，以 `wire` 和 `assign` 完成逐元素映射：

```systemverilog
module lane_order #(
  parameter integer N = 4,
  parameter integer WIDTH = 8,
  parameter bit REVERSE = 0
) (
  input  wire [WIDTH - 1:0] data_in [N],
  output wire [WIDTH - 1:0] data_out [N]
);
  // N >= 1, WIDTH >= 1.
  if (REVERSE) begin : reverse_order
    for (genvar i = 0; i < N; i++) begin : gen_lane
      assign data_out[i] = data_in[N - 1 - i];
    end
  end else begin : original_order
    for (genvar i = 0; i < N; i++) begin : gen_lane
      assign data_out[i] = data_in[i];
    end
  end
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
  wire [SUM_WIDTH - 1:0] sum_stage [N + 1];

  assign sum_stage[0] = '0;
  for (genvar i = 0; i < N; i++) begin : gen_sum
    assign sum_stage[i + 1] = sum_stage[i] + SUM_WIDTH'(data[i]);
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

## 矩阵计算与批量模块例化

```systemverilog
wire [INPUTS - 1:0] match [OUTPUTS];
wire [WIDTH - 1:0] selected [OUTPUTS];

for (genvar o = 0; o < OUTPUTS; o++) begin : gen_selector
  for (genvar i = 0; i < INPUTS; i++) begin : gen_match
    assign match[o][i] = valid[i] && (tag[i] == target[o]);
  end

  data_selector #(
    .WIDTH(WIDTH),
    .INPUTS(INPUTS)
  ) u_select (
    .select(match[o]),
    .data(data),
    .result(selected[o])
  );
end
```

`data_selector` 代表既有辅助模块；是否一热、优先级或多命中合并由接口规定，不能在重构时自行改变。矩阵和结果向量在外部声明，每行匹配计算与对应实例放在同一个 generate 循环中。

## genvar 相关切片与局部连线

逐 lane 连线优先使用 generate 循环和连续赋值；切片边界、宽度或参数依赖循环索引时，使用 `genvar` 保持常量表达式和相关连线在一起。不要为了避免 generate 内赋值，把自然的切片改成逐 bit 过程式循环。

```systemverilog
// ENQ_NUM >= 1, DEPTH >= ENQ_NUM; io_enq_valid has ENQ_NUM bits.
localparam integer ADDR_WIDTH = (DEPTH == 1) ? 1 : $clog2(DEPTH);
wire [ADDR_WIDTH:0] enq_rank [ENQ_NUM];

// Lane 0 has an empty prefix; no popcount instance is needed.
assign enq_rank[0] = '0;

for (genvar e = 1; e < ENQ_NUM; e++) begin : gen_rank_count
  localparam integer PREFIX_WIDTH = $clog2(e + 1);
  assign enq_rank[e][ADDR_WIDTH:PREFIX_WIDTH] = '0;
  SvPopcount #(
    .INPUT_SIZE(e)
  ) u_rank_count (
    .io_in(io_enq_valid[e - 1:0]),
    .io_out(enq_rank[e][PREFIX_WIDTH - 1:0])
  );
end
```

- `SvPopcount` 的输出宽度为 `$clog2(INPUT_SIZE + 1)`；`e` 与 `PREFIX_WIDTH` 在展开时均为常量，补零和子模块输出驱动不重叠的 bit。
- 第 0 路单独置零，避免零宽实例；给定参数约束保证上方切片合法。其他场景若可能出现空切片，用逐 lane 的 generate if 处理，不依赖非法或反向范围。
- 这类 generate 用法不豁免单驱动、每 bit 单次赋值、组合赋值完整性或数组式归约规则，也不保证自动形成平衡树。
