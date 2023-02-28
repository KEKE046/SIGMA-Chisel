circuit FlexDPE :
  module XBar :
    input clock : Clock
    input reset : Reset
    output io : { flip data : { valid : UInt<1>, bits : UInt<8>}[8], flip dest : { valid : UInt<1>, bits : UInt<3>}[8], out : { valid : UInt<1>, bits : UInt<8>}[8]}

    when io.dest[0].valid : @[SIGMA.scala 16:23]
      io.out[0] <= io.data[io.dest[0].bits] @[SIGMA.scala 17:13]
    else :
      io.out[0].valid <= UInt<1>("h0") @[SIGMA.scala 19:19]
      io.out[0].bits is invalid @[SIGMA.scala 20:18]
    when io.dest[1].valid : @[SIGMA.scala 16:23]
      io.out[1] <= io.data[io.dest[1].bits] @[SIGMA.scala 17:13]
    else :
      io.out[1].valid <= UInt<1>("h0") @[SIGMA.scala 19:19]
      io.out[1].bits is invalid @[SIGMA.scala 20:18]
    when io.dest[2].valid : @[SIGMA.scala 16:23]
      io.out[2] <= io.data[io.dest[2].bits] @[SIGMA.scala 17:13]
    else :
      io.out[2].valid <= UInt<1>("h0") @[SIGMA.scala 19:19]
      io.out[2].bits is invalid @[SIGMA.scala 20:18]
    when io.dest[3].valid : @[SIGMA.scala 16:23]
      io.out[3] <= io.data[io.dest[3].bits] @[SIGMA.scala 17:13]
    else :
      io.out[3].valid <= UInt<1>("h0") @[SIGMA.scala 19:19]
      io.out[3].bits is invalid @[SIGMA.scala 20:18]
    when io.dest[4].valid : @[SIGMA.scala 16:23]
      io.out[4] <= io.data[io.dest[4].bits] @[SIGMA.scala 17:13]
    else :
      io.out[4].valid <= UInt<1>("h0") @[SIGMA.scala 19:19]
      io.out[4].bits is invalid @[SIGMA.scala 20:18]
    when io.dest[5].valid : @[SIGMA.scala 16:23]
      io.out[5] <= io.data[io.dest[5].bits] @[SIGMA.scala 17:13]
    else :
      io.out[5].valid <= UInt<1>("h0") @[SIGMA.scala 19:19]
      io.out[5].bits is invalid @[SIGMA.scala 20:18]
    when io.dest[6].valid : @[SIGMA.scala 16:23]
      io.out[6] <= io.data[io.dest[6].bits] @[SIGMA.scala 17:13]
    else :
      io.out[6].valid <= UInt<1>("h0") @[SIGMA.scala 19:19]
      io.out[6].bits is invalid @[SIGMA.scala 20:18]
    when io.dest[7].valid : @[SIGMA.scala 16:23]
      io.out[7] <= io.data[io.dest[7].bits] @[SIGMA.scala 17:13]
    else :
      io.out[7].valid <= UInt<1>("h0") @[SIGMA.scala 19:19]
      io.out[7].bits is invalid @[SIGMA.scala 20:18]

  module PE :
    input clock : Clock
    input reset : Reset
    output io : { flip in_data : { valid : UInt<1>, bits : UInt<8>}, flip in_stationary : UInt<1>, out_data : { valid : UInt<1>, bits : UInt<8>}}

    reg buf : UInt<8>, clock with :
      reset => (UInt<1>("h0"), buf) @[SIGMA.scala 35:24]
    reg buf_valid : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[SIGMA.scala 36:34]
    node _T = and(io.in_stationary, io.in_data.valid) @[SIGMA.scala 38:25]
    when _T : @[SIGMA.scala 38:46]
      buf <= io.in_data.bits @[SIGMA.scala 39:9]
      buf_valid <= UInt<1>("h1") @[SIGMA.scala 40:15]
    wire out : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 43:25]
    node mul_valid = and(buf_valid, io.in_data.valid) @[SIGMA.scala 44:37]
    node mul_a = mux(mul_valid, io.in_data.bits, UInt<1>("h0")) @[SIGMA.scala 45:26]
    node mul_b = mux(mul_valid, buf, UInt<1>("h0")) @[SIGMA.scala 46:26]
    node _mul_T = mul(mul_b, UInt<1>("h0")) @[Arithmetic.scala 40:49]
    node _mul_T_1 = add(_mul_T, mul_a) @[Arithmetic.scala 40:54]
    node mul = tail(_mul_T_1, 1) @[Arithmetic.scala 40:54]
    out.valid <= mul_valid @[SIGMA.scala 48:13]
    out.bits <= mul @[SIGMA.scala 49:13]
    io.out_data <= out @[SIGMA.scala 50:15]

  module PE_1 :
    input clock : Clock
    input reset : Reset
    output io : { flip in_data : { valid : UInt<1>, bits : UInt<8>}, flip in_stationary : UInt<1>, out_data : { valid : UInt<1>, bits : UInt<8>}}

    reg buf : UInt<8>, clock with :
      reset => (UInt<1>("h0"), buf) @[SIGMA.scala 35:24]
    reg buf_valid : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[SIGMA.scala 36:34]
    node _T = and(io.in_stationary, io.in_data.valid) @[SIGMA.scala 38:25]
    when _T : @[SIGMA.scala 38:46]
      buf <= io.in_data.bits @[SIGMA.scala 39:9]
      buf_valid <= UInt<1>("h1") @[SIGMA.scala 40:15]
    wire out : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 43:25]
    node mul_valid = and(buf_valid, io.in_data.valid) @[SIGMA.scala 44:37]
    node mul_a = mux(mul_valid, io.in_data.bits, UInt<1>("h0")) @[SIGMA.scala 45:26]
    node mul_b = mux(mul_valid, buf, UInt<1>("h0")) @[SIGMA.scala 46:26]
    node _mul_T = mul(mul_b, UInt<1>("h0")) @[Arithmetic.scala 40:49]
    node _mul_T_1 = add(_mul_T, mul_a) @[Arithmetic.scala 40:54]
    node mul = tail(_mul_T_1, 1) @[Arithmetic.scala 40:54]
    out.valid <= mul_valid @[SIGMA.scala 48:13]
    out.bits <= mul @[SIGMA.scala 49:13]
    io.out_data <= out @[SIGMA.scala 50:15]

  module PE_2 :
    input clock : Clock
    input reset : Reset
    output io : { flip in_data : { valid : UInt<1>, bits : UInt<8>}, flip in_stationary : UInt<1>, out_data : { valid : UInt<1>, bits : UInt<8>}}

    reg buf : UInt<8>, clock with :
      reset => (UInt<1>("h0"), buf) @[SIGMA.scala 35:24]
    reg buf_valid : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[SIGMA.scala 36:34]
    node _T = and(io.in_stationary, io.in_data.valid) @[SIGMA.scala 38:25]
    when _T : @[SIGMA.scala 38:46]
      buf <= io.in_data.bits @[SIGMA.scala 39:9]
      buf_valid <= UInt<1>("h1") @[SIGMA.scala 40:15]
    wire out : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 43:25]
    node mul_valid = and(buf_valid, io.in_data.valid) @[SIGMA.scala 44:37]
    node mul_a = mux(mul_valid, io.in_data.bits, UInt<1>("h0")) @[SIGMA.scala 45:26]
    node mul_b = mux(mul_valid, buf, UInt<1>("h0")) @[SIGMA.scala 46:26]
    node _mul_T = mul(mul_b, UInt<1>("h0")) @[Arithmetic.scala 40:49]
    node _mul_T_1 = add(_mul_T, mul_a) @[Arithmetic.scala 40:54]
    node mul = tail(_mul_T_1, 1) @[Arithmetic.scala 40:54]
    out.valid <= mul_valid @[SIGMA.scala 48:13]
    out.bits <= mul @[SIGMA.scala 49:13]
    io.out_data <= out @[SIGMA.scala 50:15]

  module PE_3 :
    input clock : Clock
    input reset : Reset
    output io : { flip in_data : { valid : UInt<1>, bits : UInt<8>}, flip in_stationary : UInt<1>, out_data : { valid : UInt<1>, bits : UInt<8>}}

    reg buf : UInt<8>, clock with :
      reset => (UInt<1>("h0"), buf) @[SIGMA.scala 35:24]
    reg buf_valid : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[SIGMA.scala 36:34]
    node _T = and(io.in_stationary, io.in_data.valid) @[SIGMA.scala 38:25]
    when _T : @[SIGMA.scala 38:46]
      buf <= io.in_data.bits @[SIGMA.scala 39:9]
      buf_valid <= UInt<1>("h1") @[SIGMA.scala 40:15]
    wire out : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 43:25]
    node mul_valid = and(buf_valid, io.in_data.valid) @[SIGMA.scala 44:37]
    node mul_a = mux(mul_valid, io.in_data.bits, UInt<1>("h0")) @[SIGMA.scala 45:26]
    node mul_b = mux(mul_valid, buf, UInt<1>("h0")) @[SIGMA.scala 46:26]
    node _mul_T = mul(mul_b, UInt<1>("h0")) @[Arithmetic.scala 40:49]
    node _mul_T_1 = add(_mul_T, mul_a) @[Arithmetic.scala 40:54]
    node mul = tail(_mul_T_1, 1) @[Arithmetic.scala 40:54]
    out.valid <= mul_valid @[SIGMA.scala 48:13]
    out.bits <= mul @[SIGMA.scala 49:13]
    io.out_data <= out @[SIGMA.scala 50:15]

  module PE_4 :
    input clock : Clock
    input reset : Reset
    output io : { flip in_data : { valid : UInt<1>, bits : UInt<8>}, flip in_stationary : UInt<1>, out_data : { valid : UInt<1>, bits : UInt<8>}}

    reg buf : UInt<8>, clock with :
      reset => (UInt<1>("h0"), buf) @[SIGMA.scala 35:24]
    reg buf_valid : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[SIGMA.scala 36:34]
    node _T = and(io.in_stationary, io.in_data.valid) @[SIGMA.scala 38:25]
    when _T : @[SIGMA.scala 38:46]
      buf <= io.in_data.bits @[SIGMA.scala 39:9]
      buf_valid <= UInt<1>("h1") @[SIGMA.scala 40:15]
    wire out : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 43:25]
    node mul_valid = and(buf_valid, io.in_data.valid) @[SIGMA.scala 44:37]
    node mul_a = mux(mul_valid, io.in_data.bits, UInt<1>("h0")) @[SIGMA.scala 45:26]
    node mul_b = mux(mul_valid, buf, UInt<1>("h0")) @[SIGMA.scala 46:26]
    node _mul_T = mul(mul_b, UInt<1>("h0")) @[Arithmetic.scala 40:49]
    node _mul_T_1 = add(_mul_T, mul_a) @[Arithmetic.scala 40:54]
    node mul = tail(_mul_T_1, 1) @[Arithmetic.scala 40:54]
    out.valid <= mul_valid @[SIGMA.scala 48:13]
    out.bits <= mul @[SIGMA.scala 49:13]
    io.out_data <= out @[SIGMA.scala 50:15]

  module PE_5 :
    input clock : Clock
    input reset : Reset
    output io : { flip in_data : { valid : UInt<1>, bits : UInt<8>}, flip in_stationary : UInt<1>, out_data : { valid : UInt<1>, bits : UInt<8>}}

    reg buf : UInt<8>, clock with :
      reset => (UInt<1>("h0"), buf) @[SIGMA.scala 35:24]
    reg buf_valid : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[SIGMA.scala 36:34]
    node _T = and(io.in_stationary, io.in_data.valid) @[SIGMA.scala 38:25]
    when _T : @[SIGMA.scala 38:46]
      buf <= io.in_data.bits @[SIGMA.scala 39:9]
      buf_valid <= UInt<1>("h1") @[SIGMA.scala 40:15]
    wire out : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 43:25]
    node mul_valid = and(buf_valid, io.in_data.valid) @[SIGMA.scala 44:37]
    node mul_a = mux(mul_valid, io.in_data.bits, UInt<1>("h0")) @[SIGMA.scala 45:26]
    node mul_b = mux(mul_valid, buf, UInt<1>("h0")) @[SIGMA.scala 46:26]
    node _mul_T = mul(mul_b, UInt<1>("h0")) @[Arithmetic.scala 40:49]
    node _mul_T_1 = add(_mul_T, mul_a) @[Arithmetic.scala 40:54]
    node mul = tail(_mul_T_1, 1) @[Arithmetic.scala 40:54]
    out.valid <= mul_valid @[SIGMA.scala 48:13]
    out.bits <= mul @[SIGMA.scala 49:13]
    io.out_data <= out @[SIGMA.scala 50:15]

  module PE_6 :
    input clock : Clock
    input reset : Reset
    output io : { flip in_data : { valid : UInt<1>, bits : UInt<8>}, flip in_stationary : UInt<1>, out_data : { valid : UInt<1>, bits : UInt<8>}}

    reg buf : UInt<8>, clock with :
      reset => (UInt<1>("h0"), buf) @[SIGMA.scala 35:24]
    reg buf_valid : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[SIGMA.scala 36:34]
    node _T = and(io.in_stationary, io.in_data.valid) @[SIGMA.scala 38:25]
    when _T : @[SIGMA.scala 38:46]
      buf <= io.in_data.bits @[SIGMA.scala 39:9]
      buf_valid <= UInt<1>("h1") @[SIGMA.scala 40:15]
    wire out : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 43:25]
    node mul_valid = and(buf_valid, io.in_data.valid) @[SIGMA.scala 44:37]
    node mul_a = mux(mul_valid, io.in_data.bits, UInt<1>("h0")) @[SIGMA.scala 45:26]
    node mul_b = mux(mul_valid, buf, UInt<1>("h0")) @[SIGMA.scala 46:26]
    node _mul_T = mul(mul_b, UInt<1>("h0")) @[Arithmetic.scala 40:49]
    node _mul_T_1 = add(_mul_T, mul_a) @[Arithmetic.scala 40:54]
    node mul = tail(_mul_T_1, 1) @[Arithmetic.scala 40:54]
    out.valid <= mul_valid @[SIGMA.scala 48:13]
    out.bits <= mul @[SIGMA.scala 49:13]
    io.out_data <= out @[SIGMA.scala 50:15]

  module PE_7 :
    input clock : Clock
    input reset : Reset
    output io : { flip in_data : { valid : UInt<1>, bits : UInt<8>}, flip in_stationary : UInt<1>, out_data : { valid : UInt<1>, bits : UInt<8>}}

    reg buf : UInt<8>, clock with :
      reset => (UInt<1>("h0"), buf) @[SIGMA.scala 35:24]
    reg buf_valid : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[SIGMA.scala 36:34]
    node _T = and(io.in_stationary, io.in_data.valid) @[SIGMA.scala 38:25]
    when _T : @[SIGMA.scala 38:46]
      buf <= io.in_data.bits @[SIGMA.scala 39:9]
      buf_valid <= UInt<1>("h1") @[SIGMA.scala 40:15]
    wire out : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 43:25]
    node mul_valid = and(buf_valid, io.in_data.valid) @[SIGMA.scala 44:37]
    node mul_a = mux(mul_valid, io.in_data.bits, UInt<1>("h0")) @[SIGMA.scala 45:26]
    node mul_b = mux(mul_valid, buf, UInt<1>("h0")) @[SIGMA.scala 46:26]
    node _mul_T = mul(mul_b, UInt<1>("h0")) @[Arithmetic.scala 40:49]
    node _mul_T_1 = add(_mul_T, mul_a) @[Arithmetic.scala 40:54]
    node mul = tail(_mul_T_1, 1) @[Arithmetic.scala 40:54]
    out.valid <= mul_valid @[SIGMA.scala 48:13]
    out.bits <= mul @[SIGMA.scala 49:13]
    io.out_data <= out @[SIGMA.scala 50:15]

  module FanNode :
    input clock : Clock
    input reset : Reset
    output io : { flip in_l : { valid : UInt<1>, bits : UInt<8>}, flip in_r : { valid : UInt<1>, bits : UInt<8>}, flip same_l_border : UInt<1>, flip same_l : UInt<1>, flip same_m : UInt<1>, flip same_r : UInt<1>, flip same_r_border : UInt<1>, flip left_child : UInt<1>, out_l : { valid : UInt<1>, bits : UInt<8>}, out_r : { valid : UInt<1>, bits : UInt<8>}, out_final : { valid : UInt<1>, bits : UInt<8>}, out_same_l_border : UInt<1>, out_same : UInt<1>, out_same_r_border : UInt<1>}

    node _io_out_same_T = and(io.same_l, io.same_m) @[SIGMA.scala 73:28]
    node _io_out_same_T_1 = and(_io_out_same_T, io.same_r) @[SIGMA.scala 73:41]
    io.out_same <= _io_out_same_T_1 @[SIGMA.scala 73:15]
    io.out_same_l_border <= io.same_l_border @[SIGMA.scala 74:24]
    io.out_same_r_border <= io.same_r_border @[SIGMA.scala 75:24]
    wire NoOutput : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 76:30]
    NoOutput.valid <= UInt<1>("h0") @[SIGMA.scala 77:18]
    NoOutput.bits is invalid @[SIGMA.scala 78:17]
    io.out_l <= NoOutput @[SIGMA.scala 79:12]
    io.out_r <= NoOutput @[SIGMA.scala 80:12]
    io.out_final <= NoOutput @[SIGMA.scala 81:16]
    node merge_l = and(io.same_l_border, io.same_l) @[SIGMA.scala 82:42]
    node merge_r = and(io.same_r_border, io.same_r) @[SIGMA.scala 83:42]
    when io.same_m : @[SIGMA.scala 84:19]
      node _T = and(io.in_l.valid, io.in_r.valid) @[SIGMA.scala 85:24]
      when _T : @[SIGMA.scala 85:42]
        wire sum : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 86:21]
        sum.valid <= UInt<1>("h1") @[SIGMA.scala 87:17]
        node _sum_bits_T = add(io.in_l.bits, io.in_r.bits) @[Arithmetic.scala 41:38]
        node _sum_bits_T_1 = tail(_sum_bits_T, 1) @[Arithmetic.scala 41:38]
        sum.bits <= _sum_bits_T_1 @[SIGMA.scala 88:16]
        node _T_1 = and(merge_l, merge_r) @[SIGMA.scala 89:20]
        when _T_1 : @[SIGMA.scala 89:32]
          when io.left_child : @[SIGMA.scala 90:29]
            io.out_r <= sum @[SIGMA.scala 91:20]
          else :
            io.out_l <= sum @[SIGMA.scala 93:20]
        else :
          when merge_l : @[SIGMA.scala 95:28]
            io.out_l <= sum @[SIGMA.scala 96:18]
          else :
            when merge_r : @[SIGMA.scala 97:28]
              io.out_r <= sum @[SIGMA.scala 98:18]
            else :
              io.out_final <= sum @[SIGMA.scala 100:22]
    else :
      when merge_l : @[SIGMA.scala 104:19]
        io.out_l <= io.in_l @[SIGMA.scala 105:16]
      else :
        io.out_final <= io.in_l @[SIGMA.scala 107:20]
      when io.same_r : @[SIGMA.scala 109:21]
        io.out_r <= io.in_r @[SIGMA.scala 110:16]

  module FanNodeWrapper :
    input clock : Clock
    input reset : Reset
    output io : { flip in_l : { valid : UInt<1>, bits : UInt<8>}[1], flip in_r : { valid : UInt<1>, bits : UInt<8>}[1], flip same_l_border : UInt<1>, flip same_l : UInt<1>, flip same_m : UInt<1>, flip same_r : UInt<1>, flip same_r_border : UInt<1>, flip left_child : UInt<1>, out_l : { valid : UInt<1>, bits : UInt<8>}, out_r : { valid : UInt<1>, bits : UInt<8>}, out_data : { valid : UInt<1>, bits : UInt<8>}, out_same_l_border : UInt<1>, out_same : UInt<1>, out_same_r_border : UInt<1>}

    inst inner of FanNode @[SIGMA.scala 148:31]
    inner.clock <= clock
    inner.reset <= reset
    inner.io.in_l.bits <= io.in_l[0].bits @[SIGMA.scala 149:19]
    inner.io.in_l.valid <= io.in_l[0].valid @[SIGMA.scala 149:19]
    inner.io.in_r.bits <= io.in_r[0].bits @[SIGMA.scala 150:19]
    inner.io.in_r.valid <= io.in_r[0].valid @[SIGMA.scala 150:19]
    inner.io.same_l <= io.same_l @[SIGMA.scala 151:21]
    inner.io.same_r <= io.same_r @[SIGMA.scala 152:21]
    inner.io.same_m <= io.same_m @[SIGMA.scala 153:21]
    inner.io.same_l_border <= io.same_l_border @[SIGMA.scala 154:28]
    inner.io.same_r_border <= io.same_r_border @[SIGMA.scala 155:28]
    inner.io.left_child <= io.left_child @[SIGMA.scala 156:25]
    reg io_out_l_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_l_v <= inner.io.out_l.valid @[Valid.scala 130:22]
    reg io_out_l_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_l_b) @[Reg.scala 19:16]
    when inner.io.out_l.valid : @[Reg.scala 20:18]
      io_out_l_b <= inner.io.out_l.bits @[Reg.scala 20:22]
    wire io_out_l_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_l_out.valid <= io_out_l_v @[Valid.scala 126:17]
    io_out_l_out.bits <= io_out_l_b @[Valid.scala 127:16]
    io.out_l <= io_out_l_out @[SIGMA.scala 157:14]
    reg io_out_r_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_r_v <= inner.io.out_r.valid @[Valid.scala 130:22]
    reg io_out_r_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_r_b) @[Reg.scala 19:16]
    when inner.io.out_r.valid : @[Reg.scala 20:18]
      io_out_r_b <= inner.io.out_r.bits @[Reg.scala 20:22]
    wire io_out_r_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_r_out.valid <= io_out_r_v @[Valid.scala 126:17]
    io_out_r_out.bits <= io_out_r_b @[Valid.scala 127:16]
    io.out_r <= io_out_r_out @[SIGMA.scala 158:14]
    reg io_out_data_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_data_v <= inner.io.out_final.valid @[Valid.scala 130:22]
    reg io_out_data_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_data_b) @[Reg.scala 19:16]
    when inner.io.out_final.valid : @[Reg.scala 20:18]
      io_out_data_b <= inner.io.out_final.bits @[Reg.scala 20:22]
    reg io_out_data_outPipe_valid : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_data_outPipe_valid <= io_out_data_v @[Valid.scala 130:22]
    reg io_out_data_outPipe_bits : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_data_outPipe_bits) @[Reg.scala 19:16]
    when io_out_data_v : @[Reg.scala 20:18]
      io_out_data_outPipe_bits <= io_out_data_b @[Reg.scala 20:22]
    wire io_out_data_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_data_out.valid <= io_out_data_outPipe_valid @[Valid.scala 126:17]
    io_out_data_out.bits <= io_out_data_outPipe_bits @[Valid.scala 127:16]
    io.out_data <= io_out_data_out @[SIGMA.scala 159:17]
    reg io_out_same_l_border_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_l_border_REG) @[SIGMA.scala 160:36]
    io_out_same_l_border_REG <= inner.io.out_same_l_border @[SIGMA.scala 160:36]
    io.out_same_l_border <= io_out_same_l_border_REG @[SIGMA.scala 160:26]
    reg io_out_same_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_REG) @[SIGMA.scala 161:27]
    io_out_same_REG <= inner.io.out_same @[SIGMA.scala 161:27]
    io.out_same <= io_out_same_REG @[SIGMA.scala 161:17]
    reg io_out_same_r_border_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_r_border_REG) @[SIGMA.scala 162:36]
    io_out_same_r_border_REG <= inner.io.out_same_r_border @[SIGMA.scala 162:36]
    io.out_same_r_border <= io_out_same_r_border_REG @[SIGMA.scala 162:26]

  module FanNode_1 :
    input clock : Clock
    input reset : Reset
    output io : { flip in_l : { valid : UInt<1>, bits : UInt<8>}, flip in_r : { valid : UInt<1>, bits : UInt<8>}, flip same_l_border : UInt<1>, flip same_l : UInt<1>, flip same_m : UInt<1>, flip same_r : UInt<1>, flip same_r_border : UInt<1>, flip left_child : UInt<1>, out_l : { valid : UInt<1>, bits : UInt<8>}, out_r : { valid : UInt<1>, bits : UInt<8>}, out_final : { valid : UInt<1>, bits : UInt<8>}, out_same_l_border : UInt<1>, out_same : UInt<1>, out_same_r_border : UInt<1>}

    node _io_out_same_T = and(io.same_l, io.same_m) @[SIGMA.scala 73:28]
    node _io_out_same_T_1 = and(_io_out_same_T, io.same_r) @[SIGMA.scala 73:41]
    io.out_same <= _io_out_same_T_1 @[SIGMA.scala 73:15]
    io.out_same_l_border <= io.same_l_border @[SIGMA.scala 74:24]
    io.out_same_r_border <= io.same_r_border @[SIGMA.scala 75:24]
    wire NoOutput : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 76:30]
    NoOutput.valid <= UInt<1>("h0") @[SIGMA.scala 77:18]
    NoOutput.bits is invalid @[SIGMA.scala 78:17]
    io.out_l <= NoOutput @[SIGMA.scala 79:12]
    io.out_r <= NoOutput @[SIGMA.scala 80:12]
    io.out_final <= NoOutput @[SIGMA.scala 81:16]
    node merge_l = and(io.same_l_border, io.same_l) @[SIGMA.scala 82:42]
    node merge_r = and(io.same_r_border, io.same_r) @[SIGMA.scala 83:42]
    when io.same_m : @[SIGMA.scala 84:19]
      node _T = and(io.in_l.valid, io.in_r.valid) @[SIGMA.scala 85:24]
      when _T : @[SIGMA.scala 85:42]
        wire sum : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 86:21]
        sum.valid <= UInt<1>("h1") @[SIGMA.scala 87:17]
        node _sum_bits_T = add(io.in_l.bits, io.in_r.bits) @[Arithmetic.scala 41:38]
        node _sum_bits_T_1 = tail(_sum_bits_T, 1) @[Arithmetic.scala 41:38]
        sum.bits <= _sum_bits_T_1 @[SIGMA.scala 88:16]
        node _T_1 = and(merge_l, merge_r) @[SIGMA.scala 89:20]
        when _T_1 : @[SIGMA.scala 89:32]
          when io.left_child : @[SIGMA.scala 90:29]
            io.out_r <= sum @[SIGMA.scala 91:20]
          else :
            io.out_l <= sum @[SIGMA.scala 93:20]
        else :
          when merge_l : @[SIGMA.scala 95:28]
            io.out_l <= sum @[SIGMA.scala 96:18]
          else :
            when merge_r : @[SIGMA.scala 97:28]
              io.out_r <= sum @[SIGMA.scala 98:18]
            else :
              io.out_final <= sum @[SIGMA.scala 100:22]
    else :
      when merge_l : @[SIGMA.scala 104:19]
        io.out_l <= io.in_l @[SIGMA.scala 105:16]
      else :
        io.out_final <= io.in_l @[SIGMA.scala 107:20]
      when io.same_r : @[SIGMA.scala 109:21]
        io.out_r <= io.in_r @[SIGMA.scala 110:16]

  module FanNodeWrapper_1 :
    input clock : Clock
    input reset : Reset
    output io : { flip in_l : { valid : UInt<1>, bits : UInt<8>}[1], flip in_r : { valid : UInt<1>, bits : UInt<8>}[1], flip same_l_border : UInt<1>, flip same_l : UInt<1>, flip same_m : UInt<1>, flip same_r : UInt<1>, flip same_r_border : UInt<1>, flip left_child : UInt<1>, out_l : { valid : UInt<1>, bits : UInt<8>}, out_r : { valid : UInt<1>, bits : UInt<8>}, out_data : { valid : UInt<1>, bits : UInt<8>}, out_same_l_border : UInt<1>, out_same : UInt<1>, out_same_r_border : UInt<1>}

    inst inner of FanNode_1 @[SIGMA.scala 148:31]
    inner.clock <= clock
    inner.reset <= reset
    inner.io.in_l.bits <= io.in_l[0].bits @[SIGMA.scala 149:19]
    inner.io.in_l.valid <= io.in_l[0].valid @[SIGMA.scala 149:19]
    inner.io.in_r.bits <= io.in_r[0].bits @[SIGMA.scala 150:19]
    inner.io.in_r.valid <= io.in_r[0].valid @[SIGMA.scala 150:19]
    inner.io.same_l <= io.same_l @[SIGMA.scala 151:21]
    inner.io.same_r <= io.same_r @[SIGMA.scala 152:21]
    inner.io.same_m <= io.same_m @[SIGMA.scala 153:21]
    inner.io.same_l_border <= io.same_l_border @[SIGMA.scala 154:28]
    inner.io.same_r_border <= io.same_r_border @[SIGMA.scala 155:28]
    inner.io.left_child <= io.left_child @[SIGMA.scala 156:25]
    reg io_out_l_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_l_v <= inner.io.out_l.valid @[Valid.scala 130:22]
    reg io_out_l_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_l_b) @[Reg.scala 19:16]
    when inner.io.out_l.valid : @[Reg.scala 20:18]
      io_out_l_b <= inner.io.out_l.bits @[Reg.scala 20:22]
    wire io_out_l_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_l_out.valid <= io_out_l_v @[Valid.scala 126:17]
    io_out_l_out.bits <= io_out_l_b @[Valid.scala 127:16]
    io.out_l <= io_out_l_out @[SIGMA.scala 157:14]
    reg io_out_r_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_r_v <= inner.io.out_r.valid @[Valid.scala 130:22]
    reg io_out_r_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_r_b) @[Reg.scala 19:16]
    when inner.io.out_r.valid : @[Reg.scala 20:18]
      io_out_r_b <= inner.io.out_r.bits @[Reg.scala 20:22]
    wire io_out_r_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_r_out.valid <= io_out_r_v @[Valid.scala 126:17]
    io_out_r_out.bits <= io_out_r_b @[Valid.scala 127:16]
    io.out_r <= io_out_r_out @[SIGMA.scala 158:14]
    reg io_out_data_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_data_v <= inner.io.out_final.valid @[Valid.scala 130:22]
    reg io_out_data_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_data_b) @[Reg.scala 19:16]
    when inner.io.out_final.valid : @[Reg.scala 20:18]
      io_out_data_b <= inner.io.out_final.bits @[Reg.scala 20:22]
    reg io_out_data_outPipe_valid : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_data_outPipe_valid <= io_out_data_v @[Valid.scala 130:22]
    reg io_out_data_outPipe_bits : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_data_outPipe_bits) @[Reg.scala 19:16]
    when io_out_data_v : @[Reg.scala 20:18]
      io_out_data_outPipe_bits <= io_out_data_b @[Reg.scala 20:22]
    wire io_out_data_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_data_out.valid <= io_out_data_outPipe_valid @[Valid.scala 126:17]
    io_out_data_out.bits <= io_out_data_outPipe_bits @[Valid.scala 127:16]
    io.out_data <= io_out_data_out @[SIGMA.scala 159:17]
    reg io_out_same_l_border_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_l_border_REG) @[SIGMA.scala 160:36]
    io_out_same_l_border_REG <= inner.io.out_same_l_border @[SIGMA.scala 160:36]
    io.out_same_l_border <= io_out_same_l_border_REG @[SIGMA.scala 160:26]
    reg io_out_same_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_REG) @[SIGMA.scala 161:27]
    io_out_same_REG <= inner.io.out_same @[SIGMA.scala 161:27]
    io.out_same <= io_out_same_REG @[SIGMA.scala 161:17]
    reg io_out_same_r_border_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_r_border_REG) @[SIGMA.scala 162:36]
    io_out_same_r_border_REG <= inner.io.out_same_r_border @[SIGMA.scala 162:36]
    io.out_same_r_border <= io_out_same_r_border_REG @[SIGMA.scala 162:26]

  module FanNode_2 :
    input clock : Clock
    input reset : Reset
    output io : { flip in_l : { valid : UInt<1>, bits : UInt<8>}, flip in_r : { valid : UInt<1>, bits : UInt<8>}, flip same_l_border : UInt<1>, flip same_l : UInt<1>, flip same_m : UInt<1>, flip same_r : UInt<1>, flip same_r_border : UInt<1>, flip left_child : UInt<1>, out_l : { valid : UInt<1>, bits : UInt<8>}, out_r : { valid : UInt<1>, bits : UInt<8>}, out_final : { valid : UInt<1>, bits : UInt<8>}, out_same_l_border : UInt<1>, out_same : UInt<1>, out_same_r_border : UInt<1>}

    node _io_out_same_T = and(io.same_l, io.same_m) @[SIGMA.scala 73:28]
    node _io_out_same_T_1 = and(_io_out_same_T, io.same_r) @[SIGMA.scala 73:41]
    io.out_same <= _io_out_same_T_1 @[SIGMA.scala 73:15]
    io.out_same_l_border <= io.same_l_border @[SIGMA.scala 74:24]
    io.out_same_r_border <= io.same_r_border @[SIGMA.scala 75:24]
    wire NoOutput : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 76:30]
    NoOutput.valid <= UInt<1>("h0") @[SIGMA.scala 77:18]
    NoOutput.bits is invalid @[SIGMA.scala 78:17]
    io.out_l <= NoOutput @[SIGMA.scala 79:12]
    io.out_r <= NoOutput @[SIGMA.scala 80:12]
    io.out_final <= NoOutput @[SIGMA.scala 81:16]
    node merge_l = and(io.same_l_border, io.same_l) @[SIGMA.scala 82:42]
    node merge_r = and(io.same_r_border, io.same_r) @[SIGMA.scala 83:42]
    when io.same_m : @[SIGMA.scala 84:19]
      node _T = and(io.in_l.valid, io.in_r.valid) @[SIGMA.scala 85:24]
      when _T : @[SIGMA.scala 85:42]
        wire sum : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 86:21]
        sum.valid <= UInt<1>("h1") @[SIGMA.scala 87:17]
        node _sum_bits_T = add(io.in_l.bits, io.in_r.bits) @[Arithmetic.scala 41:38]
        node _sum_bits_T_1 = tail(_sum_bits_T, 1) @[Arithmetic.scala 41:38]
        sum.bits <= _sum_bits_T_1 @[SIGMA.scala 88:16]
        node _T_1 = and(merge_l, merge_r) @[SIGMA.scala 89:20]
        when _T_1 : @[SIGMA.scala 89:32]
          when io.left_child : @[SIGMA.scala 90:29]
            io.out_r <= sum @[SIGMA.scala 91:20]
          else :
            io.out_l <= sum @[SIGMA.scala 93:20]
        else :
          when merge_l : @[SIGMA.scala 95:28]
            io.out_l <= sum @[SIGMA.scala 96:18]
          else :
            when merge_r : @[SIGMA.scala 97:28]
              io.out_r <= sum @[SIGMA.scala 98:18]
            else :
              io.out_final <= sum @[SIGMA.scala 100:22]
    else :
      when merge_l : @[SIGMA.scala 104:19]
        io.out_l <= io.in_l @[SIGMA.scala 105:16]
      else :
        io.out_final <= io.in_l @[SIGMA.scala 107:20]
      when io.same_r : @[SIGMA.scala 109:21]
        io.out_r <= io.in_r @[SIGMA.scala 110:16]

  module FanNodeWrapper_2 :
    input clock : Clock
    input reset : Reset
    output io : { flip in_l : { valid : UInt<1>, bits : UInt<8>}[1], flip in_r : { valid : UInt<1>, bits : UInt<8>}[1], flip same_l_border : UInt<1>, flip same_l : UInt<1>, flip same_m : UInt<1>, flip same_r : UInt<1>, flip same_r_border : UInt<1>, flip left_child : UInt<1>, out_l : { valid : UInt<1>, bits : UInt<8>}, out_r : { valid : UInt<1>, bits : UInt<8>}, out_data : { valid : UInt<1>, bits : UInt<8>}, out_same_l_border : UInt<1>, out_same : UInt<1>, out_same_r_border : UInt<1>}

    inst inner of FanNode_2 @[SIGMA.scala 148:31]
    inner.clock <= clock
    inner.reset <= reset
    inner.io.in_l.bits <= io.in_l[0].bits @[SIGMA.scala 149:19]
    inner.io.in_l.valid <= io.in_l[0].valid @[SIGMA.scala 149:19]
    inner.io.in_r.bits <= io.in_r[0].bits @[SIGMA.scala 150:19]
    inner.io.in_r.valid <= io.in_r[0].valid @[SIGMA.scala 150:19]
    inner.io.same_l <= io.same_l @[SIGMA.scala 151:21]
    inner.io.same_r <= io.same_r @[SIGMA.scala 152:21]
    inner.io.same_m <= io.same_m @[SIGMA.scala 153:21]
    inner.io.same_l_border <= io.same_l_border @[SIGMA.scala 154:28]
    inner.io.same_r_border <= io.same_r_border @[SIGMA.scala 155:28]
    inner.io.left_child <= io.left_child @[SIGMA.scala 156:25]
    reg io_out_l_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_l_v <= inner.io.out_l.valid @[Valid.scala 130:22]
    reg io_out_l_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_l_b) @[Reg.scala 19:16]
    when inner.io.out_l.valid : @[Reg.scala 20:18]
      io_out_l_b <= inner.io.out_l.bits @[Reg.scala 20:22]
    wire io_out_l_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_l_out.valid <= io_out_l_v @[Valid.scala 126:17]
    io_out_l_out.bits <= io_out_l_b @[Valid.scala 127:16]
    io.out_l <= io_out_l_out @[SIGMA.scala 157:14]
    reg io_out_r_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_r_v <= inner.io.out_r.valid @[Valid.scala 130:22]
    reg io_out_r_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_r_b) @[Reg.scala 19:16]
    when inner.io.out_r.valid : @[Reg.scala 20:18]
      io_out_r_b <= inner.io.out_r.bits @[Reg.scala 20:22]
    wire io_out_r_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_r_out.valid <= io_out_r_v @[Valid.scala 126:17]
    io_out_r_out.bits <= io_out_r_b @[Valid.scala 127:16]
    io.out_r <= io_out_r_out @[SIGMA.scala 158:14]
    reg io_out_data_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_data_v <= inner.io.out_final.valid @[Valid.scala 130:22]
    reg io_out_data_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_data_b) @[Reg.scala 19:16]
    when inner.io.out_final.valid : @[Reg.scala 20:18]
      io_out_data_b <= inner.io.out_final.bits @[Reg.scala 20:22]
    reg io_out_data_outPipe_valid : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_data_outPipe_valid <= io_out_data_v @[Valid.scala 130:22]
    reg io_out_data_outPipe_bits : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_data_outPipe_bits) @[Reg.scala 19:16]
    when io_out_data_v : @[Reg.scala 20:18]
      io_out_data_outPipe_bits <= io_out_data_b @[Reg.scala 20:22]
    wire io_out_data_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_data_out.valid <= io_out_data_outPipe_valid @[Valid.scala 126:17]
    io_out_data_out.bits <= io_out_data_outPipe_bits @[Valid.scala 127:16]
    io.out_data <= io_out_data_out @[SIGMA.scala 159:17]
    reg io_out_same_l_border_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_l_border_REG) @[SIGMA.scala 160:36]
    io_out_same_l_border_REG <= inner.io.out_same_l_border @[SIGMA.scala 160:36]
    io.out_same_l_border <= io_out_same_l_border_REG @[SIGMA.scala 160:26]
    reg io_out_same_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_REG) @[SIGMA.scala 161:27]
    io_out_same_REG <= inner.io.out_same @[SIGMA.scala 161:27]
    io.out_same <= io_out_same_REG @[SIGMA.scala 161:17]
    reg io_out_same_r_border_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_r_border_REG) @[SIGMA.scala 162:36]
    io_out_same_r_border_REG <= inner.io.out_same_r_border @[SIGMA.scala 162:36]
    io.out_same_r_border <= io_out_same_r_border_REG @[SIGMA.scala 162:26]

  module FanNode_3 :
    input clock : Clock
    input reset : Reset
    output io : { flip in_l : { valid : UInt<1>, bits : UInt<8>}, flip in_r : { valid : UInt<1>, bits : UInt<8>}, flip same_l_border : UInt<1>, flip same_l : UInt<1>, flip same_m : UInt<1>, flip same_r : UInt<1>, flip same_r_border : UInt<1>, flip left_child : UInt<1>, out_l : { valid : UInt<1>, bits : UInt<8>}, out_r : { valid : UInt<1>, bits : UInt<8>}, out_final : { valid : UInt<1>, bits : UInt<8>}, out_same_l_border : UInt<1>, out_same : UInt<1>, out_same_r_border : UInt<1>}

    node _io_out_same_T = and(io.same_l, io.same_m) @[SIGMA.scala 73:28]
    node _io_out_same_T_1 = and(_io_out_same_T, io.same_r) @[SIGMA.scala 73:41]
    io.out_same <= _io_out_same_T_1 @[SIGMA.scala 73:15]
    io.out_same_l_border <= io.same_l_border @[SIGMA.scala 74:24]
    io.out_same_r_border <= io.same_r_border @[SIGMA.scala 75:24]
    wire NoOutput : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 76:30]
    NoOutput.valid <= UInt<1>("h0") @[SIGMA.scala 77:18]
    NoOutput.bits is invalid @[SIGMA.scala 78:17]
    io.out_l <= NoOutput @[SIGMA.scala 79:12]
    io.out_r <= NoOutput @[SIGMA.scala 80:12]
    io.out_final <= NoOutput @[SIGMA.scala 81:16]
    node merge_l = and(io.same_l_border, io.same_l) @[SIGMA.scala 82:42]
    node merge_r = and(io.same_r_border, io.same_r) @[SIGMA.scala 83:42]
    when io.same_m : @[SIGMA.scala 84:19]
      node _T = and(io.in_l.valid, io.in_r.valid) @[SIGMA.scala 85:24]
      when _T : @[SIGMA.scala 85:42]
        wire sum : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 86:21]
        sum.valid <= UInt<1>("h1") @[SIGMA.scala 87:17]
        node _sum_bits_T = add(io.in_l.bits, io.in_r.bits) @[Arithmetic.scala 41:38]
        node _sum_bits_T_1 = tail(_sum_bits_T, 1) @[Arithmetic.scala 41:38]
        sum.bits <= _sum_bits_T_1 @[SIGMA.scala 88:16]
        node _T_1 = and(merge_l, merge_r) @[SIGMA.scala 89:20]
        when _T_1 : @[SIGMA.scala 89:32]
          when io.left_child : @[SIGMA.scala 90:29]
            io.out_r <= sum @[SIGMA.scala 91:20]
          else :
            io.out_l <= sum @[SIGMA.scala 93:20]
        else :
          when merge_l : @[SIGMA.scala 95:28]
            io.out_l <= sum @[SIGMA.scala 96:18]
          else :
            when merge_r : @[SIGMA.scala 97:28]
              io.out_r <= sum @[SIGMA.scala 98:18]
            else :
              io.out_final <= sum @[SIGMA.scala 100:22]
    else :
      when merge_l : @[SIGMA.scala 104:19]
        io.out_l <= io.in_l @[SIGMA.scala 105:16]
      else :
        io.out_final <= io.in_l @[SIGMA.scala 107:20]
      when io.same_r : @[SIGMA.scala 109:21]
        io.out_r <= io.in_r @[SIGMA.scala 110:16]

  module FanNodeWrapper_3 :
    input clock : Clock
    input reset : Reset
    output io : { flip in_l : { valid : UInt<1>, bits : UInt<8>}[1], flip in_r : { valid : UInt<1>, bits : UInt<8>}[1], flip same_l_border : UInt<1>, flip same_l : UInt<1>, flip same_m : UInt<1>, flip same_r : UInt<1>, flip same_r_border : UInt<1>, flip left_child : UInt<1>, out_l : { valid : UInt<1>, bits : UInt<8>}, out_r : { valid : UInt<1>, bits : UInt<8>}, out_data : { valid : UInt<1>, bits : UInt<8>}, out_same_l_border : UInt<1>, out_same : UInt<1>, out_same_r_border : UInt<1>}

    inst inner of FanNode_3 @[SIGMA.scala 148:31]
    inner.clock <= clock
    inner.reset <= reset
    inner.io.in_l.bits <= io.in_l[0].bits @[SIGMA.scala 149:19]
    inner.io.in_l.valid <= io.in_l[0].valid @[SIGMA.scala 149:19]
    inner.io.in_r.bits <= io.in_r[0].bits @[SIGMA.scala 150:19]
    inner.io.in_r.valid <= io.in_r[0].valid @[SIGMA.scala 150:19]
    inner.io.same_l <= io.same_l @[SIGMA.scala 151:21]
    inner.io.same_r <= io.same_r @[SIGMA.scala 152:21]
    inner.io.same_m <= io.same_m @[SIGMA.scala 153:21]
    inner.io.same_l_border <= io.same_l_border @[SIGMA.scala 154:28]
    inner.io.same_r_border <= io.same_r_border @[SIGMA.scala 155:28]
    inner.io.left_child <= io.left_child @[SIGMA.scala 156:25]
    reg io_out_l_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_l_v <= inner.io.out_l.valid @[Valid.scala 130:22]
    reg io_out_l_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_l_b) @[Reg.scala 19:16]
    when inner.io.out_l.valid : @[Reg.scala 20:18]
      io_out_l_b <= inner.io.out_l.bits @[Reg.scala 20:22]
    wire io_out_l_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_l_out.valid <= io_out_l_v @[Valid.scala 126:17]
    io_out_l_out.bits <= io_out_l_b @[Valid.scala 127:16]
    io.out_l <= io_out_l_out @[SIGMA.scala 157:14]
    reg io_out_r_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_r_v <= inner.io.out_r.valid @[Valid.scala 130:22]
    reg io_out_r_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_r_b) @[Reg.scala 19:16]
    when inner.io.out_r.valid : @[Reg.scala 20:18]
      io_out_r_b <= inner.io.out_r.bits @[Reg.scala 20:22]
    wire io_out_r_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_r_out.valid <= io_out_r_v @[Valid.scala 126:17]
    io_out_r_out.bits <= io_out_r_b @[Valid.scala 127:16]
    io.out_r <= io_out_r_out @[SIGMA.scala 158:14]
    reg io_out_data_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_data_v <= inner.io.out_final.valid @[Valid.scala 130:22]
    reg io_out_data_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_data_b) @[Reg.scala 19:16]
    when inner.io.out_final.valid : @[Reg.scala 20:18]
      io_out_data_b <= inner.io.out_final.bits @[Reg.scala 20:22]
    reg io_out_data_outPipe_valid : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_data_outPipe_valid <= io_out_data_v @[Valid.scala 130:22]
    reg io_out_data_outPipe_bits : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_data_outPipe_bits) @[Reg.scala 19:16]
    when io_out_data_v : @[Reg.scala 20:18]
      io_out_data_outPipe_bits <= io_out_data_b @[Reg.scala 20:22]
    wire io_out_data_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_data_out.valid <= io_out_data_outPipe_valid @[Valid.scala 126:17]
    io_out_data_out.bits <= io_out_data_outPipe_bits @[Valid.scala 127:16]
    io.out_data <= io_out_data_out @[SIGMA.scala 159:17]
    reg io_out_same_l_border_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_l_border_REG) @[SIGMA.scala 160:36]
    io_out_same_l_border_REG <= inner.io.out_same_l_border @[SIGMA.scala 160:36]
    io.out_same_l_border <= io_out_same_l_border_REG @[SIGMA.scala 160:26]
    reg io_out_same_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_REG) @[SIGMA.scala 161:27]
    io_out_same_REG <= inner.io.out_same @[SIGMA.scala 161:27]
    io.out_same <= io_out_same_REG @[SIGMA.scala 161:17]
    reg io_out_same_r_border_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_r_border_REG) @[SIGMA.scala 162:36]
    io_out_same_r_border_REG <= inner.io.out_same_r_border @[SIGMA.scala 162:36]
    io.out_same_r_border <= io_out_same_r_border_REG @[SIGMA.scala 162:26]

  module FanNode_4 :
    input clock : Clock
    input reset : Reset
    output io : { flip in_l : { valid : UInt<1>, bits : UInt<8>}, flip in_r : { valid : UInt<1>, bits : UInt<8>}, flip same_l_border : UInt<1>, flip same_l : UInt<1>, flip same_m : UInt<1>, flip same_r : UInt<1>, flip same_r_border : UInt<1>, flip left_child : UInt<1>, out_l : { valid : UInt<1>, bits : UInt<8>}, out_r : { valid : UInt<1>, bits : UInt<8>}, out_final : { valid : UInt<1>, bits : UInt<8>}, out_same_l_border : UInt<1>, out_same : UInt<1>, out_same_r_border : UInt<1>}

    node _io_out_same_T = and(io.same_l, io.same_m) @[SIGMA.scala 73:28]
    node _io_out_same_T_1 = and(_io_out_same_T, io.same_r) @[SIGMA.scala 73:41]
    io.out_same <= _io_out_same_T_1 @[SIGMA.scala 73:15]
    io.out_same_l_border <= io.same_l_border @[SIGMA.scala 74:24]
    io.out_same_r_border <= io.same_r_border @[SIGMA.scala 75:24]
    wire NoOutput : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 76:30]
    NoOutput.valid <= UInt<1>("h0") @[SIGMA.scala 77:18]
    NoOutput.bits is invalid @[SIGMA.scala 78:17]
    io.out_l <= NoOutput @[SIGMA.scala 79:12]
    io.out_r <= NoOutput @[SIGMA.scala 80:12]
    io.out_final <= NoOutput @[SIGMA.scala 81:16]
    node merge_l = and(io.same_l_border, io.same_l) @[SIGMA.scala 82:42]
    node merge_r = and(io.same_r_border, io.same_r) @[SIGMA.scala 83:42]
    when io.same_m : @[SIGMA.scala 84:19]
      node _T = and(io.in_l.valid, io.in_r.valid) @[SIGMA.scala 85:24]
      when _T : @[SIGMA.scala 85:42]
        wire sum : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 86:21]
        sum.valid <= UInt<1>("h1") @[SIGMA.scala 87:17]
        node _sum_bits_T = add(io.in_l.bits, io.in_r.bits) @[Arithmetic.scala 41:38]
        node _sum_bits_T_1 = tail(_sum_bits_T, 1) @[Arithmetic.scala 41:38]
        sum.bits <= _sum_bits_T_1 @[SIGMA.scala 88:16]
        node _T_1 = and(merge_l, merge_r) @[SIGMA.scala 89:20]
        when _T_1 : @[SIGMA.scala 89:32]
          when io.left_child : @[SIGMA.scala 90:29]
            io.out_r <= sum @[SIGMA.scala 91:20]
          else :
            io.out_l <= sum @[SIGMA.scala 93:20]
        else :
          when merge_l : @[SIGMA.scala 95:28]
            io.out_l <= sum @[SIGMA.scala 96:18]
          else :
            when merge_r : @[SIGMA.scala 97:28]
              io.out_r <= sum @[SIGMA.scala 98:18]
            else :
              io.out_final <= sum @[SIGMA.scala 100:22]
    else :
      when merge_l : @[SIGMA.scala 104:19]
        io.out_l <= io.in_l @[SIGMA.scala 105:16]
      else :
        io.out_final <= io.in_l @[SIGMA.scala 107:20]
      when io.same_r : @[SIGMA.scala 109:21]
        io.out_r <= io.in_r @[SIGMA.scala 110:16]

  module FanNodeWrapper_4 :
    input clock : Clock
    input reset : Reset
    output io : { flip in_l : { valid : UInt<1>, bits : UInt<8>}[1], flip in_r : { valid : UInt<1>, bits : UInt<8>}[1], flip same_l_border : UInt<1>, flip same_l : UInt<1>, flip same_m : UInt<1>, flip same_r : UInt<1>, flip same_r_border : UInt<1>, flip left_child : UInt<1>, out_l : { valid : UInt<1>, bits : UInt<8>}, out_r : { valid : UInt<1>, bits : UInt<8>}, out_data : { valid : UInt<1>, bits : UInt<8>}, out_same_l_border : UInt<1>, out_same : UInt<1>, out_same_r_border : UInt<1>}

    inst inner of FanNode_4 @[SIGMA.scala 148:31]
    inner.clock <= clock
    inner.reset <= reset
    inner.io.in_l.bits <= io.in_l[0].bits @[SIGMA.scala 149:19]
    inner.io.in_l.valid <= io.in_l[0].valid @[SIGMA.scala 149:19]
    inner.io.in_r.bits <= io.in_r[0].bits @[SIGMA.scala 150:19]
    inner.io.in_r.valid <= io.in_r[0].valid @[SIGMA.scala 150:19]
    inner.io.same_l <= io.same_l @[SIGMA.scala 151:21]
    inner.io.same_r <= io.same_r @[SIGMA.scala 152:21]
    inner.io.same_m <= io.same_m @[SIGMA.scala 153:21]
    inner.io.same_l_border <= io.same_l_border @[SIGMA.scala 154:28]
    inner.io.same_r_border <= io.same_r_border @[SIGMA.scala 155:28]
    inner.io.left_child <= io.left_child @[SIGMA.scala 156:25]
    reg io_out_l_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_l_v <= inner.io.out_l.valid @[Valid.scala 130:22]
    reg io_out_l_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_l_b) @[Reg.scala 19:16]
    when inner.io.out_l.valid : @[Reg.scala 20:18]
      io_out_l_b <= inner.io.out_l.bits @[Reg.scala 20:22]
    wire io_out_l_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_l_out.valid <= io_out_l_v @[Valid.scala 126:17]
    io_out_l_out.bits <= io_out_l_b @[Valid.scala 127:16]
    io.out_l <= io_out_l_out @[SIGMA.scala 157:14]
    reg io_out_r_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_r_v <= inner.io.out_r.valid @[Valid.scala 130:22]
    reg io_out_r_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_r_b) @[Reg.scala 19:16]
    when inner.io.out_r.valid : @[Reg.scala 20:18]
      io_out_r_b <= inner.io.out_r.bits @[Reg.scala 20:22]
    wire io_out_r_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_r_out.valid <= io_out_r_v @[Valid.scala 126:17]
    io_out_r_out.bits <= io_out_r_b @[Valid.scala 127:16]
    io.out_r <= io_out_r_out @[SIGMA.scala 158:14]
    reg io_out_data_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_data_v <= inner.io.out_final.valid @[Valid.scala 130:22]
    reg io_out_data_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_data_b) @[Reg.scala 19:16]
    when inner.io.out_final.valid : @[Reg.scala 20:18]
      io_out_data_b <= inner.io.out_final.bits @[Reg.scala 20:22]
    wire io_out_data_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_data_out.valid <= io_out_data_v @[Valid.scala 126:17]
    io_out_data_out.bits <= io_out_data_b @[Valid.scala 127:16]
    io.out_data <= io_out_data_out @[SIGMA.scala 159:17]
    reg io_out_same_l_border_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_l_border_REG) @[SIGMA.scala 160:36]
    io_out_same_l_border_REG <= inner.io.out_same_l_border @[SIGMA.scala 160:36]
    io.out_same_l_border <= io_out_same_l_border_REG @[SIGMA.scala 160:26]
    reg io_out_same_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_REG) @[SIGMA.scala 161:27]
    io_out_same_REG <= inner.io.out_same @[SIGMA.scala 161:27]
    io.out_same <= io_out_same_REG @[SIGMA.scala 161:17]
    reg io_out_same_r_border_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_r_border_REG) @[SIGMA.scala 162:36]
    io_out_same_r_border_REG <= inner.io.out_same_r_border @[SIGMA.scala 162:36]
    io.out_same_r_border <= io_out_same_r_border_REG @[SIGMA.scala 162:26]

  module FanNode_5 :
    input clock : Clock
    input reset : Reset
    output io : { flip in_l : { valid : UInt<1>, bits : UInt<8>}, flip in_r : { valid : UInt<1>, bits : UInt<8>}, flip same_l_border : UInt<1>, flip same_l : UInt<1>, flip same_m : UInt<1>, flip same_r : UInt<1>, flip same_r_border : UInt<1>, flip left_child : UInt<1>, out_l : { valid : UInt<1>, bits : UInt<8>}, out_r : { valid : UInt<1>, bits : UInt<8>}, out_final : { valid : UInt<1>, bits : UInt<8>}, out_same_l_border : UInt<1>, out_same : UInt<1>, out_same_r_border : UInt<1>}

    node _io_out_same_T = and(io.same_l, io.same_m) @[SIGMA.scala 73:28]
    node _io_out_same_T_1 = and(_io_out_same_T, io.same_r) @[SIGMA.scala 73:41]
    io.out_same <= _io_out_same_T_1 @[SIGMA.scala 73:15]
    io.out_same_l_border <= io.same_l_border @[SIGMA.scala 74:24]
    io.out_same_r_border <= io.same_r_border @[SIGMA.scala 75:24]
    wire NoOutput : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 76:30]
    NoOutput.valid <= UInt<1>("h0") @[SIGMA.scala 77:18]
    NoOutput.bits is invalid @[SIGMA.scala 78:17]
    io.out_l <= NoOutput @[SIGMA.scala 79:12]
    io.out_r <= NoOutput @[SIGMA.scala 80:12]
    io.out_final <= NoOutput @[SIGMA.scala 81:16]
    node merge_l = and(io.same_l_border, io.same_l) @[SIGMA.scala 82:42]
    node merge_r = and(io.same_r_border, io.same_r) @[SIGMA.scala 83:42]
    when io.same_m : @[SIGMA.scala 84:19]
      node _T = and(io.in_l.valid, io.in_r.valid) @[SIGMA.scala 85:24]
      when _T : @[SIGMA.scala 85:42]
        wire sum : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 86:21]
        sum.valid <= UInt<1>("h1") @[SIGMA.scala 87:17]
        node _sum_bits_T = add(io.in_l.bits, io.in_r.bits) @[Arithmetic.scala 41:38]
        node _sum_bits_T_1 = tail(_sum_bits_T, 1) @[Arithmetic.scala 41:38]
        sum.bits <= _sum_bits_T_1 @[SIGMA.scala 88:16]
        node _T_1 = and(merge_l, merge_r) @[SIGMA.scala 89:20]
        when _T_1 : @[SIGMA.scala 89:32]
          when io.left_child : @[SIGMA.scala 90:29]
            io.out_r <= sum @[SIGMA.scala 91:20]
          else :
            io.out_l <= sum @[SIGMA.scala 93:20]
        else :
          when merge_l : @[SIGMA.scala 95:28]
            io.out_l <= sum @[SIGMA.scala 96:18]
          else :
            when merge_r : @[SIGMA.scala 97:28]
              io.out_r <= sum @[SIGMA.scala 98:18]
            else :
              io.out_final <= sum @[SIGMA.scala 100:22]
    else :
      when merge_l : @[SIGMA.scala 104:19]
        io.out_l <= io.in_l @[SIGMA.scala 105:16]
      else :
        io.out_final <= io.in_l @[SIGMA.scala 107:20]
      when io.same_r : @[SIGMA.scala 109:21]
        io.out_r <= io.in_r @[SIGMA.scala 110:16]

  module FanNodeWrapper_5 :
    input clock : Clock
    input reset : Reset
    output io : { flip in_l : { valid : UInt<1>, bits : UInt<8>}[1], flip in_r : { valid : UInt<1>, bits : UInt<8>}[1], flip same_l_border : UInt<1>, flip same_l : UInt<1>, flip same_m : UInt<1>, flip same_r : UInt<1>, flip same_r_border : UInt<1>, flip left_child : UInt<1>, out_l : { valid : UInt<1>, bits : UInt<8>}, out_r : { valid : UInt<1>, bits : UInt<8>}, out_data : { valid : UInt<1>, bits : UInt<8>}, out_same_l_border : UInt<1>, out_same : UInt<1>, out_same_r_border : UInt<1>}

    inst inner of FanNode_5 @[SIGMA.scala 148:31]
    inner.clock <= clock
    inner.reset <= reset
    inner.io.in_l.bits <= io.in_l[0].bits @[SIGMA.scala 149:19]
    inner.io.in_l.valid <= io.in_l[0].valid @[SIGMA.scala 149:19]
    inner.io.in_r.bits <= io.in_r[0].bits @[SIGMA.scala 150:19]
    inner.io.in_r.valid <= io.in_r[0].valid @[SIGMA.scala 150:19]
    inner.io.same_l <= io.same_l @[SIGMA.scala 151:21]
    inner.io.same_r <= io.same_r @[SIGMA.scala 152:21]
    inner.io.same_m <= io.same_m @[SIGMA.scala 153:21]
    inner.io.same_l_border <= io.same_l_border @[SIGMA.scala 154:28]
    inner.io.same_r_border <= io.same_r_border @[SIGMA.scala 155:28]
    inner.io.left_child <= io.left_child @[SIGMA.scala 156:25]
    reg io_out_l_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_l_v <= inner.io.out_l.valid @[Valid.scala 130:22]
    reg io_out_l_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_l_b) @[Reg.scala 19:16]
    when inner.io.out_l.valid : @[Reg.scala 20:18]
      io_out_l_b <= inner.io.out_l.bits @[Reg.scala 20:22]
    wire io_out_l_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_l_out.valid <= io_out_l_v @[Valid.scala 126:17]
    io_out_l_out.bits <= io_out_l_b @[Valid.scala 127:16]
    io.out_l <= io_out_l_out @[SIGMA.scala 157:14]
    reg io_out_r_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_r_v <= inner.io.out_r.valid @[Valid.scala 130:22]
    reg io_out_r_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_r_b) @[Reg.scala 19:16]
    when inner.io.out_r.valid : @[Reg.scala 20:18]
      io_out_r_b <= inner.io.out_r.bits @[Reg.scala 20:22]
    wire io_out_r_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_r_out.valid <= io_out_r_v @[Valid.scala 126:17]
    io_out_r_out.bits <= io_out_r_b @[Valid.scala 127:16]
    io.out_r <= io_out_r_out @[SIGMA.scala 158:14]
    reg io_out_data_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_data_v <= inner.io.out_final.valid @[Valid.scala 130:22]
    reg io_out_data_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_data_b) @[Reg.scala 19:16]
    when inner.io.out_final.valid : @[Reg.scala 20:18]
      io_out_data_b <= inner.io.out_final.bits @[Reg.scala 20:22]
    wire io_out_data_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_data_out.valid <= io_out_data_v @[Valid.scala 126:17]
    io_out_data_out.bits <= io_out_data_b @[Valid.scala 127:16]
    io.out_data <= io_out_data_out @[SIGMA.scala 159:17]
    reg io_out_same_l_border_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_l_border_REG) @[SIGMA.scala 160:36]
    io_out_same_l_border_REG <= inner.io.out_same_l_border @[SIGMA.scala 160:36]
    io.out_same_l_border <= io_out_same_l_border_REG @[SIGMA.scala 160:26]
    reg io_out_same_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_REG) @[SIGMA.scala 161:27]
    io_out_same_REG <= inner.io.out_same @[SIGMA.scala 161:27]
    io.out_same <= io_out_same_REG @[SIGMA.scala 161:17]
    reg io_out_same_r_border_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_r_border_REG) @[SIGMA.scala 162:36]
    io_out_same_r_border_REG <= inner.io.out_same_r_border @[SIGMA.scala 162:36]
    io.out_same_r_border <= io_out_same_r_border_REG @[SIGMA.scala 162:26]

  module FanNode_6 :
    input clock : Clock
    input reset : Reset
    output io : { flip in_l : { valid : UInt<1>, bits : UInt<8>}, flip in_r : { valid : UInt<1>, bits : UInt<8>}, flip same_l_border : UInt<1>, flip same_l : UInt<1>, flip same_m : UInt<1>, flip same_r : UInt<1>, flip same_r_border : UInt<1>, flip left_child : UInt<1>, out_l : { valid : UInt<1>, bits : UInt<8>}, out_r : { valid : UInt<1>, bits : UInt<8>}, out_final : { valid : UInt<1>, bits : UInt<8>}, out_same_l_border : UInt<1>, out_same : UInt<1>, out_same_r_border : UInt<1>}

    node _io_out_same_T = and(io.same_l, io.same_m) @[SIGMA.scala 73:28]
    node _io_out_same_T_1 = and(_io_out_same_T, io.same_r) @[SIGMA.scala 73:41]
    io.out_same <= _io_out_same_T_1 @[SIGMA.scala 73:15]
    io.out_same_l_border <= io.same_l_border @[SIGMA.scala 74:24]
    io.out_same_r_border <= io.same_r_border @[SIGMA.scala 75:24]
    wire NoOutput : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 76:30]
    NoOutput.valid <= UInt<1>("h0") @[SIGMA.scala 77:18]
    NoOutput.bits is invalid @[SIGMA.scala 78:17]
    io.out_l <= NoOutput @[SIGMA.scala 79:12]
    io.out_r <= NoOutput @[SIGMA.scala 80:12]
    io.out_final <= NoOutput @[SIGMA.scala 81:16]
    node merge_l = and(io.same_l_border, io.same_l) @[SIGMA.scala 82:42]
    node merge_r = and(io.same_r_border, io.same_r) @[SIGMA.scala 83:42]
    when io.same_m : @[SIGMA.scala 84:19]
      node _T = and(io.in_l.valid, io.in_r.valid) @[SIGMA.scala 85:24]
      when _T : @[SIGMA.scala 85:42]
        wire sum : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 86:21]
        sum.valid <= UInt<1>("h1") @[SIGMA.scala 87:17]
        node _sum_bits_T = add(io.in_l.bits, io.in_r.bits) @[Arithmetic.scala 41:38]
        node _sum_bits_T_1 = tail(_sum_bits_T, 1) @[Arithmetic.scala 41:38]
        sum.bits <= _sum_bits_T_1 @[SIGMA.scala 88:16]
        node _T_1 = and(merge_l, merge_r) @[SIGMA.scala 89:20]
        when _T_1 : @[SIGMA.scala 89:32]
          when io.left_child : @[SIGMA.scala 90:29]
            io.out_r <= sum @[SIGMA.scala 91:20]
          else :
            io.out_l <= sum @[SIGMA.scala 93:20]
        else :
          when merge_l : @[SIGMA.scala 95:28]
            io.out_l <= sum @[SIGMA.scala 96:18]
          else :
            when merge_r : @[SIGMA.scala 97:28]
              io.out_r <= sum @[SIGMA.scala 98:18]
            else :
              io.out_final <= sum @[SIGMA.scala 100:22]
    else :
      when merge_l : @[SIGMA.scala 104:19]
        io.out_l <= io.in_l @[SIGMA.scala 105:16]
      else :
        io.out_final <= io.in_l @[SIGMA.scala 107:20]
      when io.same_r : @[SIGMA.scala 109:21]
        io.out_r <= io.in_r @[SIGMA.scala 110:16]

  module FanNodeWrapper_6 :
    input clock : Clock
    input reset : Reset
    output io : { flip in_l : { valid : UInt<1>, bits : UInt<8>}[2], flip in_r : { valid : UInt<1>, bits : UInt<8>}[2], flip same_l_border : UInt<1>, flip same_l : UInt<1>, flip same_m : UInt<1>, flip same_r : UInt<1>, flip same_r_border : UInt<1>, flip left_child : UInt<1>, out_l : { valid : UInt<1>, bits : UInt<8>}, out_r : { valid : UInt<1>, bits : UInt<8>}, out_data : { valid : UInt<1>, bits : UInt<8>}, out_same_l_border : UInt<1>, out_same : UInt<1>, out_same_r_border : UInt<1>}

    inst inner of FanNode_6 @[SIGMA.scala 148:31]
    inner.clock <= clock
    inner.reset <= reset
    reg inner_io_in_l_upPipe_valid : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    inner_io_in_l_upPipe_valid <= io.in_l[0].valid @[Valid.scala 130:22]
    reg inner_io_in_l_upPipe_bits : UInt<8>, clock with :
      reset => (UInt<1>("h0"), inner_io_in_l_upPipe_bits) @[Reg.scala 19:16]
    when io.in_l[0].valid : @[Reg.scala 20:18]
      inner_io_in_l_upPipe_bits <= io.in_l[0].bits @[Reg.scala 20:22]
    wire inner_io_in_l_up : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    inner_io_in_l_up.valid <= inner_io_in_l_upPipe_valid @[Valid.scala 126:17]
    inner_io_in_l_up.bits <= inner_io_in_l_upPipe_bits @[Valid.scala 127:16]
    wire inner_io_in_l_out : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 126:21]
    node _inner_io_in_l_out_valid_T = or(inner_io_in_l_up.valid, io.in_l[1].valid) @[SIGMA.scala 127:29]
    inner_io_in_l_out.valid <= _inner_io_in_l_out_valid_T @[SIGMA.scala 127:17]
    node _inner_io_in_l_out_bits_T = mux(inner_io_in_l_up.valid, inner_io_in_l_up.bits, io.in_l[1].bits) @[SIGMA.scala 128:22]
    inner_io_in_l_out.bits <= _inner_io_in_l_out_bits_T @[SIGMA.scala 128:16]
    inner.io.in_l.bits <= inner_io_in_l_out.bits @[SIGMA.scala 149:19]
    inner.io.in_l.valid <= inner_io_in_l_out.valid @[SIGMA.scala 149:19]
    reg inner_io_in_r_upPipe_valid : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    inner_io_in_r_upPipe_valid <= io.in_r[0].valid @[Valid.scala 130:22]
    reg inner_io_in_r_upPipe_bits : UInt<8>, clock with :
      reset => (UInt<1>("h0"), inner_io_in_r_upPipe_bits) @[Reg.scala 19:16]
    when io.in_r[0].valid : @[Reg.scala 20:18]
      inner_io_in_r_upPipe_bits <= io.in_r[0].bits @[Reg.scala 20:22]
    wire inner_io_in_r_up : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    inner_io_in_r_up.valid <= inner_io_in_r_upPipe_valid @[Valid.scala 126:17]
    inner_io_in_r_up.bits <= inner_io_in_r_upPipe_bits @[Valid.scala 127:16]
    wire inner_io_in_r_out : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 126:21]
    node _inner_io_in_r_out_valid_T = or(inner_io_in_r_up.valid, io.in_r[1].valid) @[SIGMA.scala 127:29]
    inner_io_in_r_out.valid <= _inner_io_in_r_out_valid_T @[SIGMA.scala 127:17]
    node _inner_io_in_r_out_bits_T = mux(inner_io_in_r_up.valid, inner_io_in_r_up.bits, io.in_r[1].bits) @[SIGMA.scala 128:22]
    inner_io_in_r_out.bits <= _inner_io_in_r_out_bits_T @[SIGMA.scala 128:16]
    inner.io.in_r.bits <= inner_io_in_r_out.bits @[SIGMA.scala 150:19]
    inner.io.in_r.valid <= inner_io_in_r_out.valid @[SIGMA.scala 150:19]
    inner.io.same_l <= io.same_l @[SIGMA.scala 151:21]
    inner.io.same_r <= io.same_r @[SIGMA.scala 152:21]
    inner.io.same_m <= io.same_m @[SIGMA.scala 153:21]
    inner.io.same_l_border <= io.same_l_border @[SIGMA.scala 154:28]
    inner.io.same_r_border <= io.same_r_border @[SIGMA.scala 155:28]
    inner.io.left_child <= io.left_child @[SIGMA.scala 156:25]
    reg io_out_l_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_l_v <= inner.io.out_l.valid @[Valid.scala 130:22]
    reg io_out_l_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_l_b) @[Reg.scala 19:16]
    when inner.io.out_l.valid : @[Reg.scala 20:18]
      io_out_l_b <= inner.io.out_l.bits @[Reg.scala 20:22]
    wire io_out_l_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_l_out.valid <= io_out_l_v @[Valid.scala 126:17]
    io_out_l_out.bits <= io_out_l_b @[Valid.scala 127:16]
    io.out_l <= io_out_l_out @[SIGMA.scala 157:14]
    reg io_out_r_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_r_v <= inner.io.out_r.valid @[Valid.scala 130:22]
    reg io_out_r_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_r_b) @[Reg.scala 19:16]
    when inner.io.out_r.valid : @[Reg.scala 20:18]
      io_out_r_b <= inner.io.out_r.bits @[Reg.scala 20:22]
    wire io_out_r_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_r_out.valid <= io_out_r_v @[Valid.scala 126:17]
    io_out_r_out.bits <= io_out_r_b @[Valid.scala 127:16]
    io.out_r <= io_out_r_out @[SIGMA.scala 158:14]
    wire io_out_data_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_data_out.valid <= inner.io.out_final.valid @[Valid.scala 126:17]
    io_out_data_out.bits <= inner.io.out_final.bits @[Valid.scala 127:16]
    io.out_data <= io_out_data_out @[SIGMA.scala 159:17]
    reg io_out_same_l_border_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_l_border_REG) @[SIGMA.scala 160:36]
    io_out_same_l_border_REG <= inner.io.out_same_l_border @[SIGMA.scala 160:36]
    io.out_same_l_border <= io_out_same_l_border_REG @[SIGMA.scala 160:26]
    reg io_out_same_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_REG) @[SIGMA.scala 161:27]
    io_out_same_REG <= inner.io.out_same @[SIGMA.scala 161:27]
    io.out_same <= io_out_same_REG @[SIGMA.scala 161:17]
    reg io_out_same_r_border_REG : UInt<1>, clock with :
      reset => (UInt<1>("h0"), io_out_same_r_border_REG) @[SIGMA.scala 162:36]
    io_out_same_r_border_REG <= inner.io.out_same_r_border @[SIGMA.scala 162:36]
    io.out_same_r_border <= io_out_same_r_border_REG @[SIGMA.scala 162:26]

  module FanNetwork :
    input clock : Clock
    input reset : Reset
    output io : { flip in_data : { valid : UInt<1>, bits : UInt<8>}[8], flip in_same : UInt<1>[7], out_data : { valid : UInt<1>, bits : UInt<8>}[8]}

    inst nodes_0_0 of FanNodeWrapper @[SIGMA.scala 169:44]
    nodes_0_0.clock <= clock
    nodes_0_0.reset <= reset
    inst nodes_0_1 of FanNodeWrapper_1 @[SIGMA.scala 169:44]
    nodes_0_1.clock <= clock
    nodes_0_1.reset <= reset
    inst nodes_0_2 of FanNodeWrapper_2 @[SIGMA.scala 169:44]
    nodes_0_2.clock <= clock
    nodes_0_2.reset <= reset
    inst nodes_0_3 of FanNodeWrapper_3 @[SIGMA.scala 169:44]
    nodes_0_3.clock <= clock
    nodes_0_3.reset <= reset
    nodes_0_0.io.left_child <= UInt<1>("h1") @[SIGMA.scala 172:25]
    nodes_0_0.io.same_l <= UInt<1>("h1") @[SIGMA.scala 174:23]
    nodes_0_0.io.same_r <= UInt<1>("h1") @[SIGMA.scala 175:23]
    nodes_0_0.io.same_m <= io.in_same[0] @[SIGMA.scala 176:23]
    nodes_0_0.io.same_l_border <= UInt<1>("h0") @[SIGMA.scala 177:43]
    nodes_0_0.io.same_r_border <= io.in_same[1] @[SIGMA.scala 180:35]
    nodes_0_0.io.in_l[0].bits <= io.in_data[0].bits @[SIGMA.scala 181:26]
    nodes_0_0.io.in_l[0].valid <= io.in_data[0].valid @[SIGMA.scala 181:26]
    nodes_0_0.io.in_r[0].bits <= io.in_data[1].bits @[SIGMA.scala 182:26]
    nodes_0_0.io.in_r[0].valid <= io.in_data[1].valid @[SIGMA.scala 182:26]
    io.out_data[0] <= nodes_0_0.io.out_data @[SIGMA.scala 183:26]
    nodes_0_1.io.left_child <= UInt<1>("h0") @[SIGMA.scala 172:25]
    nodes_0_1.io.same_l <= UInt<1>("h1") @[SIGMA.scala 174:23]
    nodes_0_1.io.same_r <= UInt<1>("h1") @[SIGMA.scala 175:23]
    nodes_0_1.io.same_m <= io.in_same[2] @[SIGMA.scala 176:23]
    nodes_0_1.io.same_l_border <= io.in_same[1] @[SIGMA.scala 178:35]
    nodes_0_1.io.same_r_border <= io.in_same[3] @[SIGMA.scala 180:35]
    nodes_0_1.io.in_l[0].bits <= io.in_data[2].bits @[SIGMA.scala 181:26]
    nodes_0_1.io.in_l[0].valid <= io.in_data[2].valid @[SIGMA.scala 181:26]
    nodes_0_1.io.in_r[0].bits <= io.in_data[3].bits @[SIGMA.scala 182:26]
    nodes_0_1.io.in_r[0].valid <= io.in_data[3].valid @[SIGMA.scala 182:26]
    io.out_data[2] <= nodes_0_1.io.out_data @[SIGMA.scala 183:26]
    nodes_0_2.io.left_child <= UInt<1>("h1") @[SIGMA.scala 172:25]
    nodes_0_2.io.same_l <= UInt<1>("h1") @[SIGMA.scala 174:23]
    nodes_0_2.io.same_r <= UInt<1>("h1") @[SIGMA.scala 175:23]
    nodes_0_2.io.same_m <= io.in_same[4] @[SIGMA.scala 176:23]
    nodes_0_2.io.same_l_border <= io.in_same[3] @[SIGMA.scala 178:35]
    nodes_0_2.io.same_r_border <= io.in_same[5] @[SIGMA.scala 180:35]
    nodes_0_2.io.in_l[0].bits <= io.in_data[4].bits @[SIGMA.scala 181:26]
    nodes_0_2.io.in_l[0].valid <= io.in_data[4].valid @[SIGMA.scala 181:26]
    nodes_0_2.io.in_r[0].bits <= io.in_data[5].bits @[SIGMA.scala 182:26]
    nodes_0_2.io.in_r[0].valid <= io.in_data[5].valid @[SIGMA.scala 182:26]
    io.out_data[4] <= nodes_0_2.io.out_data @[SIGMA.scala 183:26]
    nodes_0_3.io.left_child <= UInt<1>("h0") @[SIGMA.scala 172:25]
    nodes_0_3.io.same_l <= UInt<1>("h1") @[SIGMA.scala 174:23]
    nodes_0_3.io.same_r <= UInt<1>("h1") @[SIGMA.scala 175:23]
    nodes_0_3.io.same_m <= io.in_same[6] @[SIGMA.scala 176:23]
    nodes_0_3.io.same_l_border <= io.in_same[5] @[SIGMA.scala 178:35]
    nodes_0_3.io.same_r_border <= UInt<1>("h0") @[SIGMA.scala 179:49]
    nodes_0_3.io.in_l[0].bits <= io.in_data[6].bits @[SIGMA.scala 181:26]
    nodes_0_3.io.in_l[0].valid <= io.in_data[6].valid @[SIGMA.scala 181:26]
    nodes_0_3.io.in_r[0].bits <= io.in_data[7].bits @[SIGMA.scala 182:26]
    nodes_0_3.io.in_r[0].valid <= io.in_data[7].valid @[SIGMA.scala 182:26]
    io.out_data[6] <= nodes_0_3.io.out_data @[SIGMA.scala 183:26]
    inst nodes_1_0 of FanNodeWrapper_4 @[SIGMA.scala 169:44]
    nodes_1_0.clock <= clock
    nodes_1_0.reset <= reset
    inst nodes_1_1 of FanNodeWrapper_5 @[SIGMA.scala 169:44]
    nodes_1_1.clock <= clock
    nodes_1_1.reset <= reset
    nodes_1_0.io.left_child <= UInt<1>("h1") @[SIGMA.scala 172:25]
    nodes_1_0.io.same_l <= nodes_0_0.io.out_same @[SIGMA.scala 188:23]
    nodes_1_0.io.same_r <= nodes_0_1.io.out_same @[SIGMA.scala 189:23]
    nodes_1_0.io.same_m <= nodes_0_1.io.same_l_border @[SIGMA.scala 190:23]
    nodes_1_0.io.same_l_border <= nodes_0_0.io.out_same_l_border @[SIGMA.scala 191:30]
    nodes_1_0.io.same_r_border <= nodes_0_1.io.out_same_r_border @[SIGMA.scala 192:30]
    nodes_1_0.io.in_l[0].bits <= nodes_0_0.io.out_r.bits @[SIGMA.scala 193:52]
    nodes_1_0.io.in_l[0].valid <= nodes_0_0.io.out_r.valid @[SIGMA.scala 193:52]
    nodes_1_0.io.in_r[0].bits <= nodes_0_1.io.out_l.bits @[SIGMA.scala 194:52]
    nodes_1_0.io.in_r[0].valid <= nodes_0_1.io.out_l.valid @[SIGMA.scala 194:52]
    io.out_data[1] <= nodes_1_0.io.out_data @[SIGMA.scala 195:26]
    nodes_1_1.io.left_child <= UInt<1>("h0") @[SIGMA.scala 172:25]
    nodes_1_1.io.same_l <= nodes_0_2.io.out_same @[SIGMA.scala 188:23]
    nodes_1_1.io.same_r <= nodes_0_3.io.out_same @[SIGMA.scala 189:23]
    nodes_1_1.io.same_m <= nodes_0_3.io.same_l_border @[SIGMA.scala 190:23]
    nodes_1_1.io.same_l_border <= nodes_0_2.io.out_same_l_border @[SIGMA.scala 191:30]
    nodes_1_1.io.same_r_border <= nodes_0_3.io.out_same_r_border @[SIGMA.scala 192:30]
    nodes_1_1.io.in_l[0].bits <= nodes_0_2.io.out_r.bits @[SIGMA.scala 193:52]
    nodes_1_1.io.in_l[0].valid <= nodes_0_2.io.out_r.valid @[SIGMA.scala 193:52]
    nodes_1_1.io.in_r[0].bits <= nodes_0_3.io.out_l.bits @[SIGMA.scala 194:52]
    nodes_1_1.io.in_r[0].valid <= nodes_0_3.io.out_l.valid @[SIGMA.scala 194:52]
    io.out_data[5] <= nodes_1_1.io.out_data @[SIGMA.scala 195:26]
    inst nodes_2_0 of FanNodeWrapper_6 @[SIGMA.scala 169:44]
    nodes_2_0.clock <= clock
    nodes_2_0.reset <= reset
    nodes_2_0.io.left_child <= UInt<1>("h1") @[SIGMA.scala 172:25]
    nodes_2_0.io.same_l <= nodes_1_0.io.out_same @[SIGMA.scala 188:23]
    nodes_2_0.io.same_r <= nodes_1_1.io.out_same @[SIGMA.scala 189:23]
    nodes_2_0.io.same_m <= nodes_1_1.io.same_l_border @[SIGMA.scala 190:23]
    nodes_2_0.io.same_l_border <= nodes_1_0.io.out_same_l_border @[SIGMA.scala 191:30]
    nodes_2_0.io.same_r_border <= nodes_1_1.io.out_same_r_border @[SIGMA.scala 192:30]
    nodes_2_0.io.in_l[0].bits <= nodes_0_1.io.out_r.bits @[SIGMA.scala 193:52]
    nodes_2_0.io.in_l[0].valid <= nodes_0_1.io.out_r.valid @[SIGMA.scala 193:52]
    nodes_2_0.io.in_l[1].bits <= nodes_1_0.io.out_r.bits @[SIGMA.scala 193:52]
    nodes_2_0.io.in_l[1].valid <= nodes_1_0.io.out_r.valid @[SIGMA.scala 193:52]
    nodes_2_0.io.in_r[0].bits <= nodes_0_2.io.out_l.bits @[SIGMA.scala 194:52]
    nodes_2_0.io.in_r[0].valid <= nodes_0_2.io.out_l.valid @[SIGMA.scala 194:52]
    nodes_2_0.io.in_r[1].bits <= nodes_1_1.io.out_l.bits @[SIGMA.scala 194:52]
    nodes_2_0.io.in_r[1].valid <= nodes_1_1.io.out_l.valid @[SIGMA.scala 194:52]
    io.out_data[3] <= nodes_2_0.io.out_data @[SIGMA.scala 195:26]
    reg io_out_data_7_upPipe_valid : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_data_7_upPipe_valid <= nodes_0_3.io.out_r.valid @[Valid.scala 130:22]
    reg io_out_data_7_upPipe_bits : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_data_7_upPipe_bits) @[Reg.scala 19:16]
    when nodes_0_3.io.out_r.valid : @[Reg.scala 20:18]
      io_out_data_7_upPipe_bits <= nodes_0_3.io.out_r.bits @[Reg.scala 20:22]
    wire io_out_data_7_up : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_data_7_up.valid <= io_out_data_7_upPipe_valid @[Valid.scala 126:17]
    io_out_data_7_up.bits <= io_out_data_7_upPipe_bits @[Valid.scala 127:16]
    wire io_out_data_7_out : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 126:21]
    node _io_out_data_7_out_valid_T = or(io_out_data_7_up.valid, nodes_1_1.io.out_r.valid) @[SIGMA.scala 127:29]
    io_out_data_7_out.valid <= _io_out_data_7_out_valid_T @[SIGMA.scala 127:17]
    node _io_out_data_7_out_bits_T = mux(io_out_data_7_up.valid, io_out_data_7_up.bits, nodes_1_1.io.out_r.bits) @[SIGMA.scala 128:22]
    io_out_data_7_out.bits <= _io_out_data_7_out_bits_T @[SIGMA.scala 128:16]
    reg io_out_data_7_upPipe_valid_1 : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    io_out_data_7_upPipe_valid_1 <= io_out_data_7_out.valid @[Valid.scala 130:22]
    reg io_out_data_7_upPipe_bits_1 : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_data_7_upPipe_bits_1) @[Reg.scala 19:16]
    when io_out_data_7_out.valid : @[Reg.scala 20:18]
      io_out_data_7_upPipe_bits_1 <= io_out_data_7_out.bits @[Reg.scala 20:22]
    wire io_out_data_7_up_1 : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    io_out_data_7_up_1.valid <= io_out_data_7_upPipe_valid_1 @[Valid.scala 126:17]
    io_out_data_7_up_1.bits <= io_out_data_7_upPipe_bits_1 @[Valid.scala 127:16]
    wire io_out_data_7_out_1 : { valid : UInt<1>, bits : UInt<8>} @[SIGMA.scala 126:21]
    node _io_out_data_7_out_valid_T_1 = or(io_out_data_7_up_1.valid, nodes_2_0.io.out_r.valid) @[SIGMA.scala 127:29]
    io_out_data_7_out_1.valid <= _io_out_data_7_out_valid_T_1 @[SIGMA.scala 127:17]
    node _io_out_data_7_out_bits_T_1 = mux(io_out_data_7_up_1.valid, io_out_data_7_up_1.bits, nodes_2_0.io.out_r.bits) @[SIGMA.scala 128:22]
    io_out_data_7_out_1.bits <= _io_out_data_7_out_bits_T_1 @[SIGMA.scala 128:16]
    io.out_data[7] <= io_out_data_7_out_1 @[SIGMA.scala 200:24]

  module FlexDPE :
    input clock : Clock
    input reset : UInt<1>
    output io : { flip in_data : { valid : UInt<1>, bits : UInt<8>}[8], flip in_dest : { valid : UInt<1>, bits : UInt<3>}[8], flip in_stationary : UInt<1>, flip in_same : UInt<1>[7], out_data : { valid : UInt<1>, bits : UInt<8>}[8]}

    inst xbar of XBar @[SIGMA.scala 206:28]
    xbar.clock <= clock
    xbar.reset <= reset
    inst pe_array_0 of PE @[SIGMA.scala 207:46]
    pe_array_0.clock <= clock
    pe_array_0.reset <= reset
    inst pe_array_1 of PE_1 @[SIGMA.scala 207:46]
    pe_array_1.clock <= clock
    pe_array_1.reset <= reset
    inst pe_array_2 of PE_2 @[SIGMA.scala 207:46]
    pe_array_2.clock <= clock
    pe_array_2.reset <= reset
    inst pe_array_3 of PE_3 @[SIGMA.scala 207:46]
    pe_array_3.clock <= clock
    pe_array_3.reset <= reset
    inst pe_array_4 of PE_4 @[SIGMA.scala 207:46]
    pe_array_4.clock <= clock
    pe_array_4.reset <= reset
    inst pe_array_5 of PE_5 @[SIGMA.scala 207:46]
    pe_array_5.clock <= clock
    pe_array_5.reset <= reset
    inst pe_array_6 of PE_6 @[SIGMA.scala 207:46]
    pe_array_6.clock <= clock
    pe_array_6.reset <= reset
    inst pe_array_7 of PE_7 @[SIGMA.scala 207:46]
    pe_array_7.clock <= clock
    pe_array_7.reset <= reset
    inst fan of FanNetwork @[SIGMA.scala 208:27]
    fan.clock <= clock
    fan.reset <= reset
    xbar.io.data[0].bits <= io.in_data[0].bits @[SIGMA.scala 216:16]
    xbar.io.data[0].valid <= io.in_data[0].valid @[SIGMA.scala 216:16]
    xbar.io.data[1].bits <= io.in_data[1].bits @[SIGMA.scala 216:16]
    xbar.io.data[1].valid <= io.in_data[1].valid @[SIGMA.scala 216:16]
    xbar.io.data[2].bits <= io.in_data[2].bits @[SIGMA.scala 216:16]
    xbar.io.data[2].valid <= io.in_data[2].valid @[SIGMA.scala 216:16]
    xbar.io.data[3].bits <= io.in_data[3].bits @[SIGMA.scala 216:16]
    xbar.io.data[3].valid <= io.in_data[3].valid @[SIGMA.scala 216:16]
    xbar.io.data[4].bits <= io.in_data[4].bits @[SIGMA.scala 216:16]
    xbar.io.data[4].valid <= io.in_data[4].valid @[SIGMA.scala 216:16]
    xbar.io.data[5].bits <= io.in_data[5].bits @[SIGMA.scala 216:16]
    xbar.io.data[5].valid <= io.in_data[5].valid @[SIGMA.scala 216:16]
    xbar.io.data[6].bits <= io.in_data[6].bits @[SIGMA.scala 216:16]
    xbar.io.data[6].valid <= io.in_data[6].valid @[SIGMA.scala 216:16]
    xbar.io.data[7].bits <= io.in_data[7].bits @[SIGMA.scala 216:16]
    xbar.io.data[7].valid <= io.in_data[7].valid @[SIGMA.scala 216:16]
    xbar.io.dest[0].bits <= io.in_dest[0].bits @[SIGMA.scala 217:16]
    xbar.io.dest[0].valid <= io.in_dest[0].valid @[SIGMA.scala 217:16]
    xbar.io.dest[1].bits <= io.in_dest[1].bits @[SIGMA.scala 217:16]
    xbar.io.dest[1].valid <= io.in_dest[1].valid @[SIGMA.scala 217:16]
    xbar.io.dest[2].bits <= io.in_dest[2].bits @[SIGMA.scala 217:16]
    xbar.io.dest[2].valid <= io.in_dest[2].valid @[SIGMA.scala 217:16]
    xbar.io.dest[3].bits <= io.in_dest[3].bits @[SIGMA.scala 217:16]
    xbar.io.dest[3].valid <= io.in_dest[3].valid @[SIGMA.scala 217:16]
    xbar.io.dest[4].bits <= io.in_dest[4].bits @[SIGMA.scala 217:16]
    xbar.io.dest[4].valid <= io.in_dest[4].valid @[SIGMA.scala 217:16]
    xbar.io.dest[5].bits <= io.in_dest[5].bits @[SIGMA.scala 217:16]
    xbar.io.dest[5].valid <= io.in_dest[5].valid @[SIGMA.scala 217:16]
    xbar.io.dest[6].bits <= io.in_dest[6].bits @[SIGMA.scala 217:16]
    xbar.io.dest[6].valid <= io.in_dest[6].valid @[SIGMA.scala 217:16]
    xbar.io.dest[7].bits <= io.in_dest[7].bits @[SIGMA.scala 217:16]
    xbar.io.dest[7].valid <= io.in_dest[7].valid @[SIGMA.scala 217:16]
    reg delay_stationary : UInt<1>, clock with :
      reset => (UInt<1>("h0"), delay_stationary) @[Reg.scala 19:16]
    when UInt<1>("h1") : @[Reg.scala 20:18]
      delay_stationary <= io.in_stationary @[Reg.scala 20:22]
    reg pe_array_0_io_in_data_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    pe_array_0_io_in_data_v <= xbar.io.out[0].valid @[Valid.scala 130:22]
    reg pe_array_0_io_in_data_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), pe_array_0_io_in_data_b) @[Reg.scala 19:16]
    when xbar.io.out[0].valid : @[Reg.scala 20:18]
      pe_array_0_io_in_data_b <= xbar.io.out[0].bits @[Reg.scala 20:22]
    wire pe_array_0_io_in_data_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    pe_array_0_io_in_data_out.valid <= pe_array_0_io_in_data_v @[Valid.scala 126:17]
    pe_array_0_io_in_data_out.bits <= pe_array_0_io_in_data_b @[Valid.scala 127:16]
    pe_array_0.io.in_data.bits <= pe_array_0_io_in_data_out.bits @[SIGMA.scala 221:21]
    pe_array_0.io.in_data.valid <= pe_array_0_io_in_data_out.valid @[SIGMA.scala 221:21]
    pe_array_0.io.in_stationary <= delay_stationary @[SIGMA.scala 222:27]
    reg pe_array_1_io_in_data_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    pe_array_1_io_in_data_v <= xbar.io.out[1].valid @[Valid.scala 130:22]
    reg pe_array_1_io_in_data_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), pe_array_1_io_in_data_b) @[Reg.scala 19:16]
    when xbar.io.out[1].valid : @[Reg.scala 20:18]
      pe_array_1_io_in_data_b <= xbar.io.out[1].bits @[Reg.scala 20:22]
    wire pe_array_1_io_in_data_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    pe_array_1_io_in_data_out.valid <= pe_array_1_io_in_data_v @[Valid.scala 126:17]
    pe_array_1_io_in_data_out.bits <= pe_array_1_io_in_data_b @[Valid.scala 127:16]
    pe_array_1.io.in_data.bits <= pe_array_1_io_in_data_out.bits @[SIGMA.scala 221:21]
    pe_array_1.io.in_data.valid <= pe_array_1_io_in_data_out.valid @[SIGMA.scala 221:21]
    pe_array_1.io.in_stationary <= delay_stationary @[SIGMA.scala 222:27]
    reg pe_array_2_io_in_data_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    pe_array_2_io_in_data_v <= xbar.io.out[2].valid @[Valid.scala 130:22]
    reg pe_array_2_io_in_data_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), pe_array_2_io_in_data_b) @[Reg.scala 19:16]
    when xbar.io.out[2].valid : @[Reg.scala 20:18]
      pe_array_2_io_in_data_b <= xbar.io.out[2].bits @[Reg.scala 20:22]
    wire pe_array_2_io_in_data_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    pe_array_2_io_in_data_out.valid <= pe_array_2_io_in_data_v @[Valid.scala 126:17]
    pe_array_2_io_in_data_out.bits <= pe_array_2_io_in_data_b @[Valid.scala 127:16]
    pe_array_2.io.in_data.bits <= pe_array_2_io_in_data_out.bits @[SIGMA.scala 221:21]
    pe_array_2.io.in_data.valid <= pe_array_2_io_in_data_out.valid @[SIGMA.scala 221:21]
    pe_array_2.io.in_stationary <= delay_stationary @[SIGMA.scala 222:27]
    reg pe_array_3_io_in_data_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    pe_array_3_io_in_data_v <= xbar.io.out[3].valid @[Valid.scala 130:22]
    reg pe_array_3_io_in_data_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), pe_array_3_io_in_data_b) @[Reg.scala 19:16]
    when xbar.io.out[3].valid : @[Reg.scala 20:18]
      pe_array_3_io_in_data_b <= xbar.io.out[3].bits @[Reg.scala 20:22]
    wire pe_array_3_io_in_data_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    pe_array_3_io_in_data_out.valid <= pe_array_3_io_in_data_v @[Valid.scala 126:17]
    pe_array_3_io_in_data_out.bits <= pe_array_3_io_in_data_b @[Valid.scala 127:16]
    pe_array_3.io.in_data.bits <= pe_array_3_io_in_data_out.bits @[SIGMA.scala 221:21]
    pe_array_3.io.in_data.valid <= pe_array_3_io_in_data_out.valid @[SIGMA.scala 221:21]
    pe_array_3.io.in_stationary <= delay_stationary @[SIGMA.scala 222:27]
    reg pe_array_4_io_in_data_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    pe_array_4_io_in_data_v <= xbar.io.out[4].valid @[Valid.scala 130:22]
    reg pe_array_4_io_in_data_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), pe_array_4_io_in_data_b) @[Reg.scala 19:16]
    when xbar.io.out[4].valid : @[Reg.scala 20:18]
      pe_array_4_io_in_data_b <= xbar.io.out[4].bits @[Reg.scala 20:22]
    wire pe_array_4_io_in_data_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    pe_array_4_io_in_data_out.valid <= pe_array_4_io_in_data_v @[Valid.scala 126:17]
    pe_array_4_io_in_data_out.bits <= pe_array_4_io_in_data_b @[Valid.scala 127:16]
    pe_array_4.io.in_data.bits <= pe_array_4_io_in_data_out.bits @[SIGMA.scala 221:21]
    pe_array_4.io.in_data.valid <= pe_array_4_io_in_data_out.valid @[SIGMA.scala 221:21]
    pe_array_4.io.in_stationary <= delay_stationary @[SIGMA.scala 222:27]
    reg pe_array_5_io_in_data_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    pe_array_5_io_in_data_v <= xbar.io.out[5].valid @[Valid.scala 130:22]
    reg pe_array_5_io_in_data_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), pe_array_5_io_in_data_b) @[Reg.scala 19:16]
    when xbar.io.out[5].valid : @[Reg.scala 20:18]
      pe_array_5_io_in_data_b <= xbar.io.out[5].bits @[Reg.scala 20:22]
    wire pe_array_5_io_in_data_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    pe_array_5_io_in_data_out.valid <= pe_array_5_io_in_data_v @[Valid.scala 126:17]
    pe_array_5_io_in_data_out.bits <= pe_array_5_io_in_data_b @[Valid.scala 127:16]
    pe_array_5.io.in_data.bits <= pe_array_5_io_in_data_out.bits @[SIGMA.scala 221:21]
    pe_array_5.io.in_data.valid <= pe_array_5_io_in_data_out.valid @[SIGMA.scala 221:21]
    pe_array_5.io.in_stationary <= delay_stationary @[SIGMA.scala 222:27]
    reg pe_array_6_io_in_data_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    pe_array_6_io_in_data_v <= xbar.io.out[6].valid @[Valid.scala 130:22]
    reg pe_array_6_io_in_data_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), pe_array_6_io_in_data_b) @[Reg.scala 19:16]
    when xbar.io.out[6].valid : @[Reg.scala 20:18]
      pe_array_6_io_in_data_b <= xbar.io.out[6].bits @[Reg.scala 20:22]
    wire pe_array_6_io_in_data_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    pe_array_6_io_in_data_out.valid <= pe_array_6_io_in_data_v @[Valid.scala 126:17]
    pe_array_6_io_in_data_out.bits <= pe_array_6_io_in_data_b @[Valid.scala 127:16]
    pe_array_6.io.in_data.bits <= pe_array_6_io_in_data_out.bits @[SIGMA.scala 221:21]
    pe_array_6.io.in_data.valid <= pe_array_6_io_in_data_out.valid @[SIGMA.scala 221:21]
    pe_array_6.io.in_stationary <= delay_stationary @[SIGMA.scala 222:27]
    reg pe_array_7_io_in_data_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    pe_array_7_io_in_data_v <= xbar.io.out[7].valid @[Valid.scala 130:22]
    reg pe_array_7_io_in_data_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), pe_array_7_io_in_data_b) @[Reg.scala 19:16]
    when xbar.io.out[7].valid : @[Reg.scala 20:18]
      pe_array_7_io_in_data_b <= xbar.io.out[7].bits @[Reg.scala 20:22]
    wire pe_array_7_io_in_data_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    pe_array_7_io_in_data_out.valid <= pe_array_7_io_in_data_v @[Valid.scala 126:17]
    pe_array_7_io_in_data_out.bits <= pe_array_7_io_in_data_b @[Valid.scala 127:16]
    pe_array_7.io.in_data.bits <= pe_array_7_io_in_data_out.bits @[SIGMA.scala 221:21]
    pe_array_7.io.in_data.valid <= pe_array_7_io_in_data_out.valid @[SIGMA.scala 221:21]
    pe_array_7.io.in_stationary <= delay_stationary @[SIGMA.scala 222:27]
    reg r : UInt<1>[7], clock with :
      reset => (UInt<1>("h0"), r) @[Reg.scala 19:16]
    when UInt<1>("h1") : @[Reg.scala 20:18]
      r[0] <= io.in_same[0] @[Reg.scala 20:22]
      r[1] <= io.in_same[1] @[Reg.scala 20:22]
      r[2] <= io.in_same[2] @[Reg.scala 20:22]
      r[3] <= io.in_same[3] @[Reg.scala 20:22]
      r[4] <= io.in_same[4] @[Reg.scala 20:22]
      r[5] <= io.in_same[5] @[Reg.scala 20:22]
      r[6] <= io.in_same[6] @[Reg.scala 20:22]
    reg r_1 : UInt<1>[7], clock with :
      reset => (UInt<1>("h0"), r_1) @[Reg.scala 19:16]
    when UInt<1>("h1") : @[Reg.scala 20:18]
      r_1[0] <= r[0] @[Reg.scala 20:22]
      r_1[1] <= r[1] @[Reg.scala 20:22]
      r_1[2] <= r[2] @[Reg.scala 20:22]
      r_1[3] <= r[3] @[Reg.scala 20:22]
      r_1[4] <= r[4] @[Reg.scala 20:22]
      r_1[5] <= r[5] @[Reg.scala 20:22]
      r_1[6] <= r[6] @[Reg.scala 20:22]
    fan.io.in_same[0] <= r_1[0] @[SIGMA.scala 224:18]
    fan.io.in_same[1] <= r_1[1] @[SIGMA.scala 224:18]
    fan.io.in_same[2] <= r_1[2] @[SIGMA.scala 224:18]
    fan.io.in_same[3] <= r_1[3] @[SIGMA.scala 224:18]
    fan.io.in_same[4] <= r_1[4] @[SIGMA.scala 224:18]
    fan.io.in_same[5] <= r_1[5] @[SIGMA.scala 224:18]
    fan.io.in_same[6] <= r_1[6] @[SIGMA.scala 224:18]
    reg fan_io_in_data_0_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    fan_io_in_data_0_v <= pe_array_0.io.out_data.valid @[Valid.scala 130:22]
    reg fan_io_in_data_0_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), fan_io_in_data_0_b) @[Reg.scala 19:16]
    when pe_array_0.io.out_data.valid : @[Reg.scala 20:18]
      fan_io_in_data_0_b <= pe_array_0.io.out_data.bits @[Reg.scala 20:22]
    wire fan_io_in_data_0_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    fan_io_in_data_0_out.valid <= fan_io_in_data_0_v @[Valid.scala 126:17]
    fan_io_in_data_0_out.bits <= fan_io_in_data_0_b @[Valid.scala 127:16]
    fan.io.in_data[0].bits <= fan_io_in_data_0_out.bits @[SIGMA.scala 226:35]
    fan.io.in_data[0].valid <= fan_io_in_data_0_out.valid @[SIGMA.scala 226:35]
    reg fan_io_in_data_1_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    fan_io_in_data_1_v <= pe_array_1.io.out_data.valid @[Valid.scala 130:22]
    reg fan_io_in_data_1_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), fan_io_in_data_1_b) @[Reg.scala 19:16]
    when pe_array_1.io.out_data.valid : @[Reg.scala 20:18]
      fan_io_in_data_1_b <= pe_array_1.io.out_data.bits @[Reg.scala 20:22]
    wire fan_io_in_data_1_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    fan_io_in_data_1_out.valid <= fan_io_in_data_1_v @[Valid.scala 126:17]
    fan_io_in_data_1_out.bits <= fan_io_in_data_1_b @[Valid.scala 127:16]
    fan.io.in_data[1].bits <= fan_io_in_data_1_out.bits @[SIGMA.scala 226:35]
    fan.io.in_data[1].valid <= fan_io_in_data_1_out.valid @[SIGMA.scala 226:35]
    reg fan_io_in_data_2_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    fan_io_in_data_2_v <= pe_array_2.io.out_data.valid @[Valid.scala 130:22]
    reg fan_io_in_data_2_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), fan_io_in_data_2_b) @[Reg.scala 19:16]
    when pe_array_2.io.out_data.valid : @[Reg.scala 20:18]
      fan_io_in_data_2_b <= pe_array_2.io.out_data.bits @[Reg.scala 20:22]
    wire fan_io_in_data_2_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    fan_io_in_data_2_out.valid <= fan_io_in_data_2_v @[Valid.scala 126:17]
    fan_io_in_data_2_out.bits <= fan_io_in_data_2_b @[Valid.scala 127:16]
    fan.io.in_data[2].bits <= fan_io_in_data_2_out.bits @[SIGMA.scala 226:35]
    fan.io.in_data[2].valid <= fan_io_in_data_2_out.valid @[SIGMA.scala 226:35]
    reg fan_io_in_data_3_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    fan_io_in_data_3_v <= pe_array_3.io.out_data.valid @[Valid.scala 130:22]
    reg fan_io_in_data_3_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), fan_io_in_data_3_b) @[Reg.scala 19:16]
    when pe_array_3.io.out_data.valid : @[Reg.scala 20:18]
      fan_io_in_data_3_b <= pe_array_3.io.out_data.bits @[Reg.scala 20:22]
    wire fan_io_in_data_3_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    fan_io_in_data_3_out.valid <= fan_io_in_data_3_v @[Valid.scala 126:17]
    fan_io_in_data_3_out.bits <= fan_io_in_data_3_b @[Valid.scala 127:16]
    fan.io.in_data[3].bits <= fan_io_in_data_3_out.bits @[SIGMA.scala 226:35]
    fan.io.in_data[3].valid <= fan_io_in_data_3_out.valid @[SIGMA.scala 226:35]
    reg fan_io_in_data_4_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    fan_io_in_data_4_v <= pe_array_4.io.out_data.valid @[Valid.scala 130:22]
    reg fan_io_in_data_4_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), fan_io_in_data_4_b) @[Reg.scala 19:16]
    when pe_array_4.io.out_data.valid : @[Reg.scala 20:18]
      fan_io_in_data_4_b <= pe_array_4.io.out_data.bits @[Reg.scala 20:22]
    wire fan_io_in_data_4_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    fan_io_in_data_4_out.valid <= fan_io_in_data_4_v @[Valid.scala 126:17]
    fan_io_in_data_4_out.bits <= fan_io_in_data_4_b @[Valid.scala 127:16]
    fan.io.in_data[4].bits <= fan_io_in_data_4_out.bits @[SIGMA.scala 226:35]
    fan.io.in_data[4].valid <= fan_io_in_data_4_out.valid @[SIGMA.scala 226:35]
    reg fan_io_in_data_5_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    fan_io_in_data_5_v <= pe_array_5.io.out_data.valid @[Valid.scala 130:22]
    reg fan_io_in_data_5_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), fan_io_in_data_5_b) @[Reg.scala 19:16]
    when pe_array_5.io.out_data.valid : @[Reg.scala 20:18]
      fan_io_in_data_5_b <= pe_array_5.io.out_data.bits @[Reg.scala 20:22]
    wire fan_io_in_data_5_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    fan_io_in_data_5_out.valid <= fan_io_in_data_5_v @[Valid.scala 126:17]
    fan_io_in_data_5_out.bits <= fan_io_in_data_5_b @[Valid.scala 127:16]
    fan.io.in_data[5].bits <= fan_io_in_data_5_out.bits @[SIGMA.scala 226:35]
    fan.io.in_data[5].valid <= fan_io_in_data_5_out.valid @[SIGMA.scala 226:35]
    reg fan_io_in_data_6_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    fan_io_in_data_6_v <= pe_array_6.io.out_data.valid @[Valid.scala 130:22]
    reg fan_io_in_data_6_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), fan_io_in_data_6_b) @[Reg.scala 19:16]
    when pe_array_6.io.out_data.valid : @[Reg.scala 20:18]
      fan_io_in_data_6_b <= pe_array_6.io.out_data.bits @[Reg.scala 20:22]
    wire fan_io_in_data_6_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    fan_io_in_data_6_out.valid <= fan_io_in_data_6_v @[Valid.scala 126:17]
    fan_io_in_data_6_out.bits <= fan_io_in_data_6_b @[Valid.scala 127:16]
    fan.io.in_data[6].bits <= fan_io_in_data_6_out.bits @[SIGMA.scala 226:35]
    fan.io.in_data[6].valid <= fan_io_in_data_6_out.valid @[SIGMA.scala 226:35]
    reg fan_io_in_data_7_v : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[Valid.scala 130:22]
    fan_io_in_data_7_v <= pe_array_7.io.out_data.valid @[Valid.scala 130:22]
    reg fan_io_in_data_7_b : UInt<8>, clock with :
      reset => (UInt<1>("h0"), fan_io_in_data_7_b) @[Reg.scala 19:16]
    when pe_array_7.io.out_data.valid : @[Reg.scala 20:18]
      fan_io_in_data_7_b <= pe_array_7.io.out_data.bits @[Reg.scala 20:22]
    wire fan_io_in_data_7_out : { valid : UInt<1>, bits : UInt<8>} @[Valid.scala 125:21]
    fan_io_in_data_7_out.valid <= fan_io_in_data_7_v @[Valid.scala 126:17]
    fan_io_in_data_7_out.bits <= fan_io_in_data_7_b @[Valid.scala 127:16]
    fan.io.in_data[7].bits <= fan_io_in_data_7_out.bits @[SIGMA.scala 226:35]
    fan.io.in_data[7].valid <= fan_io_in_data_7_out.valid @[SIGMA.scala 226:35]
    io.out_data[0] <= fan.io.out_data[0] @[SIGMA.scala 229:32]
    io.out_data[1] <= fan.io.out_data[1] @[SIGMA.scala 229:32]
    io.out_data[2] <= fan.io.out_data[2] @[SIGMA.scala 229:32]
    io.out_data[3] <= fan.io.out_data[3] @[SIGMA.scala 229:32]
    io.out_data[4] <= fan.io.out_data[4] @[SIGMA.scala 229:32]
    io.out_data[5] <= fan.io.out_data[5] @[SIGMA.scala 229:32]
    io.out_data[6] <= fan.io.out_data[6] @[SIGMA.scala 229:32]
    io.out_data[7] <= fan.io.out_data[7] @[SIGMA.scala 229:32]
