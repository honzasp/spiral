  .text

  .globl spiral_rt_call_fun
  .align 16,0x90
  .type spiral_rt_call_fun,@function
spiral_rt_call_fun:
  movl  4(%esp), %edi
  movl  12(%esp), %eax
  movl  %eax, -4(%esp)
  movl  $0xffffffff, -8(%esp)
  subl  $8, %esp
  calll *16(%esp)
  addl  $8, %esp
  ret
.Lend_spiral_rt_call_fun:
  .size spiral_rt_call_fun, .Lend_spiral_rt_call_fun - spiral_rt_call_fun

  .globl spiral_rt_argc_mismatch
  .align 16,0x90
  .type spiral_rt_argc_mismatch,@function
spiral_rt_argc_mismatch:
  pushl %eax
  pushl %edx
  pushl %ecx
  pushl %edi
  calll _ZN6spiral19panic_argc_mismatchEPNS_2BgEPvjj

  .globl spiral_rt_invalid_fun
  .align 16,0x90
  .type spiral_rt_invalid_fun,@function
spiral_rt_invalid_fun:
  pushl %ecx
  pushl %edi
  calll _ZN6spiral17panic_invalid_funEPNS_2BgEj
