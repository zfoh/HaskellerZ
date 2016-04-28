
==================== Asm code ====================
.data
.align 3
.align 0
.globl __stginit_main@main:Main
__stginit_main@main:Main:



==================== Asm code ====================
.data
.align 3
.align 0
.globl main4_closure
main4_closure:
	.quad	I#_static_info
	.quad	0



==================== Asm code ====================
.data
.align 3
.align 0
.globl main3_closure
main3_closure:
	.quad	D_static_info
	.quad	main4_closure+1
	.quad	3



==================== Asm code ====================
.data
.align 3
.align 0
.globl main_go_closure
main_go_closure:
	.quad	main_go_info



==================== Asm code ====================
.text
.align 3
sat_s4SE_info_dsp:
.align 3
	.quad	4294967296
	.quad	18
sat_s4SE_info:
_c4TW:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb _c4TX
_c4TY:
	movq stg_upd_frame_info@GOTPCREL(%rip),%rax
	movq %rax,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq 16(%rbx),%rax
	cmpq $10000000,%rax
	jne _c4TU
_c4TV:
	leaq []_closure+1(%rip),%rbx
	addq $-16,%rbp
	jmp *(%rbp)
_c4TX:
	jmp *-16(%r13)
_c4TU:
	leaq 1(%rax),%r14
	addq $-16,%rbp
	jmp main_go_info
	.long  sat_s4SE_info - sat_s4SE_info_dsp



==================== Asm code ====================
.text
.align 3
main_go_info_dsp:
.align 3
	.quad	4294967300
	.quad	0
	.quad	15
.globl main_go_info
main_go_info:
_c4U4:
	addq $80,%r12
	cmpq 856(%r13),%r12
	ja _c4U8
_c4U7:
	leaq sat_s4SE_info(%rip),%rax
	movq %rax,-72(%r12)
	movq %r14,-56(%r12)
	leaq I#_con_info(%rip),%rax
	movq %rax,-48(%r12)
	movq %r14,-40(%r12)
	leaq D_con_info(%rip),%rax
	movq %rax,-32(%r12)
	leaq -47(%r12),%rax
	movq %rax,-24(%r12)
	leaq :_con_info(%rip),%rax
	movq %rax,-16(%r12)
	leaq -31(%r12),%rax
	movq %rax,-8(%r12)
	leaq -72(%r12),%rax
	movq %rax,(%r12)
	leaq -14(%r12),%rbx
	jmp *(%rbp)
_c4U8:
	movq $80,904(%r13)
	leaq main_go_closure(%rip),%rbx
	jmp *-8(%r13)
	.long  main_go_info - main_go_info_dsp



==================== Asm code ====================
.data
.align 3
.align 0
.globl main5_closure
main5_closure:
	.quad	main5_info
	.quad	0
	.quad	0
	.quad	0



==================== Asm code ====================
.text
.align 3
main5_info_dsp:
.align 3
	.quad	0
	.quad	22
.globl main5_info
main5_info:
_c4Uu:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb _c4Uv
_c4Uw:
	movq %r13,%rdi
	movq %rbx,%rsi
	subq $8,%rsp
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je _c4Ut
_c4Us:
	movq stg_bh_upd_frame_info@GOTPCREL(%rip),%rbx
	movq %rbx,-16(%rbp)
	movq %rax,-8(%rbp)
	movl $1,%r14d
	addq $-16,%rbp
	jmp main_go_info
_c4Uv:
	jmp *-16(%r13)
_c4Ut:
	jmp *(%rbx)
	.long  main5_info - main5_info_dsp



==================== Asm code ====================
.const
.align 3
.align 0
c4UE_str:
	.byte	68
	.byte	0



==================== Asm code ====================
.data
.align 3
.align 0
.globl $tcD1_closure
$tcD1_closure:
	.quad	TrNameS_static_info
	.quad	c4UE_str



==================== Asm code ====================
.const
.align 3
.align 0
c4UI_str:
	.byte	39
	.byte	68
	.byte	0



==================== Asm code ====================
.data
.align 3
.align 0
.globl $tc'D1_closure
$tc'D1_closure:
	.quad	TrNameS_static_info
	.quad	c4UI_str



==================== Asm code ====================
.const
.align 3
.align 0
c4UM_str:
	.byte	77
	.byte	97
	.byte	105
	.byte	110
	.byte	0



==================== Asm code ====================
.data
.align 3
.align 0
.globl $trModule1_closure
$trModule1_closure:
	.quad	TrNameS_static_info
	.quad	c4UM_str



==================== Asm code ====================
.const
.align 3
.align 0
c4UQ_str:
	.byte	109
	.byte	97
	.byte	105
	.byte	110
	.byte	0



==================== Asm code ====================
.data
.align 3
.align 0
.globl $trModule2_closure
$trModule2_closure:
	.quad	TrNameS_static_info
	.quad	c4UQ_str



==================== Asm code ====================
.data
.align 3
.align 0
.globl $trModule_closure
$trModule_closure:
	.quad	Module_static_info
	.quad	$trModule2_closure+1
	.quad	$trModule1_closure+1
	.quad	3



==================== Asm code ====================
.data
.align 3
.align 0
.globl $tc'D_closure
$tc'D_closure:
	.quad	TyCon_static_info
	.quad	$trModule_closure+1
	.quad	$tc'D1_closure+1
	.quad	689042227747467966
	.quad	3828627186432438340
	.quad	3



==================== Asm code ====================
.data
.align 3
.align 0
.globl $tcD_closure
$tcD_closure:
	.quad	TyCon_static_info
	.quad	$trModule_closure+1
	.quad	$tcD1_closure+1
	.quad	-3249470807205730661
	.quad	-4778227944584170908
	.quad	3



==================== Asm code ====================
.data
.align 3
.align 0
.globl main_$sfoldr_closure
main_$sfoldr_closure:
	.quad	main_$sfoldr_info



==================== Asm code ====================
.text
.align 3
sat_s4ST_info_dsp:
.align 3
	.quad	2
	.quad	19
sat_s4ST_info:
_c4VD:
	leaq -32(%rbp),%rax
	cmpq %r15,%rax
	jb _c4VZ
_c4W0:
	movq stg_upd_frame_info@GOTPCREL(%rip),%rax
	movq %rax,-16(%rbp)
	movq %rbx,-8(%rbp)
	leaq block_c4VA_info(%rip),%rax
	movq %rax,-32(%rbp)
	movq 16(%rbx),%rax
	movq 24(%rbx),%rbx
	movq %rax,-24(%rbp)
	addq $-32,%rbp
	testb $7,%bl
	jne _c4VA
_c4VB:
	jmp *(%rbx)
_c4W6:
	movq $16,904(%r13)
	jmp *stg_gc_unpt_r1@GOTPCREL(%rip)
.align 3
	.quad	65
	.quad	32
block_c4VQ_info:
_c4VQ:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja _c4W6
_c4W5:
	leaq 7(%rbx),%rax
	movq 8(%rbp),%rbx
	addq (%rax),%rbx
	leaq I#_con_info(%rip),%rax
	movq %rax,-8(%r12)
	movq %rbx,(%r12)
	leaq -7(%r12),%rbx
	addq $16,%rbp
	jmp *(%rbp)
_c4VZ:
	jmp *-16(%r13)
.align 3
	.quad	65
	.quad	32
block_c4VL_info:
_c4VL:
	leaq block_c4VQ_info(%rip),%rax
	movq %rax,(%rbp)
	movq 7(%rbx),%rbx
	testb $7,%bl
	jne _c4VQ
_c4VR:
	jmp *(%rbx)
.align 3
	.quad	1
	.quad	32
block_c4VG_info:
_c4VG:
	leaq block_c4VL_info(%rip),%rax
	movq %rax,(%rbp)
	movq 7(%rbx),%rax
	movq 8(%rbp),%rbx
	movq %rax,8(%rbp)
	testb $7,%bl
	jne _c4VL
_c4VM:
	jmp *(%rbx)
.align 3
	.quad	1
	.quad	32
block_c4VA_info:
_c4VA:
	leaq block_c4VG_info(%rip),%rax
	movq %rax,(%rbp)
	movq 7(%rbx),%rbx
	testb $7,%bl
	jne _c4VG
_c4VH:
	jmp *(%rbx)
	.long  sat_s4ST_info - sat_s4ST_info_dsp



==================== Asm code ====================
.text
.align 3
main_$sfoldr_info_dsp:
.align 3
	.quad	8589934607
	.quad	0
	.quad	15
.globl main_$sfoldr_info
main_$sfoldr_info:
_c4Wb:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jae _c4V5
_c4Wc:
	leaq main_$sfoldr_closure(%rip),%rbx
	jmp *-8(%r13)
_c4Wi:
	movq $48,904(%r13)
	jmp *stg_gc_unpt_r1@GOTPCREL(%rip)
.align 3
	.quad	1
	.quad	32
block_c4V8_info:
_c4V8:
	movq 8(%rbp),%rax
	movq %rbx,%rcx
	andl $7,%ecx
	cmpq $1,%rcx
	jne _c4W9
_c4W8:
	movq %rax,%rbx
	andq $-8,%rbx
	addq $16,%rbp
	jmp *(%rbx)
_c4W9:
	addq $48,%r12
	cmpq 856(%r13),%r12
	ja _c4Wi
_c4Wh:
	movq 6(%rbx),%rcx
	movq 14(%rbx),%rbx
	leaq sat_s4ST_info(%rip),%rdx
	movq %rdx,-40(%r12)
	movq %rax,-24(%r12)
	movq %rcx,-16(%r12)
	leaq D_con_info(%rip),%rax
	movq %rax,-8(%r12)
	leaq -40(%r12),%rax
	movq %rax,(%r12)
	addq $16,%rbp
	leaq -7(%r12),%rax
_n4WG:
	movq %rax,%rsi
	movq %rbx,%r14
_c4V5:
	leaq block_c4V8_info(%rip),%rax
	movq %rax,-16(%rbp)
	movq %r14,%rbx
	movq %rsi,-8(%rbp)
	addq $-16,%rbp
	testb $7,%bl
	jne _c4V8
_c4V9:
	jmp *(%rbx)
	.long  main_$sfoldr_info - main_$sfoldr_info_dsp



==================== Asm code ====================
.data
.align 3
.align 0
.globl get_closure
get_closure:
	.quad	get_info



==================== Asm code ====================
.text
.align 3
get_info_dsp:
.align 3
	.quad	4294967301
	.quad	0
	.quad	15
.globl get_info
get_info:
_c4WS:
	leaq -8(%rbp),%rax
	cmpq %r15,%rax
	jb _c4WT
_c4WU:
	leaq block_c4WP_info(%rip),%rax
	movq %rax,-8(%rbp)
	movq %r14,%rbx
	addq $-8,%rbp
	testb $7,%bl
	jne _c4WP
_c4WQ:
	jmp *(%rbx)
_c4WT:
	leaq get_closure(%rip),%rbx
	jmp *-8(%r13)
.align 3
	.quad	0
	.quad	32
block_c4WP_info:
_c4WP:
	movq 7(%rbx),%rbx
	andq $-8,%rbx
	addq $8,%rbp
	jmp *(%rbx)
	.long  get_info - get_info_dsp



==================== Asm code ====================
.data
.align 3
.align 0
.globl $fShowD4_closure
$fShowD4_closure:
	.quad	$fShowD4_info
	.quad	0
	.quad	0
	.quad	0



==================== Asm code ====================
.const
.align 3
.align 0
c4Xb_str:
	.byte	68
	.byte	32
	.byte	123
	.byte	0



==================== Asm code ====================
.text
.align 3
$fShowD4_info_dsp:
.align 3
	.quad	0
	.quad	22
.globl $fShowD4_info
$fShowD4_info:
_c4Xc:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb _c4Xd
_c4Xe:
	movq %r13,%rdi
	movq %rbx,%rsi
	subq $8,%rsp
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je _c4Xa
_c4X9:
	movq stg_bh_upd_frame_info@GOTPCREL(%rip),%rbx
	movq %rbx,-16(%rbp)
	movq %rax,-8(%rbp)
	leaq c4Xb_str(%rip),%r14
	addq $-16,%rbp
	jmp unpackCString#_info
_c4Xd:
	jmp *-16(%r13)
_c4Xa:
	jmp *(%rbx)
	.long  $fShowD4_info - $fShowD4_info_dsp



==================== Asm code ====================
.data
.align 3
.align 0
.globl $fShowD3_closure
$fShowD3_closure:
	.quad	$fShowD3_info
	.quad	0
	.quad	0
	.quad	0



==================== Asm code ====================
.const
.align 3
.align 0
c4Xs_str:
	.byte	103
	.byte	101
	.byte	116
	.byte	32
	.byte	61
	.byte	32
	.byte	0



==================== Asm code ====================
.text
.align 3
$fShowD3_info_dsp:
.align 3
	.quad	0
	.quad	22
.globl $fShowD3_info
$fShowD3_info:
_c4Xt:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb _c4Xu
_c4Xv:
	movq %r13,%rdi
	movq %rbx,%rsi
	subq $8,%rsp
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je _c4Xr
_c4Xq:
	movq stg_bh_upd_frame_info@GOTPCREL(%rip),%rbx
	movq %rbx,-16(%rbp)
	movq %rax,-8(%rbp)
	leaq c4Xs_str(%rip),%r14
	addq $-16,%rbp
	jmp unpackCString#_info
_c4Xu:
	jmp *-16(%r13)
_c4Xr:
	jmp *(%rbx)
	.long  $fShowD3_info - $fShowD3_info_dsp



==================== Asm code ====================
.data
.align 3
.align 0
.globl $fShowD2_closure
$fShowD2_closure:
	.quad	$fShowD2_info
	.quad	0
	.quad	0
	.quad	0



==================== Asm code ====================
.const
.align 3
.align 0
c4XJ_str:
	.byte	125
	.byte	0



==================== Asm code ====================
.text
.align 3
$fShowD2_info_dsp:
.align 3
	.quad	0
	.quad	22
.globl $fShowD2_info
$fShowD2_info:
_c4XK:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb _c4XL
_c4XM:
	movq %r13,%rdi
	movq %rbx,%rsi
	subq $8,%rsp
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je _c4XI
_c4XH:
	movq stg_bh_upd_frame_info@GOTPCREL(%rip),%rbx
	movq %rbx,-16(%rbp)
	movq %rax,-8(%rbp)
	leaq c4XJ_str(%rip),%r14
	addq $-16,%rbp
	jmp unpackCString#_info
_c4XL:
	jmp *-16(%r13)
_c4XI:
	jmp *(%rbx)
	.long  $fShowD2_info - $fShowD2_info_dsp



==================== Asm code ====================
.data
.align 3
.align 0
.globl $w$cshowsPrec_closure
$w$cshowsPrec_closure:
	.quad	$w$cshowsPrec_info
	.quad	0



==================== Asm code ====================
.text
.align 3
sat_s4T5_info_dsp:
.align 3
	.quad	S4Zl_srt-(sat_s4T5_info)+0
	.quad	1
	.quad	4294967313
sat_s4T5_info:
_c4Yx:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb _c4Yy
_c4Yz:
	movq stg_upd_frame_info@GOTPCREL(%rip),%rax
	movq %rax,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq 16(%rbx),%rsi
	leaq $fShowD2_closure(%rip),%r14
	addq $-16,%rbp
	jmp ++_info
_c4Yy:
	jmp *-16(%r13)
	.long  sat_s4T5_info - sat_s4T5_info_dsp



==================== Asm code ====================
.text
.align 3
sat_s4T9_info_dsp:
.align 3
	.quad	S4Zl_srt-(sat_s4T9_info)+0
	.quad	2
	.quad	4294967315
sat_s4T9_info:
_c4YA:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb _c4YE
_c4YF:
	leaq block_c4Yo_info(%rip),%rax
	movq %rax,-16(%rbp)
	movq 24(%rbx),%rax
	movq 16(%rbx),%rbx
	movq %rax,-8(%rbp)
	addq $-16,%rbp
	testb $7,%bl
	jne _c4Yo
_c4Yp:
	jmp *(%rbx)
_c4YL:
	movq $24,904(%r13)
	jmp *stg_gc_pp@GOTPCREL(%rip)
.align 3
	.quad	0
	.quad	32
block_c4YB_info:
_c4YB:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja _c4YL
_c4YK:
	leaq :_con_info(%rip),%rax
	movq %rax,-16(%r12)
	movq %rbx,-8(%r12)
	movq %r14,(%r12)
	leaq -14(%r12),%rbx
	addq $8,%rbp
	jmp *(%rbp)
_c4YI:
	movq $24,904(%r13)
	jmp *stg_gc_unpt_r1@GOTPCREL(%rip)
.align 3
	.quad	S4Zl_srt-(block_c4Yo_info)+0
	.quad	1
	.quad	4294967328
block_c4Yo_info:
_c4Yo:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja _c4YI
_c4YH:
	movq 7(%rbx),%rax
	leaq sat_s4T5_info(%rip),%rbx
	movq %rbx,-16(%r12)
	movq 8(%rbp),%rbx
	movq %rbx,(%r12)
	leaq block_c4YB_info(%rip),%rbx
	movq %rbx,8(%rbp)
	leaq -16(%r12),%rdi
	movq %rax,%rsi
	xorl %r14d,%r14d
	addq $8,%rbp
	jmp $wshowSignedInt_info
_c4YE:
	jmp *-16(%r13)
	.long  sat_s4T9_info - sat_s4T9_info_dsp



==================== Asm code ====================
.text
.align 3
sat_s4Ta_info_dsp:
.align 3
	.quad	S4Zl_srt-(sat_s4Ta_info)+0
	.quad	2
	.quad	12884901907
sat_s4Ta_info:
_c4YM:
	addq $32,%r12
	cmpq 856(%r13),%r12
	ja _c4YQ
_c4YP:
	movq 16(%rbx),%rax
	movq 24(%rbx),%rbx
	leaq sat_s4T9_info(%rip),%rcx
	movq %rcx,-24(%r12)
	movq %rax,-8(%r12)
	movq %rbx,(%r12)
	leaq -24(%r12),%rsi
	leaq $fShowD3_closure(%rip),%r14
	jmp ++_info
_c4YQ:
	movq $32,904(%r13)
	jmp *-16(%r13)
	.long  sat_s4Ta_info - sat_s4Ta_info_dsp



==================== Asm code ====================
.text
.align 3
p_s4T1_info_dsp:
.align 3
	.quad	S4Zl_srt-(p_s4T1_info)+0
	.quad	4294967301
	.quad	1
	.quad	30064771082
p_s4T1_info:
_c4YR:
	addq $32,%r12
	cmpq 856(%r13),%r12
	ja _c4YV
_c4YU:
	movq 7(%rbx),%rax
	leaq sat_s4Ta_info(%rip),%rbx
	movq %rbx,-24(%r12)
	movq %rax,-8(%r12)
	movq %r14,(%r12)
	leaq -24(%r12),%rsi
	leaq $fShowD4_closure(%rip),%r14
	jmp ++_info
_c4YV:
	movq $32,904(%r13)
	jmp *-8(%r13)
	.long  p_s4T1_info - p_s4T1_info_dsp



==================== Asm code ====================
.text
.align 3
sat_s4Te_info_dsp:
.align 3
	.quad	S4Zl_srt-(sat_s4Te_info)+0
	.quad	2
	.quad	30064771091
sat_s4Te_info:
_c4Z7:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb _c4Z8
_c4Z9:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja _c4Zb
_c4Za:
	movq stg_upd_frame_info@GOTPCREL(%rip),%rax
	movq %rax,-16(%rbp)
	movq %rbx,-8(%rbp)
	movq 16(%rbx),%rax
	movq 24(%rbx),%rbx
	leaq :_con_info(%rip),%rcx
	movq %rcx,-16(%r12)
	leaq shows6_closure(%rip),%rcx
	movq %rcx,-8(%r12)
	movq %rax,(%r12)
	leaq -14(%r12),%r14
	addq $-16,%rbp
	jmp p_s4T1_info
_c4Zb:
	movq $24,904(%r13)
_c4Z8:
	jmp *-16(%r13)
	.long  sat_s4Te_info - sat_s4Te_info_dsp



==================== Asm code ====================
.text
.align 3
$w$cshowsPrec_info_dsp:
.align 3
	.quad	S4Zl_srt-($w$cshowsPrec_info)+0
	.quad	12884901907
	.quad	0
	.quad	64424509455
.globl $w$cshowsPrec_info
$w$cshowsPrec_info:
_c4Zc:
	addq $72,%r12
	cmpq 856(%r13),%r12
	ja _c4Zg
_c4Zf:
	leaq p_s4T1_info(%rip),%rax
	movq %rax,-64(%r12)
	movq %rsi,-56(%r12)
	leaq -63(%r12),%rax
	cmpq $11,%r14
	jge _c4Zk
_c4Zi:
	addq $-56,%r12
	movq %rdi,%r14
	movq %rax,%rbx
	jmp p_s4T1_info
_c4Zk:
	leaq sat_s4Te_info(%rip),%rbx
	movq %rbx,-48(%r12)
	movq %rdi,-32(%r12)
	movq %rax,-24(%r12)
	leaq :_con_info(%rip),%rax
	movq %rax,-16(%r12)
	leaq shows9_closure(%rip),%rax
	movq %rax,-8(%r12)
	leaq -48(%r12),%rax
	movq %rax,(%r12)
	leaq -14(%r12),%rbx
	jmp *(%rbp)
_c4Zg:
	movq $72,904(%r13)
	leaq $w$cshowsPrec_closure(%rip),%rbx
	jmp *-8(%r13)
	.long  $w$cshowsPrec_info - $w$cshowsPrec_info_dsp



==================== Asm code ====================
.data
.align 3
.align 0
.globl $fShowD_$cshowsPrec_closure
$fShowD_$cshowsPrec_closure:
	.quad	$fShowD_$cshowsPrec_info
	.quad	0



==================== Asm code ====================
.text
.align 3
$fShowD_$cshowsPrec_info_dsp:
.align 3
	.quad	S4Zl_srt-($fShowD_$cshowsPrec_info)+24
	.quad	12884901911
	.quad	0
	.quad	12884901903
.globl $fShowD_$cshowsPrec_info
$fShowD_$cshowsPrec_info:
_c506:
	leaq -24(%rbp),%rax
	cmpq %r15,%rax
	jb _c50a
_c50b:
	leaq block_c503_info(%rip),%rax
	movq %rax,-24(%rbp)
	movq %r14,%rbx
	movq %rsi,-16(%rbp)
	movq %rdi,-8(%rbp)
	addq $-24,%rbp
	testb $7,%bl
	jne _c503
_c504:
	jmp *(%rbx)
.align 3
	.quad	S4Zl_srt-(block_c503_info)+24
	.quad	2
	.quad	4294967328
block_c503_info:
_c503:
	leaq block_c509_info(%rip),%rax
	movq %rax,(%rbp)
	movq 7(%rbx),%rax
	movq 8(%rbp),%rbx
	movq %rax,8(%rbp)
	testb $7,%bl
	jne _c509
_c50d:
	jmp *(%rbx)
_c50a:
	leaq $fShowD_$cshowsPrec_closure(%rip),%rbx
	jmp *-8(%r13)
.align 3
	.quad	S4Zl_srt-(block_c509_info)+24
	.quad	66
	.quad	4294967328
block_c509_info:
_c509:
	movq 16(%rbp),%rdi
	movq 7(%rbx),%rsi
	movq 8(%rbp),%r14
	addq $24,%rbp
	jmp $w$cshowsPrec_info
	.long  $fShowD_$cshowsPrec_info - $fShowD_$cshowsPrec_info_dsp



==================== Asm code ====================
.data
.align 3
.align 0
.globl $fShowD1_closure
$fShowD1_closure:
	.quad	$fShowD1_info
	.quad	0



==================== Asm code ====================
.text
.align 3
$fShowD1_info_dsp:
.align 3
	.quad	S4Zl_srt-($fShowD1_info)+24
	.quad	8589934607
	.quad	0
	.quad	21474836495
.globl $fShowD1_info
$fShowD1_info:
_c50z:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb _c50A
_c50B:
	leaq block_c50w_info(%rip),%rax
	movq %rax,-16(%rbp)
	movq %r14,%rbx
	movq %rsi,-8(%rbp)
	addq $-16,%rbp
	testb $7,%bl
	jne _c50w
_c50x:
	jmp *(%rbx)
_c50A:
	leaq $fShowD1_closure(%rip),%rbx
	jmp *-8(%r13)
.align 3
	.quad	S4Zl_srt-(block_c50w_info)+24
	.quad	1
	.quad	4294967328
block_c50w_info:
_c50w:
	movq 8(%rbp),%rdi
	movq 7(%rbx),%rsi
	xorl %r14d,%r14d
	addq $16,%rbp
	jmp $w$cshowsPrec_info
	.long  $fShowD1_info - $fShowD1_info_dsp



==================== Asm code ====================
.data
.align 3
.align 0
.globl $fShowD_$cshowList_closure
$fShowD_$cshowList_closure:
	.quad	$fShowD_$cshowList_info
	.quad	0



==================== Asm code ====================
.text
.align 3
$fShowD_$cshowList_info_dsp:
.align 3
	.quad	S4Zl_srt-($fShowD_$cshowList_info)+40
	.quad	8589934607
	.quad	0
	.quad	4294967311
.globl $fShowD_$cshowList_info
$fShowD_$cshowList_info:
_c50P:
	movq %rsi,%rdi
	movq %r14,%rsi
	leaq $fShowD1_closure+2(%rip),%r14
	jmp showList___info
	.long  $fShowD_$cshowList_info - $fShowD_$cshowList_info_dsp



==================== Asm code ====================
.data
.align 3
.align 0
.globl $fShowD_$cshow_closure
$fShowD_$cshow_closure:
	.quad	$fShowD_$cshow_info
	.quad	0



==================== Asm code ====================
.text
.align 3
$fShowD_$cshow_info_dsp:
.align 3
	.quad	S4Zl_srt-($fShowD_$cshow_info)+24
	.quad	4294967301
	.quad	0
	.quad	38654705679
.globl $fShowD_$cshow_info
$fShowD_$cshow_info:
_c516:
	leaq -8(%rbp),%rax
	cmpq %r15,%rax
	jb _c517
_c518:
	leaq block_c513_info(%rip),%rax
	movq %rax,-8(%rbp)
	movq %r14,%rbx
	addq $-8,%rbp
	testb $7,%bl
	jne _c513
_c514:
	jmp *(%rbx)
_c517:
	leaq $fShowD_$cshow_closure(%rip),%rbx
	jmp *-8(%r13)
.align 3
	.quad	S4Zl_srt-(block_c513_info)+24
	.quad	0
	.quad	4294967328
block_c513_info:
_c513:
	leaq []_closure+1(%rip),%rdi
	movq 7(%rbx),%rsi
	xorl %r14d,%r14d
	addq $8,%rbp
	jmp $w$cshowsPrec_info
	.long  $fShowD_$cshow_info - $fShowD_$cshow_info_dsp



==================== Asm code ====================
.data
.align 3
.align 0
.globl $fShowD_closure
$fShowD_closure:
	.quad	C:Show_static_info
	.quad	$fShowD_$cshowsPrec_closure+3
	.quad	$fShowD_$cshow_closure+1
	.quad	$fShowD_$cshowList_closure+2
	.quad	0



==================== Asm code ====================
.data
.align 3
.align 0
.globl main2_closure
main2_closure:
	.quad	main2_info
	.quad	0
	.quad	0
	.quad	0



==================== Asm code ====================
.text
.align 3
main2_info_dsp:
.align 3
	.quad	S4Zl_srt-(main2_info)+24
	.quad	0
	.quad	73014444054
.globl main2_info
main2_info:
_c51v:
	leaq -24(%rbp),%rax
	cmpq %r15,%rax
	jb _c51w
_c51x:
	movq %r13,%rdi
	movq %rbx,%rsi
	subq $8,%rsp
	xorl %eax,%eax
	call newCAF
	addq $8,%rsp
	testq %rax,%rax
	je _c51s
_c51r:
	movq stg_bh_upd_frame_info@GOTPCREL(%rip),%rbx
	movq %rbx,-16(%rbp)
	movq %rax,-8(%rbp)
	leaq block_c51t_info(%rip),%rax
	movq %rax,-24(%rbp)
	leaq main3_closure+1(%rip),%rsi
	leaq main5_closure(%rip),%r14
	addq $-24,%rbp
	jmp main_$sfoldr_info
_c51w:
	jmp *-16(%r13)
.align 3
	.quad	S4Zl_srt-(block_c51t_info)+24
	.quad	0
	.quad	4294967328
block_c51t_info:
_c51t:
	leaq []_closure+1(%rip),%rdi
	movq 7(%rbx),%rsi
	xorl %r14d,%r14d
	addq $8,%rbp
	jmp $w$cshowsPrec_info
_c51s:
	jmp *(%rbx)
	.long  main2_info - main2_info_dsp



==================== Asm code ====================
.data
.align 3
.align 0
.globl main1_closure
main1_closure:
	.quad	main1_info
	.quad	0



==================== Asm code ====================
.text
.align 3
main1_info_dsp:
.align 3
	.quad	S4Zl_srt-(main1_info)+64
	.quad	4294967299
	.quad	0
	.quad	30064771087
.globl main1_info
main1_info:
_c51M:
	leaq True_closure+2(%rip),%rdi
	leaq main2_closure(%rip),%rsi
	leaq stdout_closure(%rip),%r14
	jmp hPutStr2_info
	.long  main1_info - main1_info_dsp



==================== Asm code ====================
.data
.align 3
.align 0
.globl main_closure
main_closure:
	.quad	main_info
	.quad	0



==================== Asm code ====================
.text
.align 3
main_info_dsp:
.align 3
	.quad	S4Zl_srt-(main_info)+88
	.quad	4294967299
	.quad	0
	.quad	4294967311
.globl main_info
main_info:
_c51X:
	jmp main1_info
	.long  main_info - main_info_dsp



==================== Asm code ====================
.data
.align 3
.align 0
.globl main6_closure
main6_closure:
	.quad	main6_info
	.quad	0



==================== Asm code ====================
.text
.align 3
main6_info_dsp:
.align 3
	.quad	S4Zl_srt-(main6_info)+88
	.quad	4294967299
	.quad	0
	.quad	12884901903
.globl main6_info
main6_info:
_c528:
	leaq main1_closure+1(%rip),%r14
	jmp runMainIO1_info
	.long  main6_info - main6_info_dsp



==================== Asm code ====================
.data
.align 3
.align 0
.globl main_closure
main_closure:
	.quad	main_info
	.quad	0



==================== Asm code ====================
.text
.align 3
main_info_dsp:
.align 3
	.quad	S4Zl_srt-(main_info)+104
	.quad	4294967299
	.quad	0
	.quad	4294967311
.globl main_info
main_info:
_c52j:
	jmp main6_info
	.long  main_info - main_info_dsp



==================== Asm code ====================
.data
.align 3
.align 0
.globl D_closure
D_closure:
	.quad	D_info



==================== Asm code ====================
.text
.align 3
D_info_dsp:
.align 3
	.quad	4294967301
	.quad	0
	.quad	15
D_info:
_c52v:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja _c52z
_c52y:
	leaq D_con_info(%rip),%rax
	movq %rax,-8(%r12)
	movq %r14,(%r12)
	leaq -7(%r12),%rbx
	jmp *(%rbp)
_c52z:
	movq $16,904(%r13)
	leaq D_closure(%rip),%rbx
	jmp *-8(%r13)
	.long  D_info - D_info_dsp



==================== Asm code ====================
.const
.align 3
.align 0
i52H_str:
	.byte	109
	.byte	97
	.byte	105
	.byte	110
	.byte	58
	.byte	77
	.byte	97
	.byte	105
	.byte	110
	.byte	46
	.byte	68
	.byte	0



==================== Asm code ====================
.text
.align 3
D_con_info_dsp:
.align 3
	.quad	i52H_str-(D_con_info)+0
	.quad	1
	.quad	2
.globl D_con_info
D_con_info:
_c52F:
	incq %rbx
	jmp *(%rbp)
	.long  D_con_info - D_con_info_dsp



==================== Asm code ====================
.const
.align 3
.align 0
i52I_str:
	.byte	109
	.byte	97
	.byte	105
	.byte	110
	.byte	58
	.byte	77
	.byte	97
	.byte	105
	.byte	110
	.byte	46
	.byte	68
	.byte	0



==================== Asm code ====================
.text
.align 3
D_static_info_dsp:
.align 3
	.quad	i52I_str-(D_static_info)+0
	.quad	1
	.quad	7
.globl D_static_info
D_static_info:
_c52G:
	incq %rbx
	jmp *(%rbp)
	.long  D_static_info - D_static_info_dsp



==================== Asm code ====================
.const_data
.align 3
.align 0
S4Zl_srt:
	.quad	$fShowD2_closure
	.quad	$fShowD3_closure
	.quad	$fShowD4_closure
	.quad	$w$cshowsPrec_closure
	.quad	$fShowD_$cshowsPrec_closure
	.quad	$fShowD1_closure
	.quad	$fShowD_$cshow_closure
	.quad	main5_closure
	.quad	main2_closure
	.quad	stdout_closure
	.quad	hPutStr2_closure
	.quad	main1_closure
	.quad	runMainIO1_closure
	.quad	main6_closure


