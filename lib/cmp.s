; ==============================================================================
; Word-sized comparisons. These all clobber the accumulator & flags.
; So far none of these macros use X/Y.
; ==============================================================================

	; Equal/unequal word tests.

	.macro if_eq a_lo a_hi b_lo b_hi dest
	lda %a_lo
	cmp %b_lo
	bne skip
	lda %a_hi
	cmp %b_hi
	beq %dest
skip:
	.mend
	
	.macro if_ne a_lo a_hi b_lo b_hi dest
	lda %a_lo
	cmp %b_lo
	bne %dest
	lda %a_hi
	cmp %b_hi
	bne %dest
	.mend

	; Unsigned word comparisons.

	.macro if_gtu a_lo a_hi b_lo b_hi dest
	lda %b_hi
	cmp %a_hi
	bcc %dest
	bne skip
	lda %b_lo
	cmp %a_lo
	bcc %dest
skip:
	.mend

	.macro if_ltu a_lo a_hi b_lo b_hi dest
	@if_gtu %b_lo, %b_hi, %a_lo, %a_hi, %dest
	.mend

	.macro if_geu a_lo a_hi b_lo b_hi dest
	lda %b_hi
	cmp %a_hi
	bcc %dest
	bne skip
	lda %a_lo
	cmp %b_lo
	bcs %dest
skip:
	.mend
	
	.macro if_leu a_lo a_hi b_lo b_hi dest
	@if_geu %b_lo, %b_hi, %a_lo, %a_hi, %dest
	.mend
	
	; Signed word comparisons.
	
	.macro if_ge a_lo a_hi b_lo b_hi dest
	lda %b_lo
	cmp %a_lo
	lda %b_hi
	sbc %a_hi
	bvc skip
	eor #$80
skip:
	bpl %dest
	.mend
	
	.macro if_le a_lo a_hi b_lo b_hi dest
	@if_ge %b_lo, %b_hi, %a_lo, %a_hi, %dest
	.mend
	
	.macro if_gt a_lo a_hi b_lo b_hi dest
	lda %a_lo
	cmp %b_lo
	lda %a_hi
	sbc %b_hi
	bvc skip
	eor #$80
skip:
	bmi %dest
	.mend
	
	.macro if_lt a_lo a_hi b_lo b_hi dest
	@if_gt %b_lo, %b_hi, %a_lo, %a_hi, %dest
	.mend
	
	; Helper macros for various useful cases.
	
	.macro if_gtu_abs a b dest
	@if_gtu %a, %a+1, %b, %b+1, %dest
	.mend

	.macro if_gtu_imm a b dest
	@if_gtu %a, %a+1, {#<%b}, {#>%b}, %dest
	.mend

	.macro if_ltu_abs a b dest
	@if_ltu %a, %a+1, %b, %b+1, %dest
	.mend

	.macro if_ltu_imm a b dest
	@if_ltu %a, %a+1, {#<%b}, {#>%b}, %dest
	.mend

	.macro if_geu_abs a b dest
	@if_geu %a, %a+1, %b, %b+1, %dest
	.mend

	.macro if_geu_imm a b dest
	@if_geu %a, %a+1, {#<%b}, {#>%b}, %dest
	.mend
	
	.macro if_leu_abs a b dest
	@if_leu %a, %a+1, %b, %b+1, %dest
	.mend

	.macro if_leu_imm a b dest
	@if_leu %a, %a+1, {#<%b}, {#>%b}, %dest
	.mend

	.macro if_eq_abs a b dest
	@if_eq %a, %a+1, %b, %b+1, %dest
	.mend

	.macro if_eq_imm a b dest
	@if_eq %a, %a+1, {#<%b}, {#>%b}, %dest
	.mend
	
	.macro if_ne_abs a b dest
	@if_ne %a, %a+1, %b, %b+1, %dest
	.mend

	.macro if_ne_imm a b dest
	@if_ne %a, %a+1, {#<%b}, {#>%b}, %dest
	.mend

	.macro if_ge_abs a b dest
	@if_ge %a, %a+1, %b, %b+1, %dest
	.mend

	.macro if_ge_imm a b dest
	@if_ge %a, %a+1, {#<%b}, {#>%b}, %dest
	.mend
	
	.macro if_le_abs a b dest
	@if_le %a, %a+1, %b, %b+1, %dest
	.mend

	.macro if_le_imm a b dest
	@if_le %a, %a+1, {#<%b}, {#>%b}, %dest
	.mend
	
	.macro if_gt_abs a b dest
	@if_gt %a, %a+1, %b, %b+1, %dest
	.mend

	.macro if_gt_imm a b dest
	@if_gt %a, %a+1, {#<%b}, {#>%b}, %dest
	.mend

	.macro if_lt_abs a b dest
	@if_lt %a, %a+1, %b, %b+1, %dest
	.mend

	.macro if_lt_imm a b dest
	@if_lt %a, %a+1, {#<%b}, {#>%b}, %dest
	.mend

