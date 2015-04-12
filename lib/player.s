	; player entry points. Must keep in sync with list in vgmp.s!
	.alias player_init $e00
	.alias player_poll_noirq $e03
	.alias player_poll_irq $e06
	.alias player_vsync_event_enable $e09
	.alias player_vsync_event_disable $e0c
	.alias player_select_sram $e0f
	.alias player_unselect_sram $e12

	; a global variable! Use for timing. Disable interrupts to read safely!
	.alias player_frame_no $28
