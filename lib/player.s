	.alias player_base $2700
	.alias player_zp_base $50

	; player entry points. Must keep in sync with list in vgmp.s!
	.alias player_init		player_base
	.alias player_poll		player_base + 3
	.alias player_deinit		player_base + 6
	.alias player_vsync_enable	player_base + 12
	.alias player_vsync_disable	player_base + 15
	.alias player_tune_finished	player_base + 18
	.alias player_pulser		player_base + 21

	.notemps player_poll, player_vsync_disable, player_vsync_enable
	.notemps player_tune_finished
